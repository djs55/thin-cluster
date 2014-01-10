(*
 * Copyright (C) 2011-2013 Citrix Inc
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published
 * by the Free Software Foundation; version 2.1 only. with the special
 * exception on linking described in file LICENSE.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *)

open Common
open Cmdliner
open Dmthin
open Result

let require name arg = match arg with
  | None -> failwith (Printf.sprintf "Please supply a %s argument" name)
  | Some x -> x

let finally f g =
  try
    let x = f () in
    g ();
    x
  with e ->
    g ();
    raise e

let iso8601_of_float x =
  let time = Unix.gmtime x in
  Printf.sprintf "%04d%02d%02dT%02d:%02d:%02dZ"
    (time.Unix.tm_year+1900)
    (time.Unix.tm_mon+1)
    time.Unix.tm_mday
    time.Unix.tm_hour
    time.Unix.tm_min
    time.Unix.tm_sec

let load common =
  if common.Common.debug
  then IO.debug_output := (fun s ->
    Printf.fprintf stderr "%s %s\n%!" (iso8601_of_float (Unix.gettimeofday ())) s
  );
  Dmsetup.status common.Common.table >>= fun status ->
  Thin.dump status.Dmsetup.Status.metadata

let status common =
  match load common with
  | `Ok t ->
    let size x = Printf.sprintf "%s (%Ld blocks)" (Common.size (Int64.(mul x (of_int t.Superblock.data_block_size)))) x in
    begin match Superblock.reserved_for_other_hosts t with
    | None ->
      Printf.printf "Metadata has not been initialized: see the 'initialise' command\n"
    | Some reserved ->
      let total_disk_size = size t.Superblock.total_blocks in
      let reserved_other_hosts = size (Allocator.size reserved) in
      let local_allocation = size (Allocator.size (Superblock.free_for_local_allocation t)) in
      let total_available_volumes = List.length t.Superblock.devices - 1 in
      let available_volumes =
        List.map
          (fun device ->
            [ Printf.sprintf "Volume %d size" device.Device.id;
              size (Device.size device) ]
          ) (List.filter (fun device -> device.Device.id <> 0) t.Superblock.devices) in
      let table = [
        [ "Total disk size"; total_disk_size ];
        [ "Reserved for other hosts"; reserved_other_hosts ];
        [ "Available for local allocation"; local_allocation ];
        [ "Total available volumes"; string_of_int total_available_volumes ];
      ] @ available_volumes in
      Common.print_table [ "Key"; "Value" ] table;
    end;
    (* Sexplib.Sexp.output_hum_indent 2 stdout (Superblock.sexp_of_t t); *)
    `Ok ()
  | `Error msg -> `Error(false, msg)

let write_sexp_to filename s =
  if filename = "stdout:"
  then print_string (Sexplib.Sexp.to_string_hum s)
  else Sexplib.Sexp.save_hum filename s

let read_sexp_from filename =
  if filename = "stdin:"
  then Sexplib.Sexp.input_sexp stdin
  else Sexplib.Sexp.load_sexp filename

let export common volume filename =
  match load common with
  | `Ok t ->
    begin match Superblock.find_device t volume with
    | Some d ->
      let s = Device.sexp_of_t d in
      write_sexp_to filename s;
      `Ok ()
    | None -> `Error(false, Printf.sprintf "Failed to find volume %d" volume)
    end
  | `Error x -> `Error(false, x)

(* NB: don't run this in parallel with other operations on the same table *)
let rewrite_metadata common t =
  let open Dmsetup in
  status common.Common.table >>= fun s ->
  ( if s.Status.state <> Suspended then suspend common.Common.table else `Ok ()) >>= fun () ->
  Thin.restore t s.Status.metadata >>= fun () ->
  ( if s.Status.state = Active then resume common.Common.table else `Ok ()) >>= fun () ->
  `Ok ()

let dont_print_usage = function
  | `Ok x -> `Ok x
  | `Error x -> `Error(false, x)

let attach common filename =
  dont_print_usage (
    load common >>= fun t ->
    let s = read_sexp_from filename in
    let d = Device.t_of_sexp s in
    Superblock.attach t d >>= fun t ->
    rewrite_metadata common t
  )

let detach common volume =
  dont_print_usage (
    load common >>= fun t ->
    Superblock.detach t volume >>= fun t ->
    rewrite_metadata common t
  )

let snapshot common volume id =
  dont_print_usage (
    load common >>= fun t ->
    Superblock.snapshot t volume id >>= fun t ->
    rewrite_metadata common t
  )

let clone input output id =
  let s = read_sexp_from input in
  let d = Device.share_all_blocks (Device.t_of_sexp s) in
  let d' = { d with Device.id } in
  write_sexp_to input (Device.sexp_of_t d);
  write_sexp_to output (Device.sexp_of_t d');
  `Ok ()

let initialise common metadata data block_size low_water_mark =
  dont_print_usage (
    begin match Thin.dump metadata with
    | `Ok t ->
      if t.Superblock.devices <> [] then begin
        Printf.fprintf stderr "WARNING: multiple devices already exist in the metadata.\n";
        let rec confirm () =
          Printf.fprintf stderr "Type 'erase' if you want to erase them\n%!";
          match String.lowercase (input_line stdin) with
          | "erase" -> ()
          | _ -> confirm () in
        confirm ();
      end
    | `Error _ -> () (* good *)
    end;
    Thin.erase metadata >>= fun () ->
    ( match Block.blkgetsize data with
    | `Ok size -> `Ok size
    | `Error `Disconnected
    | `Error `Is_read_only
    | `Error `Unimplemented
    | `Error (`Unknown _) -> `Error (Printf.sprintf "BLKGETSIZE64 %s failed" data) ) >>= fun size ->
    let block_size = parse_size block_size in
    Dmsetup.create ~name:common.Common.table ~size ~metadata ~data ~block_size ~low_water_mark () >>= fun () ->
    load common >>= fun t ->
    let t = Superblock.initialise t in
    rewrite_metadata common t
  )

let use common filename =
  let s = read_sexp_from filename in
  let allocation = Allocator.t_of_sexp s in
  dont_print_usage (
    load common >>= fun t ->
    Superblock.free t allocation >>= fun t ->
    rewrite_metadata common t
  )

let free common space filename =
  let space = Common.parse_size space in
  dont_print_usage (
    load common >>= fun t ->
    let block_size = Int64.of_int t.Superblock.data_block_size in
    let required = Int64.(div (sub (add space block_size) 1L) block_size) in
    Superblock.allocate t required >>= fun (allocation, t) ->
    let s = Allocator.sexp_of_t allocation in
    write_sexp_to filename s;
    rewrite_metadata common t
  )
