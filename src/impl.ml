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
  let ic = open_in common.metadata_input in
  finally
    (fun () ->
      let input = Superblock.make_input (`Channel ic) in
      Superblock.of_input input
    ) (fun () -> close_in ic)

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

let output_metadata common t =
  let oc = open_out common.Common.metadata_output in
  let output = Superblock.make_output (`Channel oc) in
  Superblock.to_output t output;
  close_out oc

let attach common filename =
  match load common with
  | `Ok t ->
    let s = read_sexp_from filename in
    let d = Device.t_of_sexp s in
    begin match Superblock.attach t d with
    | `Ok t ->
      output_metadata common t;
      `Ok ()
    | `Error x -> `Error(false, x)
    end
  | `Error x -> `Error(false, x)

let detach common volume =
  match load common with
  | `Ok t ->
    begin match Superblock.detach t volume with
    | `Ok t ->
      output_metadata common t;
      `Ok ()
    | `Error x -> `Error(false, x)
   end
  | `Error x -> `Error(false, x)

let snapshot common volume id =
  match load common with
  | `Ok t ->
    begin match Superblock.snapshot t volume id with
    | `Ok t ->
      output_metadata common t;
      `Ok ()
    | `Error x -> `Error(false, x)
    end
  | `Error x -> `Error(false, x)

let clone input output id =
  let s = read_sexp_from input in
  let d = Device.share_all_blocks (Device.t_of_sexp s) in
  let d' = { d with Device.id } in
  write_sexp_to input (Device.sexp_of_t d);
  write_sexp_to output (Device.sexp_of_t d');
  `Ok ()

let initialise common = match load common with
  | `Ok t ->
    if t.Superblock.devices <> [] then begin
      Printf.fprintf stderr "WARNING: multiple devices already exist in the metadata.\n";
      let rec confirm () =
        Printf.fprintf stderr "Type 'erase' if you want to erase them\n%!";
        match String.lowercase (input_line stdin) with
        | "erase" -> ()
        | _ -> confirm () in
      confirm ();
    end;
    let t = Superblock.initialise t in
    output_metadata common t;
    `Ok ()
  | `Error msg ->
    `Error(false, msg)

let use common filename =
  let s = read_sexp_from filename in
  let allocation = Allocator.t_of_sexp s in
  match load common with
  | `Ok t ->
    begin match Superblock.free t allocation with
    | `Ok t ->
      output_metadata common t;
      `Ok ()
    | `Error msg ->
      `Error(false, msg)
    end
  | `Error msg ->
    `Error(false, msg)

let free common space filename =
  let space = Common.parse_size space in
  match load common with
  | `Ok t ->
    let block_size = Int64.of_int t.Superblock.data_block_size in
    let required = Int64.(div (sub (add space block_size) 1L) block_size) in
    begin match Superblock.allocate t required with
    | `Ok (allocation, t) ->
      let s = Allocator.sexp_of_t allocation in
      write_sexp_to filename s;
      output_metadata common t;
      `Ok ()
    | `Error msg ->
      `Error(false, msg)
    end
  | `Error msg -> `Error(false, msg)
