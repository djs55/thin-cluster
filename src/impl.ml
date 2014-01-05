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

let load common = 
  let ic = open_in common.metadata in
  finally
    (fun () ->
      let input = Superblock.make_input (`Channel ic) in
      Superblock.of_input common.total_size input
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

let export common volume filename =
  `Ok ()

let attach common filename =
  `Error(false, "Not implemented")

let detach common volume =
  `Error(false, "Not implemented")

let use common filename =
  `Error(false, "Not implemented")

let free common space =
  let space = Common.parse_size space in
  match load common with
  | `Ok t ->
    let block_size = Int64.of_int t.Superblock.data_block_size in
    let required = Int64.(div (sub (add space block_size) 1L) block_size) in
    begin match Superblock.allocate t required with
    | `Ok t ->
      print_string (Jsonrpc.to_string (Allocator.rpc_of_t t));
      `Ok ()
    | `Error msg ->
      `Error(false, msg)
    end
  | `Error msg -> `Error(false, msg)
