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
      Superblock.of_input input
    ) (fun () -> close_in ic)

let status common =
  match load common with
  | `Ok t ->
    let total_blocks = Int64.(div common.total_size (of_int t.Superblock.data_block_size)) in
    let size x = Printf.sprintf "%s (%Ld blocks)" (Common.size (Int64.(mul x (of_int t.Superblock.data_block_size)))) x in
    let whole_disk = [ "", (0L, total_blocks) ] in
    begin match Superblock.find_device t 0 with
    | None ->
      Printf.printf "Metadata has not been initialized: see the 'initialise' command\n"
    | Some device ->
      let total_disk_size = size total_blocks in
      let reserved = Device.to_physical_area device in
      let reserved_other_hosts = size (Lvm.Allocator.size reserved) in
      let free = Lvm.Allocator.sub whole_disk (Superblock.to_physical_area t) in
      let local_allocation = size (Lvm.Allocator.size free) in
      let total_available_volumes = List.length t.Superblock.devices - 1 in
      let available_volumes =
        List.map
          (fun device ->
            [ Printf.sprintf "Volume %d size" device.Device.id;
              size (Lvm.Allocator.size (Device.to_physical_area device)) ]
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
  `Error(false, "Not implemented")

