(*
 * Copyright (C) 2014 Citrix Systems Inc.
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
open Sexplib.Std

type t = {
  uuid: string;
  total_blocks: int64;
  time: string;
  transaction: string;
  data_block_size: int;
  devices: Device.t list;
} with sexp

let find_device t id =
  try
    Some (List.find (fun d -> d.Device.id = id) t.devices)
  with Not_found -> None

let to_physical_area t =
  List.fold_left (fun acc device ->
    Lvm.Allocator.merge acc (Device.to_physical_area device)
  ) [] t.devices

let whole_disk t = [ "", (0L, t.total_blocks) ]

let free_for_local_allocation t = Lvm.Allocator.sub (whole_disk t) (to_physical_area t)

let reserved_for_other_hosts t = match find_device t 0 with
  | None -> None
  | Some device ->
    Some (Device.to_physical_area device)

(* Create the simplest-possible mapping which uses the allocation. This is
   used to create a mapping for the reserved device. *)
let mapping_of_allocation a =
  let open Mapping in
  let mapping next = function
  | _, (start, 1L) -> Single { origin_block = next; data_block = start }, Int64.succ next
  | _, (start, length) -> Range { origin_begin = next; data_begin = start; length }, Int64.add next length in
  let mapping, _ = List.fold_left (fun (acc, next) area ->
    let this, next = mapping next area in
    this :: acc, next
  ) ([], 0L) a in
  List.rev mapping

let allocate t blocks =
  let free = free_for_local_allocation t in
  match Lvm.Allocator.find free blocks with
  | `Ok allocation ->
    begin match find_device t 0 with
    | Some reserved_device ->
      let old_reserved_allocation = Device.to_physical_area reserved_device in
      let new_reserved_allocation = Lvm.Allocator.merge old_reserved_allocation allocation in
      let reserved_device = { reserved_device with Device.mappings = mapping_of_allocation new_reserved_allocation } in
      let devices = reserved_device :: (List.filter (fun d -> d.Device.id <> 0) t.devices) in
      `Ok (allocation, { t with devices })
    | None ->
      `Error "Unable to find the reserved device: has this volume been initialised?"
    end
  | `Error free -> `Error (Printf.sprintf "Unable to allocate %Ld blocks; only %Ld free" blocks free)

open Result
open Xml

let of_input size input = match Xmlm.input input with
  | `Dtd _ -> begin match Xmlm.input input with
    | `El_start (("", "superblock"), attr) ->
      attribute "uuid" attr >>= fun uuid ->
      attribute "time" attr >>= fun time ->
      attribute "transaction" attr >>= fun transaction ->
      attribute "data_block_size" attr >>= fun data_block_size ->
      int data_block_size >>= fun data_block_size ->
      let rec devices acc = match Xmlm.peek input with
      | `El_end -> return (List.rev acc)
      | _ ->
        Device.of_input input >>= fun device ->
        devices (device :: acc) in
      devices [] >>= fun devices ->
      let total_blocks = Int64.(div size (of_int data_block_size)) in
      return { uuid; total_blocks; time; transaction; data_block_size; devices }
    | e -> fail ("expected <superblock>, got " ^ (string_of_signal e))
    end
  | e -> fail ("expected DTD, got " ^ (string_of_signal e))

let make_input x = Xmlm.make_input ~strip:true x
