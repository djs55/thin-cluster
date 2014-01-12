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

let magic = 27022010

cstruct superblock {
  uint32_t checksum;
  uint32_t flags;
  uint64_t this_block_number;
  uint8_t uuid[16];
  uint64_t magic;
  uint32_t version;
  uint32_t time;
  uint64_t transaction;
  uint64_t held_root;
  uint8_t space_map_root[128];
  uint8_t metadata_space_map_root[128];
  uint64_t data_mapping_root;
  uint64_t device_details_root; (* dev id -> device_details *)
  uint32_t data_block_size; (* multiples of 512 *)
  uint32_t metadata_block_size; (* multiples of 512 *)
  uint64_t metadata_blocks;
  uint32_t compat_flags;
  uint32_t compat_ro_flags;
  uint32_t incompat_flags;
} as little_endian

type superblock = {
  checksum: int32;
  flags: int32;
  this_block_number: int64;
  uuid: string;
  magic: int64;
  version: int32;
  time: int32;
  transaction: int64;
  held_root: int64;
  space_map_root: string;
  metadata_space_map_root: string;
  data_mapping_root: int64;
  device_details_root: int64;
  data_block_size: int32;
  metadata_block_size: int32;
  metadata_blocks: int64;
  compat_flags: int32;
  compat_ro_flags: int32;
  incompat_flags: int32;
} with sexp

let of_cstruct buf = {
  checksum = get_superblock_checksum buf;
  flags = get_superblock_flags buf;
  this_block_number = get_superblock_this_block_number buf;
  uuid = copy_superblock_uuid buf;
  magic = get_superblock_magic buf;
  version = get_superblock_version buf;
  time = get_superblock_time buf;
  transaction = get_superblock_transaction buf;
  held_root = get_superblock_held_root buf;
  space_map_root = copy_superblock_space_map_root buf;
  metadata_space_map_root = copy_superblock_metadata_space_map_root buf;
  data_mapping_root = get_superblock_data_mapping_root buf;
  device_details_root = get_superblock_device_details_root buf;
  data_block_size = get_superblock_data_block_size buf;
  metadata_block_size = get_superblock_metadata_block_size buf;
  metadata_blocks = get_superblock_metadata_blocks buf;
  compat_flags = get_superblock_compat_flags buf;
  compat_ro_flags = get_superblock_compat_ro_flags buf;
  incompat_flags = get_superblock_incompat_flags buf;
}

module type VALUE = sig
  type t with sexp

  val of_cstruct: Cstruct.t -> t
end

module Device_details = struct
  cstruct t {
    uint64_t mapped_blocks;
    uint64_t transaction;
    uint32_t creation_time;
    uint32_t snapshotted_time;
  } as little_endian

  type t = {
    mapped_blocks: int64;
    transaction: int64;
    creation_time: int32;
    snapshotted_time: int32;
  } with sexp

  let of_cstruct c = {
    mapped_blocks = get_t_mapped_blocks c;
    transaction = get_t_transaction c;
    creation_time = get_t_creation_time c;
    snapshotted_time = get_t_snapshotted_time c;
  }
end

module Cstruct = struct
  include Cstruct
  type t_ = string with sexp
  let sexp_of_t t = sexp_of_t_ (Cstruct.to_string t)
  let t_of_sexp s = Cstruct.of_string (t__of_sexp s)
end

module Btree_node = struct
  cstruct t {
    uint32_t checksum;
    uint32_t flags;
    uint64_t blocknr;
    uint32_t nr_entries;
    uint32_t max_entries;
    uint32_t value_size;
    uint32_t _padding;
  } as little_endian

  let _ = assert (sizeof_t mod 8 = 0)

  type contents =
  | References of (int64 * int64) array
  | Data of (int64 * Cstruct.t) array
  with sexp

  type t = {
    checksum: int32;
    blocknr: int64;
    nr_entries: int;
    max_entries: int;
    value_size: int;
    contents: contents;
  } with sexp

  let of_cstruct c =
    let checksum = get_t_checksum c in
    let flags = get_t_flags c in
    let blocknr = get_t_blocknr c in
    let nr_entries = Int32.to_int (get_t_nr_entries c) in
    let max_entries = Int32.to_int (get_t_max_entries c) in
    let value_size = Int32.to_int (get_t_value_size c) in
    let internal = Int32.(logand flags 1l = 1l) in
    let leaf = Int32.(logand flags 2l = 2l) in

    if internal && leaf || (not internal && (not leaf))
    then Printf.fprintf stderr "block %Ld is neither an internal node (%b) nor a leaf (%b)" blocknr internal leaf;

    let keys = Cstruct.shift c sizeof_t in
    let values = Cstruct.shift keys (8 * max_entries) in
    let contents =
      if leaf
      then Data (Array.init nr_entries (fun i ->
        Cstruct.LE.get_uint64 keys (i * 8),
        Cstruct.sub values (i * value_size) value_size
      ))
      else References (Array.init nr_entries (fun i ->
        Cstruct.LE.get_uint64 keys (i * 8),
        Cstruct.LE.get_uint64 values (i * 8)
      )) in
    {
      checksum; blocknr;
      nr_entries; max_entries; value_size; contents;
    }
end

type 'a contents =
| Leaf of (int64 * 'a) array
| Node of (int64 * 'a tree Lazy.t) array
and 'a tree = {
  node: Btree_node.t;
  contents: 'a contents
}
with sexp

module type DISK = sig
  val read: int64 -> Cstruct.t (* XXX: need to add an I/O type here *)
end

module Btree (D: DISK) (V: VALUE) = struct

  type t = V.t tree with sexp

  let rec of_cstruct c =
    let node = Btree_node.of_cstruct c in
    let contents = match node.Btree_node.contents with
    | Btree_node.Data x -> Leaf (Array.map (fun (k, v) -> k, V.of_cstruct v) x)
    | Btree_node.References x -> Node (Array.map (fun (k, v) ->
Printf.fprintf stderr "References %Ld -> %Ld\n%!" k v;
 k, lazy (of_cstruct (D.read v))) x) in
    { node; contents }
end

module String_value = struct
  type t = string with sexp
  let of_cstruct = Cstruct.to_string
end

module Int64_value = struct
  type t = int64 with sexp
  let of_cstruct c = Cstruct.LE.get_uint64 c 0
end

module Indirect(D: DISK)(V: VALUE) = struct
  type t = V.t with sexp
  let of_cstruct c = V.of_cstruct (D.read (Cstruct.LE.get_uint64 c 0))
end

module Block_time_value = struct
  type t = {
    time: int;
    block: int;
  } with sexp

  let of_cstruct c =
    let raw = Cstruct.LE.get_uint64 c 0 in
    let time = Int64.(to_int (logand raw (sub (shift_left 1L 25) 1L))) in
    let block = Int64.(to_int (shift_right_logical raw 24)) in
    { time; block }
end

let test device =
  let fd = Unix.openfile device [ Unix.O_RDONLY ] 0o0 in
  let ba = Bigarray.Array1.map_file fd Bigarray.char Bigarray.c_layout false 512 in
  let c = Cstruct.of_bigarray ba in
  let t = of_cstruct c in
  Printf.printf "Superblock:\n";
  Sexplib.Sexp.output_hum_indent 2 stdout (sexp_of_superblock t);
  let block_length = Int32.to_int t.metadata_block_size * 512 in
  let file_size = Int64.to_int (Unix.LargeFile.stat device).Unix.LargeFile.st_size in
  let total = Int64.to_int t.metadata_blocks * block_length in
  let fd = Unix.openfile device [ Unix.O_RDONLY ] 0o0 in
  let ba = Bigarray.Array1.map_file fd Bigarray.char Bigarray.c_layout false (min file_size total) in
  let c = Cstruct.of_bigarray ba in
  let module Disk = struct
    let read n =
      Printf.fprintf stderr "reading block %Ld\n%!" n;
      Cstruct.sub c (Int64.to_int n * block_length) block_length
  end in
  let module Device_details_tree = Btree(Disk)(Device_details) in
  let block = Disk.read t.device_details_root in
  let root = Device_details_tree.of_cstruct block in
  Printf.printf "\ndevice details root:\n";
  Sexplib.Sexp.output_hum_indent 2 stdout (Device_details_tree.sexp_of_t root);

  let block = Disk.read t.data_mapping_root in
  let module Data_mapping_tree = Btree(Disk)(Indirect(Disk)(Btree(Disk)(Block_time_value))) in
(*
  let module Data_mapping_tree = Btree(Disk)(Btree(Disk)(Block_time_value)) in
*)
  let root = Data_mapping_tree.of_cstruct block in
  Printf.printf "\ndata mapping root:\n";
  Sexplib.Sexp.output_hum_indent 2 stdout (Data_mapping_tree.sexp_of_t root)

type t = {
  uuid: string;
  total_blocks: int64;
  time: string;
  transaction: string;
  data_block_size: int;
  devices: Device.t list;
} with sexp

(* used by to_frag below *)
type node =
  | Superblock of t
  | Device of Device.t
  | Mapping of Mapping.t

let find_device t id =
  let lookup id =
    try
      Some (List.find (fun d -> d.Device.id = id) t.devices)
    with Not_found -> None in
  if id = 0
  then lookup id
  else match lookup 0, lookup id with
  | Some reserved_device, Some device ->
    (* recompute the set of shared blocks *)
    let device' = Device.to_physical_area device in
    let reserved_device' = Device.to_physical_area reserved_device in
    let shared_blocks = Allocator.intersection device' reserved_device' in
    Some { device with Device.shared_blocks }
  | _, _ -> None

let to_physical_area t =
  List.fold_left (fun acc device ->
    Allocator.union acc (Device.to_physical_area device)
  ) Allocator.empty t.devices

let whole_disk t = Allocator.create 0L t.total_blocks

let free_for_local_allocation t = Allocator.difference (whole_disk t) (to_physical_area t)

let reserved_for_other_hosts t = match find_device t 0 with
  | None -> None
  | Some device ->
    Some (Device.to_physical_area device)

let validate t =
  let e = Allocator.get_end (to_physical_area t) in
  if e > t.total_blocks
  then `Error (Printf.sprintf "Disk is too small: total_blocks = %Ld; end of allocations = %Ld" t.total_blocks e)
  else `Ok t

(* Create the simplest-possible mapping which uses the allocation. This is
   used to create a mapping for the reserved device. *)
let mapping_of_allocation a =
  let open Mapping in
  let mapping next = function
  | start, 1L -> Single { origin_block = next; data_block = start; time = "0" }, Int64.succ next
  | start, length -> Range { origin_begin = next; data_begin = start; length; time = "0" }, Int64.add next length in
  let mapping, _ = List.fold_left (fun (acc, next) area ->
    let this, next = mapping next area in
    this :: acc, next
  ) ([], 0L) (Allocator.to_list a) in
  List.rev mapping

let initialise t =
  let reserved_device = {
    Device.id = 0;
    mapped_blocks = t.total_blocks;
    transaction = "0";
    creation_time = "0";
    snap_time = "0";
    mappings = mapping_of_allocation (whole_disk t);
    shared_blocks = Allocator.empty;
  } in
  { t with devices = [ reserved_device ] }

let update_reserved_device t f = match find_device t 0 with
  | Some reserved_device ->
    let old_size = Device.size reserved_device in
    let reserved_device = f reserved_device in
    let new_size = Device.size reserved_device in
    let mapped_blocks = Int64.(add reserved_device.Device.mapped_blocks (sub new_size old_size)) in
    let reserved_device = { reserved_device with Device.mapped_blocks } in
    let devices = reserved_device :: (List.filter (fun d -> d.Device.id <> 0) t.devices) in
    let t = { t with devices } in
    validate t
  | None ->
    `Error "Unable to find the reserved device: has this volume been initialised?"

let attach t d = match find_device t d.Device.id with
  | Some _ -> `Error (Printf.sprintf "device with id = %d already exists" d.Device.id)
  | None ->
    (* We only remove the private blocks from the reserved map *)
    let allocation = Device.to_private_allocation d in
    begin match update_reserved_device t
      (fun reserved_device ->
        let old_reserved_allocation = Device.to_physical_area reserved_device in
        let new_reserved_allocation = Allocator.difference old_reserved_allocation allocation in
        { reserved_device with Device.mappings = mapping_of_allocation new_reserved_allocation }) with
    | `Ok t ->  `Ok { t with devices = d :: t.devices }
    | `Error x -> `Error x
    end

let detach t id = match find_device t id with
  | None -> `Error (Printf.sprintf "device with id = %d does not exist" id)
  | Some d ->
    let allocation = Device.to_physical_area d in
    begin match update_reserved_device t
      (fun reserved_device ->
        let old_reserved_allocation = Device.to_physical_area reserved_device in
        let new_reserved_allocation = Allocator.union old_reserved_allocation allocation in
        { reserved_device with Device.mappings = mapping_of_allocation new_reserved_allocation }) with
    | `Ok t ->  `Ok { t with devices = List.filter (fun d -> d.Device.id <> id) t.devices }
    | `Error x -> `Error x
    end

let snapshot t id id' = match find_device t id with
  | None -> `Error (Printf.sprintf "device with id = %d does not exist" id)
  | Some d ->
    begin match find_device t id' with
    | Some _ -> `Error (Printf.sprintf "device with id = %d already exists" id')
    | None ->
      (* We don't need to recompute block sharing here, because find_device will do it *)
      let d' = { d with Device.id = id' } in
      let allocation = Device.to_physical_area d in
      begin match update_reserved_device t
        (fun reserved_device ->
          let old_reserved_allocation = Device.to_physical_area reserved_device in
          let new_reserved_allocation = Allocator.union old_reserved_allocation allocation in
          { reserved_device with Device.mappings = mapping_of_allocation new_reserved_allocation }) with
      | `Ok t ->  `Ok { t with devices = d' :: t.devices }
      | `Error x -> `Error x
      end
    end

let allocate t blocks =
  let free = free_for_local_allocation t in
  match Allocator.choose free blocks with
  | `Ok allocation ->
    begin match update_reserved_device t
      (fun reserved_device ->
        let old_reserved_allocation = Device.to_physical_area reserved_device in
        let new_reserved_allocation = Allocator.union old_reserved_allocation allocation in
        { reserved_device with Device.mappings = mapping_of_allocation new_reserved_allocation }) with
    | `Ok t ->  `Ok (allocation, t)
    | `Error x -> `Error x
    end
  | `Error x -> `Error x

let free t allocation =
  update_reserved_device t
    (fun reserved_device ->
      let old_reserved_allocation = Device.to_physical_area reserved_device in
      let new_reserved_allocation = Allocator.difference old_reserved_allocation allocation in
      { reserved_device with Device.mappings = mapping_of_allocation new_reserved_allocation })

open Result
open Xml

let of_input input = match Xmlm.input input with
  | `Dtd _ -> begin match Xmlm.input input with
    | `El_start (("", "superblock"), attr) ->
      attribute "uuid" attr >>= fun uuid ->
      attribute "time" attr >>= fun time ->
      attribute "transaction" attr >>= fun transaction ->
      attribute "data_block_size" attr >>= fun data_block_size ->
      attribute "nr_data_blocks" attr >>= fun total_blocks ->
      int data_block_size >>= fun data_block_size ->
      int64 total_blocks >>= fun total_blocks ->
      let rec devices acc = match Xmlm.peek input with
      | `El_end -> return (List.rev acc)
      | _ ->
        Device.of_input input >>= fun device ->
        devices (device :: acc) in
      devices [] >>= fun devices ->
      let t = { uuid; total_blocks; time; transaction; data_block_size; devices } in
      validate t
    | e -> fail ("expected <superblock>, got " ^ (string_of_signal e))
    end
  | e -> fail ("expected DTD, got " ^ (string_of_signal e))

let make_input x = Xmlm.make_input ~strip:true x

let make_output x = Xmlm.make_output ~decl:false ~indent:(Some 2) x

let to_frag = function
  | Superblock t ->
    let attributes = [
      ("", "uuid"), t.uuid;
      ("", "time"), t.time;
      ("", "transaction"), t.transaction;
      ("", "data_block_size"), string_of_int t.data_block_size;
      ("", "nr_data_blocks"), Int64.to_string t.total_blocks;
    ] in
    let tag = (("", "superblock"), attributes) in
    `El (tag, List.map (fun x -> Device x) t.devices)
  | Device t ->
    let open Device in
    let attributes = [
      ("", "dev_id"), string_of_int t.id;
      ("", "mapped_blocks"), Int64.to_string t.mapped_blocks;
      ("", "transaction"), t.transaction;
      ("", "creation_time"), t.creation_time;
      ("", "snap_time"), t.snap_time
    ] in
    let tag = (("", "device"), attributes) in
    `El (tag, List.map (fun x -> Mapping x) t.mappings)
  | Mapping (Mapping.Range x) ->
    let open Mapping in
    let attributes = [
      ("", "origin_begin"), Int64.to_string x.origin_begin;
      ("", "data_begin"), Int64.to_string x.data_begin;
      ("", "length"), Int64.to_string x.length;
      ("", "time"), x.time;
    ] in
    let tag = (("", "range_mapping"), attributes) in
    `El (tag, [])
  | Mapping (Mapping.Single x) ->
    let open Mapping in
    let attributes = [
      ("", "origin_block"), Int64.to_string x.origin_block;
      ("", "data_block"), Int64.to_string x.data_block;
      ("", "time"), x.time
    ] in
    let tag = (("", "single_mapping"), attributes) in
    `El (tag, [])

let to_output t output = Xmlm.output_doc_tree to_frag output (None, Superblock t)
