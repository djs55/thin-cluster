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

open OUnit
open Dmthin

let ( |> ) a b = b a

let empty = {
  Superblock.uuid = "uuid";
  total_blocks = 1024L;
  time = "0";
  transaction = "0";
  data_block_size = 1;
  devices = [];
}

(* After initialising, there is no free space *)
let initialise_no_free_space () =
  let size = empty |> Superblock.initialise |> Superblock.free_for_local_allocation |> Allocator.size in
  assert_equal ~printer:Int64.to_string 0L size

(* After initialising, an allocation fails *)
let initialise_allocation_fails () =
  let t = Superblock.initialise empty in
  match Superblock.allocate t 1L with
  | `Ok _ -> failwith "succeeded in allocating 1 block when all were reserved"
  | `Error _ -> ()

let device = {
  Device.id = 1;
  mapped_blocks = 16L;
  transaction = "0";
  creation_time = "0";
  snap_time = "0";
  mappings = [ Mapping.( Range { origin_begin = 0L; data_begin = 0L; length = 16L; time = "0" } ) ];
  shared_blocks = Allocator.empty;
}

let fail_on_error = function
| `Ok x -> x
| `Error x -> failwith x

(* After initialising and attaching a volume, the volume and the reserved
   device have no intersection, and their sizes sum to total_blocks *)
let initialise_attach () =
  let t = Superblock.initialise empty in
  let t = fail_on_error (Superblock.attach t device) in
  match Superblock.find_device t 0 with
  | Some reserved ->
    let reserved' = Device.to_physical_area reserved in
    let device' = Device.to_physical_area device in
    let i = Allocator.intersection reserved' device' in
    let size = Allocator.size i in
    assert_equal ~printer:Int64.to_string 0L size;
    let total = Int64.add (Allocator.size reserved') (Allocator.size device') in
    assert_equal ~printer:Int64.to_string t.Superblock.total_blocks total
  | None ->
    failwith "Failed to find reserved device"

(* After initialising, attach, detach, there is no free space *)
let initialise_attach_detach () =
  let t = Superblock.initialise empty in
  let t = fail_on_error (Superblock.attach t device) in
  let t = fail_on_error (Superblock.detach t device.Device.id) in
  let size = Allocator.size (Superblock.free_for_local_allocation t) in
  assert_equal ~printer:Int64.to_string 0L size

let allocation = Allocator.of_list [ 1L, 16L ]

(* After initialising, free, there is free space *)
let initialise_free () =
  let t = Superblock.initialise empty in
  let t = fail_on_error (Superblock.free t allocation) in
  let size = Allocator.size (Superblock.free_for_local_allocation t) in
  assert_equal ~printer:Int64.to_string (Allocator.size allocation) size

(* After initialising, free, an allocation succeeds *)
let initialise_free_allocate () =
  let t = Superblock.initialise empty in
  let t = fail_on_error (Superblock.free t allocation) in
  let a, t = fail_on_error (Superblock.allocate t (Allocator.size allocation)) in
  assert_equal ~printer:Int64.to_string (Allocator.size allocation) (Allocator.size a);
  (* ... and there is no space left *)
  let size = Allocator.size (Superblock.free_for_local_allocation t) in
  assert_equal ~printer:Int64.to_string 0L size

let finally f g =
  try
    let result = f () in
    g ();
    result
  with e ->
    g ();
    raise e

let string_of_file filename =
  let b = Buffer.create 1 in
  let ic = open_in filename in
  finally
    (fun () ->
      try
        while true do
          Buffer.add_string b (input_line ic);
          Buffer.add_string b "\n"
        done; ""
      with End_of_file ->
        Buffer.contents b
    ) (fun () -> close_in ic)

let dmsetup_status_active () =
  let x = string_of_file "lib_test/dmsetup-status-active.txt" in
  let y = string_of_file "lib_test/dmsetup-table.txt" in
  let status = fail_on_error (Dmsetup.Status.of_string (x, y)) in
  assert_equal Dmsetup.Active status.Dmsetup.Status.state

let dmsetup_status_suspended () =
  let x = string_of_file "lib_test/dmsetup-status-suspended.txt" in
  let y = string_of_file "lib_test/dmsetup-table.txt" in
  let status = fail_on_error (Dmsetup.Status.of_string (x, y)) in
  assert_equal Dmsetup.Suspended status.Dmsetup.Status.state

let dmsetup_devices () =
  let x = string_of_file "lib_test/dmsetup-status-suspended.txt" in
  let y = string_of_file "lib_test/dmsetup-table.txt" in
  let status = fail_on_error (Dmsetup.Status.of_string (x, y)) in
  assert_equal "/dev/block/202:32" status.Dmsetup.Status.metadata;
  assert_equal "/dev/block/202:48" status.Dmsetup.Status.data

let dmsetup_version () =
  let x = string_of_file "lib_test/dmsetup-version.txt" in
  fail_on_error (Dmsetup.Debug.check_version_string x)

let _ =
  let verbose = ref false in
  Arg.parse [
    "-verbose", Arg.Unit (fun _ -> verbose := true), "Run in verbose mode";
  ] (fun x -> Printf.fprintf stderr "Ignoring argument: %s" x)
    "thin-cluster test suite";

  let suite = "thin-cluster" >::: [
    "after initialise, there is no free space" >:: initialise_no_free_space;
    "after initialise, allocation fails" >:: initialise_allocation_fails;
    "after initialise, attach, space is all accounted for" >:: initialise_attach;
    "after initialise, attach, detach, there is no free space" >:: initialise_attach_detach;
    "after initialise, free, there is free space" >:: initialise_free;
    "after initialise, free, allocate succeeds" >:: initialise_free_allocate;
    "parse dmsetup --version" >:: dmsetup_version;
    "parse dmsetup status active" >:: dmsetup_status_active;
    "parse dmsetup status suspended" >:: dmsetup_status_suspended;
    "parse dmsetup table" >:: dmsetup_devices;
  ] in

  run_test_tt ~verbose:!verbose suite
