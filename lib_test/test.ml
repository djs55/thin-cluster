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

(* After initialising, there is no free space *)

(* After initialising, an allocation fails *)

(* After initialising and attaching a volume, the volume and the reserved
   device have no intersection, and their sizes sum to total_blocks *)

(* After initialising, attach, detach, there is no free space *)

(* After initialising, use, there is free space *)

(* After initialising, use, an allocation succeeds *)

(* After initialising, use, free, there is no free space *)

let _ =
  let verbose = ref false in
  Arg.parse [
    "-verbose", Arg.Unit (fun _ -> verbose := true), "Run in verbose mode";
  ] (fun x -> Printf.fprintf stderr "Ignoring argument: %s" x)
    "thin-cluster test suite";

  let suite = "thin-cluster" >::: [
  ] in

  run_test_tt ~verbose:!verbose suite
