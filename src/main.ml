(*
 * Copyright (C) 2014 Citrix Inc
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

let project_url = "http://github.com/djs55/thin-cluster"
let version = "0.0.1"

open Common
open Cmdliner

(* Help sections common to all commands *)

let _common_options = "COMMON OPTIONS"
let help = [ 
 `S _common_options; 
 `P "These options are common to all commands.";
 `S "MORE HELP";
 `P "Use `$(mname) $(i,COMMAND) --help' for help on a single command."; `Noblank;
 `S "BUGS"; `P (Printf.sprintf "Check bug reports at %s" project_url);
]

let common_options_t = 
  let docs = _common_options in 
  let debug = 
    let doc = "Give only debug output." in
    Arg.(value & flag & info ["debug"] ~docs ~doc) in
  let verb =
    let doc = "Give verbose output." in
    let verbose = true, Arg.info ["v"; "verbose"] ~docs ~doc in 
    Arg.(last & vflag_all [false] [verbose]) in
  let dummy =
    let doc = "Use 'dummy' mode for testing." in
    Arg.(value & flag & info ["dummy"] ~docs ~doc) in
  let table =
    let doc = "Name of the device mapper table" in
    Arg.(value & opt string "thin" & info [ "table" ] ~docs ~doc) in
  Term.(pure Common.make $ debug $ verb $ dummy $ table)

let volume =
  let doc = "The volume identifier" in
  Arg.(value & pos 0 int 0 & info [] ~doc)

let output_filename =
  let doc = "The filename to write volume metadata to" in
  Arg.(value & opt string "stdout:" & info [ "output-filename" ] ~doc)

let input_filename =
  let doc = "The filename to read volume metadata from" in
  Arg.(value & opt file "stdin:" & info [ "input-filename" ] ~doc)

let size =
  let doc = "Amount of space (e.g. 1GiB)" in
  Arg.(value & opt string "0" & info [ "size" ] ~doc)

let export_cmd =
  let doc = "export volume metadata" in
  let man = [
    `S "DESCRIPTION";
    `P "Export the metadata of the specified volume for offline storage. This operation leaves the metadata intact; use the 'detach' command to remove it altogether.";
  ] @ help in
  Term.(ret(pure Impl.export $ common_options_t $ volume $ output_filename )),
  Term.info "export" ~sdocs:_common_options ~doc ~man

let attach_cmd =
  let doc = "attach an offline volume" in
  let man = [
    `S "DESCRIPTION";
    `P "Import the volume metadata to bring the volume online. Once this command returns, dmsetup may be used to instantiate a block device for this volume.";
  ] @ help in
  Term.(ret(pure Impl.attach $ common_options_t $ input_filename)),
  Term.info "attach" ~sdocs:_common_options ~doc ~man

let detach_cmd =
  let doc = "detach an online volume" in
  let man = [
    `S "DESCRIPTION";
    `P "Remove the volume metadata to take this volume online. Once this command returns, it will not be possible for dmsetup to instantiate a block device for this volume. Note: the blocks previously used by this volume are not considered free: they are added to the special volume 0, representing the fact that they are still in use elsewhere in the cluster.";
  ] @ help in
  Term.(ret(pure Impl.detach $ common_options_t $ volume)),
  Term.info "detach" ~sdocs:_common_options ~doc ~man

let id =
  let doc = "ID to use for new volume" in
  Arg.(value & opt int 1 & info [ "new-volume-id" ] ~doc)

let snapshot_cmd =
  let doc = "snapshot an online volume" in
  let man = [
    `S "DESCRIPTION";
    `P "Create a snapshot of an online volume where all the blocks are marked as shared.";
  ] @ help in
  Term.(ret(pure Impl.snapshot $ common_options_t $ volume $ id)),
  Term.info "snapshot" ~sdocs:_common_options ~doc ~man

let clone_cmd =
  let doc = "clone an offline volume" in
  let man = [
    `S "DESCRIPTION";
    `P "Create a clone of an offline volume where all the blocks are marked as shared.";
  ] @ help in
  let input =
    let doc = "Filename containing offline metadata for volume to clone" in
    Arg.(value & pos 0 file "" & info [] ~doc) in
  let output =
    let doc = "Filename to contain offline metadata for new volume" in
    Arg.(value & pos 1 string "" & info [] ~doc) in
  Term.(ret(pure Impl.clone $ input $ output $ id)),
  Term.info "clone" ~sdocs:_common_options ~doc ~man

let status_cmd =
  let doc = "display the status of the local thin pool" in
  let man = [
    `S "DESCRIPTION";
    `P "Display the state of the local thin pool, including: (1) all the attached volumes; (2) the space reserved for other hosts in the pool; and (3) the free space reserved for local allocation.";
  ] @ help in
  Term.(ret(pure Impl.status $ common_options_t)),
  Term.info "status" ~sdocs:_common_options ~doc ~man

let use_cmd =
  let doc = "use the specified blocks for local allocations" in
  let man = [
    `S "DESCRIPTION";
    `P "Use the specified blocks for local allocations. Note: it is the user's responsibility to ensure that the same blocks are only ever 'used' by one host at a time.";
  ] @ help in
  Term.(ret(pure Impl.use $ common_options_t $ input_filename)),
  Term.info "use" ~sdocs:_common_options ~doc ~man

let free_cmd =
  let doc = "free the specified amount of space" in
  let man = [
    `S "DESCRIPTION";
    `P "Allocate blocks and mark them as used, allowing the space to be 'used' by another host.";
  ] @ help in
  Term.(ret(pure Impl.free $ common_options_t $ size $ output_filename)),
  Term.info "free" ~sdocs:_common_options ~doc ~man

let initialise_cmd =
  let doc = "initialise a local metadata area" in
  let man = [
    `S "DESCRIPTION";
    `P "Initialise the local metadata area by reserving all space. The 'use' command should then be used to register some free space for local allocation.";
  ] @ help in
  
  let metadata =
    let doc = "The metadata device" in
    Arg.(value & opt (some file) None & info [ "metadata" ] ~doc) in
  let data =
    let doc = "The data device" in
    Arg.(value & opt (some file) None & info [ "data" ] ~doc) in
  let block_size =
    let doc = "The size of a block" in
    Arg.(value & opt string "64KiB" & info [ "block-size" ] ~doc) in
  let low_water_mark =
    let doc = "The low water mark in blocks" in
    Arg.(value & opt int64 0L & info [ "low-water-mark" ] ~doc) in

  Term.(ret(pure Impl.initialise $ common_options_t $ metadata $ data $ block_size $ low_water_mark)),
  Term.info "initialise" ~sdocs:_common_options ~doc ~man

let cmds = [ export_cmd; attach_cmd; detach_cmd; status_cmd;
             use_cmd; free_cmd; initialise_cmd;
             snapshot_cmd ]

let default_cmd = 
  let doc = "manipulate dmthin metadata" in
  let man = help in
  Term.(ret (pure (fun _ -> `Help (`Pager, None)) $ common_options_t)),
  Term.info (Sys.argv.(0)) ~version ~sdocs:_common_options ~doc ~man

let _ =
  match Term.eval_choice default_cmd cmds with 
  | `Error _ -> exit 1
  | _ -> exit 0
