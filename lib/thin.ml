(*
 * Copyright (C) Citrix Systems Inc.
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

let _dump = "thin_dump"
let _restore = "thin_restore"

let minimum_major = 0
let minimum_minor = 2
let minimum_micro = 8

let dot = Re_str.regexp_string "."

let check_version_exn () =
  let v = IO.run_exn _dump [ "--version" ] in
  match Re_str.split_delim dot v with
  | [ major; minor; micro ] ->
    let major = int_of_string major in
    let minor = int_of_string minor in
    let micro = int_of_string micro in
    if
    not(major > minimum_major
      || (major = minimum_major
        && (minor > minimum_minor
            || (minor = minimum_minor
               && micro > minimum_micro))))
    then failwith
      (Printf.sprintf "%s too old: %d.%d.%d < %d.%d.%d"
        _dump major minor micro minimum_major minimum_minor minimum_micro
      )
  | _ -> failwith (Printf.sprintf "Failed to parse version: \"%s\"" v)

let finally f g =
  try
    let result = f () in
    g ();
    result
  with e ->
    g ();
    raise e

let with_temp_file f =
  let tmp = Filename.temp_file "dmthin" ".xml" in
  finally (fun () -> f tmp) (fun () -> Unix.unlink tmp)

let with_ic filename f =
  let ic = open_in filename in
  finally (fun () -> f ic) (fun () -> close_in ic)

let with_oc filename f =
  let oc = open_out filename in
  finally (fun () -> f oc) (fun () -> close_out oc)

let dump filename total_size =
  try
    check_version_exn ();
    with_temp_file
      (fun tmp ->
        ignore(IO.run_exn _dump [ "-f"; "xml"; "-o"; tmp ]);
        with_ic tmp
          (fun ic ->
            let input = Superblock.make_input (`Channel ic) in
            Superblock.of_input total_size input
          )
      )
  with e -> `Error(Printexc.to_string e)

let restore metadata filename =
  try
    check_version_exn ();
    with_temp_file
      (fun tmp ->
        with_oc tmp
          (fun oc ->
            let output = Superblock.make_output (`Channel oc) in
            Superblock.to_output metadata output;
            ignore(IO.run_exn _restore [ "-i"; tmp; "-o"; filename ]);
            `Ok ()
          )
      )
  with e -> `Error(Printexc.to_string e)
