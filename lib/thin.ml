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

let minimum = Result.fail_on_error (Version.of_string "0.2.8")

open Result

let check_version () =
  let open Version in
  IO.run _dump [ "--version" ] >>= fun x ->
  of_string x >>= fun v ->
  if v < minimum
  then `Error (Printf.sprintf "%s too old: (minimum is %s)" (to_string v) (to_string minimum))
  else `Ok ()

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
    check_version () >>= fun () ->
    with_temp_file
      (fun tmp ->
        let (_: string) = fail_on_error (IO.run _dump [ "-f"; "xml"; "-o"; tmp ]) in
        with_ic tmp
          (fun ic ->
            let input = Superblock.make_input (`Channel ic) in
            Superblock.of_input total_size input
          )
      )
  with e -> `Error(Printexc.to_string e)

let restore metadata filename =
  try
    check_version () >>= fun () ->
    with_temp_file
      (fun tmp ->
        with_oc tmp
          (fun oc ->
            let output = Superblock.make_output (`Channel oc) in
            Superblock.to_output metadata output;
            IO.run _restore [ "-i"; tmp; "-o"; filename ] >>= fun _ ->
            `Ok ()
          )
      )
  with e -> `Error(Printexc.to_string e)
