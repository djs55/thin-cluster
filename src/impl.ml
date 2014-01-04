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
    Sexplib.Sexp.output_hum_indent 2 stdout (Superblock.sexp_of_t t);
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

