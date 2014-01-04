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

let require name arg = match arg with
  | None -> failwith (Printf.sprintf "Please supply a %s argument" name)
  | Some x -> x

let apply common = ()

let status common =
  apply common;
  `Error(false, "Not implemented")

let export common volume filename =
  apply common;
  `Ok ()

let attach common filename =
  apply common;
  `Error(false, "Not implemented")

let detach common volume =
  apply common;
  `Error(false, "Not implemented")

let use common filename =
  apply common;
  `Error(false, "Not implemented")

let free common space =
  apply common;
  `Error(false, "Not implemented")

