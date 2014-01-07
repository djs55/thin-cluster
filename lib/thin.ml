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

let minimum_version = [ 0; 2; 8 ]

let check_version () = failwith "too old"

let dump filename =
  check_version ();
  failwith "unimplemented"

let restore metadata filename =
  check_version ();
  failwith "unimplemented"
