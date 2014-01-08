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

type t = int * int * int (* major, minor, micro *)

let dot = Re_str.regexp_string "."

let of_string v =
  match Re_str.split_delim dot v with
  | [ major; minor; micro ] ->
    let major = int_of_string major in
    let minor = int_of_string minor in
    let micro = int_of_string micro in
    `Ok (major, minor, micro)
  | _ ->
    `Error (Printf.sprintf "Failed to parse version: %s" v)

let to_string (major, minor, micro) = Printf.sprintf "%d.%d.%d" major minor micro

let ( < ) (major, minor, micro) (minimum_major, minimum_minor, minimum_micro) =
    major < minimum_major
      || (major = minimum_major
          && (minor < minimum_minor
              || (minor = minimum_minor
                 && micro < minimum_micro)))
