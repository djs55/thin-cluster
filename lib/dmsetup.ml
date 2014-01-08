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

type state = Active | Suspended

let state_of_string = function
| "ACTIVE" -> `Ok Active
| "SUSPENDED" -> `Ok Suspended
| x -> `Error("Unknown dmsetup state: " ^ x)

type status = {
  state: state;
}

let _dmsetup = "dmsetup"

open Result

let library_minimum = fail_on_error (Version.of_string "1.2.77")
let driver_minimum = fail_on_error (Version.of_string "4.25.0")

let newline = Re_str.regexp_string "\n"
let colon = Re_str.regexp_string ":"
let space = Re_str.regexp_string " "

let strip x =
  let whitespace = function
  | '\r' | '\n' | '\t' | ' ' -> true
  | _ -> false in
  let rec find i =
    if i = String.length x || not(whitespace x.[i]) then i else find (i + 1) in
  let start = find 0 in
  let rec find i =
    if i = 0 || not(whitespace x.[i]) then i else find (i - 1) in
  let last = find (String.length x - 1) in
  String.sub x start (last - start + 1)

let to_pair line = match Re_str.bounded_split_delim colon line 2 with
| [ key; value ] -> key, strip value
| _ -> failwith (Printf.sprintf "to_pair failed: %s" line)

let _library_version = "Library version"
let _driver_version = "Driver version"

let find key pairs =
  if List.mem_assoc key pairs
  then `Ok (List.assoc key pairs)
  else `Error ("Failed to find key: " ^ key)

let check_version_string v =
  let open Version in
  let lines = Re_str.split_delim newline v in
  let trim line = List.hd (Re_str.split_delim space line) in
  let pairs = List.map to_pair lines in
  find _library_version pairs >>= fun x ->
  of_string (trim x) >>= fun library_version ->
  find _driver_version pairs >>= fun x ->
  of_string (trim x) >>= fun driver_version ->
  if library_version < library_minimum
  then `Error (Printf.sprintf "Library version %s < %s" (to_string library_version) (to_string library_minimum))
  else if driver_version < driver_minimum
  then `Error (Printf.sprintf "Driver version %s < %s" (to_string driver_version) (to_string driver_minimum))
  else `Ok ()

let check_version () =
  IO.run _dmsetup [ "--version" ] >>= fun v ->
  check_version_string v

let _state = "State"

let status_of_string x =
  let lines = Re_str.split_delim newline x in
  (* the first part is separated from the second by an empty line *)
  let lines = fst (List.fold_left (fun (acc, looking) line -> match looking, line with
    | false, _ -> acc, false
    | true, (""|"\n"|"\r\n") -> acc, false
    | true, x -> x :: acc, true
  ) ([], true) lines) in
  let pairs = List.map to_pair lines in
  find _state pairs >>= fun x ->
  state_of_string x >>= fun state ->
  `Ok { state }

let status x =
  IO.run _dmsetup [ "status"; x ] >>= fun txt ->
  status_of_string txt
