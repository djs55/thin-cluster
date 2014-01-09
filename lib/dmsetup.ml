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

let until_blank_line lines =
  (* the first part is separated from the second by an empty line *)
  fst (List.fold_left (fun (acc, looking) line -> match looking, line with
    | false, _ -> acc, false
    | true, (""|"\n"|"\r\n") -> acc, false
    | true, x -> x :: acc, true
  ) ([], true) lines)

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
  let lines = until_blank_line lines in
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

module Status = struct
  type t = {
    state: state;
  }

  let of_string x =
    let lines = Re_str.split_delim newline x in
    let lines = until_blank_line lines in
    let pairs = List.map to_pair lines in
    find _state pairs >>= fun x ->
    state_of_string x >>= fun state ->
    `Ok { state }
end

let status x =
  IO.run _dmsetup [ "status"; x; "-v" ] >>= fun txt ->
  Status.of_string txt

let create ~name ~size ~metadata ~data ~block_size ~low_water_mark () =
  (* block_size must be a multiple of 64 KiB *)
  let multiple = Int64.(mul 64L 1024L) in
  ( if Int64.rem block_size multiple <> 0L
    then `Error (Printf.sprintf "block size must be a multiple of 64KiB, was: %Ld" block_size)
    else `Ok () ) >>= fun () ->
  let block_size = Int64.(div block_size 512L) in
  let size = Int64.(div size 512L) in
  IO.run _dmsetup [ "create"; name; "--table";
    Printf.sprintf "0 %Ld thin-pool %s %s %Ld %Ld" size metadata data block_size low_water_mark
  ] >>= fun _ ->
  `Ok ()

let activate pool volume total_size =
  let total_size = Int64.(div total_size 512L) in
  IO.run _dmsetup [ "create"; "thin"; "--table";
    Printf.sprintf "0 %Ld thin %s %d" total_size pool volume
  ] >>= fun _ ->
  `Ok ()

module Debug = struct
  let check_version_string = check_version_string
end
