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

open Result

let attribute name attr =
  if not(List.mem_assoc ("", name) attr)
  then fail (Printf.sprintf "Expected attribute: %s" name)
  else return (List.assoc ("", name) attr)

let int x =
  try
    return (int_of_string x)
  with _ ->
    fail (Printf.sprintf "Expected an integer: %s" x)

let int64 x =
  try
    return (Int64.of_string x)
  with _ ->
    fail (Printf.sprintf "Expected an int64: %s" x)

let string_of_name (ns, x) = if ns = "" then x else ns ^ ":" ^ x

let string_of_signal = function
| `Data x -> Printf.sprintf "`Data %s" x
| `Dtd dtd -> Printf.sprintf "`Dtd %s" (match dtd with None -> "None" | Some x -> "Some " ^ x)
| `El_end -> "`El_end"
| `El_start(name, acc) -> Printf.sprintf "`El_start %s %s" (string_of_name name) (String.concat " " (List.map (fun (name, v) -> string_of_name name ^ " = " ^ v) acc))

let expect_end input = match Xmlm.input input with
| `El_end -> return ()
| e -> fail ("expected closing tag, got " ^ (string_of_signal e))

