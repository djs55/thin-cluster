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

module M = Extents.Make(Int64)

include M

open Sexplib.Std

type t_ = (int64 * int64) list with sexp
let sexp_of_t t = sexp_of_t_ (to_list t)
let t_of_sexp s = of_list (t__of_sexp s)

let create start length = of_list [ start, length ]

let size = fold_left (fun acc (_, len) -> Int64.add acc len) 0L

let get_end = fold_left (fun acc (start, len) -> max acc (Int64.add start len)) 0L

let choose t length =
  let t, remaining =
    fold_left (fun (acc, length) (start, len) ->
      if length = 0L
      then (acc, length)
      else
        let available = min length len in
        (start, available) :: acc, Int64.sub length available
     ) ([], length) t in
  if remaining = 0L
  then `Ok (of_list t)
  else `Error (Printf.sprintf "Insufficient free space: tried to allocate %Ld blocks but only %Ld available"
    length (Int64.sub length remaining)
  )
