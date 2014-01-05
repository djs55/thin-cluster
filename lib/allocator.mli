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

module Interval = struct
  type t = int64 * int64

  let create start length = start, length

  let difference (a_start, a_length) (b_start, b_length) =
    failwith "unimplemented"

  let union (a_start, a_length) (b_start, b_length) =
    failwith "unimplemented"

  let is_empty (_, x) = x = 0L
end

type t = Interval.t list

let create start length = [ Interval.create start length ]

let difference a b =
  let one b = List.fold_left Interval.difference b a in
  List.fold_left one 
