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

type t with sexp

val empty: t

val create: int64 -> int64 -> t

val difference: t -> t -> t

val union: t -> t -> t

val size: t -> int64

val to_list: t -> (int64 * int64) list

val get_end: t -> int64

val choose: t -> int64 -> (t, string) Result.t
