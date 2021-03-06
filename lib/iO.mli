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

val debug_output: (string -> unit) ref
(** Called to print every debug line *)

val debug: ('a, unit, string, unit) format4 -> 'a
(** Write to the debug log *)

val run: string -> string list -> (string, string) Result.t
(** [run cmd args] returns the stdout of "run args" or raises Failure *)

val strip: string -> string
(** [strip x] returns the largest sub-string of [x] without any whitespace
    on the beginning or end. *)
