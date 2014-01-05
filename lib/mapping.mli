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

type range = {
  origin_begin: int64; (** virtual address in the thin disk *)
  data_begin: int64;   (** physical address in the real disk *)
  length: int64;       (** number of contiguous blocks *)
}

type single = {
  origin_block: int64; (** virtual address in the thin disk *)
  data_block: int64;   (** physical address in the real disk *)
}

type t =
  | Range of range
  | Single of single
with sexp

val of_input: Xmlm.input -> (t, string) Result.t

val to_physical_area: t -> Lvm.Allocator.t
(** [to_physical_area t] returns a description of the physical area
    occupied by the mapping [t] *)

val size: t -> int64
(** [size t] returns the total size of the data within [t] *)
