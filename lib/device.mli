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

type t = {
  id: int;
  mapped_blocks: int64;
  transaction: string;
  creation_time: string;
  snap_time: string;
  mappings: Mappings.t;
} with sexp

val of_input: Xmlm.input -> (t, string) Result.t

val size: t -> int64
(** [size t] returns the total size occupied by device [t] *)

val to_physical_area: t -> Lvm.Allocator.t
(** [to_physical_area t] returns a representation of the physical space
    occupied by the device [t] *)
