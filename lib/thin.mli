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

val dump: string -> (Superblock.t, string) Result.t
(** [dump device] reads the metadata from [device] *)

val restore: Superblock.t -> string -> (unit, string) Result.t
(** [restore metadata device] writes the [metadata] to [device] *)

val erase: string -> (unit, string) Result.t
(** [erase device] wipes all metadata from [device] *)
