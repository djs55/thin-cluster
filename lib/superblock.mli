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
  uuid: string;
  time: string;
  transaction: string;
  data_block_size: int;
  devices: Device.t list;
} with sexp

val find_device: t -> int -> Device.t option
(** [find_device t id] returns [Some device] where [device] is one of
    [t.devices] where [device.id = id], or [None] *)

val to_physical_area: t -> Lvm.Allocator.t
(** [to_physical_area t] returns a representation of the physical space
    occupied by the device [t] *)

val of_input: Xmlm.input -> (t, string) Result.t

val make_input: Xmlm.source -> Xmlm.input
