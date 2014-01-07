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
  total_blocks: int64; (** total number of blocks on the data volume *)
  time: string;
  transaction: string;
  data_block_size: int;
  devices: Device.t list;
} with sexp

val find_device: t -> int -> Device.t option
(** [find_device t id] returns [Some device] where [device] is one of
    [t.devices] where [device.id = id], or [None] *)

val to_physical_area: t -> Allocator.t
(** [to_physical_area t] returns a representation of the physical space
    occupied by the device [t] *)

val whole_disk: t -> Allocator.t
(** [whole_disk t] returns a representation of the whole physical disk *)

val initialise: t -> t
(** [initialise t] returns an initialised [t] with the same size as [t] *)

val reserved_for_other_hosts: t -> Allocator.t option
(** [reserved_for_other_hosts t] returns the region which is reserved for
    use by other hosts. *)

val free_for_local_allocation: t -> Allocator.t
(** [free_for_local_allocation t] returns the region available for
    local allocations *)

val attach: t -> Device.t -> (t, string) Result.t
(** [attach t device] returns a new [t] with [device] added. *)

val detach: t -> int -> (t, string) Result.t
(** [detach t id] returns a new [t] with the device with [id] removed. *)

val snapshot: t -> int -> int -> (t, string) Result.t
(** [snapshot t id id'] returns [t'] where [id'] is a fully-shared snapshot
    of the device [id] *)

val allocate: t -> int64 -> ((Allocator.t * t), string) Result.t
(** [allocate t blocks]: allocates [blocks] worth of blocks from the free
    space of [t], returning [allocation, t'] *)

val free: t -> Allocator.t -> (t, string) Result.t
(** [free t allocation]: marks the [allocation] as free for local allocation *)

val of_input: int64 -> Xmlm.input -> (t, string) Result.t
(** [of_input bytes input] reads the superblock from [input] and returns
    [t] representing a physical disk of size [bytes]. *)

val to_output: t -> Xmlm.output -> unit
(** [to_output t output] writes [t] as xml to [output] *)

val make_input: Xmlm.source -> Xmlm.input
(** [make_input source] makes an input parser compatible with the 'thin_*' tools *)

val make_output: Xmlm.dest -> Xmlm.output
(** [make_output dest] makes an output printer compatible with the 'thin_*' tools *)
