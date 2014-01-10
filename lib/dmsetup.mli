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

type state = Active | Suspended

module Status : sig
  type t = {
    state: state;
    metadata: string; (** path to the metadata device *)
    data: string;     (** path to the data device *)
  }

  val of_string: string * string -> (t, string) Result.t
  (** for testing only *)
end

val status: string -> (Status.t, string) Result.t

val suspend: string -> (unit, string) Result.t
(** [suspend pool] moves the [pool] into the suspended state.
    Note this function is idempotent. *)

val resume: string -> (unit, string) Result.t
(** [resume pool] moves the [pool] into the active state.
    Note this function is idempotent. *)

val create:
  name:string             (** device mapper device name *)
  -> size:int64           (** total size of data in bytes *)
  -> metadata:string      (** device to store metadata on *)
  -> data:string          (** device to store data *)
  -> block_size:int64     (** block size on data device *)
  -> low_water_mark:int64 (** number of blocks to trigger a message *)
  -> unit -> (unit, string) Result.t

val activate: string -> int -> int64 -> (unit, string) Result.t
(** [activate pool volume size] creates a block device corresponding to
    [volume], allocated via [pool], with total [size] *)

module Debug : sig

  val check_version_string: string -> (unit, string) Result.t

end
