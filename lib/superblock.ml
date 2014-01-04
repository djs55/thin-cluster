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
open Sexplib.Std

type t = {
  uuid: string;
  time: string;
  transaction: string;
  data_block_size: int;
  devices: Device.t list;
} with sexp

open Result
open Xml

let of_input input = match Xmlm.input input with
  | `Dtd _ -> begin match Xmlm.input input with
    | `El_start (("", "superblock"), attr) ->
      attribute "uuid" attr >>= fun uuid ->
      attribute "time" attr >>= fun time ->
      attribute "transaction" attr >>= fun transaction ->
      attribute "data_block_size" attr >>= fun data_block_size ->
      int data_block_size >>= fun data_block_size ->
      let rec devices acc = match Xmlm.peek input with
      | `El_end -> return (List.rev acc)
      | _ ->
        Device.of_input input >>= fun device ->
        devices (device :: acc) in
      devices [] >>= fun devices ->
      return { uuid; time; transaction; data_block_size; devices }
    | e -> fail ("expected <superblock>, got " ^ (string_of_signal e))
    end
  | e -> fail ("expected DTD, got " ^ (string_of_signal e))

let make_input x = Xmlm.make_input ~strip:true x
