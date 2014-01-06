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
  id: int;
  mapped_blocks: int64;
  transaction: string;
  creation_time: string;
  snap_time: string;
  mappings: Mappings.t;
} with sexp

let to_physical_area t =
  List.fold_left (fun acc mapping ->
    Allocator.union acc (Mapping.to_physical_area mapping)
  ) Allocator.empty t.mappings

let size t = Allocator.size (to_physical_area t)

open Result
open Xml

let of_input input = match Xmlm.input input with
  | `El_start (("", "device"), attr) ->
    attribute "dev_id" attr >>= fun id ->
    attribute "mapped_blocks" attr >>= fun mapped_blocks ->
    attribute "transaction" attr >>= fun transaction ->
    attribute "creation_time" attr >>= fun creation_time ->
    attribute "snap_time" attr >>= fun snap_time ->
    int id >>= fun id ->
    int64 mapped_blocks >>= fun mapped_blocks ->
    let rec mappings acc = match Xmlm.peek input with
    | `El_end -> return (List.rev acc)
    | _ ->
      Mapping.of_input input >>= fun mapping ->
      mappings (mapping :: acc) in
    mappings [] >>= fun mappings ->
    expect_end input >>= fun () ->
    return { id; mapped_blocks; transaction; creation_time; snap_time; mappings }
  | e -> fail ("expected <device>, got " ^ (string_of_signal e))

