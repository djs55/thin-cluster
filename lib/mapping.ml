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

type range = {
  origin_begin: int64;
  data_begin: int64;
  length: int64;
} with sexp

type single = {
  origin_block: int64;
  data_block: int64;
} with sexp

type t =
  | Range of range
  | Single of single
with sexp

let to_physical_area = function
  | Single x -> Allocator.create x.data_block 1L
  | Range  x -> Allocator.create x.data_begin x.length

let size = function
  | Single _ -> 1L
  | Range x -> x.length

open Result
open Xml

let of_input input = match Xmlm.input input with
  | `El_start (("", "range_mapping"), attr) ->
    attribute "origin_begin" attr >>= fun origin_begin ->
    attribute "data_begin" attr >>= fun data_begin ->
    attribute "length" attr >>= fun length ->
    int64 origin_begin >>= fun origin_begin ->
    int64 data_begin >>= fun data_begin ->
    int64 length >>= fun length ->
    expect_end input >>= fun () ->
    return (Range { origin_begin; data_begin; length })
  | `El_start (("", "single_mapping"), attr) ->
    attribute "origin_block" attr >>= fun origin_block ->
    attribute "data_block" attr >>= fun data_block ->
    int64 origin_block >>= fun origin_block ->
    int64 data_block >>= fun data_block ->
    expect_end input >>= fun () ->
    return (Single { origin_block; data_block })
  | e -> fail ("expected <range_mapping> or <single_mapping>, got " ^ (string_of_signal e))

