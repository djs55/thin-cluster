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
  origin_begin: int64;
  data_begin: int64;
  length: int64;
}

type single = {
  origin_block: int64;
  data_block: int64;
}

type t =
  | Range of range
  | Single of single

val of_input: Xmlm.input -> (t, string) Result.t
