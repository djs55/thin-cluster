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

let debug_output = ref (fun _ -> ())

let debug fmt = Printf.kprintf !debug_output fmt

let iso8601_of_float x = 
  let time = Unix.gmtime x in
  Printf.sprintf "%04d%02d%02dT%02d:%02d:%02dZ"
    (time.Unix.tm_year+1900)
    (time.Unix.tm_mon+1)
    time.Unix.tm_mday
    time.Unix.tm_hour
    time.Unix.tm_min
    time.Unix.tm_sec

(* Note: when running 'Unix.create_process' from the top-level, if
   the binary cannot be executed then the exception from execvp gets
   caught by the forked toplevel, and everything blocks. Attempt to
   work around this by performing the necessary checks before we fork() *)

let colon = Re_str.regexp_string ":"

let canonicalise x =
  if (try ignore(String.index x '/'); true with Not_found -> false)
  then x
  else begin
    let paths = Re_str.split colon (Sys.getenv "PATH") in
    let first_hit = List.fold_left (fun found path -> match found with
    | Some hit -> found
    | None ->
      let possibility = Filename.concat path x in
      if (try Unix.access possibility [ Unix.X_OK ]; true with _ -> false)
      then Some possibility
      else None
    ) None paths in
    match first_hit with
    | None ->
      failwith (Printf.sprintf "Failed to find %s on $PATH ( = %s)" x (Sys.getenv "PATH"));
      x
    | Some hit -> hit
  end

let run_exn cmd args =
  let cmd = canonicalise cmd in
  debug "exec %s %s" cmd (String.concat " " args);
  let null = Unix.openfile "/dev/null" [ Unix.O_RDWR ] 0 in
  let to_close = ref [ null ] in
  let close fd =
    if List.mem fd !to_close then begin
      to_close := List.filter (fun x -> x <> fd) !to_close;
      Unix.close fd
    end in
  let close_all () = List.iter close !to_close in
  try
    let b = Buffer.create 128 in
    let tmp = String.make 4096 '\000' in
    let readable, writable = Unix.pipe () in
    to_close := readable :: writable :: !to_close;
    let pid = Unix.create_process cmd (Array.of_list (cmd :: args)) null writable null in
    close writable;
    let finished = ref false in
    while not !finished do
      let n = Unix.read readable tmp 0 (String.length tmp) in
      Buffer.add_substring b tmp 0 n;
      finished := n = 0
    done;
    close_all ();
    let _, status = Unix.waitpid [] pid in
    match status with
    | Unix.WEXITED 0 -> Buffer.contents b
    | Unix.WEXITED n ->
      failwith (Printf.sprintf "%s %s: %d (%s)" cmd (String.concat " " args) n (Buffer.contents b))
    | _ ->
      failwith (Printf.sprintf "%s %s failed" cmd (String.concat " " args))
  with e ->
    close_all ();
    raise e

let run cmd args =
  try `Ok (run_exn cmd args)
  with e -> `Error (Printf.sprintf "%s %s: %s" cmd (String.concat " " args) (Printexc.to_string e))
