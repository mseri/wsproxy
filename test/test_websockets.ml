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

open OUnit
open Wslib

module TestWsIteratee = Websockets.Wsprotocol(Test.StringMonad)
module I = Iteratees.Iteratee(Test.StringMonad)
open TestWsIteratee
open I

let teststr = "\x81\x05\x48\x65\x6c\x6c\x6f"

let get_str str =
  let ret = ref "" in
  let i = ref 6 in
  while !i < String.length str do
    for j = 0 to 1 do
      if (!i + j) < String.length str then
      begin
        i := !i + j;
        ret := Printf.sprintf "%s%c" !ret str.[!i]
      end
    done;
    i := !i + 3;
  done;
  !ret

let test_wsframe () =
  let frame = wsframe (writer Test.StringMonad.strwr "foo") in
  let x = enum_nchunk teststr 2 frame in
  assert_equal "Hello" (get_str (Test.StringMonad.getstr x))

let teststr_old = "\x00Hello\xff\x00There\xff"
let assert_ready_state state () = assert_equal state "Ready"

let test_base64encode () =
  let frame = base64encode (writer Test.StringMonad.strwr "foo") in
  let x = enum_nchunk teststr 3 frame in
  assert_ready_state (state (Test.StringMonad.getdata x)) ()

let test_wsframe_old () =
  let frame = wsframe_old (writer Test.StringMonad.strwr "foo") in
  let x = enum_nchunk teststr 3 frame in
  assert_ready_state (state (Test.StringMonad.getdata x)) ()

let test_wsunframe () =
  let frame = wsunframe (writer Test.StringMonad.strwr "foo") in
  let x = enum_nchunk teststr 3 frame in
  assert_ready_state (state (Test.StringMonad.getdata x)) ()

let test_wsunframe_old () =
  let frame = wsunframe_old (writer Test.StringMonad.strwr "foo") in
  let x = enum_nchunk teststr_old 3 frame in
  assert_ready_state (state (Test.StringMonad.getdata x)) ()

let test =
  "test_websockets" >:::
  [
    "test_base64encode" >:: test_base64encode;
    "test_wsframe" >:: test_wsframe;
    "test_wsframe_old" >:: test_wsframe_old;
    "test_wsunframe" >:: test_wsunframe;
    "test_wsunframe_old" >:: test_wsunframe_old;
  ]
