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

module I = Iteratees.Iteratee(Test.StringMonad)
module TestWsIteratee = Websockets.Wsprotocol(Test.StringMonad)
open TestWsIteratee
open I

let teststr = "\x81\x05\x48\x65\x6c\x6c\x6f"
let teststr1 = teststr ^ "\x88\x00"
let it = wsunframe (writer Test.StringMonad.strwr "foo")

let assert_ready_state state () = assert_equal state "Ready"
let assert_done_state state () = assert_equal state "Done"

let test_heads () =
  assert_ready_state (state (heads teststr)) ()

let test_drop () =
  assert_ready_state (state (drop 1)) ()

let test_readn () =
  assert_ready_state (state (readn 1)) ()

let test_read_int8 () =
  assert_ready_state (state read_int8) ()

let test_read_int16 () =
  assert_ready_state (state read_int16) ()

let test_read_int32 () =
  assert_ready_state (state read_int32) ()

let test_accumulate () =
  assert_ready_state (state accumulate) ()

let test_peek () =
  assert_ready_state (state peek) ()

let test_head () =
  assert_ready_state (state head) ()

let alter = function | '\n' -> true | _ -> false

let test_break () =
  let st = break alter in
  assert_ready_state (state st) ()

let test_drop_while () =
  let st = drop_while alter in
  assert_ready_state (state st) ()

let test_apply () =
  let fun_str = function | _ -> () in
  let st = apply fun_str in
  assert_ready_state (state st) ()

let test_enum_nchunk () =
  let x = enum_nchunk teststr 3 it in
  assert_ready_state (state (Test.StringMonad.getdata x)) ()

let test_enum_1chunk () =
  let x = enum_1chunk teststr it in
  assert_ready_state (state (Test.StringMonad.getdata x)) ()

let test_enum_eof () =
  let x = Test.StringMonad.bind (enum_nchunk teststr1 3 it) enum_eof in
  assert_done_state (state (Test.StringMonad.getdata x)) ()

let test_liftI () =
  let x = liftI (enum_1chunk teststr it) in
  assert_ready_state (state x) ()

let test_extract_result_from_iteratee () =
  let x = extract_result_from_iteratee (IE_done it) in
  assert_ready_state (state x) ()

let test_take () =
  let x = take 1 it in
  assert_ready_state (state x) ()

let test_stream_printer () =
  let x = stream_printer "test" it in
  assert_ready_state (state x) ()

let test_modify () =
  let x = modify (fun s -> s) it in
  assert_ready_state (state x) ()

let test =
  "test_iteratees" >:::
  [
    "test_heads" >:: test_heads;
    "test_drop" >:: test_drop;
    "test_readn" >:: test_readn;
    "test_read_int8" >:: test_read_int8;
    "test_read_int16" >:: test_read_int16;
    "test_read_int32" >:: test_read_int32;
    "test_accumulate" >:: test_accumulate;
    "test_peek" >:: test_peek;
    "test_head" >:: test_head;
    "test_break" >:: test_break;
    "test_alter" >:: test_drop_while;
    "test_apply" >:: test_apply;
    "test_enum_nchunk" >:: test_enum_nchunk;
    "test_enum_1chunk" >:: test_enum_1chunk;
    "test_enum_eof" >:: test_enum_eof;
    "test_liftI" >:: test_liftI;
    "test_extract_result_from_iteratee " >:: test_extract_result_from_iteratee;
    "test_take" >:: test_take;
    "test_stream_printer" >:: test_stream_printer;
    "test_modify" >:: test_modify;
  ]
