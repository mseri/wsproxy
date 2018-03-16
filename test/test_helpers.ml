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

let gen_nums n generator =
  QCheck.Gen.generate n generator

let test_split () =
  let (a, b) = Helpers.split (Bytes.of_string "helper") 2 in
  assert_equal a @@ Bytes.of_string "he";
  assert_equal b @@ Bytes.of_string "lper"

let test_break () =
  let pred = function | 'x' -> true | _ -> false in
  let none = Helpers.break pred @@ Bytes.of_string "helper" in
  assert_equal none None;
  match Helpers.break pred @@ Bytes.of_string "helxper" with
  | Some(a,b) ->
    assert_equal a @@ Bytes.of_string "hel";
    assert_equal b @@ Bytes.of_string "xper"
  | None -> failwith "Expected some, found none"

let test_str_drop_while () =
  let pred = function | 'x' -> true | _ -> false in
  let a = Helpers.drop_while pred @@ Bytes.of_string "helper" in
  assert_equal a @@ Bytes.of_string "helper";
  let b = Helpers.drop_while pred @@ Bytes.of_string "xhelper" in
  assert_equal b @@ Bytes.of_string "helper"

let test_marshal_unmarshal_int () =
  let generator = QCheck.Gen.ui64 in
  let nums = gen_nums 10 generator in
  List.iter (fun i ->
    assert_equal i (Helpers.unmarshal_int 8 (Helpers.marshal_int 8 i))
  ) nums

let test_marshal_unmarshal_int8 () =
  let generator = QCheck.Gen.int_bound 255 in
  let nums = gen_nums 10 generator in
  List.iter (fun i ->
    assert_equal i (Helpers.unmarshal_int8 (Helpers.marshal_int8 i))
  ) nums

let test_marshal_unmarshal_int16 () =
  let generator = QCheck.Gen.int_bound 65535 in
  let nums = gen_nums 10 generator in
  List.iter (fun i ->
    assert_equal i (Helpers.unmarshal_int16 (Helpers.marshal_int16 i))
  ) nums

let test_marshal_unmarshal_int32 () =
  let generator = QCheck.Gen.ui32 in
  let nums = gen_nums 10 generator in
  List.iter (fun i ->
    assert_equal i (Helpers.unmarshal_int32 (Helpers.marshal_int32 i))
  ) nums

let test_marshal_unmarshal_int64 () =
  let generator = QCheck.Gen.ui64 in
  let nums = gen_nums 10 generator in
  List.iter (fun i ->
    assert_equal i (Helpers.unmarshal_int64 (Helpers.marshal_int64 i))
  ) nums

let test_unmask () =
  let a = Helpers.unmask "01010101" @@ Bytes.of_string "\x01\x01\x01\x01\x01\x01\x01\x01" in
  assert_equal a @@ Bytes.of_string "10101010"

let test =
  "test_helpers" >:::
  [
    "test_split" >:: test_split;
    "test_break" >:: test_break;
    "test_str_drop_while" >:: test_str_drop_while;
    "test_marshal_unmarshal_int" >:: test_marshal_unmarshal_int;
    "test_marshal_unmarshal_int8" >:: test_marshal_unmarshal_int8;
    "test_marshal_unmarshal_int16" >:: test_marshal_unmarshal_int16;
    "test_marshal_unmarshal_int32" >:: test_marshal_unmarshal_int32;
    "test_marshal_unmarshal_int64" >:: test_marshal_unmarshal_int64;
    "test_unmask" >:: test_unmask;
  ]
