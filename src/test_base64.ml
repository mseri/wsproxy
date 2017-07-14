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
open Base64

let gen_strings n =
  let generator = QCheck.Gen.string in
  QCheck.Gen.generate n generator

let test_of_to_char () =
  String.iter (fun c -> assert_equal c (Base64.to_char (Base64.of_char c))) Base64.code

let test_strip_whitespace () =
  let str = Base64.strip_whitespace "  a\tb\nc\rx \t\ty\n\nz  " in
  assert_equal str "abcxyz"

let test_explode_implode () =
  List.iter (fun str -> assert_equal str (Base64.implode (Base64.explode str))) (gen_strings 10)

let test_encode_decode () =
  List.iter (fun str -> assert_equal str (Base64.decode (Base64.encode str))) (gen_strings 10)

let test =
  "test_base64" >:::
  [
    "test_of_to_char" >:: test_of_to_char;
    "test_strip_whitespace" >:: test_strip_whitespace;
    "test_explode_implode" >:: test_explode_implode;
    "test_encode_decode" >:: test_encode_decode;
  ]
