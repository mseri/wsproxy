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

let get_data = function
  | IE_done x -> Some x
  | IE_cont _ -> None

let test_heads () =
  let res str =
    match get_data (Test.StringMonad.getdata (enum_1chunk (Bytes.of_string "test") (heads str))) with
    | Some x -> x
    | None -> 1234
  in
  assert_equal (res @@ Bytes.of_string "t") 1;
  assert_equal (res @@ Bytes.of_string "te") 2;
  assert_equal (res @@ Bytes.of_string "x") 0

let test_drop () =
  assert_equal (Test.StringMonad.getdata (enum_1chunk (Bytes.of_string "test") (drop 1))) (IE_done ())

let test_readn () =
  let res i =
    match get_data (Test.StringMonad.getdata (enum_1chunk (Bytes.of_string "test") (readn i))) with
    | Some x -> x
    | None -> Bytes.of_string "xxxxx"
  in
  assert_equal (res 0) @@ Bytes.of_string "";
  assert_equal (res 1) @@ Bytes.of_string "t";
  assert_equal (res 2) @@ Bytes.of_string "te";
  assert_equal (res 4) @@ Bytes.of_string "test"

let test_read_int8 () =
  let res str =
    match get_data (Test.StringMonad.getdata (enum_1chunk str (read_int8))) with
    | Some x -> x
    | None -> 0
  in
  assert_equal 97 (res @@ Bytes.of_string "a");
  assert_equal 65 (res @@ Bytes.of_string "A");
  assert_equal 125 (res @@ Bytes.of_string "}")

let test_peek () =
  let res str =
    match get_data (Test.StringMonad.getdata (enum_1chunk str (peek))) with
    | Some x -> begin match x with | Some c -> c | None -> 'g' end
    | None -> 'g'
  in
  assert_equal 'a' (res @@ Bytes.of_string "abc");
  assert_equal 'x' (res @@ Bytes.of_string "xyz")

let test_head () =
  let res str =
    match get_data (Test.StringMonad.getdata (enum_1chunk str (head))) with
    | Some x -> begin match x with | Some c -> c | None -> 'g' end
    | None -> 'g'
  in
  assert_equal 'a' (res @@ Bytes.of_string "abc");
  assert_equal 'x' (res @@ Bytes.of_string "xyz")

let test_break () =
  let alter = function | '\n' -> true | _ -> false in
  let res str =
    match get_data (Test.StringMonad.getdata (enum_1chunk str (break alter))) with
    | Some x -> x
    | None -> Bytes.of_string "xxxxx"
  in
  assert_equal (Bytes.of_string "") (res @@ Bytes.of_string "\ntest");
  assert_equal (Bytes.of_string "test") (res @@ Bytes.of_string "test\nabc");
  assert_equal (Bytes.of_string "abcxyz") (res @@ Bytes.of_string "abcxyz\n")

let test =
  "test_iteratees" >:::
  [
    "test_heads" >:: test_heads;
    "test_drop" >:: test_drop;
    "test_readn" >:: test_readn;
    "test_read_int8" >:: test_read_int8;
    "test_peek" >:: test_peek;
    "test_head" >:: test_head;
    "test_break" >:: test_break;
  ]
