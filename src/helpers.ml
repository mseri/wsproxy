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

(* Basic helper functions *)

let split buf n =
  let l = Bytes.length buf in
  if n>l
  then (buf, Bytes.of_string "")
  else (Bytes.sub buf 0 n, Bytes.sub buf n (l-n))

let break pred buf =
  let l = Bytes.length buf in
  let rec inner = function
    | 0 -> None
    | n ->
      if pred (Bytes.get buf (l-n))
      then Some (split buf (l-n))
      else inner (n-1)
  in inner l

let drop_while pred buf =
  let l = Bytes.length buf in
  let rec inner = function
    | 0 -> Bytes.of_string ""
    | n ->
      if pred (Bytes.get buf (l-n))
      then inner (n-1)
      else Bytes.sub buf (l-n) n
  in inner l

let marshal_int ?(bigendian=true) n x =
  let b = Bytes.create n in
  let f =
    if bigendian
    then function x -> n-x-1
    else function x -> x
  in
  let rec inner num i =
    if i=n then b else
      let chr = Int64.logand 0xffL num in
      Bytes.set b
        (f i)
        (char_of_int (Int64.to_int chr));
      inner (Int64.shift_right_logical num 8) (i+1)
  in
  inner x 0 |> Bytes.unsafe_to_string

let marshal_int8 x = marshal_int 1 (Int64.of_int x)
let marshal_int16 x = marshal_int 2 (Int64.of_int x)
let marshal_int32 x = marshal_int 4 (Int64.of_int32 x)
let marshal_int64 x = marshal_int 8 x

let unmarshal_int ?(bigendian=true) n s =
  if String.length s < n then failwith "Invalid argument";
  let f =
    if bigendian
    then function x -> x
    else function x -> n-x-1
  in
  let rec inner acc i =
    if i=n
    then acc
    else
      let newacc = Int64.logor
          (Int64.shift_left acc 8)
          (Int64.of_int (int_of_char s.[f i]))
      in
      inner newacc (i+1)
  in inner 0L 0

let unmarshal_int8 s = Int64.to_int (unmarshal_int 1 s)
let unmarshal_int16 s = Int64.to_int (unmarshal_int 2 s)
let unmarshal_int32 s = Int64.to_int32 (unmarshal_int 4 s)
let unmarshal_int64 s = unmarshal_int 8 s

let unmask mask buf =
  if Bytes.length buf = 0 then buf
  else begin
    for i=0 to Bytes.length buf - 1 do
      let j = i mod 4 in
      let new_char =
        let buf_i = Bytes.get buf i |> int_of_char in
        let mask_j = String.get mask j |> int_of_char in
        buf_i lxor mask_j |> char_of_int
      in
      Bytes.set buf i new_char
    done;
    buf
  end
