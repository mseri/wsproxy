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

(* Websockets protocol here *)

module Wsprotocol (IO : Iteratees.Monad) = struct

  module I = Iteratees.Iteratee(IO)
  open I

  type 'a t = 'a I.t

  let sanitize s =
    (* ignore control characters: see RFC4648.1 and RFC4648.3
     * https://tools.ietf.org/html/rfc4648#section-3
     * Note: \t = \009, \n = \012, \r = \015, \s = \032 *)
    let result = Buffer.create (String.length s) in
    for i = 0 to String.length s - 1 do
      if (String.unsafe_get s i >= '\000' && String.unsafe_get s i <= '\032')
      || String.unsafe_get s i = '\127'
      then ()
      else Buffer.add_char result (String.unsafe_get s i)
    done;
    Buffer.contents result

  let base64encode s = modify (fun x -> Bytes.unsafe_of_string @@ B64.encode @@ Bytes.unsafe_to_string x) s
  let base64decode s =
    let decode x = Bytes.unsafe_of_string @@ B64.decode @@ sanitize @@ Bytes.unsafe_to_string x in
    modify decode s

  let writer = I.writer

  let wsframe s = modify (fun s ->
      let s = Bytes.unsafe_to_string s in
      let l = String.length s in begin
      if l < 126
      then
        Printf.sprintf "%c%c%s" (char_of_int 0x82) (char_of_int l) s
      else if l < 65535
      then
        Printf.sprintf "%c%c%s%s" (char_of_int 0x82) (char_of_int 126)
          (Helpers.marshal_int16 l) s
      else
        Printf.sprintf "%c%c%s%s" (char_of_int 0x82) (char_of_int 127)
          (Helpers.marshal_int32 (Int32.of_int l)) s
      end
      |> Bytes.unsafe_of_string) s

  let wsframe_old s = modify (fun s -> Bytes.unsafe_of_string @@ Printf.sprintf "\x00%s\xff" @@ Bytes.unsafe_to_string s) s

  let rec wsunframe x =
    let read_sz =
      read_int8 >>= fun sz ->
      return (sz >= 128, sz land 0x7f)
    in
    let read_size sz =
      if sz < 126
      then return sz
      else if sz = 126 then
        read_int16
      else (* sz = 127 *)
        read_int32 >>= fun x -> return (Int32.to_int x)
    in
    let read_mask has_mask =
      if has_mask
      then readn 4
      else return (Bytes.of_string "\x00\x00\x00\x00")
    in
    let rec inner acc s =
      match s with
      | IE_cont (None, k) ->
        begin
          read_int8                    >>= fun op ->
          read_sz                      >>= fun (has_mask, sz) ->
          read_size sz                 >>= fun size ->
          read_mask has_mask           >>= fun mask ->
          readn size                   >>= fun str ->
          let real_str = Helpers.unmask (Bytes.unsafe_to_string mask) str in
          if op land 0x0f = 0x08
          then (* close frame *)
            return s
          else
          if not (op land 0x80 = 0x80)
          then begin
            inner (Bytes.concat Bytes.empty [acc; real_str]) s
          end else begin
            liftI (IO.bind (k (Iteratees.Chunk (Bytes.concat Bytes.empty [acc; real_str]))) (fun (i, _) ->
                IO.return (wsunframe i)))
          end
        end
      | _ -> return s
    in inner Bytes.empty x

  let rec wsunframe_old s =
    match s with
    | IE_cont (None, k) ->
      begin
        heads (Bytes.of_string "\x00") >>= fun _ ->
        break ((=) '\xff') >>= fun str ->
        drop 1 >>= fun () ->
        liftI (IO.bind (k (Iteratees.Chunk str)) (fun (i,_) ->
            IO.return (wsunframe_old i)))
      end
    | _ -> return s

end
