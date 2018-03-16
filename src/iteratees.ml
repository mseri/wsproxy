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

open Helpers

type err = string

type stream =
  | Eof of err option
  | Chunk of bytes

let string_of_stream = function
  | Eof (Some x) -> Printf.sprintf "Eof (Some '%s')" x
  | Eof None -> "Eof None"
  | Chunk b -> Printf.sprintf "Chunk '%s'" (Bytes.to_string b)


module type Monad = sig
  type 'a t

  val return : 'a -> 'a t
  val bind : 'a t -> ('a -> 'b t) -> 'b t
end

module Iteratee (IO : Monad) = struct
  type 'a t =
    | IE_done of 'a
    | IE_cont of err option * (stream -> ('a t * stream) IO.t)

  let return x = IE_done x

  let rec bind i f =
    match i with
    | IE_done result -> f result
    | IE_cont (e, k) ->
      let docase = function
        | (IE_done x, stream) ->
          begin match f x with
            | IE_cont (None, k) -> k stream
            | x -> IO.return (x, stream)
          end
        | (x, stream) ->
          IO.return (bind x f, stream)
      in
      IE_cont (e, fun s -> IO.bind (k s) docase)

  let (>>=) = bind

  let ie_contM k x = IO.return (IE_cont (None,k), x)
  let ie_doneM res x = IO.return (IE_done res, x)
  let ie_errM msg k x = IO.return (IE_cont (Some msg, k), x)

  let state = function
    | IE_done _           -> "Done"
    | IE_cont (None,_)    -> "Ready"
    | IE_cont (Some e, _) -> Printf.sprintf "Error (%s)" e

  (* Simplest iteratees *)

  let rec peek =
    let step st =
      match st with
      | Chunk b ->
        if Bytes.length b = 0
        then IO.return (peek, st)
        else IO.return (IE_done (Some (Bytes.get b 0)), st)
      | _ -> IO.return (IE_done None, st)
    in
    IE_cont (None, step)

  let rec head =
    let rec step st =
      match st with
      | Chunk b ->
        let len = Bytes.length b in
        if len = 0
        then IO.return (head, st)
        else IO.return (IE_done (Some (Bytes.get b 0)), Chunk (Bytes.sub b 1 (len - 1)))
      | _ -> IO.return (IE_cont ((Some "Eof"),step), st)
    in
    IE_cont (None, step)

  let writer really_write _ =
    let rec step st =
      match st with
      | Chunk s ->
        IO.bind (really_write s)
          (fun () -> IO.return (IE_cont (None, step), Chunk Bytes.empty))
      | Eof _ ->
        IO.return (IE_done (), st)
    in
    IE_cont (None, step)

  (* More complex one *)

  let break pred =
    let rec step before st =
      match st with
      | Chunk b when (Bytes.unsafe_to_string b) = "" -> ie_contM (step before) st
      | Chunk b ->
        begin
          match break pred b with
          | None -> ie_contM (step (Bytes.concat Bytes.empty [before; b])) (Chunk Bytes.empty)
          | Some(str,tail) -> ie_doneM (Bytes.concat Bytes.empty [before; str]) (Chunk tail)
        end
      | _ -> IO.return (IE_done before, st)
    in IE_cont (None, step Bytes.empty)

  let heads buf =
    let rec step cnt buf stream =
      match (stream,buf) with
      | (_, e) when (Bytes.unsafe_to_string e = "") ->
        IO.return (IE_done cnt, stream)
      | Eof _, _ ->
        IO.return (IE_done cnt, stream)
      | Chunk b, buf ->
        if Bytes.length b = 0
        then IO.return (IE_cont (None, step 0 buf), stream)
        else
        if (Bytes.get b 0) = (Bytes.get buf 0)
        then let (_, tl) = split buf 1 in step (cnt+1) tl (Chunk (snd (split b 1)))
        else IO.return (IE_done cnt, stream)
    in
    IE_cont (None, step 0 buf)

  let drop = function
    | 0 -> IE_done ()
    | n -> begin
        let rec step n st = match st with
          | Chunk b ->
            let len = Bytes.length b in
            if len < n
            then ie_contM (step (n-len)) (Chunk Bytes.empty)
            else ie_doneM () (Chunk (Bytes.sub b n (len-n)))
          | Eof _ -> ie_doneM () st
        in IE_cont (None, step n)
      end

  let readn = function
    | 0 -> IE_done Bytes.empty
    | n -> begin
        let rec step acc n st =
          match st with
          | Chunk b ->
            let len = Bytes.length b in
            if len < n
            then ie_contM (step (Bytes.concat Bytes.empty [acc; b]) (n-len)) (Chunk Bytes.empty)
            else
              let (s1,s2) = split b n in
              ie_doneM (Bytes.concat Bytes.empty [acc; s1]) (Chunk s2)
          | Eof _ -> ie_errM "EOF" (step acc n) st
        in IE_cont (None, step Bytes.empty n)
      end

  let read_int8 = readn 1 >>= (fun s -> return (unmarshal_int8 @@ Bytes.unsafe_to_string s))
  let read_int16 = readn 2 >>= (fun s -> return (unmarshal_int16 @@ Bytes.unsafe_to_string s))
  let read_int32 = readn 4 >>= (fun s -> return (unmarshal_int32 @@ Bytes.unsafe_to_string s))

  let drop_while pred =
    let rec step st = match st with
      | Chunk b ->
        let news = drop_while pred b in
        if news=Bytes.empty
        then ie_contM step (Chunk Bytes.empty)
        else ie_doneM () (Chunk news)
      | Eof _ ->
        ie_doneM () st
    in
    IE_cont (None, step)

  let accumulate =
    let rec step acc st = match st with
      | Chunk b ->
        ie_contM (step (Bytes.concat Bytes.empty [acc; b])) (Chunk Bytes.empty)
      | Eof _ -> ie_doneM acc st
    in IE_cont (None, step Bytes.empty)

  let apply f =
    let rec step st = match st with
      | Chunk s ->
        f s;
        ie_contM step (Chunk Bytes.empty)
      | Eof _ -> ie_doneM () st
    in IE_cont (None, step)

  let liftI m =
    let step st i =
      match i with
      | IE_cont (None, k) -> k st
      | IE_cont (Some _, _) | IE_done _ -> IO.return (i,st)
    in
    IE_cont (None, fun s -> IO.bind m (step s))



  (* ****************************** ENUMERATORS *********************************)

  type 'a enumerator = 'a t -> ('a t) IO.t

  (* Simplest enumarator *)

  let enum_eof i =
    let result =
      match i with
      | IE_cont (None, f) -> IO.bind (f (Eof None)) (fun x -> IO.return (fst x))
      | _ -> IO.return i
    in
    IO.bind result (function
        | IE_done _ -> result
        | IE_cont (Some _, _) -> result
        | _ -> failwith "Divergent Iteratee")

  let enum_1chunk str = function
    | IE_cont (None, f) -> IO.bind (f (Chunk str)) (fun x -> IO.return (fst x))
    | x -> IO.return x

  let rec enum_nchunk buf n =
    if buf=Bytes.empty then (fun x -> IO.return x) else
      let (str1,str2) = split buf n in
      function
      | IE_cont (None, f) ->
        IO.bind (IO.bind (f (Chunk str1))
                   (fun x -> IO.return (fst x))) (enum_nchunk str2 n)
      | x -> IO.return x

  let extract_result_from_iteratee = function
    | IE_done x -> x
    | _ -> failwith "Not done!"

  type 'a enumeratee = 'a t -> ('a t) t

  let rec take =
    let step n k s =
      match s with
      | Chunk buf ->
        let len = Bytes.length buf in
        if len < n
        then
          IO.bind (k s) (fun (i, _) ->
              IO.return (take (n-len) i, Chunk Bytes.empty))
        else
          let (str1,str2) = split buf n in
          IO.bind (k (Chunk str1)) (fun (i,_) ->
              IO.return (IE_done i, Chunk str2))
      | Eof _ ->
        IO.bind (k s) (fun (i, _) -> IO.return (IE_done i, s))
    in
    function
    | 0 -> return
    | n ->
      fun s -> match s with
        | IE_cont (None,k) -> IE_cont (None, (step n k))
        | IE_cont (Some _, _)
        | IE_done _ -> bind (drop n) (fun () -> return s)

  let stream_printer name =
    let rec step k s =
      Printf.printf "%s: %s\n" name (string_of_stream s);
      IO.bind (k s) (fun i ->
          match i with
          | (IE_cont (None, f), s) -> IO.return (IE_cont (None, step f), s)
          | (IE_cont (err, f), s) -> IO.return (IE_cont (err, step f), s)
          | (i, s) -> IO.return (IE_done i, s))
    in fun s -> match s with
      | IE_cont (None,k) -> IE_cont (None, (step k))
      | IE_cont (Some _, _)
      | IE_done _ -> return s

  let modify f =
    let rec step k s =
      match s with
      | Chunk c ->
        let s = try f c with e -> Printf.printf "got exception %s\n%!" (Printexc.to_string e); raise e in
        IO.bind (k (Chunk s)) (fun i ->
            match i with
            | (IE_cont (None, f), s) -> IO.return (IE_cont (None, step f), s)
            | (IE_cont (err, f), s) -> IO.return (IE_cont (err, step f), s)
            | (i, s) -> IO.return (IE_done i, s))
      | Eof _ ->
        IO.bind (k s) (fun (i,_) -> IO.return (IE_done i, s))
    in fun s -> match s with
      | IE_cont (None, k) ->
        IE_cont (None, step k)
      | IE_cont (Some _, _) ->
        return s
      | IE_done _ -> return s

  type 'a either = Left of 'a | Right of 'a

  let read_lines =
    let (>>=) = bind in
    let iscrlf = function | '\r' | '\n' -> true | _ -> false in
    let terminators = heads (Bytes.of_string "\r\n") >>= function | 0 -> heads (Bytes.of_string "\n") | n -> return n in
    let rec lines' acc = break iscrlf >>= fun l -> terminators >>= check acc l
    and check acc l n =
      match (l,n) with
      | (_,0)  -> return (Left (List.rev acc))
      | (e,_) when (Bytes.unsafe_to_string e = "") -> return (Right (List.rev acc))
      | (l,_)  -> lines' (l::acc)
    in
    lines' []

end


















