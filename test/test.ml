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


module NoOpMonad = struct
  type 'a t = 'a

  let return a = a
  let bind x f = f x
end

module StringMonad = struct
  type 'a t =
    { data : 'a;
      str : bytes }
  let return a = { data=a; str=Bytes.empty; }
  let bind x f =
    let newstr = f x.data in
    {newstr with str = Bytes.concat Bytes.empty [x.str; newstr.str]}

  let strwr x =
    { data=(); str=x }
  let getstr x = x.str
  let getdata x = x.data
end



