(* Copyright (c) 2013-2017 The Cervoise developers. *)
(* See the LICENSE file at the top-level directory. *)

let fmt = Printf.sprintf

let string_of_uchar c =
  let b = Buffer.create 1 in
  Uutf.Buffer.add_utf_8 b c;
  Buffer.contents b

let string_of_doc doc =
  let buf = Buffer.create 1024 in
  PPrint.ToBuffer.pretty 0.9 80 buf doc;
  Buffer.contents buf

let rec string_of_list f = function
  | [] -> ""
  | x::[] -> f x
  | x::xs -> f x ^ ", " ^ string_of_list f xs

let rec remove_last = function
  | [] | [_] -> []
  | x::xs -> x :: remove_last xs

let rec detach_last = function
  | [] -> assert false
  | [last] -> ([], last)
  | x::xs ->
      let (xs, last) = detach_last xs in
      (x :: xs, last)

let rec last = function
  | [] -> assert false
  | [last] -> last
  | _::xs -> last xs

let swap_list idx l =
  try
    let x = List.nth l idx in
    let l = List.remove_at_idx idx l in
    x :: l
  with
  | Failure _ -> raise Not_found

let mkdir name =
  let aux name =
    try Unix.mkdir name 0o750
    with Unix.Unix_error (Unix.EEXIST, _, _) -> ()
  in
  let l = remove_last (String.Split.list_cpy name ~by:Filename.dir_sep) in
  let l =
    let aux acc x =
      match acc with
      | [] -> [x]
      | y::_ ->
          Filename.concat y x :: acc
    in
    List.rev (List.fold_left aux [] l)
  in
  List.iter aux l

let combine_compare l =
  let rec aux = function
    | [] ->
        0
    | x::xs ->
        let x = x () in
        if Int.equal x 0 then
          aux xs
        else
          x
  in
  aux l

let exec_command cmd args =
  match Unix.fork () with
  | 0 ->
      begin try
        Unix.execvp cmd (Array.of_list (cmd :: args))
      with
      | _ ->
          prerr_endline (fmt "Command %s failed" cmd);
          exit 1
      end
  | -1 ->
      raise (Sys_error "Fatal error: Fork")
  | pid ->
      match Unix.waitpid [] pid with
      | (_, Unix.WEXITED code) -> code
      | (_, Unix.WSIGNALED code) -> code
      | (_, Unix.WSTOPPED code) -> code

module CCIO = struct
  include CCIO

  let with_in ?mode ?(flags=[Open_rdonly; Open_text]) filename f =
    with_in ?mode ~flags filename f

  let with_out ?mode ?(flags=[Open_wronly; Open_creat; Open_trunc; Open_text]) filename f =
    with_out ?mode ~flags filename f
end

module PPrint = struct
  include PPrint

  let check_empty f x y =
    if Equal.physical x empty then y
    else if Equal.physical y empty then x
    else f x y

  let str = string
  let (^^^) = check_empty (fun x y -> x ^^ space ^^ y)
  let (^/^) = check_empty (^/^)
  let (^//^) = check_empty (^//^)

  let parens x = group (parens x)
end
