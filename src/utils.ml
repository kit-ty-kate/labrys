(* Copyright (c) 2013-2017 The Cervoise developers. *)
(* See the LICENSE file at the top-level directory. *)

open Containers
open Monomorphic.None

let fmt = Printf.sprintf

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

module StrListSet = Set.Make(struct
    type t = string list

    let compare = List.compare String.compare
  end)

module type EQ = sig
  type t

  val equal : t -> t -> bool
end

module type EQSET = sig
  type elt
  type t

  val empty : t
  val mem : elt -> t -> bool
  val fold : (elt -> 'b -> 'b) -> t -> 'b -> 'b
  val add : elt -> t -> t
  val singleton : elt -> t
  val of_list : elt list -> t
  val remove : elt -> t -> t
  val is_empty : t -> bool
  val cardinal : t -> int
  val map : (elt -> elt) -> t -> t
  val for_all : (elt -> bool) -> t -> bool
  val equal : t -> t -> bool
  val subset : t -> t -> bool
  val union : t -> t -> t
  val union3 : t -> t -> t -> t
end

module EqSet (I : EQ) = struct
  type elt = I.t
  type t = elt list

  let empty = []

  let mem x self = List.exists (I.equal x) self

  let fold f self acc = List.fold_left (fun acc x -> f x acc) acc self

  let add x self =
    let self = List.filter (fun y -> not (I.equal x y)) self in
    x :: self

  let singleton x = [x]

  let of_list l =
    List.fold_right add l empty

  let remove x self =
    let rec aux = function
      | [] ->
          []
      | y::self ->
          if I.equal x y then
            self
          else
            y :: aux self
    in
    aux self

  let is_empty = List.is_empty

  let cardinal = List.length

  let map f self =
    fold (fun x -> add (f x)) self empty

  let for_all = List.for_all

  let equal l l' =
    Int.equal (List.length l) (List.length l')
    && List.for_all (fun x -> mem x l') l

  let subset l l' = List.for_all (fun x -> mem x l') l

  let union l l' = fold add l l'
  let union3 l l' l'' = union (union l l') l''
end

module type EQMAP = sig
  type key
  type 'a t

  module Set : EQSET with type elt = key

  val empty : 'a t
  val is_empty : 'a t -> bool
  val mem : key -> 'a t -> bool
  val fold : (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
  val add : key -> 'a -> 'a t -> 'a t
  val find : key -> 'a t -> 'a option
  val modify_def : 'a -> key -> ('a -> 'a) -> 'a t -> 'a t
  val bindings : 'a t -> (key * 'a) list
  val singleton : key -> 'a -> 'a t
  val equal : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool
  val filter : (key -> 'a -> bool) -> 'a t -> 'a t
  val remove : key -> 'a t -> 'a t
  val map_keys : (key -> key) -> 'a t -> 'a t
  val find_binding : key -> 'a t -> (key * 'a) option
  val merge : 'a t -> 'a t -> 'a t
  val iter : (key -> 'a -> unit) -> 'a t -> unit
  val to_set : _ t -> Set.t
end

module EqMap (I : EQ) = struct
  type key = I.t
  type 'a t = (key * 'a) list

  module Set = EqSet(I)

  let empty = []

  let is_empty = List.is_empty

  let mem x self = List.exists (fun (y, _) -> I.equal x y) self

  let fold f self acc = List.fold_left (fun acc (k, x) -> f k x acc) acc self

  let add k x self =
    let self = List.filter (fun (y, _) -> not (I.equal k y)) self in
    (k, x) :: self

  let find_binding x self =
    List.find_pred (fun (y, _) -> I.equal x y) self

  let find x self =
    Option.map snd (find_binding x self)

  let modify_def x k f self =
    match find k self with
    | Some x -> add k (f x) self
    | None -> add k (f x) self

  let bindings = Fun.id

  let singleton k x = [(k, x)]

  let equal f l l' =
    let eq (k, x) (k', y) = I.equal k k' && f x y in
    Int.equal (List.length l) (List.length l')
    && List.for_all (fun x -> List.exists (fun y -> eq x y) l') l

  let filter f self = List.filter (fun (k, x) -> f k x) self

  let remove k self =
    let rec aux = function
      | [] ->
          []
      | (k', x)::self ->
          if I.equal k k' then
            self
          else
            (k', x) :: aux self
    in
    aux self

  let map_keys f self =
    fold (fun k -> add (f k)) self empty

  let merge self self' = fold add self self'

  let iter f self =
    List.iter (fun (k, x) -> f k x) self

  let to_set self =
    List.map (fun (k, _) -> k) self
end

module CCIO = struct
  include CCIO

  let with_in ?mode ?(flags=[Open_rdonly; Open_text]) filename f =
    with_in ?mode ~flags filename f

  let with_out ?mode ?(flags=[Open_wronly; Open_creat; Open_trunc; Open_text]) filename f =
    with_out ?mode ~flags filename f
end
