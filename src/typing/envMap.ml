(* Copyright (c) 2013-2017 The Labrys developers. *)
(* See the LICENSE file at the top-level directory. *)

module type S = sig
  include Map.S

  val find : key -> 'a t -> 'a
  val diff : 'a t -> 'a t -> 'a t
end

module Make
    (I : module type of Ident.Name)
    (E : sig val fail : I.t -> 'a end) =
struct
  include Map.Make(I)

  let find k self =
    match get k self with
    | Some x -> x
    | None -> E.fail k

  let diff x y =
    filter (fun k _ -> not (mem k y)) x
end

module Value = Make(Ident.Name)(struct
    let fail k =
      Err.fail
        ~loc:(Ident.Name.loc k)
        "The value '%s' was not found in Γ"
        (Ident.Name.to_string k)
  end)

module Constr = struct
  include Make(Ident.Constr)(struct
      let fail k =
        Err.fail
          ~loc:(Ident.Constr.loc k)
          "The data constructor '%s' was not found in Γ"
          (Ident.Constr.to_string k)
    end)

  let add k x map =
    if mem k map then
      Err.fail
        ~loc:(Ident.Constr.loc k)
        "A module cannot contain several times the data constructor '%s'"
        (Ident.Constr.to_string k);
    add k x map
end

module Type = struct
  include Make(Ident.Type)(struct
      let fail k =
        Err.fail
          ~loc:(Ident.Type.loc k)
          "The type '%s' was not found in Γ"
          (Ident.Type.to_string k)
    end)

  let add k x map =
    if mem k map then
      Err.fail
        ~loc:(Ident.Type.loc k)
        "A module cannot contain several times the type '%s'"
        (Ident.Type.to_string k);
    add k x map
end
