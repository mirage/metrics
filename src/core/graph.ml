type t = Graph_type.t

type v =
  { title : string option
  ; ylabel : string option
  ; yunit : string option
  ; id : int
  ; mutable active : bool
  ; mutable fields : Src.SrcFieldSet.t }

let tbl = Hashtbl.create 27

let v ?title ?ylabel ?yunit () =
  let id = Oo.id (object end) in
  let t =
    {id; yunit; title; ylabel; active = false; fields = Src.SrcFieldSet.empty}
  in
  Hashtbl.add tbl id t; id

let get id = Hashtbl.find tbl id
let title t = (get t).title
let ylabel t = (get t).ylabel
let yunit t = (get t).yunit
let id t = (get t).id
let enable t = (get t).active <- true
let disable t = (get t).active <- false
let is_active t = (get t).active
let list () = Hashtbl.fold (fun x _ acc -> x :: acc) tbl []
let fields g = Src.SrcFieldSet.fold (fun f acc -> f :: acc) (get g).fields []

let add_field g (src: Src.t) f =
  let g = get g in
  g.fields <- Src.SrcFieldSet.add (src, f) g.fields

let remove_field g src f =
  let g = get g in
  g.fields
  <- Src.SrcFieldSet.filter
      (fun (x, y) -> not (Src.equal x src && String.equal f (Field.key y)))
      g.fields