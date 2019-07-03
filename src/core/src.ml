(* inspiration from From logs/Src *)

type predicate = {mutable all : bool; mutable tags : Keys.t}

let _tags = {all = false; tags = Keys.empty}

type ('a, 'b) src =
  { uid : int
  ; name : string
  ; doc : string
  ; dom : Keys.t
  ; tags : 'a Tags.t
  ; data : 'b
  ; dmap : Data.t -> Data.t
  ; mutable active : bool
  ; duration : bool
  ; status : bool
  ; mutable data_fields : string list option }

type t = Src : ('a, 'b) src -> t

let uid =
  let id = ref (-1) in
  fun () -> incr id; !id

let list = ref []

let active tags =
  if _tags.all then true
  else not (Keys.is_empty (Keys.inter _tags.tags tags))

let v ?(doc = "undocumented") ?(duration = false) ?(status = false) ~tags
    ~data name =
  let dom = Tags.domain tags in
  let active = active dom in
  let dmap x = x in
  let src =
    { duration
    ; status
    ; dom
    ; uid = uid ()
    ; name
    ; doc
    ; tags
    ; data
    ; active
    ; dmap
    ; data_fields = None }
  in
  list := Src src :: !list;
  src

let is_active (Src s) = s.active
let enable (Src s) = s.active <- true
let disable (Src s) = s.active <- false
let name (Src s) = s.name
let doc (Src s) = s.doc
let tags (Src s) = Keys.elements s.dom
let equal (Src src0) (Src src1) = src0.uid = src1.uid
let compare (Src src0) (Src src1) = Pervasives.compare src0.uid src1.uid
let duration (Src s) = s.duration
let status (Src s) = s.status
let data (Src s) = match s.data_fields with None -> [] | Some l -> l
let set_data_fields (Src s) fields = s.data_fields <- Some fields

type tags = Field.t list
let tag : type a b. (a, b) src -> a =
  fun src ->
  let rec aux : type a. tags -> a Tags.t -> a =
    fun tags -> function
      | Tags.([]) -> List.rev tags
      | Tags.(h :: t) ->
        fun a ->
          let tags = Field.make h.k (Other h.pp) a :: tags in
          aux tags t
  in
  aux [] src.tags


let pp_strings ppf l =
  Fmt.pf ppf "@[<1>(%a)@]" Fmt.(list ~sep:(unit " ") string) l

let pp ppf (Src src) =
  let tags = Keys.elements (Tags.domain src.tags) in
  let data = match src.data_fields with None -> [] | Some l -> l in
  Format.fprintf ppf
    "@[<1>(src@   @[<1>(name %S)@]@   @[<1>(uid %d)@]   @[<1>(doc %S)@])   \
     @[<1>(tags (%a))@]   @[<1>(data (%a))@] @]"
    src.name src.uid src.doc pp_strings tags pp_strings data

let list () = !list
let update () = List.iter (fun (Src s) -> s.active <- active s.dom) (list ())

module SrcFieldSet = Set.Make (struct
    type nonrec t = t * Field.t

    let compare (a, x) (b, y) =
      match compare a b with
      | 0 -> String.compare (Field.key x) (Field.key y)
      | i -> i
  end)
