type 'a ty =
  | String : string ty
  | Bool : bool ty
  | Float : float ty
  | Int : int ty
  | Int32 : int32 ty
  | Int64 : int64 ty
  | Uint : int ty
  | Uint32 : int32 ty
  | Uint64 : int64 ty
  | Other : 'a Fmt.t -> 'a ty

type 'a v = {ty : 'a ty; v : 'a}

type key = string

type t =
  | F :
      { key : key
      ; unit : string option
      ; doc : string option
      ; graphs : Graph_type.t list option
      ; v : 'a v }
      -> t

let key (F {key; _}) = key
let doc (F {doc; _}) = doc
let unit (F {unit; _}) = unit
let graphs (F {graphs; _}) = graphs

type 'a field_f =
  ?doc:string
  -> ?unit:string
  -> ?graph:Graph_type.t
  -> ?graphs:Graph_type.t list
  -> key
  -> 'a
  -> t

let make ?doc ?unit ?graph ?graphs key ty v =
  let graphs =
    match graph, graphs with
    | None, None -> None
    | Some g, None -> Some [g]
    | None, Some gs -> Some gs
    | Some g, Some gs -> Some (g :: gs)
  in
  F {key; doc; unit; v = {ty; v}; graphs}

let ff ty ?doc ?unit ?graph ?graphs k v =
  make ?doc ?unit ?graph ?graphs k ty v

let string = ff String
let bool = ff Bool
let float = ff Float
let int = ff Int
let int32 = ff Int32
let int64 = ff Int64
let uint = ff Uint
let uint32 = ff Uint32
let uint64 = ff Uint64

type status = [`Ok | `Error]

let string_of_status = function `Ok -> "ok" | `Error -> "error"

let status v = make Key.status (Other (Fmt.of_to_string string_of_status)) v
let duration i = int64 Key.duration i

let pp : type a. a ty -> a Fmt.t =
 fun ty ppf v ->
  match ty with
  | String -> Fmt.string ppf v
  | Bool -> Fmt.bool ppf v
  | Int -> Fmt.int ppf v
  | Int32 -> Fmt.int32 ppf v
  | Int64 -> Fmt.int64 ppf v
  | Float -> Fmt.float ppf v
  | Uint -> Fmt.uint ppf v
  | Uint32 -> Fmt.uint32 ppf v
  | Uint64 -> Fmt.uint64 ppf v
  | Other pp -> pp ppf v

type value = V : 'a ty * 'a -> value

let index_key ~fields f =
  let rec aux n = function
    | [] -> raise Not_found
    | h :: t -> if h = f then n else aux (n + 1) t
  in
  aux 0 fields

let index ~fields f = index_key ~fields (key f)

let pp_key ppf f = Fmt.string ppf (key f)
let pp_value ppf (F {v = {ty; v}; _}) = pp ty ppf v
let value (F {v = {ty; v}; _}) = V (ty, v)
