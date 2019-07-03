(** The type for metric fields. *)
type t

(** The type for field keys. *)
type key = string

(** The type of supported values in metric fields. *)
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

(** The type for field functions. *)
type 'a field_f =
  ?doc:string
  -> ?unit:string
  -> ?graph:Graph_type.t
  -> ?graphs:Graph_type.t list
  -> key
  -> 'a
  -> t

val make :
  ?doc:string
  -> ?unit:string
  -> ?graph:Graph_type.t
  -> ?graphs:Graph_type.t list
  -> string
  -> 'a ty
  -> 'a
  -> t
(** [field ?doc ?unit k ty v] is the field whose key is [k], value type is [ty]
    and value is [v]. *)

(** {3 Reading Fields} *)

val key : t -> string
(** [key f] is [f]'s key. *)

val doc : t -> string option
(** [doc f] is [f]'s documentation. *)

val unit : t -> string option
(** [unit t] are [t]'s units. *)

(* HACK: this creates a cyclic dependency problem when splitting into
   multiple modules -- graph must know about field. Remove this altogether? *)

val graphs : t -> Graph_type.t list option
(** [graphs t] is the graphs where [t] appears. *)

val string : string field_f
(** [string ?doc k v] is the field whose key is [k] and value is [v]. *)

val int : int field_f
(** [int ?doc k i] is the field whose key is [k] and value is [i]. *)

val uint : int field_f
(** [uint ?doc k i] is the field whose key is [k] and value is [i]. *)

val int32 : int32 field_f
(** [int32 k i] is the field whose key is [k] and value is [i]. *)

val uint32 : int32 field_f
(** [uint32 ?doc k i] is the field whose key is [k] and value is [i]. *)

val int64 : int64 field_f
(** [int64 ?doc k i] is the field whose key is [k] and value is [i]. *)

val uint64 : int64 field_f
(** [uint64 ?doc k i] is the field whose key is [k] and value is [i]. *)

val float : float field_f
(** [uint ?doc k f] is the field whose key is [k] and value is [i]. *)

val bool : bool field_f
(** [uint ?doc k b] is the field whose key is [k] and value is [i]. *)

val duration : int64 -> t
(** [duration t] is the field [("duration", t, "ns")]. *)

(** The type for process status. *)
type status = [`Ok | `Error]

val status : status -> t
(** [status t] is the field [("status", "ok")] or [("status", "error")]. *)

(** {3 Custom fields} *)


type value = V : 'a ty * 'a -> value  (** Type for values. *)

val value : t -> value
(** [value f] is [f]'s value. *)

val index : fields:string list -> t -> int
(** [index ~fields f] is [f]'s index in the list of field keys [fields]. Raise
    [Not_found] if [f] is not a field of [t]. *)

val index_key : fields:string list -> string -> int
(** Same as {!index} but using field keys instead. *)

(** {3 Pretty-printing Fields} *)

val pp_key : t Fmt.t
(** [pp_key] is the pretty-printer for field keys. *)

val pp_value : t Fmt.t
(** [pp_value] is the pretty-printer for field values, using sensible default. *)
