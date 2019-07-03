type 'a v = {k : string; pp : Format.formatter -> 'a -> unit}

let v pp k = {k; pp}
let string = v Fmt.string
let float = v Fmt.float
let int = v Fmt.int
let uint = v Fmt.uint
let int32 = v Fmt.int32
let uint32 = v Fmt.uint32
let int64 = v Fmt.int64
let uint64 = v Fmt.uint64
let bool = v Fmt.bool

type 'a t = [] : Field.t list t | ( :: ) : 'a v * 'b t -> ('a -> 'b) t

let rec domain : type a. a t -> Keys.t = function
  | [] -> Keys.empty
  | h :: t -> Keys.add h.k (domain t)
