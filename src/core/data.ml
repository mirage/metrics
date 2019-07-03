type timestamp = string
type t = {timestamp : string option; fields : Field.t list}

let keys t = List.map Field.key t.fields
let timestamp t = t.timestamp
let fields t = t.fields
let cons h t = {t with fields = h :: t.fields}
let v ?timestamp fields = {timestamp; fields}
