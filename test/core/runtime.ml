(*************)
(* influxdb line protocol reporter *)
(* from https://docs.influxdata.com/influxdb/v1.5/write_protocols/line_protocol_reference/ *)
(* example line: weather,location=us-midwest temperature=82 1465839830100400200 *)
(*************)

open Astring

let avoid_keyword =
  let keywords = String.Set.of_list [
    "ALL" ; "ALTER" ; "ANY" ; "AS" ; "ASC" ; "BEGIN" ;
    "BY" ; "CREATE" ; "CONTINUOUS" ; "DATABASE" ; "DATABASES" ; "DEFAULT" ;
    "DELETE" ; "DESC" ; "DESTINATIONS" ; "DIAGNOSTICS" ; "DISTINCT" ; "DROP" ;
    "DURATION" ; "END" ; "EVERY" ; "EXPLAIN" ; "FIELD" ; "FOR" ;
    "FROM" ; "GRANT" ; "GRANTS" ; "GROUP" ; "GROUPS" ; "IN" ;
    "INF" ; "INSERT" ; "INTO" ; "KEY" ; "KEYS" ; "KILL" ;
    "LIMIT" ; "SHOW" ; "MEASUREMENT" ; "MEASUREMENTS" ; "NAME" ; "OFFSET" ;
    "ON" ; "ORDER" ; "PASSWORD" ; "POLICY" ; "POLICIES" ; "PRIVILEGES" ;
    "QUERIES" ; "QUERY" ; "READ" ; "REPLICATION" ; "RESAMPLE" ; "RETENTION" ;
    "REVOKE" ; "SELECT" ; "SERIES" ; "SET" ; "SHARD" ; "SHARDS" ;
    "SLIMIT" ; "SOFFSET" ; "STATS" ; "SUBSCRIPTION" ; "SUBSCRIPTIONS" ; "TAG" ;
    "TO" ; "USER" ; "USERS" ; "VALUES" ; "WHERE" ; "WITH" ; "WRITE"
  ] in
  (fun m ->
     if String.(Set.mem (Ascii.uppercase m) keywords) then
       "o" ^ m
     else
       m)

let escape =
  List.fold_right (fun e m' -> String.(concat ~sep:("\\" ^ e) (cuts ~sep:e m')))

let escape_measurement m =
  escape  [ "," ; " " ] (avoid_keyword m)

let escape_name m =
  escape [ "," ; "=" ; " " ] (avoid_keyword m)

let set_simple_reporter () =
  let start = Mtime_clock.now () in
  let report ~tags ~data ~over src k =
    let open Metrics in
    let data_fields = Data.fields data in
    let name = escape_measurement (Src.name src) in
    let pp_val (str : string Fmt.t) ppf f =
      match value f with
      | V (String, s) -> str ppf s
      | V (Int, i) -> Fmt.pf ppf "%di" i
      | V (Int32, i32) -> Fmt.pf ppf "%ldi" i32
      | V (Int64, i64) -> Fmt.pf ppf "%Ldi" i64
      | V (Uint, u) -> Fmt.pf ppf "%ui" u
      | V (Uint32, u32) -> Fmt.pf ppf "%lui" u32
      | V (Uint64, u64) -> Fmt.pf ppf "%Lui" u64
      | _ -> pp_value ppf f
    in
    let pp_field_str ppf s = Fmt.pf ppf "%S" s in
    let pp_field ppf f =
      Fmt.(pair ~sep:(unit "=") string (pp_val pp_field_str)) ppf
        (escape_name (key f), f)
    in
    let pp_fields = Fmt.(list ~sep:(unit ",") pp_field) in
    let pp_tag_str ppf s = Fmt.string ppf (escape_name s) in
    let pp_tag ppf f =
      Fmt.(pair ~sep:(unit "=") string (pp_val pp_tag_str)) ppf
        (escape_name (key f), f)
    in
    let pp_tags = Fmt.(list ~sep:(unit ",") pp_tag) in
    let timestamp = match Data.timestamp data with
      | Some ts -> ts
      | None    -> Fmt.to_to_string Mtime.Span.pp
                     (Mtime.span start (Mtime_clock.now ()))
    in
    Fmt.pr "%s,%a %a %s\n%!" name pp_tags tags pp_fields data_fields timestamp;
    over ();
    k ()
  in
  let now () = Mtime_clock.now () |> Mtime.to_uint64_ns in
  Metrics.set_reporter { Metrics.report; now }

(*************)
(*   Tests   *)
(*************)

let src =
  let open Metrics in
  let tags = Tags.[
      string "hostname";
    ] in
  let data stat =
    Data.v [
      float "minor words" stat.Gc.minor_words ;
      float "promoted words" stat.Gc.promoted_words ;
      float "major words" stat.Gc.major_words ;
      uint "minor collections" stat.Gc.minor_collections ;
      uint "major collections" stat.Gc.major_collections ;
      uint "heap words" stat.Gc.heap_words ;
      uint "heap chunks" stat.Gc.heap_chunks ;
      uint "compactions" stat.Gc.compactions ;
      uint "top heap words" stat.Gc.top_heap_words ;
      uint "stack size" stat.Gc.stack_size ;
    ] in
  Src.v "quick runtime" ~tags ~data

let full_src =
  let open Metrics in
  let tags = Tags.[
      string "hostname";
    ] in
  let data stat =
    Data.v [
      float "minor words" stat.Gc.minor_words ;
      float "promoted words" stat.Gc.promoted_words ;
      float "major words" stat.Gc.major_words ;
      uint "minor collections" stat.Gc.minor_collections ;
      uint "major collections" stat.Gc.major_collections ;
      uint "heap words" stat.Gc.heap_words ;
      uint "heap chunks" stat.Gc.heap_chunks ;
      uint "compactions" stat.Gc.compactions ;
      uint "live words" stat.Gc.live_words ;
      uint "live blocks" stat.Gc.live_blocks ;
      uint "free words" stat.Gc.free_words ;
      uint "free blocks" stat.Gc.free_blocks ;
      uint "largest free" stat.Gc.largest_free ;
      uint "fragments" stat.Gc.fragments ;
      uint "top heap words" stat.Gc.top_heap_words ;
      uint "stack size" stat.Gc.stack_size ;
    ] in
  Src.v "runtime" ~tags ~data

let i = Metrics.v src "localhost"
let i2 = Metrics.v full_src "localhost"

let timer =
  let open Metrics in
  let tags = Tags.[string "function"] in
  Src.timer "duration" ~tags

let m_quick = Metrics.v timer "quick_stat"
let m_stat = Metrics.v timer "stat"

let f () =
  let _ = Metrics.run m_quick Gc.quick_stat in
  Metrics.add i2 (fun m -> m (Metrics.run m_stat (fun () -> Gc.stat ()))) ;
  Metrics.add i (fun m -> m (Metrics.run m_quick (fun () -> Gc.quick_stat ())))

let mem = Hashtbl.create 2

let rec alloc n =
  match Hashtbl.find_opt mem n with
  | Some v -> v
  | None ->
    let r =
      let rec alloc = function
        | 0 -> ["hallo"]
        | n -> string_of_int n :: alloc (pred n)
      in
      List.init n alloc
    in
    Hashtbl.add mem n r ;
    r

let run () =
  for i = 0 to 1000 do
    f () ;
    let _ = alloc i in
    Unix.sleep 1
  done

let () =
  Metrics.enable_all ();
  set_simple_reporter ();
  run ();
