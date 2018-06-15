(*************)
(* Reporters *)
(*************)

let set_simple_reporter () =
  let start = Mtime_clock.now () in
  let report ~tags ~data ~over src k =
    let data_fields = Metrics.Data.fields data in
    let name = Metrics.Src.name src in
    let pp_field ppf f =
      Fmt.(pair ~sep:(unit "=") Metrics.pp_key Metrics.pp_value) ppf (f, f)
    in
    let pp = Fmt.(list ~sep:(unit ",") pp_field) in
    let timestamp = match Metrics.Data.timestamp data with
      | Some ts -> ts
      | None    -> Fmt.to_to_string Mtime.Span.pp
                     (Mtime.span start (Mtime_clock.now ()))
    in
    Fmt.pr "%s %a %a %s\n%!" name pp tags pp data_fields timestamp;
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
      alloc n
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
