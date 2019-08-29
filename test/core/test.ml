(*
 * Copyright (c) 2018 Thomas Gazagnaire <thomas@gazagnaire.org>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *)

(*************)
(* Reporters *)
(*************)

let data_ = Queue.create ()

let data () = Queue.pop data_

let now =
  let n = ref ~-1 in
  fun () ->
    incr n;
    Int64.of_int !n

let set_mem_reporter () =
  let report ~tags ~data ~over src k =
    let data_fields = Metrics.Data.fields data in
    let name = Metrics.Src.name src in
    let field f =
      (Fmt.to_to_string Metrics.pp_key f, Fmt.to_to_string Metrics.pp_value f)
    in
    let fields = List.map field in
    let timestamp =
      match Metrics.Data.timestamp data with
      | Some ts -> ts
      | None -> Int64.to_string (now ())
    in
    let d = (name, fields tags, fields data_fields, timestamp) in
    Queue.push d data_;
    over ();
    k ()
  in
  let at_exit () = () in
  Metrics.set_reporter { Metrics.report; now; at_exit }

(*************)
(*   Tests   *)
(*************)

let src =
  let open Metrics in
  let tags = Tags.[ int "foo"; string "bar" ] in
  let data i =
    Data.v [ string "toto" ("XXX" ^ string_of_int i); int "titi" i ]
  in
  Src.v "test" ~tags ~data

let f tags =
  Metrics.add src tags (fun m -> m 42);
  Metrics.add src tags (fun m -> m 43)

let i0 t = t 42 "hi!"

let i1 t = t 12 "toto"

let timer =
  let open Metrics in
  let tags = Tags.[ string "truc" ] in
  let data (_ : (unit, string) rresult) = Data.v [] in
  Src.v "sleep" ~tags ~data ~duration:true ~status:true

let m1 t = t "foo"

let m2 t = t "bar"

let status =
  let open Metrics in
  let tags = Tags.[] in
  let data (_ : (unit, unit) rresult) = Data.v [] in
  Src.v "status" ~tags ~data ~duration:false ~status:true

let d =
  let pp_string = Fmt.fmt "%S" in
  let pp_field = Fmt.Dump.pair pp_string pp_string in
  let pp ppf (n, t, f, x) =
    Fmt.pf ppf "(%S, %a, %a, %S)" n
      Fmt.(Dump.list pp_field)
      t
      Fmt.(Dump.list pp_field)
      f x
  in
  Alcotest.testable pp ( = )

let test_f () =
  f i0;
  Alcotest.check d "i0" (data ())
    ( "test",
      [ ("foo", "42"); ("bar", "hi!") ],
      [ ("toto", "XXX42"); ("titi", "42") ],
      "0" );
  Alcotest.check d "i0" (data ())
    ( "test",
      [ ("foo", "42"); ("bar", "hi!") ],
      [ ("toto", "XXX43"); ("titi", "43") ],
      "1" );
  f i1;
  Alcotest.check d "i0" (data ())
    ( "test",
      [ ("foo", "12"); ("bar", "toto") ],
      [ ("toto", "XXX42"); ("titi", "42") ],
      "2" );
  Alcotest.check d "i1" (data ())
    ( "test",
      [ ("foo", "12"); ("bar", "toto") ],
      [ ("toto", "XXX43"); ("titi", "43") ],
      "3" )

let test_timer () =
  let _ = Metrics.rrun timer m1 (fun () -> Ok (Unix.sleep 1)) in
  Alcotest.check d "m1-ok" (data ())
    ("sleep", [ ("truc", "foo") ], [ ("duration", "1"); ("status", "ok") ], "6");
  let _ =
    try Metrics.rrun timer m1 (fun () -> raise Not_found)
    with Not_found -> Ok ()
  in
  Alcotest.check d "m1-error" (data ())
    ( "sleep",
      [ ("truc", "foo") ],
      [ ("duration", "1"); ("status", "error") ],
      "9" )

let test_status () =
  let _ = Metrics.rrun status (fun t -> t) (fun () -> Ok ()) in
  Alcotest.check d "status" (data ()) ("status", [], [ ("status", "ok") ], "12");
  let _ = Metrics.rrun status (fun t -> t) (fun () -> Error ()) in
  Alcotest.check d "status" (data ())
    ("status", [], [ ("status", "error") ], "15");
  ()

let () =
  Metrics.enable_all ();
  set_mem_reporter ();
  Alcotest.run "metrics"
    [
      ( "base",
        [
          ("f", `Quick, test_f);
          ("timer", `Quick, test_timer);
          ("status", `Quick, test_status);
        ] );
    ]
