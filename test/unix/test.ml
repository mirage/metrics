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
(*   Tests   *)
(*************)

let src =
  let open Metrics in
  let tags = Tags.[ int "pid"; string "hostname" ] in
  let data i =
    Data.v
      [ float "CPU" ~unit:"%" (float_of_int i ** 2.); int "MEM" ~unit:"KiB" i ]
  in
  Src.v "test" ~tags ~data

let i0 t = t 42 "foo.local"

let i1 t = t 12 "toto.com"

let f tags i = Metrics.add src tags (fun m -> m i)

let run () =
  for i = 0 to 100 do
    f i0 i;
    f i1 (2 * i)
  done

let src =
  let open Metrics in
  let tags = Tags.[ string "truc" ] in
  let graph = Graph.v ~title:"Nice graph!" ~yunit:"yay" ~ylabel:"toto" () in
  let data i =
    Data.v [ float "CPU" ~graph (float_of_int i ** 2.); int "MEM" ~graph i ]
  in
  Src.v "test" ~tags ~data

let i0 t = t "foo"

let i1 t = t "bar"

let f tags i = Metrics.add src tags (fun m -> m i)

let run2 () =
  for i = 0 to 100 do
    f i0 i;
    f i1 (2 * i)
  done

let timer =
  let open Metrics in
  let tags = Tags.[] in
  let graph = Graph.v ~title:"Timers!!" () in
  let data = function
    | Ok t ->
      Data.v [ int ~graph "timer" (int_of_float @@ (t *. 1_000_000_000.)) ]
    | Error _ -> Data.v [ float ~graph "timer" 0. ]
  in
  Src.v "sleep" ~tags ~data ~duration:true ~status:false

let run3 () =
  let open Lwt.Infix in
  let rec aux = function
    | 0 -> Lwt.return ()
    | i ->
      Metrics_lwt.run timer
        (fun x -> x)
        (fun () ->
          let t = Random.float 1. in
          Lwt_unix.sleep t >|= fun _ -> t)
      >>= fun _ -> aux (i - 1)
  in
  aux 10

let () =
  Metrics.enable_all ();
  Metrics_gnuplot.set_reporter ();
  Metrics_unix.monitor_gc 0.1;
  run ();
  run2 ();
  Lwt_main.run (run3 ())
