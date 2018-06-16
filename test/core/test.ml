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

let set_simple_reporter () =
  let report ~tags ~data ~over src k =
    let data_fields = Metrics.Data.fields data in
    let name = Metrics.Src.name src in
    let pp_field ppf f =
      Fmt.(pair ~sep:(unit "=") Metrics.pp_key Metrics.pp_value) ppf (f, f)
    in
    let pp = Fmt.(list ~sep:(unit ",") pp_field) in
    let timestamp = match Metrics.Data.timestamp data with
      | Some ts -> ts
      | None    -> Fmt.to_to_string Mtime.pp (Mtime_clock.now ())
    in
    Fmt.pr "%s %a %a %s\n%!" name pp tags pp data_fields timestamp;
    over ();
    k ()
  in
  let now () = Mtime_clock.now () |> Mtime.to_uint64_ns in
  let at_exit () = () in
  Metrics.set_reporter { Metrics.report; now; at_exit }

(*************)
(*   Tests   *)
(*************)

let src =
  let open Metrics in
  let tags = Tags.[
      int    "foo";
      string "bar";
    ] in
  let data i =
    Data.v [
      string "toto" ("XXX" ^ string_of_int i);
      int    "titi" i;
    ] in
  Src.v "test" ~tags ~data

let i0 = Metrics.v src 42 "hi!"
let i1 = Metrics.v src 12 "toto"

let f i =
  Metrics.add i (fun m -> m 42);
  Metrics.add i (fun m -> m 43)

let timer =
  let open Metrics in
  let tags = Tags.[string "truc"] in
  Src.timer "sleep" ~tags

let m1 = Metrics.v timer "foo"
let m2 = Metrics.v timer "bar"

let status =
  let open Metrics in
  let tags = Tags.[] in
  v (Src.status "status" ~tags)


let run () =
  f i0;
  f i1;
  let _ = Metrics.run_with_result m1 (fun () -> Ok (Unix.sleep 1)) in
  let () =
    try Metrics.run m1 (fun () -> raise Not_found)
    with Not_found -> ()
  in
  Metrics.check status (Ok ());
  Metrics.check status (Error ())

let () =
  Metrics.enable_all ();
  set_simple_reporter ();
  run ();
