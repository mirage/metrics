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

let src =
  let open Metrics in
  let tags f ~foo ~bar = f @@ Data.v [
      "foo", Data.int foo;
      "bar", Data.string bar
    ] in
  let fields i =
    Data.v [
      "toto", Data.string ("XXX" ^ string_of_int i);
      "titi", Data.int i
    ] in
  Src.v "test" ~tags ~fields

let f () =
  Metrics.push src (fun m -> m 4 "toto" 42);
  Metrics.push src (fun m -> m 4 "toto" 43)

let set_reporter () =
  let report ~tags ~fields ?timestamp ~over src k =
    let name = Metrics.Src.name (Src src) in
    let pp = Fmt.(list ~sep:(unit ",") (pair ~sep:(unit "=") string string)) in
    let timestamp = match timestamp with
      | Some ts -> ts
      | None    -> Ptime.to_rfc3339 (Ptime_clock.now ())
    in
    Fmt.pr "%s %a %a %s\n%!" name pp tags pp fields timestamp;
    over ();
    k ()
  in
  Metrics.set_reporter { Metrics.report }

let () =
  set_reporter ();
  Metrics.enable_all ();
  f ()
