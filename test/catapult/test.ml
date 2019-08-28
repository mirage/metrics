let src =
  let open Metrics in
  let tags = Tags.[ int "foo"; string "bar" ] in
  let data i =
    Data.v [ string "toto" ("XXX" ^ string_of_int i); int "titi" i ]
  in
  Src.v "test" ~tags ~data

let geometric_alpha =
  let open Metrics in
  let tags = Tags.[] in
  let data = function
    | Ok i -> Data.v [ int "i" i ]
    | Error _ -> invalid_arg "boom"
  in
  Src.v ~duration:true "geometric_alpha" ~tags ~data

let geometric_beta =
  let open Metrics in
  let tags = Tags.[] in
  let data = function
    | Ok i -> Data.v [ int "i" i ]
    | Error _ -> invalid_arg "boom"
  in
  Src.v ~duration:true "geometric_beta" ~tags ~data

let geometric n =
  let rec inner acc =
    if Random.int (n + 1) = n then acc else (inner [@tailcall]) (acc + 1)
  in
  inner 0

let test_f () =
  Metrics.add src (fun t -> t 42 "hi") (fun m -> m 42);
  for i = 0 to 10000 do
    Printf.printf "run %d%!\r" i;
    ignore
    @@ Metrics.run geometric_alpha
         (fun x -> x)
         (fun () ->
           let dur = geometric 1000 in
           Unix.sleepf (float_of_int dur /. 1_000_000.);
           i);
    ignore
    @@ Metrics.run geometric_beta
         (fun x -> x)
         (fun () ->
           let dur = geometric 1500 in
           Unix.sleepf (float_of_int dur /. 1_000_000.);
           i)
  done

(* This should probably be banned *)
(* Metrics.add src_with_duration (fun t -> t 12 "ho") (fun m -> m (Ok 43)) *)

let () =
  Random.self_init ();
  Metrics.enable_all ();
  Metrics_catapult.set_reporter ~pretty_print:true
    ~output:"/tmp/metrics_output.log";
  test_f ()

(* let () =
 *   Metrics.enable_all ();
 *   Metrics_catapult.set_reporter ~dir:"/tmp/metrics_output.log";
 *   Alcotest.run "metrics-catapult" ["base", ["f", `Quick, test_f]] *)
