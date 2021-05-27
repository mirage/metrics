type rusage = {
  utime : (int64 * int) ;
  stime : (int64 * int) ;
  maxrss : int64 ;
  ixrss : int64 ;
  idrss : int64 ;
  isrss : int64 ;
  minflt : int64 ;
  majflt : int64 ;
  nswap : int64 ;
  inblock : int64 ;
  outblock : int64 ;
  msgsnd : int64 ;
  msgrcv : int64 ;
  nsignals : int64 ;
  nvcsw : int64 ;
  nivcsw : int64 ;
}

let pp_rusage ppf r =
  Fmt.pf ppf "utime %Lu.%06d stime %Lu.%06d maxrss %Lu ixrss %Lu idrss %Lu isrss %Lu minflt %Lu majflt %Lu nswap %Lu inblock %Lu outblock %Lu msgsnd %Lu msgrcv %Lu signals %Lu nvcsw %Lu nivcsw %Lu"
    (fst r.utime) (snd r.utime) (fst r.stime) (snd r.stime) r.maxrss r.ixrss r.idrss r.isrss r.minflt r.majflt r.nswap r.inblock r.outblock r.msgsnd r.msgrcv r.nsignals r.nvcsw r.nivcsw

let pp_rusage_mem ppf r =
  Fmt.pf ppf "maxrss %Lu ixrss %Lu idrss %Lu isrss %Lu minflt %Lu majflt %Lu"
    r.maxrss r.ixrss r.idrss r.isrss r.minflt r.majflt

type kinfo_mem = {
  vsize : int64 ;
  rss : int64 ;
  tsize : int64 ;
  dsize : int64 ;
  ssize : int64 ;
  runtime : int64 ;
  cow : int ;
  start : (int64 * int) ;
}

let pp_kinfo_mem ppf t =
  Fmt.pf ppf "virtual-size %Lu rss %Lu text-size %Lu data-size %Lu stack-size %Lu runtime %Lu cow %u start %Lu.%06d"
    t.vsize t.rss t.tsize t.dsize t.ssize t.runtime t.cow (fst t.start) (snd t.start)
    
open Rresult.R.Infix

external sysconf_clock_tick : unit -> int = "metrics_sysconf_clock_tick"

external sysctl_kinfo_proc : int -> kinfo_mem = "metrics_sysctl_kinfo_proc"

external getrusage : unit -> rusage = "metrics_rusage"

external uname : unit -> string = "metrics_uname"

let rec wrap f arg =
  try Ok (f arg) with
  | Unix.Unix_error (Unix.EINTR, _, _) -> wrap f arg
  | e -> Error (`Msg (Printexc.to_string e))

let string_of_file filename =
  try
    let fh = open_in filename in
    let content = input_line fh in
    close_in_noerr fh ;
    Ok content
  with _ -> Rresult.R.error_msgf "Error reading file %S" filename

let parse_proc_stat s =
  let stats_opt =
    match String.rindex_opt s ')' with
    | None -> None
    | Some idx -> 
      let rest = String.sub s idx (String.length s - idx) in
      Some (String.split_on_char ' ' rest)
  in
  Option.to_result ~none:(`Msg "unable to parse /proc/self/stat") stats_opt

let linux_kinfo () =
  (match Unix.stat "/proc/self" with
   | { Unix.st_ctime = start; _ } ->
     let frac = Float.rem start 1. in
     Ok (Int64.of_float start, int_of_float (frac *. 1_000_000.))
   | exception Unix.Unix_error (Unix.ENOENT,_,_) -> Error (`Msg "failed to stat process") ) >>= fun start ->
  (* reading /proc/self/stat - since it may disappear mid-time,
     best to have it in memory *)
  string_of_file "/proc/self/stat" >>= fun data ->
  parse_proc_stat data >>= fun stat_vals ->
  string_of_file "/proc/self/statm" >>= fun data ->
  let statm_vals = String.split_on_char ' ' data in
  let i64 s = try Ok (Int64.of_string s) with
      Failure _ -> Error (`Msg "couldn't parse integer")
  in
  let time_of_int64 t =
    let clock_tick = Int64.of_int (sysconf_clock_tick ()) in
    let ( * ) = Int64.mul and ( / ) = Int64.div in
    (t / clock_tick, Int64.to_int (((Int64.rem t clock_tick) * 1_000_000L) / clock_tick))
  in
  if List.length stat_vals >= 50 && List.length statm_vals >= 7 then
    i64 (List.nth stat_vals 11) >>= fun utime -> (* divide by sysconf(_SC_CLK_TCK) *)
    i64 (List.nth stat_vals 12) >>= fun stime -> (* divide by sysconf(_SC_CLK_TCK) *)
    let runtime = fst (time_of_int64 Int64.(add utime stime)) in
    i64 (List.nth stat_vals 20) >>= fun vsize -> (* in bytes *)
    i64 (List.nth stat_vals 21) >>= fun rss -> (* in pages *)
    i64 (List.nth statm_vals 3) >>= fun tsize ->
    i64 (List.nth statm_vals 5) >>= fun dsize -> (* data + stack *)
    i64 (List.nth statm_vals 5) >>= fun ssize -> (* data + stack *)
    Ok { vsize; rss; tsize; dsize; ssize; runtime; cow = 0; start }
  else
    Error (`Msg "couldn't read /proc/self/stat")

let tv (sec, usec) = Int64.to_float sec +. float_of_int usec /. 1_000_000.

open Metrics

let rusage_src ~tags =
  let doc = "System rusage counters" in
  let graph = Graph.v ~title:doc ~ylabel:"value" () in
  let data () =
    match wrap getrusage () with
    | Error (`Msg _) -> Data.v []
    | Ok ru ->
      Data.v [
          float "utime" ~graph (tv ru.utime) ;
          float "stime" ~graph (tv ru.stime) ;
          uint64 "maxrss" ~graph ru.maxrss ;
          uint64 "ixrss" ~graph ru.ixrss ;
          uint64 "idrss" ~graph ru.idrss ;
          uint64 "isrss" ~graph ru.isrss ;
          uint64 "minflt" ~graph ru.minflt ;
          uint64 "maxflt" ~graph ru.majflt ;
          uint64 "nswap" ~graph ru.nswap ;
          uint64 "inblock" ~graph ru.inblock ;
          uint64 "outblock" ~graph ru.outblock ;
          uint64 "msgsnd" ~graph ru.msgsnd ;
          uint64 "msgrcv" ~graph ru.msgrcv ;
          uint64 "nsignals" ~graph ru.nsignals ;
          uint64 "nvcsw" ~graph ru.nvcsw ;
          uint64 "nivcsw" ~graph ru.nivcsw
        ]
  in
  Src.v ~doc ~tags ~data "resource_usage"

let kinfo_mem_src ~tags =
  let doc = "System kernel information" in
  let graph = Graph.v ~title:doc ~ylabel:"value" () in
  let uname = uname () and pid = Unix.getpid () in
  let kinfo () = match uname with
    | "FreeBSD" -> wrap sysctl_kinfo_proc pid
    | "Linux" -> linux_kinfo ()
    | s -> Error (`Msg ("unsupported operating system " ^ s))
  in
  (match kinfo () with 
   | Error (`Msg msg) -> Logs.err (fun m -> m "error while collecting kinfo: %s" msg)
   | Ok _ -> ());
  let data () =
   match kinfo () with
   | Error _ -> Data.v []
   | Ok mem ->
      let now = Unix.gettimeofday () in
      let uptime = now -. tv mem.start in
      Data.v [
        uint64 "vsize" ~graph mem.vsize ;
        uint64 "rss" ~graph mem.rss ;
        uint64 "tsize" ~graph mem.tsize ;
        uint64 "dsize" ~graph mem.dsize ;
        uint64 "ssize" ~graph mem.ssize ;
        uint "cow_faults" ~graph mem.cow ;
        uint64 "runtime" ~graph mem.runtime ;
        float "uptime" ~graph uptime ;
      ]
  in
  Src.v ~doc ~tags ~data "kinfo_mem"

