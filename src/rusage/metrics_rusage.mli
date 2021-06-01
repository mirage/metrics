val rusage_src :
  tags:'a Metrics.Tags.t -> ('a, unit -> Metrics.data) Metrics.src
(** [rusage_src ~tags] is a metrics source that reports data gathered from
    [getrusage]. *)

val kinfo_mem_src :
  tags:'a Metrics.Tags.t -> ('a, unit -> Metrics.data) Metrics.src
(** [kinfo_mem_src ~tags] is a metrics source which reports data gathered from
    sysctl ctl_kern.kern_proc.kern_proc_pid.pid on FreeBSD (which returns a
    kinfo_proc (see [sys/user.h])), on Linux [/proc/self/stat] and
    [/proc/self/statm] are used for the collection.

    The data provided are virtual size, resident set size, text, data, and stack
    size, copy-on-write fauls, running time (in microseconds), and uptime. *)
