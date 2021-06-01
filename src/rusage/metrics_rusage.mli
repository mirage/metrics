val rusage_src :
  tags:'a Metrics.Tags.t -> ('a, unit -> Metrics.data) Metrics.src

val kinfo_mem_src :
  tags:'a Metrics.Tags.t -> ('a, unit -> Metrics.data) Metrics.src
