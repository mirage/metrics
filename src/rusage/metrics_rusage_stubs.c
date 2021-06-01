// (c) 2017, 2018 Hannes Mehnert, all rights reserved

#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/fail.h>
#include <caml/unixsupport.h>

#include <sys/param.h>
#include <sys/types.h>
#include <sys/time.h>
#include <sys/resource.h>
#include <sys/user.h>

#define Val32 caml_copy_int32
#define Val64 caml_copy_int64

#include <sys/utsname.h>
CAMLprim value metrics_uname(value unit) {
  CAMLparam1(unit);
  CAMLlocal1(sys);
  struct utsname u;
  int res;

  res = uname(&u);
  if (res < 0) uerror("uname", Nothing);

  sys = caml_copy_string(u.sysname);
  CAMLreturn(sys);
}

/* We only use sysconf(_SC_CLK_TCK) in Linux only, but it's well-defined in FreeBSD as well. */
#include <unistd.h>
CAMLprim value metrics_sysconf_clock_tick(value unit) {
  CAMLparam1(unit);
  long r;
  r = sysconf(_SC_CLK_TCK);
  if (r == 1)
    uerror("sysconf", Nothing);
  CAMLreturn(Val_long(r));
}

CAMLprim value metrics_rusage(value unit) {
  CAMLparam1(unit);
  CAMLlocal2(res, time);
  struct rusage ru;
  int r;

  r = getrusage(RUSAGE_SELF, &ru);

  if (r < 0)
    uerror("getrusage", Nothing);

  if (ru.ru_utime.tv_usec < 0 || ru.ru_utime.tv_usec > 999999999 ||
      ru.ru_stime.tv_usec < 0 || ru.ru_stime.tv_usec > 999999999)
    uerror("getrusage", Nothing);

  res = caml_alloc(16, 0);
  time = caml_alloc(2, 0);
  Store_field (time, 0, Val64(ru.ru_utime.tv_sec));
  Store_field (time, 1, Val_int(ru.ru_utime.tv_usec));
  Store_field (res, 0, time);
  time = caml_alloc(2, 0);
  Store_field (time, 0, Val64(ru.ru_stime.tv_sec));
  Store_field (time, 1, Val_int(ru.ru_stime.tv_usec));
  Store_field (res, 1, time);
  Store_field (res, 2, Val64(ru.ru_maxrss));
  Store_field (res, 3, Val64(ru.ru_ixrss));
  Store_field (res, 4, Val64(ru.ru_idrss));
  Store_field (res, 5, Val64(ru.ru_isrss));
  Store_field (res, 6, Val64(ru.ru_minflt));
  Store_field (res, 7, Val64(ru.ru_majflt));
  Store_field (res, 8, Val64(ru.ru_nswap));
  Store_field (res, 9, Val64(ru.ru_inblock));
  Store_field (res, 10, Val64(ru.ru_oublock));
  Store_field (res, 11, Val64(ru.ru_msgsnd));
  Store_field (res, 12, Val64(ru.ru_msgrcv));
  Store_field (res, 13, Val64(ru.ru_nsignals));
  Store_field (res, 14, Val64(ru.ru_nvcsw));
  Store_field (res, 15, Val64(ru.ru_nivcsw));

  CAMLreturn(res);
}

#ifdef __FreeBSD__
#include <sys/sysctl.h>

CAMLprim value metrics_sysctl_kinfo_proc (value pid_r) {
  CAMLparam1(pid_r);
  CAMLlocal2(res, time);
  int name[4];
  int error;
  size_t len;
  struct kinfo_proc p;
  struct rusage ru;

  len = sizeof(p);
  name[0] = CTL_KERN;
  name[1] = KERN_PROC;
  name[2] = KERN_PROC_PID;
  name[3] = Int_val(pid_r);

  error = sysctl(name, nitems(name), &p, &len, NULL, 0);
  if (error < 0)
    uerror("sysctl ctl_kern.kern_proc.kern_proc_pid", Nothing);
  if (p.ki_start.tv_usec < 0 || p.ki_start.tv_usec > 999999999)
    uerror("sysctl ctl_kern.kern_proc.kern_proc_pid", Nothing);

  res = caml_alloc(8, 0);
  Store_field (res, 0, Val64(p.ki_size));
  Store_field (res, 1, Val64(p.ki_rssize));
  Store_field (res, 2, Val64(p.ki_tsize));
  Store_field (res, 3, Val64(p.ki_dsize));
  Store_field (res, 4, Val64(p.ki_ssize));
  Store_field (res, 5, Val64(p.ki_runtime));
  Store_field (res, 6, Val_int(p.ki_cow));
  time = caml_alloc(2, 0);
  Store_field (time, 0, Val64(p.ki_start.tv_sec));
  Store_field (time, 1, Val_int(p.ki_start.tv_usec));
  Store_field (res, 7, time);

  CAMLreturn(res);
}

#else /* FreeBSD */

/* stub symbols for OS currently not supported */

CAMLprim value metrics_sysctl_kinfo_proc (value pid_r) {
  CAMLparam1(pid_r);
  uerror("sysctl_kinfo_proc", Nothing);
}

#endif
