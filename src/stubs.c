#include <caml/misc.h>
#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/config.h>
#include <caml/callback.h>
#include <caml/fail.h>
#include <caml/threads.h>
#include <caml/version.h>
#include <caml/unixsupport.h>
#include "stdlib.h"

CAMLprim value caml_fast_single_write(value fd, value buf, value vofs,
                                      value vlen)
{
  CAMLparam1(buf);
  long ofs, len;
  int ret;

  ofs = Long_val(vofs);
  len = Long_val(vlen);

  ret = write(Int_val(fd), &Byte(buf, ofs), len);
  CAMLreturn(Val_int(ret));
}

CAMLprim void caml_write_error() {
  CAMLparam0();
  caml_uerror("single_write", Nothing);
  CAMLreturn0;
}

CAMLprim value caml_fast_read(value fd, value buf, value vofs,
                                      value vlen)
{
  CAMLparam1(buf);
  long ofs, len;
  int ret;

  ofs = Long_val(vofs);
  len = Long_val(vlen);

  ret = read(Int_val(fd), &Byte(buf, ofs), len);
  CAMLreturn(Val_int(ret));
}

CAMLprim void caml_read_error() {
  CAMLparam0();
  caml_uerror("read", Nothing);
  CAMLreturn0;
}
