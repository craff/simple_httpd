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
#include <sys/eventfd.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/tcp.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <sys/sendfile.h>
#include <openssl/ssl.h>
#include <openssl/err.h>

CAMLprim value caml_sendfile(value out_fd, value in_fd, value offset, value count) {
  CAMLparam0();
  ssize_t off = Int_val(offset);
  // printf("sendfile %d %d\n",off,Int_val(count)); fflush(stdout);
  ssize_t w = sendfile(Int_val(out_fd),Int_val(in_fd),&off,Int_val(count));
  // printf("sendfile %d %d\n",off,w); fflush(stdout);
  CAMLreturn(Val_int(w));
}

CAMLprim void caml_sendfile_error() {
  CAMLparam0();
  caml_uerror("sendfile", Nothing);
  CAMLreturn0;
}

#define SSL_val(v) (*((SSL **)Data_custom_val(v)))

CAMLprim value caml_ssl_sendfile (value out_fd, value in_fd, value offset, value count) {
  CAMLparam1(out_fd);
  ERR_clear_error();
  ssize_t w = SSL_sendfile(SSL_val(out_fd),Int_val(in_fd),
			   Int_val(offset),Int_val(count), 0);
  CAMLreturn(Val_int(w));
}

CAMLprim void caml_setsockopt_cork(value socket, value val)
{
  CAMLparam1(socket);
  int optval;
  socklen_t optsize;
  optsize = sizeof(optval);
  optval = Int_val(val);

  if (setsockopt(Int_val(socket), IPPROTO_TCP, TCP_CORK, &optval, optsize))
     caml_uerror("setsocketopt_cork", Nothing);
  CAMLreturn0;
}

CAMLprim value caml_eventfd(value initval, value flags)
{
  CAMLparam0();
  int sock = eventfd(Int_val(initval),Int_val(flags));
  CAMLreturn(Val_int(sock));
}

CAMLprim value caml_efd_cloexec()
{
  CAMLparam0();
  CAMLreturn(Val_int(EFD_CLOEXEC));
}

CAMLprim value caml_efd_nonblock()
{
  CAMLparam0();
  CAMLreturn(Val_int(EFD_NONBLOCK));
}

CAMLprim value caml_efd_semaphore()
{
  CAMLparam0();
  CAMLreturn(Val_int(EFD_SEMAPHORE));
}

long caml_fast_single_write(value fd, value buf, long ofs,
                                      long len)
{
  return write(Int_val(fd), &Byte(buf, ofs), len);
}

CAMLprim value caml_byte_fast_single_write(value fd, value buf, value vofs,
				   value vlen) {
  CAMLparam0();
  CAMLreturn(Int_val(caml_fast_single_write(fd,buf,Int_val(vofs),Int_val(vlen))));
}

CAMLprim void caml_write_error() {
  CAMLparam0();
  caml_uerror("single_write", Nothing);
  CAMLreturn0;
}

long caml_fast_read(value fd, value buf, long ofs, long len)
{
  return read(Int_val(fd), &Byte(buf, ofs), len);
}

CAMLprim value caml_byte_fast_read(value fd, value buf, value vofs,
				   value vlen) {
  CAMLparam0();
  CAMLreturn(Int_val(caml_fast_read(fd,buf,Int_val(vofs),Int_val(vlen))));
}

CAMLprim void caml_read_error() {
  CAMLparam0();
  caml_uerror("read", Nothing);
  CAMLreturn0;
}
