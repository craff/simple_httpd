#define _GNU_SOURCE
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
#include <sys/stat.h>
#include <fcntl.h>
#include <netinet/tcp.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <sys/sendfile.h>
#include <openssl/ssl.h>
#include <openssl/err.h>
#include <sys/resource.h>
#include <fcntl.h>
#ifdef HAS_DIRENT
#include <dirent.h>
#else
#include <sys/dir.h>
#endif

CAMLprim value caml_byte_sendfile(value out_fd, value in_fd, value offset, value count) {
  CAMLparam0();
  ssize_t off = Int_val(offset);
  // printf("sendfile %d %d\n",off,Int_val(count)); fflush(stdout);
  ssize_t w = sendfile(Int_val(out_fd),Int_val(in_fd),&off,Int_val(count));
  // printf("sendfile %d %d\n",off,w); fflush(stdout);
  CAMLreturn(Val_int(w));
}

long caml_sendfile(long out_fd, long in_fd, long off, long count) {
  return(sendfile(out_fd,in_fd,&off,count));
}

CAMLprim void caml_sendfile_error() {
  CAMLparam0();
  caml_uerror("sendfile", Nothing);
  CAMLreturn0;
}

CAMLprim value caml_byte_splice(value in_fd, value in_off, value out_fd,
				value out_off, value count) {
  CAMLparam0();
  ssize_t in_off_p = Int_val(in_off);
  ssize_t out_off_p = Int_val(out_off);
  // printf("sendfile %d %d\n",off,Int_val(count)); fflush(stdout);
  ssize_t w = splice(Int_val(in_fd),&in_off_p,
		     Int_val(in_fd),&out_off_p,Int_val(count),0);
  // printf("sendfile %d %d\n",off,w); fflush(stdout);
  CAMLreturn(Val_int(w));
}

long caml_splice(long in_fd, long in_off, long out_fd, long out_off, long count) {
  return(splice(in_fd,&in_off,out_fd,&out_off,count,0));
}

CAMLprim void caml_splice_error() {
  CAMLparam0();
  caml_uerror("splice", Nothing);
  CAMLreturn0;
}

#define SSL_val(v) (*((SSL **)Data_custom_val(v)))
#define SSL_CTX_val(v) (*((SSL_CTX **)Data_custom_val(v)))

CAMLprim value caml_byte_ssl_sendfile (value out_fd, value in_fd, value offset, value count) {
  CAMLparam1(out_fd);
  ERR_clear_error();
  ssize_t w = SSL_sendfile(SSL_val(out_fd),Int_val(in_fd),
			   Int_val(offset),Int_val(count), 0);
  CAMLreturn(Val_int(w));
}

long caml_ssl_sendfile (value out_fd, long in_fd, long offset, long count) {
  ERR_clear_error();
  return(SSL_sendfile(SSL_val(out_fd),in_fd,offset,count, 0));
}

int alpn_select_cb(SSL *ssl, const unsigned char **out, unsigned char *outlen,
                   const unsigned char *in, unsigned int inlen, void *arg) {
    static const unsigned char alpn_protocols[] = "\x08http/1.1";
    *out = alpn_protocols + 1;
    *outlen = alpn_protocols[0];
    return SSL_TLSEXT_ERR_OK;
}

CAMLprim void caml_ssl_nonblock(value ctx_val) {
  CAMLparam1(ctx_val);
  SSL_CTX *ctx = SSL_CTX_val(ctx_val);
  SSL_CTX_clear_options(ctx, SSL_OP_NO_TICKET);
  SSL_CTX_sess_set_cache_size(ctx, 16384);
  SSL_CTX_set_session_cache_mode(ctx,
  				 SSL_SESS_CACHE_SERVER);
  SSL_CTX_clear_mode(ctx, SSL_MODE_AUTO_RETRY);
  SSL_CTX_set_mode(ctx, SSL_MODE_ENABLE_PARTIAL_WRITE);
  SSL_CTX_set_num_tickets(ctx, 1);
  SSL_CTX_set_alpn_select_cb(ctx, alpn_select_cb, NULL);
  CAMLreturn0;
}


CAMLprim value caml_byte_setsockopt_cork(value socket, value val)
{
  CAMLparam0();
  int optval;
  socklen_t optsize;
  optsize = sizeof(optval);
  optval = Int_val(val);

  CAMLreturn(Val_int(setsockopt(Int_val(socket), IPPROTO_TCP, TCP_CORK, &optval, optsize)));
}

long caml_setsockopt_cork(long socket, long val)
{
  socklen_t optsize = sizeof(val);
  return(setsockopt(socket, IPPROTO_TCP, TCP_CORK, &val, optsize));
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

long caml_fast_single_write(long fd, value buf, long ofs,
                                      long len)
{
  return write(fd, &Byte(buf, ofs), len);
}

CAMLprim value caml_byte_fast_single_write(value fd, value buf, value vofs,
				   value vlen) {
  CAMLparam0();
  CAMLreturn(Val_int(caml_fast_single_write(fd,buf,Int_val(vofs),Int_val(vlen))));
}

CAMLprim void caml_write_error() {
  CAMLparam0();
  caml_uerror("single_write", Nothing);
  CAMLreturn0;
}

long caml_fast_read(long fd, value buf, long ofs, long len)
{
  return read(fd, &Byte(buf, ofs), len);
}

CAMLprim value caml_byte_fast_read(value fd, value buf, value vofs,
				   value vlen) {
  CAMLparam0();
  CAMLreturn(Val_int(caml_fast_read(fd,buf,Int_val(vofs),Int_val(vlen))));
}

CAMLprim void caml_read_error() {
  CAMLparam0();
  caml_uerror("read", Nothing);
  CAMLreturn0;
}

CAMLprim value caml_rlimit_cur() {
  CAMLparam0();
  struct rlimit rl;
  rlim_t res = -1;
  if (getrlimit(RLIMIT_NOFILE, &rl) == 0)
    res = rl.rlim_cur;
  CAMLreturn(Val_int(res));
}

CAMLprim value caml_file_type(value name) {
  CAMLparam1(name);
  CAMLlocal3(d,r,mtime);
  struct stat st;

  int fd = open(String_val(name),O_RDONLY);
  if (fd == -1) r = Val_int(0);
  else {
    int res = fstat(fd, &st);
    if (res == -1) r = Val_int(0);
    mtime = caml_copy_double(st.st_mtime);
    switch (st.st_mode & S_IFMT) {
    case S_IFDIR:
      d = caml_alloc_small(1, Abstract_tag);
      DIR_Val(d) = fdopendir(fd);
      r = caml_alloc_small(3,1);
      Store_field(r,0,d);
      Store_field(r,1,mtime);
      Store_field(r,2,Val_int(1));
      break;
    case S_IFREG:
      r = caml_alloc_small(4,0);
      Store_field(r,0,Val_int(fd));
      Store_field(r,1,mtime);
      Store_field(r,2,Val_int(st.st_size));
      Store_field(r,3,Val_int(1));
      break;
    default:
      r = Val_int(1); break;
    }
  }
  CAMLreturn(r);
}
