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
#include <assert.h>
#include <errno.h>
#include <fcntl.h>
#include <netinet/tcp.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <sys/sendfile.h>
#include <openssl/ssl.h>
#include <openssl/err.h>
#include <sys/ioctl.h>
#include <sys/resource.h>
#include <spawn.h>
#include <fcntl.h>
#include <signal.h>
#ifdef HAS_DIRENT
#include <dirent.h>
#else
#include <sys/dir.h>
#endif

CAMLprim value caml_byte_sendfile(value out_fd, value in_fd, value offset, value count) {
  CAMLparam0();
  off_t off = Int_val(offset);
  ssize_t w = sendfile(Int_val(out_fd),Int_val(in_fd),&off,Int_val(count));
  CAMLreturn(Val_int(w));
}

long caml_sendfile(long out_fd, long in_fd, long off, long count) {
  off_t off64 = off; // needed on 32 bit
  return(sendfile(out_fd,in_fd,&off64,count));
}

CAMLprim void caml_sendfile_error() {
  CAMLparam0();
  caml_uerror("sendfile", Nothing);
  CAMLreturn0;
}

CAMLprim value caml_byte_splice(value in_fd, value in_off, value out_fd,
				value out_off, value count) {
  CAMLparam0();
  off_t in_off_p = Int_val(in_off);
  off_t out_off_p = Int_val(out_off);
  // printf("sendfile %d %d\n",off,Int_val(count)); fflush(stdout);
  ssize_t w = splice(Int_val(in_fd),&in_off_p,
		     Int_val(in_fd),&out_off_p,Int_val(count),0);
  // printf("sendfile %d %d\n",off,w); fflush(stdout);
  CAMLreturn(Val_int(w));
}

long caml_splice(long in_fd, long in_off, long out_fd, long out_off, long count) {
  off_t in_off64 = in_off; // needed on 32 bit
  off_t out_off64 = out_off; // needed on 32 bit
  return(splice(in_fd,&in_off64,out_fd,&out_off64,count,0));
}

CAMLprim void caml_splice_error() {
  CAMLparam0();
  caml_uerror("splice", Nothing);
  CAMLreturn0;
}

#define SSL_val(v) (*((SSL **)Data_custom_val(v)))
#define SSL_CTX_val(v) (*((SSL_CTX **)Data_custom_val(v)))

CAMLprim void caml_byte_set_ktls(value v) {
#ifdef SSL_OP_ENABLE_KTLS
  SSL_CTX *ssl = SSL_CTX_val(v);
  SSL_CTX_set_options(ssl, SSL_OP_ENABLE_KTLS);
#endif
}

CAMLprim value caml_byte_check_ktls(value v) {
#ifdef SSL_OP_ENABLE_KTLS
  SSL *ssl = SSL_val(v);
  int s = BIO_get_ktls_send(SSL_get_wbio(ssl));
  int r = BIO_get_ktls_recv(SSL_get_rbio(ssl));
  return(Val_int(s + 2*r));
#else
  return(Val_int(0));
#endif
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
  // SSL_CTX_set_mode(ctx, SSL_MODE_ACCEPT_MOVING_WRITE_BUFFER);
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

CAMLprim value caml_byte_flush_cork(value socket)
{
  CAMLparam0();
  int optval = 0;
  long r;
  socklen_t optsize;
  optsize = sizeof(optval);

  r = setsockopt(Int_val(socket), IPPROTO_TCP, TCP_CORK, &optval, optsize);
  if (r < 0) CAMLreturn(r);
  optval = 1;
  CAMLreturn(Val_int(setsockopt(Int_val(socket), IPPROTO_TCP, TCP_CORK, &optval, optsize)));
}

long caml_flush_cork(long socket)
{
  int optval = 0;
  long r;
  socklen_t optsize = sizeof(optval);
  r = setsockopt(socket, IPPROTO_TCP, TCP_CORK, &optval, optsize);
  if (r < 0) return r;
  optval = 1;
  return(setsockopt(socket, IPPROTO_TCP, TCP_CORK, &optval, optsize));
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

long caml_fast_lseek_set(long fd, long ofs)
{
  return lseek(fd, ofs, SEEK_SET);
}

CAMLprim value caml_byte_fast_lseek_set(value fd, value vofs) {
  CAMLparam0();
  CAMLreturn(Val_int(caml_fast_lseek_set(fd,Int_val(vofs))));
}

CAMLprim void caml_lseek_error() {
  CAMLparam0();
  caml_uerror("lseek", Nothing);
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

CAMLprim value caml_ptty_spawn(value executable, /* string */
			      value args,       /* string array */
			      value optenv,     /* string array option */
			      value usepath,
			      value resetids )
{
  CAMLparam3(executable, args, optenv);
  CAMLlocal1(res);
  char ** argv;
  char ** envp;
  const char * path;
  pid_t pid;
  int master_fd, slave_fd, control_fd;
  char *slave_name;

  caml_unix_check_path(executable, "create_process");
  path = String_val(executable);
  argv = caml_unix_cstringvect(args, "create_process");
  if (Is_some(optenv)) {
    envp = caml_unix_cstringvect(Some_val(optenv), "create_process");
  } else {
    envp = NULL;
  }
  master_fd = posix_openpt(O_RDWR | O_NOCTTY | O_CLOEXEC);
  if (master_fd < 0) goto error;

  if (grantpt(master_fd) < 0) goto error;

  if (unlockpt(master_fd) < 0) goto error;

  slave_name = ptsname(master_fd);
  if (!slave_name) goto error;
  //fprintf(stderr,"slave_name %s\n",slave_name); fflush(stderr);

  slave_fd = open(slave_name, O_RDWR);
  if (slave_fd < 0) goto error;
  //fprintf(stderr,"slave\n"); fflush(stderr);

  pid = fork();
  if (pid != 0) {
    /* This is the parent process */
    control_fd = dup(slave_fd); //  make sure we get notified at close
    close(slave_fd);
    caml_unix_cstringvect_free(argv);
    if (envp != NULL) caml_unix_cstringvect_free(envp);
    if (pid == -1) goto error;
    res = caml_alloc_small(3,0);
    Store_field(res,0,Val_int(pid));
    Store_field(res,1,Val_int(master_fd));
    Store_field(res,2,Val_int(control_fd));
    CAMLreturn(res);
  }
  // FILE* oerr = fdopen(dup(2), "w");
  /* This is the child process */
  /* Perform the redirections for stdin, stdout, stderr */
  if (isatty(0)) {
    if (ioctl(0, TIOCNOTTY) < 0) goto exit;
    //fprintf(oerr,"ioctl TIOCNOTTY\n"); fflush(oerr);
    close(0);
  }
  if (setsid() < 0) goto exit;
  //fprintf(oerr,"setsid\n"); fflush(oerr);
  if (ioctl(slave_fd, TIOCSCTTY, 1) < 0) goto exit;
  //fprintf(oerr,"ioctl TIOCSCTTY\n"); fflush(oerr);
  if (Val_int(resetids)) {
    if (setuid(getuid()) < 0) goto exit;
    if (setgid(getgid()) < 0) goto exit;
  }
  //fprintf(oerr,"resetids\n"); fflush(oerr);
  if (dup2(slave_fd, 0) < 0) goto exit;
  if (dup2(slave_fd, 1) < 0) goto exit;
  if (dup2(slave_fd, 2) < 0) goto exit;
  //fprintf(oerr,"duv\n"); fflush(oerr);
  close(slave_fd);
  /* Transfer control to the executable */
  if (Bool_val(usepath)) {
    if (envp == NULL) {
      execvp(path, argv);
    } else {
#ifdef HAS_EXECVPE
      execvpe(path, argv, envp);
#else
      /* No other thread is running in the child process, so we can change
         the global variable [environ] without bothering anyone. */
      environ = envp;
      execvp(path, argv);
#endif
    }
  } else {
    if (envp == NULL) {
      execv(path, argv);
    } else {
      execve(path, argv, envp);
    }
  }
 exit:
  // fprintf(oerr,"ptty_spawn: %s\n",strerror(errno)); fflush(oerr);
  _exit(127);
 error:
  caml_unix_cstringvect_free(argv);
  if (Is_some(optenv)) caml_unix_cstringvect_free(envp);
  caml_unix_error(errno, "create_process", executable);
  assert(0);
}


CAMLprim value caml_resize_ptty(value fd, value rows, value cols, value pid) {
  CAMLparam0();

  struct winsize ws;
  ws.ws_row = Int_val(rows);
  ws.ws_col = Int_val(cols);
  ws.ws_xpixel = 0;
  ws.ws_ypixel = 0;

  if (ioctl(Int_val(fd), TIOCSWINSZ, &ws) == -1) {
    caml_failwith(strerror(errno));
  }

  kill(Int_val(pid), SIGWINCH);

  CAMLreturn(Val_unit);
}
