#include <sys/ioctl.h>
#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/fail.h>
#include <caml/memory.h>

CAMLprim value caml_kittyimg_winsize (value vfd) {
  CAMLparam1 (vfd);

  int fd = Int_val (vfd);
  struct winsize w;

  if (ioctl (fd, TIOCGWINSZ, &w) >= 0) {
    CAMLlocal1 (result);
    result = caml_alloc_tuple(4);

    Store_field (result, 0, Val_int (w.ws_col));
    Store_field (result, 1, Val_int (w.ws_row));
    Store_field (result, 2, Val_int (w.ws_xpixel));
    Store_field (result, 3, Val_int (w.ws_ypixel));

    CAMLreturn (result);
  }

  caml_failwith("couldn't get winsize\0");
}

