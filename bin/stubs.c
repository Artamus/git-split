#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/custom.h>
#include <git2.h>

value ocaml_libgit2_version() {
    CAMLparam0();
    value ml_major, ml_minor, ml_rev;

    int major, minor, rev;
    int res = git_libgit2_version(&major, &minor, &rev);

    ml_major = Val_int(major);
    ml_minor = Val_int(minor);
    ml_rev = Val_int(rev);

    CAMLreturn(caml_alloc_3(0, ml_major, ml_minor, ml_rev));
}
