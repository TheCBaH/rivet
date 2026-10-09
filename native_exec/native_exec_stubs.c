/* Memory and call primitives for native_exec.ml. Each stub does one system
   operation and nothing else; the ordering that keeps pages from ever being
   writable and executable at once lives on the OCaml side, where it can be
   observed by tests. Addresses cross the boundary as int64. */

#define _GNU_SOURCE
#include <dlfcn.h>
#include <errno.h>
#include <stdint.h>
#include <string.h>
#include <sys/mman.h>
#include <unistd.h>

#include <caml/alloc.h>
#include <caml/bigarray.h>
#include <caml/fail.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>

#define ADDR(v) ((void *)(uintptr_t)Int64_val(v))

static void fail_errno(const char *what)
{
  char msg[256];
  int err = errno;
  strncpy(msg, what, sizeof msg - 1);
  msg[sizeof msg - 1] = '\0';
  strncat(msg, ": ", sizeof msg - strlen(msg) - 1);
  strncat(msg, strerror(err), sizeof msg - strlen(msg) - 1);
  caml_failwith(msg);
}

/* The ISA this process runs, named as the assembler's targets name
   themselves, or "" for a host no target matches. */
value native_exec_host_isa(value unit)
{
  (void)unit;
#if defined(__aarch64__)
  return caml_copy_string("aarch64");
#elif defined(__x86_64__)
  return caml_copy_string("x86_64");
#elif defined(__riscv) && __riscv_xlen == 64
  return caml_copy_string("riscv64");
#else
  return caml_copy_string("");
#endif
}

value native_exec_page_size(value unit)
{
  (void)unit;
  return Val_long(sysconf(_SC_PAGESIZE));
}

/* A fresh private anonymous mapping, readable and writable, zero-filled. */
value native_exec_map(value size)
{
  CAMLparam1(size);
  void *p = mmap(NULL, (size_t)Long_val(size), PROT_READ | PROT_WRITE,
                 MAP_PRIVATE | MAP_ANONYMOUS, -1, 0);
  if (p == MAP_FAILED) fail_errno("mmap");
  CAMLreturn(caml_copy_int64((int64_t)(uintptr_t)p));
}

value native_exec_unmap(value addr, value size)
{
  CAMLparam2(addr, size);
  if (munmap(ADDR(addr), (size_t)Long_val(size)) != 0) fail_errno("munmap");
  CAMLreturn(Val_unit);
}

/* [prot] bits: 1 read, 2 write, 4 execute. */
value native_exec_protect(value addr, value size, value prot)
{
  CAMLparam3(addr, size, prot);
  int bits = Int_val(prot);
  int p = ((bits & 1) ? PROT_READ : 0) | ((bits & 2) ? PROT_WRITE : 0) |
          ((bits & 4) ? PROT_EXEC : 0);
  if (mprotect(ADDR(addr), (size_t)Long_val(size), p) != 0) fail_errno("mprotect");
  CAMLreturn(Val_unit);
}

value native_exec_clear_cache(value addr, value size)
{
  CAMLparam2(addr, size);
  char *start = ADDR(addr);
  __builtin___clear_cache(start, start + Long_val(size));
  CAMLreturn(Val_unit);
}

value native_exec_copy_in(value addr, value bytes)
{
  CAMLparam2(addr, bytes);
  memcpy(ADDR(addr), String_val(bytes), caml_string_length(bytes));
  CAMLreturn(Val_unit);
}

value native_exec_copy_out(value addr, value len)
{
  CAMLparam2(addr, len);
  CAMLlocal1(s);
  s = caml_alloc_string((mlsize_t)Long_val(len));
  memcpy((char *)Bytes_val(s), ADDR(addr), (size_t)Long_val(len));
  CAMLreturn(s);
}

/* Calls [long entry(void *io)] at [addr], passing the bigarray's data, which
   lives outside the OCaml heap and does not move. Generated code does not
   call back into OCaml, so no runtime interaction happens during the call. */
value native_exec_call(value addr, value io)
{
  CAMLparam2(addr, io);
  long (*entry)(void *) = (long (*)(void *))(uintptr_t)Int64_val(addr);
  long r = entry(Caml_ba_data_val(io));
  CAMLreturn(caml_copy_int64((int64_t)r));
}

/* The address of a bigarray's data: outside the OCaml heap, so it does not move
   while the bigarray is alive. */
value native_exec_io_address(value io)
{
  CAMLparam1(io);
  CAMLreturn(caml_copy_int64((int64_t)(uintptr_t)Caml_ba_data_val(io)));
}

/* The address of [name] in this process's global symbol scope, or 0. */
value native_exec_symbol(value name)
{
  CAMLparam1(name);
  void *p = dlsym(RTLD_DEFAULT, String_val(name));
  CAMLreturn(caml_copy_int64((int64_t)(uintptr_t)p));
}
