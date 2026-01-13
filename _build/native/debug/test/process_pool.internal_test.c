#ifdef __cplusplus
extern "C" {
#endif

#include "moonbit.h"

#ifdef _MSC_VER
#define _Noreturn __declspec(noreturn)
#endif

#if defined(__clang__)
#pragma clang diagnostic ignored "-Wshift-op-parentheses"
#pragma clang diagnostic ignored "-Wtautological-compare"
#endif

MOONBIT_EXPORT _Noreturn void moonbit_panic(void);
MOONBIT_EXPORT void *moonbit_malloc_array(enum moonbit_block_kind kind,
                                          int elem_size_shift, int32_t len);
MOONBIT_EXPORT int moonbit_val_array_equal(const void *lhs, const void *rhs);
MOONBIT_EXPORT moonbit_string_t moonbit_add_string(moonbit_string_t s1,
                                                   moonbit_string_t s2);
MOONBIT_EXPORT void moonbit_unsafe_bytes_blit(moonbit_bytes_t dst,
                                              int32_t dst_start,
                                              moonbit_bytes_t src,
                                              int32_t src_offset, int32_t len);
MOONBIT_EXPORT moonbit_string_t moonbit_unsafe_bytes_sub_string(
    moonbit_bytes_t bytes, int32_t start, int32_t len);
MOONBIT_EXPORT void moonbit_println(moonbit_string_t str);
MOONBIT_EXPORT moonbit_bytes_t *moonbit_get_cli_args(void);
MOONBIT_EXPORT void moonbit_runtime_init(int argc, char **argv);
MOONBIT_EXPORT void moonbit_drop_object(void *);

#define Moonbit_make_regular_object_header(ptr_field_offset, ptr_field_count,  \
                                           tag)                                \
  (((uint32_t)moonbit_BLOCK_KIND_REGULAR << 30) |                              \
   (((uint32_t)(ptr_field_offset) & (((uint32_t)1 << 11) - 1)) << 19) |        \
   (((uint32_t)(ptr_field_count) & (((uint32_t)1 << 11) - 1)) << 8) |          \
   ((tag) & 0xFF))

// header manipulation macros
#define Moonbit_object_ptr_field_offset(obj)                                   \
  ((Moonbit_object_header(obj)->meta >> 19) & (((uint32_t)1 << 11) - 1))

#define Moonbit_object_ptr_field_count(obj)                                    \
  ((Moonbit_object_header(obj)->meta >> 8) & (((uint32_t)1 << 11) - 1))

#if !defined(_WIN64) && !defined(_WIN32)
void *malloc(size_t size);
void free(void *ptr);
#define libc_malloc malloc
#define libc_free free
#endif

// several important runtime functions are inlined
static void *moonbit_malloc_inlined(size_t size) {
  struct moonbit_object *ptr = (struct moonbit_object *)libc_malloc(
      sizeof(struct moonbit_object) + size);
  ptr->rc = 1;
  return ptr + 1;
}

#define moonbit_malloc(obj) moonbit_malloc_inlined(obj)
#define moonbit_free(obj) libc_free(Moonbit_object_header(obj))

static void moonbit_incref_inlined(void *ptr) {
  struct moonbit_object *header = Moonbit_object_header(ptr);
  int32_t const count = header->rc;
  if (count > 0) {
    header->rc = count + 1;
  }
}

#define moonbit_incref moonbit_incref_inlined

static void moonbit_decref_inlined(void *ptr) {
  struct moonbit_object *header = Moonbit_object_header(ptr);
  int32_t const count = header->rc;
  if (count > 1) {
    header->rc = count - 1;
  } else if (count == 1) {
    moonbit_drop_object(ptr);
  }
}

#define moonbit_decref moonbit_decref_inlined

#define moonbit_unsafe_make_string moonbit_make_string

// detect whether compiler builtins exist for advanced bitwise operations
#ifdef __has_builtin

#if __has_builtin(__builtin_clz)
#define HAS_BUILTIN_CLZ
#endif

#if __has_builtin(__builtin_ctz)
#define HAS_BUILTIN_CTZ
#endif

#if __has_builtin(__builtin_popcount)
#define HAS_BUILTIN_POPCNT
#endif

#if __has_builtin(__builtin_sqrt)
#define HAS_BUILTIN_SQRT
#endif

#if __has_builtin(__builtin_sqrtf)
#define HAS_BUILTIN_SQRTF
#endif

#if __has_builtin(__builtin_fabs)
#define HAS_BUILTIN_FABS
#endif

#if __has_builtin(__builtin_fabsf)
#define HAS_BUILTIN_FABSF
#endif

#endif

// if there is no builtin operators, use software implementation
#ifdef HAS_BUILTIN_CLZ
static inline int32_t moonbit_clz32(int32_t x) {
  return x == 0 ? 32 : __builtin_clz(x);
}

static inline int32_t moonbit_clz64(int64_t x) {
  return x == 0 ? 64 : __builtin_clzll(x);
}

#undef HAS_BUILTIN_CLZ
#else
// table for [clz] value of 4bit integer.
static const uint8_t moonbit_clz4[] = {4, 3, 2, 2, 1, 1, 1, 1,
                                       0, 0, 0, 0, 0, 0, 0, 0};

int32_t moonbit_clz32(uint32_t x) {
  /* The ideas is to:

     1. narrow down the 4bit block where the most signficant "1" bit lies,
        using binary search
     2. find the number of leading zeros in that 4bit block via table lookup

     Different time/space tradeoff can be made here by enlarging the table
     and do less binary search.
     One benefit of the 4bit lookup table is that it can fit into a single cache
     line.
  */
  int32_t result = 0;
  if (x > 0xffff) {
    x >>= 16;
  } else {
    result += 16;
  }
  if (x > 0xff) {
    x >>= 8;
  } else {
    result += 8;
  }
  if (x > 0xf) {
    x >>= 4;
  } else {
    result += 4;
  }
  return result + moonbit_clz4[x];
}

int32_t moonbit_clz64(uint64_t x) {
  int32_t result = 0;
  if (x > 0xffffffff) {
    x >>= 32;
  } else {
    result += 32;
  }
  return result + moonbit_clz32((uint32_t)x);
}
#endif

#ifdef HAS_BUILTIN_CTZ
static inline int32_t moonbit_ctz32(int32_t x) {
  return x == 0 ? 32 : __builtin_ctz(x);
}

static inline int32_t moonbit_ctz64(int64_t x) {
  return x == 0 ? 64 : __builtin_ctzll(x);
}

#undef HAS_BUILTIN_CTZ
#else
int32_t moonbit_ctz32(int32_t x) {
  /* The algorithm comes from:

       Leiserson, Charles E. et al. “Using de Bruijn Sequences to Index a 1 in a
     Computer Word.” (1998).

     The ideas is:

     1. leave only the least significant "1" bit in the input,
        set all other bits to "0". This is achieved via [x & -x]
     2. now we have [x * n == n << ctz(x)], if [n] is a de bruijn sequence
        (every 5bit pattern occurn exactly once when you cycle through the bit
     string), we can find [ctz(x)] from the most significant 5 bits of [x * n]
 */
  static const uint32_t de_bruijn_32 = 0x077CB531;
  static const uint8_t index32[] = {0,  1,  28, 2,  29, 14, 24, 3,  30, 22, 20,
                                    15, 25, 17, 4,  8,  31, 27, 13, 23, 21, 19,
                                    16, 7,  26, 12, 18, 6,  11, 5,  10, 9};
  return (x == 0) * 32 + index32[(de_bruijn_32 * (x & -x)) >> 27];
}

int32_t moonbit_ctz64(int64_t x) {
  static const uint64_t de_bruijn_64 = 0x0218A392CD3D5DBF;
  static const uint8_t index64[] = {
      0,  1,  2,  7,  3,  13, 8,  19, 4,  25, 14, 28, 9,  34, 20, 40,
      5,  17, 26, 38, 15, 46, 29, 48, 10, 31, 35, 54, 21, 50, 41, 57,
      63, 6,  12, 18, 24, 27, 33, 39, 16, 37, 45, 47, 30, 53, 49, 56,
      62, 11, 23, 32, 36, 44, 52, 55, 61, 22, 43, 51, 60, 42, 59, 58};
  return (x == 0) * 64 + index64[(de_bruijn_64 * (x & -x)) >> 58];
}
#endif

#ifdef HAS_BUILTIN_POPCNT

#define moonbit_popcnt32 __builtin_popcount
#define moonbit_popcnt64 __builtin_popcountll
#undef HAS_BUILTIN_POPCNT

#else
int32_t moonbit_popcnt32(uint32_t x) {
  /* The classic SIMD Within A Register algorithm.
     ref: [https://nimrod.blog/posts/algorithms-behind-popcount/]
 */
  x = x - ((x >> 1) & 0x55555555);
  x = (x & 0x33333333) + ((x >> 2) & 0x33333333);
  x = (x + (x >> 4)) & 0x0F0F0F0F;
  return (x * 0x01010101) >> 24;
}

int32_t moonbit_popcnt64(uint64_t x) {
  x = x - ((x >> 1) & 0x5555555555555555);
  x = (x & 0x3333333333333333) + ((x >> 2) & 0x3333333333333333);
  x = (x + (x >> 4)) & 0x0F0F0F0F0F0F0F0F;
  return (x * 0x0101010101010101) >> 56;
}
#endif

/* The following sqrt implementation comes from
   [musl](https://git.musl-libc.org/cgit/musl),
   with some helpers inlined to make it zero dependency.
 */
#ifdef MOONBIT_NATIVE_NO_SYS_HEADER
const uint16_t __rsqrt_tab[128] = {
    0xb451, 0xb2f0, 0xb196, 0xb044, 0xaef9, 0xadb6, 0xac79, 0xab43, 0xaa14,
    0xa8eb, 0xa7c8, 0xa6aa, 0xa592, 0xa480, 0xa373, 0xa26b, 0xa168, 0xa06a,
    0x9f70, 0x9e7b, 0x9d8a, 0x9c9d, 0x9bb5, 0x9ad1, 0x99f0, 0x9913, 0x983a,
    0x9765, 0x9693, 0x95c4, 0x94f8, 0x9430, 0x936b, 0x92a9, 0x91ea, 0x912e,
    0x9075, 0x8fbe, 0x8f0a, 0x8e59, 0x8daa, 0x8cfe, 0x8c54, 0x8bac, 0x8b07,
    0x8a64, 0x89c4, 0x8925, 0x8889, 0x87ee, 0x8756, 0x86c0, 0x862b, 0x8599,
    0x8508, 0x8479, 0x83ec, 0x8361, 0x82d8, 0x8250, 0x81c9, 0x8145, 0x80c2,
    0x8040, 0xff02, 0xfd0e, 0xfb25, 0xf947, 0xf773, 0xf5aa, 0xf3ea, 0xf234,
    0xf087, 0xeee3, 0xed47, 0xebb3, 0xea27, 0xe8a3, 0xe727, 0xe5b2, 0xe443,
    0xe2dc, 0xe17a, 0xe020, 0xdecb, 0xdd7d, 0xdc34, 0xdaf1, 0xd9b3, 0xd87b,
    0xd748, 0xd61a, 0xd4f1, 0xd3cd, 0xd2ad, 0xd192, 0xd07b, 0xcf69, 0xce5b,
    0xcd51, 0xcc4a, 0xcb48, 0xca4a, 0xc94f, 0xc858, 0xc764, 0xc674, 0xc587,
    0xc49d, 0xc3b7, 0xc2d4, 0xc1f4, 0xc116, 0xc03c, 0xbf65, 0xbe90, 0xbdbe,
    0xbcef, 0xbc23, 0xbb59, 0xba91, 0xb9cc, 0xb90a, 0xb84a, 0xb78c, 0xb6d0,
    0xb617, 0xb560,
};

/* returns a*b*2^-32 - e, with error 0 <= e < 1.  */
static inline uint32_t mul32(uint32_t a, uint32_t b) {
  return (uint64_t)a * b >> 32;
}
#endif

#ifdef MOONBIT_NATIVE_NO_SYS_HEADER
float sqrtf(float x) {
  uint32_t ix, m, m1, m0, even, ey;

  ix = *(uint32_t *)&x;
  if (ix - 0x00800000 >= 0x7f800000 - 0x00800000) {
    /* x < 0x1p-126 or inf or nan.  */
    if (ix * 2 == 0)
      return x;
    if (ix == 0x7f800000)
      return x;
    if (ix > 0x7f800000)
      return (x - x) / (x - x);
    /* x is subnormal, normalize it.  */
    x *= 0x1p23f;
    ix = *(uint32_t *)&x;
    ix -= 23 << 23;
  }

  /* x = 4^e m; with int e and m in [1, 4).  */
  even = ix & 0x00800000;
  m1 = (ix << 8) | 0x80000000;
  m0 = (ix << 7) & 0x7fffffff;
  m = even ? m0 : m1;

  /* 2^e is the exponent part of the return value.  */
  ey = ix >> 1;
  ey += 0x3f800000 >> 1;
  ey &= 0x7f800000;

  /* compute r ~ 1/sqrt(m), s ~ sqrt(m) with 2 goldschmidt iterations.  */
  static const uint32_t three = 0xc0000000;
  uint32_t r, s, d, u, i;
  i = (ix >> 17) % 128;
  r = (uint32_t)__rsqrt_tab[i] << 16;
  /* |r*sqrt(m) - 1| < 0x1p-8 */
  s = mul32(m, r);
  /* |s/sqrt(m) - 1| < 0x1p-8 */
  d = mul32(s, r);
  u = three - d;
  r = mul32(r, u) << 1;
  /* |r*sqrt(m) - 1| < 0x1.7bp-16 */
  s = mul32(s, u) << 1;
  /* |s/sqrt(m) - 1| < 0x1.7bp-16 */
  d = mul32(s, r);
  u = three - d;
  s = mul32(s, u);
  /* -0x1.03p-28 < s/sqrt(m) - 1 < 0x1.fp-31 */
  s = (s - 1) >> 6;
  /* s < sqrt(m) < s + 0x1.08p-23 */

  /* compute nearest rounded result.  */
  uint32_t d0, d1, d2;
  float y, t;
  d0 = (m << 16) - s * s;
  d1 = s - d0;
  d2 = d1 + s + 1;
  s += d1 >> 31;
  s &= 0x007fffff;
  s |= ey;
  y = *(float *)&s;
  /* handle rounding and inexact exception. */
  uint32_t tiny = d2 == 0 ? 0 : 0x01000000;
  tiny |= (d1 ^ d2) & 0x80000000;
  t = *(float *)&tiny;
  y = y + t;
  return y;
}
#endif

#ifdef MOONBIT_NATIVE_NO_SYS_HEADER
/* returns a*b*2^-64 - e, with error 0 <= e < 3.  */
static inline uint64_t mul64(uint64_t a, uint64_t b) {
  uint64_t ahi = a >> 32;
  uint64_t alo = a & 0xffffffff;
  uint64_t bhi = b >> 32;
  uint64_t blo = b & 0xffffffff;
  return ahi * bhi + (ahi * blo >> 32) + (alo * bhi >> 32);
}

double sqrt(double x) {
  uint64_t ix, top, m;

  /* special case handling.  */
  ix = *(uint64_t *)&x;
  top = ix >> 52;
  if (top - 0x001 >= 0x7ff - 0x001) {
    /* x < 0x1p-1022 or inf or nan.  */
    if (ix * 2 == 0)
      return x;
    if (ix == 0x7ff0000000000000)
      return x;
    if (ix > 0x7ff0000000000000)
      return (x - x) / (x - x);
    /* x is subnormal, normalize it.  */
    x *= 0x1p52;
    ix = *(uint64_t *)&x;
    top = ix >> 52;
    top -= 52;
  }

  /* argument reduction:
     x = 4^e m; with integer e, and m in [1, 4)
     m: fixed point representation [2.62]
     2^e is the exponent part of the result.  */
  int even = top & 1;
  m = (ix << 11) | 0x8000000000000000;
  if (even)
    m >>= 1;
  top = (top + 0x3ff) >> 1;

  /* approximate r ~ 1/sqrt(m) and s ~ sqrt(m) when m in [1,4)

     initial estimate:
     7bit table lookup (1bit exponent and 6bit significand).

     iterative approximation:
     using 2 goldschmidt iterations with 32bit int arithmetics
     and a final iteration with 64bit int arithmetics.

     details:

     the relative error (e = r0 sqrt(m)-1) of a linear estimate
     (r0 = a m + b) is |e| < 0.085955 ~ 0x1.6p-4 at best,
     a table lookup is faster and needs one less iteration
     6 bit lookup table (128b) gives |e| < 0x1.f9p-8
     7 bit lookup table (256b) gives |e| < 0x1.fdp-9
     for single and double prec 6bit is enough but for quad
     prec 7bit is needed (or modified iterations). to avoid
     one more iteration >=13bit table would be needed (16k).

     a newton-raphson iteration for r is
       w = r*r
       u = 3 - m*w
       r = r*u/2
     can use a goldschmidt iteration for s at the end or
       s = m*r

     first goldschmidt iteration is
       s = m*r
       u = 3 - s*r
       r = r*u/2
       s = s*u/2
     next goldschmidt iteration is
       u = 3 - s*r
       r = r*u/2
       s = s*u/2
     and at the end r is not computed only s.

     they use the same amount of operations and converge at the
     same quadratic rate, i.e. if
       r1 sqrt(m) - 1 = e, then
       r2 sqrt(m) - 1 = -3/2 e^2 - 1/2 e^3
     the advantage of goldschmidt is that the mul for s and r
     are independent (computed in parallel), however it is not
     "self synchronizing": it only uses the input m in the
     first iteration so rounding errors accumulate. at the end
     or when switching to larger precision arithmetics rounding
     errors dominate so the first iteration should be used.

     the fixed point representations are
       m: 2.30 r: 0.32, s: 2.30, d: 2.30, u: 2.30, three: 2.30
     and after switching to 64 bit
       m: 2.62 r: 0.64, s: 2.62, d: 2.62, u: 2.62, three: 2.62  */

  static const uint64_t three = 0xc0000000;
  uint64_t r, s, d, u, i;

  i = (ix >> 46) % 128;
  r = (uint32_t)__rsqrt_tab[i] << 16;
  /* |r sqrt(m) - 1| < 0x1.fdp-9 */
  s = mul32(m >> 32, r);
  /* |s/sqrt(m) - 1| < 0x1.fdp-9 */
  d = mul32(s, r);
  u = three - d;
  r = mul32(r, u) << 1;
  /* |r sqrt(m) - 1| < 0x1.7bp-16 */
  s = mul32(s, u) << 1;
  /* |s/sqrt(m) - 1| < 0x1.7bp-16 */
  d = mul32(s, r);
  u = three - d;
  r = mul32(r, u) << 1;
  /* |r sqrt(m) - 1| < 0x1.3704p-29 (measured worst-case) */
  r = r << 32;
  s = mul64(m, r);
  d = mul64(s, r);
  u = (three << 32) - d;
  s = mul64(s, u); /* repr: 3.61 */
  /* -0x1p-57 < s - sqrt(m) < 0x1.8001p-61 */
  s = (s - 2) >> 9; /* repr: 12.52 */
  /* -0x1.09p-52 < s - sqrt(m) < -0x1.fffcp-63 */

  /* s < sqrt(m) < s + 0x1.09p-52,
     compute nearest rounded result:
     the nearest result to 52 bits is either s or s+0x1p-52,
     we can decide by comparing (2^52 s + 0.5)^2 to 2^104 m.  */
  uint64_t d0, d1, d2;
  double y, t;
  d0 = (m << 42) - s * s;
  d1 = s - d0;
  d2 = d1 + s + 1;
  s += d1 >> 63;
  s &= 0x000fffffffffffff;
  s |= top << 52;
  y = *(double *)&s;
  return y;
}
#endif

#ifdef MOONBIT_NATIVE_NO_SYS_HEADER
double fabs(double x) {
  union {
    double f;
    uint64_t i;
  } u = {x};
  u.i &= 0x7fffffffffffffffULL;
  return u.f;
}
#endif

#ifdef MOONBIT_NATIVE_NO_SYS_HEADER
float fabsf(float x) {
  union {
    float f;
    uint32_t i;
  } u = {x};
  u.i &= 0x7fffffff;
  return u.f;
}
#endif

#ifdef _MSC_VER
/* MSVC treats syntactic division by zero as fatal error,
   even for float point numbers,
   so we have to use a constant variable to work around this */
static const int MOONBIT_ZERO = 0;
#else
#define MOONBIT_ZERO 0
#endif

#ifdef __cplusplus
}
#endif
struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$;

struct $$3c$String$2a$Int$3e$;

struct $$moonbitlang$core$builtin$Array$3c$String$3e$;

struct $$moonbitlang$core$builtin$ArrayView$3c$$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$$3e$;

struct $$mizchi$process_pool$moonbit_test_driver_internal_do_execute$fn$12$2d$cap;

struct $$mizchi$process_pool$moonbit_test_driver_internal_do_execute$fn$11$2d$cap;

struct $$3c$Unit$3e$$3d$$3e$Unit;

struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$;

struct $$mizchi$process_pool$moonbit_test_driver_internal_do_execute$handle_result$53$test_should_be_skipped$54$on_err$55$$2a$arm$290$$2a$arm$283$lambda$427$State$$2a$try$273;

struct $$moonbitlang$core$builtin$ArrayView$3c$$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$$3e$;

struct $Ref$3c$Int$3e$;

struct $StringView;

struct $$mizchi$process_pool$moonbit_test_driver_internal_do_execute$handle_result$53$test_should_be_skipped$54$on_err$55$$2a$arm$268$$2a$arm$261$lambda$410$State$State_1;

struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$;

struct $$mizchi$process_pool$moonbit_test_driver_internal_do_execute$handle_result$53$test_should_be_skipped$54$on_err$55$$2a$arm$268$$2a$arm$261$lambda$410$State$$2a$try$251;

struct $$3c$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit;

struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$;

struct $Result$3c$Unit$2a$Error$3e$$Ok;

struct $$mizchi$process_pool$moonbit_test_driver_internal_do_execute$fn$9$2d$cap;

struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$;

struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$async$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$;

struct $$mizchi$process_pool$moonbit_test_driver_internal_do_execute$fn$15$2d$cap;

struct $$mizchi$process_pool$moonbit_test_driver_internal_do_execute$fn$13$2d$cap;

struct $$3c$StringView$2a$Option$3c$StringView$3e$$3e$;

struct $$3c$Int$2a$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit;

struct $$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$;

struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$;

struct $$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$;

struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$;

struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$;

struct $$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$;

struct $$3c$String$2a$String$2a$Bool$3e$$3d$$3e$Unit;

struct $$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$;

struct $$mizchi$process_pool$moonbit_test_driver_internal_do_execute$fn$8$2d$cap;

struct $$moonbitlang$core$builtin$Logger;

struct $$moonbitlang$core$builtin$Array$3c$$3c$String$2a$Int$3e$$3e$;

struct $$mizchi$process_pool$moonbit_test_driver_internal_do_execute$on_err$fn$3$2d$cap;

struct $$mizchi$process_pool$moonbit_test_driver_internal_do_execute$handle_result$fn$1$2d$cap;

struct $$3c$$3e$$3d$$3e$Unit;

struct $$3c$Int$2a$Int$3e$;

struct $Error$moonbitlang$core$builtin$Failure$Failure;

struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$;

struct $$3c$$3c$Int$2a$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$;

struct $$moonbitlang$core$builtin$StringBuilder$as_$moonbitlang$core$builtin$Logger;

struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$;

struct $$mizchi$process_pool$moonbit_test_driver_internal_do_execute$fn$16$2d$cap;

struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$;

struct $$3c$$3c$Int$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$;

struct $$mizchi$process_pool$moonbit_test_driver_internal_do_execute$handle_result$53$test_should_be_skipped$54$on_err$55$$2a$arm$290$$2a$arm$283$lambda$427$State$State_1;

struct $$moonbitlang$core$builtin$ArrayView$3c$$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$;

struct $$moonbitlang$core$builtin$ArrayView$3c$$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$$3e$;

struct $$mizchi$process_pool$moonbit_test_driver_internal_do_execute$fn$5$2d$cap;

struct $Result$3c$StringView$2a$$moonbitlang$core$builtin$CreatingViewError$3e$$Err;

struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$;

struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$;

struct $$3c$Error$3e$$3d$$3e$Unit;

struct $$3c$String$2a$Error$3e$$3d$$3e$Unit;

struct $Error$moonbitlang$core$builtin$InspectError$InspectError;

struct $$moonbitlang$core$builtin$Hasher;

struct $$mizchi$process_pool$moonbit_test_driver_internal_do_execute$fn$7$2d$cap;

struct $$mizchi$process_pool$moonbit_test_driver_internal_do_execute$test_should_be_skipped$fn$2$2d$cap;

struct $$moonbitlang$core$builtin$Array$3c$async$3c$$3e$$3d$$3e$Unit$21$Error$3e$;

struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$;

struct $Error$moonbitlang$core$builtin$SnapshotError$SnapshotError;

struct $$moonbitlang$core$builtin$ArrayView$3c$$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$$3e$;

struct $$mizchi$process_pool$moonbit_test_driver_internal_do_execute$fn$4$2d$cap;

struct $Error$mizchi$process_pool$MoonBitTestDriverInternalJsError$MoonBitTestDriverInternalJsError;

struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$;

struct $Result$3c$Unit$2a$Error$3e$$Err;

struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$;

struct $Result$3c$StringView$2a$$moonbitlang$core$builtin$CreatingViewError$3e$$Ok;

struct $$moonbitlang$core$builtin$SourceLocRepr;

struct $$mizchi$process_pool$moonbit_test_driver_internal_do_execute$fn$6$2d$cap;

struct $Option$3c$StringView$3e$$Some;

struct $$moonbitlang$core$builtin$Logger$static_method_table;

struct $$moonbitlang$core$builtin$StringBuilder;

struct $$3c$Int$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$;

struct $$3c$$3c$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$;

struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$;

struct $$3c$String$2a$$moonbitlang$core$builtin$Logger$3e$;

struct $$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$;

struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$ {
  int32_t $1;
  int32_t $2;
  int32_t $3;
  int32_t $4;
  int32_t $6;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** $0;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* $5;
  
};

struct $$3c$String$2a$Int$3e$ {
  int32_t $1;
  moonbit_string_t $0;
  
};

struct $$moonbitlang$core$builtin$Array$3c$String$3e$ {
  int32_t $1;
  moonbit_string_t* $0;
  
};

struct $$moonbitlang$core$builtin$ArrayView$3c$$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$$3e$ {
  int32_t $1;
  int32_t $2;
  struct $$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** $0;
  
};

struct $$mizchi$process_pool$moonbit_test_driver_internal_do_execute$fn$12$2d$cap {
  int32_t(* code)(struct $$3c$Unit$3e$$3d$$3e$Unit*, int32_t);
  int32_t $0;
  struct $$3c$Unit$3e$$3d$$3e$Unit* $1;
  moonbit_string_t $2;
  struct $$3c$String$2a$String$2a$Bool$3e$$3d$$3e$Unit* $3;
  
};

struct $$mizchi$process_pool$moonbit_test_driver_internal_do_execute$fn$11$2d$cap {
  int32_t(* code)(struct $$3c$Error$3e$$3d$$3e$Unit*, void*);
  int32_t $0;
  struct $$3c$Error$3e$$3d$$3e$Unit* $1;
  struct $$3c$Unit$3e$$3d$$3e$Unit* $2;
  moonbit_string_t $3;
  struct $$3c$String$2a$Error$3e$$3d$$3e$Unit* $4;
  
};

struct $$3c$Unit$3e$$3d$$3e$Unit {
  int32_t(* code)(struct $$3c$Unit$3e$$3d$$3e$Unit*, int32_t);
  
};

struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$ {
  int32_t $0;
  int32_t $2;
  int32_t $3;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* $1;
  moonbit_string_t $4;
  struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* $5;
  
};

struct $$mizchi$process_pool$moonbit_test_driver_internal_do_execute$handle_result$53$test_should_be_skipped$54$on_err$55$$2a$arm$290$$2a$arm$283$lambda$427$State$$2a$try$273 {
  void* $0;
  struct $$3c$String$2a$Error$3e$$3d$$3e$Unit* $1;
  moonbit_string_t $2;
  struct $$3c$Unit$3e$$3d$$3e$Unit* $3;
  struct $$3c$Error$3e$$3d$$3e$Unit* $4;
  
};

struct $$moonbitlang$core$builtin$ArrayView$3c$$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$$3e$ {
  int32_t $1;
  int32_t $2;
  struct $$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** $0;
  
};

struct $Ref$3c$Int$3e$ {
  int32_t $0;
  
};

struct $StringView {
  int32_t $1;
  int32_t $2;
  moonbit_string_t $0;
  
};

struct $$mizchi$process_pool$moonbit_test_driver_internal_do_execute$handle_result$53$test_should_be_skipped$54$on_err$55$$2a$arm$268$$2a$arm$261$lambda$410$State$State_1 {
  int32_t $0;
  struct $$3c$String$2a$String$2a$Bool$3e$$3d$$3e$Unit* $1;
  moonbit_string_t $2;
  struct $$3c$Unit$3e$$3d$$3e$Unit* $3;
  
};

struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$ {
  int32_t $1;
  int32_t $2;
  int32_t $3;
  int32_t $4;
  int32_t $6;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** $0;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* $5;
  
};

struct $$mizchi$process_pool$moonbit_test_driver_internal_do_execute$handle_result$53$test_should_be_skipped$54$on_err$55$$2a$arm$268$$2a$arm$261$lambda$410$State$$2a$try$251 {
  void* $0;
  struct $$3c$String$2a$Error$3e$$3d$$3e$Unit* $1;
  moonbit_string_t $2;
  struct $$3c$Unit$3e$$3d$$3e$Unit* $3;
  struct $$3c$Error$3e$$3d$$3e$Unit* $4;
  
};

struct $$3c$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit {
  int32_t(* code)(
    struct $$3c$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit*,
    struct $$3c$Unit$3e$$3d$$3e$Unit*,
    struct $$3c$Error$3e$$3d$$3e$Unit*
  );
  
};

struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$ {
  int32_t $0;
  int32_t $2;
  int32_t $3;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* $1;
  moonbit_string_t $4;
  struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* $5;
  
};

struct $Result$3c$Unit$2a$Error$3e$$Ok {
  int32_t $0;
  
};

struct $$mizchi$process_pool$moonbit_test_driver_internal_do_execute$fn$9$2d$cap {
  int32_t(* code)(
    struct $$3c$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit*,
    struct $$3c$Unit$3e$$3d$$3e$Unit*,
    struct $$3c$Error$3e$$3d$$3e$Unit*
  );
  moonbit_string_t $0;
  struct $$3c$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit* $1;
  struct $$3c$String$2a$Error$3e$$3d$$3e$Unit* $2;
  struct $$3c$String$2a$String$2a$Bool$3e$$3d$$3e$Unit* $3;
  
};

struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$ {
  int32_t $0;
  int32_t $2;
  int32_t $3;
  int32_t $4;
  struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* $1;
  struct $$3c$$3c$Int$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* $5;
  
};

struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$async$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$ {
  int32_t $0;
  int32_t $2;
  int32_t $3;
  int32_t $4;
  struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$async$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* $1;
  struct $$3c$$3c$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* $5;
  
};

struct $$mizchi$process_pool$moonbit_test_driver_internal_do_execute$fn$15$2d$cap {
  int32_t(* code)(struct $$3c$Error$3e$$3d$$3e$Unit*, void*);
  int32_t $0;
  struct $$3c$Error$3e$$3d$$3e$Unit* $1;
  struct $$3c$Unit$3e$$3d$$3e$Unit* $2;
  moonbit_string_t $3;
  struct $$3c$String$2a$Error$3e$$3d$$3e$Unit* $4;
  
};

struct $$mizchi$process_pool$moonbit_test_driver_internal_do_execute$fn$13$2d$cap {
  int32_t(* code)(
    struct $$3c$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit*,
    struct $$3c$Unit$3e$$3d$$3e$Unit*,
    struct $$3c$Error$3e$$3d$$3e$Unit*
  );
  moonbit_string_t $0;
  struct $$3c$Int$2a$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit* $1;
  struct $$3c$String$2a$Error$3e$$3d$$3e$Unit* $2;
  struct $$3c$String$2a$String$2a$Bool$3e$$3d$$3e$Unit* $3;
  
};

struct $$3c$StringView$2a$Option$3c$StringView$3e$$3e$ {
  int32_t $0_1;
  int32_t $0_2;
  moonbit_string_t $0_0;
  void* $1;
  
};

struct $$3c$Int$2a$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit {
  int32_t(* code)(
    struct $$3c$Int$2a$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit*,
    int32_t,
    struct $$3c$Unit$3e$$3d$$3e$Unit*,
    struct $$3c$Error$3e$$3d$$3e$Unit*
  );
  
};

struct $$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$ {
  int32_t $0;
  struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* $1;
  
};

struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$ {
  struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$* $0;
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* $1;
  
};

struct $$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$ {
  moonbit_string_t $0;
  struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* $1;
  
};

struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$ {
  int32_t $1;
  int32_t $2;
  int32_t $3;
  int32_t $4;
  int32_t $6;
  struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$async$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$** $0;
  struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$async$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* $5;
  
};

struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$ {
  int32_t $1;
  int32_t $2;
  int32_t $3;
  int32_t $4;
  int32_t $6;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** $0;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* $5;
  
};

struct $$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$ {
  moonbit_string_t $0;
  struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* $1;
  
};

struct $$3c$String$2a$String$2a$Bool$3e$$3d$$3e$Unit {
  int32_t(* code)(
    struct $$3c$String$2a$String$2a$Bool$3e$$3d$$3e$Unit*,
    moonbit_string_t,
    moonbit_string_t,
    int32_t
  );
  
};

struct $$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$ {
  moonbit_string_t $0;
  struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* $1;
  
};

struct $$mizchi$process_pool$moonbit_test_driver_internal_do_execute$fn$8$2d$cap {
  struct moonbit_result_0(* code)(
    struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$*
  );
  moonbit_string_t $0;
  struct $$3c$Int$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$* $1;
  
};

struct $$moonbitlang$core$builtin$Logger {
  struct $$moonbitlang$core$builtin$Logger$static_method_table* $0;
  void* $1;
  
};

struct $$moonbitlang$core$builtin$Array$3c$$3c$String$2a$Int$3e$$3e$ {
  int32_t $1;
  struct $$3c$String$2a$Int$3e$** $0;
  
};

struct $$mizchi$process_pool$moonbit_test_driver_internal_do_execute$on_err$fn$3$2d$cap {
  int32_t(* code)(
    struct $$3c$String$2a$Error$3e$$3d$$3e$Unit*,
    moonbit_string_t,
    void*
  );
  struct $$3c$String$2a$String$2a$Bool$3e$$3d$$3e$Unit* $0;
  
};

struct $$mizchi$process_pool$moonbit_test_driver_internal_do_execute$handle_result$fn$1$2d$cap {
  int32_t(* code)(
    struct $$3c$String$2a$String$2a$Bool$3e$$3d$$3e$Unit*,
    moonbit_string_t,
    moonbit_string_t,
    int32_t
  );
  int32_t $0;
  moonbit_string_t $1;
  
};

struct $$3c$$3e$$3d$$3e$Unit {
  int32_t(* code)(struct $$3c$$3e$$3d$$3e$Unit*);
  
};

struct $$3c$Int$2a$Int$3e$ {
  int32_t $0;
  int32_t $1;
  
};

struct $Error$moonbitlang$core$builtin$Failure$Failure {
  moonbit_string_t $0;
  
};

struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$ {
  int32_t $1;
  int32_t $2;
  int32_t $3;
  int32_t $4;
  int32_t $6;
  struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$** $0;
  struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* $5;
  
};

struct $$3c$$3c$Int$2a$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$ {
  struct $$3c$Int$2a$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit* $0;
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* $1;
  
};

struct $$moonbitlang$core$builtin$StringBuilder$as_$moonbitlang$core$builtin$Logger {
  struct $$moonbitlang$core$builtin$Logger$static_method_table* $0;
  void* $1;
  
};

struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$ {
  int32_t $0;
  int32_t $2;
  int32_t $3;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* $1;
  moonbit_string_t $4;
  struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* $5;
  
};

struct $$mizchi$process_pool$moonbit_test_driver_internal_do_execute$fn$16$2d$cap {
  int32_t(* code)(struct $$3c$Unit$3e$$3d$$3e$Unit*, int32_t);
  int32_t $0;
  struct $$3c$Unit$3e$$3d$$3e$Unit* $1;
  moonbit_string_t $2;
  struct $$3c$String$2a$String$2a$Bool$3e$$3d$$3e$Unit* $3;
  
};

struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$ {
  int32_t $0;
  int32_t $2;
  int32_t $3;
  int32_t $4;
  struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* $1;
  struct $$3c$$3c$Int$2a$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* $5;
  
};

struct $$3c$$3c$Int$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$ {
  struct $$3c$Int$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$* $0;
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* $1;
  
};

struct $$mizchi$process_pool$moonbit_test_driver_internal_do_execute$handle_result$53$test_should_be_skipped$54$on_err$55$$2a$arm$290$$2a$arm$283$lambda$427$State$State_1 {
  int32_t $0;
  struct $$3c$String$2a$String$2a$Bool$3e$$3d$$3e$Unit* $1;
  moonbit_string_t $2;
  struct $$3c$Unit$3e$$3d$$3e$Unit* $3;
  
};

struct $$moonbitlang$core$builtin$ArrayView$3c$$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$ {
  int32_t $1;
  int32_t $2;
  struct $$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$** $0;
  
};

struct $$moonbitlang$core$builtin$ArrayView$3c$$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$$3e$ {
  int32_t $1;
  int32_t $2;
  struct $$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** $0;
  
};

struct $$mizchi$process_pool$moonbit_test_driver_internal_do_execute$fn$5$2d$cap {
  int32_t(* code)(struct $$3c$$3e$$3d$$3e$Unit*);
  moonbit_string_t $0;
  struct $$3c$String$2a$String$2a$Bool$3e$$3d$$3e$Unit* $1;
  
};

struct $Result$3c$StringView$2a$$moonbitlang$core$builtin$CreatingViewError$3e$$Err {
  void* $0;
  
};

struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$ {
  int32_t $1;
  int32_t $2;
  int32_t $3;
  int32_t $4;
  int32_t $6;
  struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$** $0;
  struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* $5;
  
};

struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$ {
  int32_t $0;
  int32_t $2;
  int32_t $3;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* $1;
  moonbit_string_t $4;
  struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* $5;
  
};

struct $$3c$Error$3e$$3d$$3e$Unit {
  int32_t(* code)(struct $$3c$Error$3e$$3d$$3e$Unit*, void*);
  
};

struct $$3c$String$2a$Error$3e$$3d$$3e$Unit {
  int32_t(* code)(
    struct $$3c$String$2a$Error$3e$$3d$$3e$Unit*,
    moonbit_string_t,
    void*
  );
  
};

struct $Error$moonbitlang$core$builtin$InspectError$InspectError {
  moonbit_string_t $0;
  
};

struct $$moonbitlang$core$builtin$Hasher {
  uint32_t $0;
  
};

struct $$mizchi$process_pool$moonbit_test_driver_internal_do_execute$fn$7$2d$cap {
  int32_t(* code)(struct $$3c$$3e$$3d$$3e$Unit*);
  moonbit_string_t $0;
  struct $$3c$String$2a$String$2a$Bool$3e$$3d$$3e$Unit* $1;
  
};

struct $$mizchi$process_pool$moonbit_test_driver_internal_do_execute$test_should_be_skipped$fn$2$2d$cap {
  moonbit_string_t $0;
  struct $$3c$String$2a$String$2a$Bool$3e$$3d$$3e$Unit* $1;
  
};

struct $$moonbitlang$core$builtin$Array$3c$async$3c$$3e$$3d$$3e$Unit$21$Error$3e$ {
  int32_t $1;
  struct $$3c$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit** $0;
  
};

struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$ {
  int32_t $0;
  int32_t $2;
  int32_t $3;
  int32_t $4;
  struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* $1;
  struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* $5;
  
};

struct $Error$moonbitlang$core$builtin$SnapshotError$SnapshotError {
  moonbit_string_t $0;
  
};

struct $$moonbitlang$core$builtin$ArrayView$3c$$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$$3e$ {
  int32_t $1;
  int32_t $2;
  struct $$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** $0;
  
};

struct $$mizchi$process_pool$moonbit_test_driver_internal_do_execute$fn$4$2d$cap {
  int32_t(* code)(struct $$3c$Error$3e$$3d$$3e$Unit*, void*);
  moonbit_string_t $0;
  struct $$3c$String$2a$Error$3e$$3d$$3e$Unit* $1;
  
};

struct $Error$mizchi$process_pool$MoonBitTestDriverInternalJsError$MoonBitTestDriverInternalJsError {
  moonbit_string_t $0;
  
};

struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$ {
  int32_t $1;
  int32_t $2;
  int32_t $3;
  int32_t $4;
  int32_t $6;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** $0;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* $5;
  
};

struct $Result$3c$Unit$2a$Error$3e$$Err {
  void* $0;
  
};

struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$ {
  int32_t $1;
  int32_t $2;
  int32_t $3;
  int32_t $4;
  int32_t $6;
  struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$** $0;
  struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* $5;
  
};

struct $Result$3c$StringView$2a$$moonbitlang$core$builtin$CreatingViewError$3e$$Ok {
  int32_t $0_1;
  int32_t $0_2;
  moonbit_string_t $0_0;
  
};

struct $$moonbitlang$core$builtin$SourceLocRepr {
  int32_t $0_1;
  int32_t $0_2;
  int32_t $1_1;
  int32_t $1_2;
  int32_t $2_1;
  int32_t $2_2;
  int32_t $3_1;
  int32_t $3_2;
  int32_t $4_1;
  int32_t $4_2;
  int32_t $5_1;
  int32_t $5_2;
  moonbit_string_t $0_0;
  moonbit_string_t $1_0;
  moonbit_string_t $2_0;
  moonbit_string_t $3_0;
  moonbit_string_t $4_0;
  moonbit_string_t $5_0;
  
};

struct $$mizchi$process_pool$moonbit_test_driver_internal_do_execute$fn$6$2d$cap {
  int32_t(* code)(struct $$3c$Error$3e$$3d$$3e$Unit*, void*);
  moonbit_string_t $0;
  struct $$3c$String$2a$Error$3e$$3d$$3e$Unit* $1;
  
};

struct $Option$3c$StringView$3e$$Some {
  int32_t $0_1;
  int32_t $0_2;
  moonbit_string_t $0_0;
  
};

struct $$moonbitlang$core$builtin$Logger$static_method_table {
  int32_t(* $method_0)(void*, moonbit_string_t);
  int32_t(* $method_1)(void*, moonbit_string_t, int32_t, int32_t);
  int32_t(* $method_2)(void*, struct $StringView);
  int32_t(* $method_3)(void*, int32_t);
  
};

struct $$moonbitlang$core$builtin$StringBuilder {
  int32_t $1;
  moonbit_bytes_t $0;
  
};

struct $$3c$Int$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$ {
  struct moonbit_result_0(* code)(
    struct $$3c$Int$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$*,
    int32_t
  );
  
};

struct $$3c$$3c$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$ {
  struct $$3c$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit* $0;
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* $1;
  
};

struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$ {
  struct moonbit_result_0(* code)(
    struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$*
  );
  
};

struct $$3c$String$2a$$moonbitlang$core$builtin$Logger$3e$ {
  moonbit_string_t $0;
  struct $$moonbitlang$core$builtin$Logger$static_method_table* $1_0;
  void* $1_1;
  
};

struct $$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$ {
  moonbit_string_t $0;
  struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* $1;
  
};

struct moonbit_result_0 {
  int tag;
  union { int32_t ok; void* err;  } data;
  
};

struct moonbit_result_1 {
  int tag;
  union { struct $StringView ok; void* err;  } data;
  
};

int32_t $mizchi$process_pool$moonbit_test_driver_internal_run_async_tests(
  struct $$moonbitlang$core$builtin$Array$3c$async$3c$$3e$$3d$$3e$Unit$21$Error$3e$* _tests$1086
);

struct $$moonbitlang$core$builtin$Array$3c$$3c$String$2a$Int$3e$$3e$* $mizchi$process_pool$moonbit_test_driver_internal_native_parse_args(
  
);

struct $$moonbitlang$core$builtin$Array$3c$String$3e$* $$mizchi$process_pool$moonbit_test_driver_internal_native_parse_args$moonbit_test_driver_internal_split_mbt_string$fn$20(
  int32_t _env$2239,
  moonbit_string_t s$1064,
  int32_t sep$1065
);

struct $$moonbitlang$core$builtin$Array$3c$String$3e$* $$mizchi$process_pool$moonbit_test_driver_internal_native_parse_args$moonbit_test_driver_internal_get_cli_args_internal$fn$19(
  int32_t moonbit_test_driver_internal_utf8_bytes_to_mbt_string$1051
);

moonbit_string_t $$mizchi$process_pool$moonbit_test_driver_internal_native_parse_args$moonbit_test_driver_internal_utf8_bytes_to_mbt_string$fn$18(
  int32_t _env$2148,
  moonbit_bytes_t bytes$1052
);

int32_t $$mizchi$process_pool$moonbit_test_driver_internal_native_parse_args$moonbit_test_driver_internal_parse_int_$fn$17(
  int32_t _env$2141,
  moonbit_string_t s$1046
);

#define $mizchi$process_pool$moonbit_test_driver_internal_get_cli_args_ffi moonbit_get_cli_args

int32_t $mizchi$process_pool$moonbit_test_driver_internal_do_execute(
  struct $$moonbitlang$core$builtin$Array$3c$async$3c$$3e$$3d$$3e$Unit$21$Error$3e$* async_tests$975,
  moonbit_string_t filename$897,
  int32_t index$900
);

int32_t $$mizchi$process_pool$moonbit_test_driver_internal_do_execute$fn$13(
  struct $$3c$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit* _env$2125,
  struct $$3c$Unit$3e$$3d$$3e$Unit* _cont$1015,
  struct $$3c$Error$3e$$3d$$3e$Unit* _err_cont$1016
);

int32_t $$mizchi$process_pool$moonbit_test_driver_internal_do_execute$fn$16(
  struct $$3c$Unit$3e$$3d$$3e$Unit* _env$2136,
  int32_t _cont_param$1035
);

int32_t $$mizchi$process_pool$moonbit_test_driver_internal_do_execute$fn$15(
  struct $$3c$Error$3e$$3d$$3e$Unit* _env$2133,
  void* _cont_param$1036
);

int32_t $$mizchi$process_pool$moonbit_test_driver_internal_do_execute$$2a$async_driver$fn$14(
  int32_t _env$2127,
  void* _state$1018
);

int32_t $$mizchi$process_pool$moonbit_test_driver_internal_do_execute$fn$9(
  struct $$3c$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit* _env$2108,
  struct $$3c$Unit$3e$$3d$$3e$Unit* _cont$976,
  struct $$3c$Error$3e$$3d$$3e$Unit* _err_cont$977
);

int32_t $$mizchi$process_pool$moonbit_test_driver_internal_do_execute$fn$12(
  struct $$3c$Unit$3e$$3d$$3e$Unit* _env$2118,
  int32_t _cont_param$996
);

int32_t $$mizchi$process_pool$moonbit_test_driver_internal_do_execute$fn$11(
  struct $$3c$Error$3e$$3d$$3e$Unit* _env$2115,
  void* _cont_param$997
);

int32_t $$mizchi$process_pool$moonbit_test_driver_internal_do_execute$$2a$async_driver$fn$10(
  int32_t _env$2110,
  void* _state$979
);

struct moonbit_result_0 $$mizchi$process_pool$moonbit_test_driver_internal_do_execute$fn$8(
  struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$* _env$2101
);

int32_t $$mizchi$process_pool$moonbit_test_driver_internal_do_execute$fn$7(
  struct $$3c$$3e$$3d$$3e$Unit* _env$2099
);

int32_t $$mizchi$process_pool$moonbit_test_driver_internal_do_execute$fn$6(
  struct $$3c$Error$3e$$3d$$3e$Unit* _env$2097,
  void* err$957
);

int32_t $$mizchi$process_pool$moonbit_test_driver_internal_do_execute$fn$5(
  struct $$3c$$3e$$3d$$3e$Unit* _env$2089
);

int32_t $$mizchi$process_pool$moonbit_test_driver_internal_do_execute$fn$4(
  struct $$3c$Error$3e$$3d$$3e$Unit* _env$2087,
  void* err$939
);

int32_t $$mizchi$process_pool$moonbit_test_driver_internal_do_execute$on_err$fn$3(
  struct $$3c$String$2a$Error$3e$$3d$$3e$Unit* _env$2082,
  moonbit_string_t name$915,
  void* err$916
);

int32_t $$mizchi$process_pool$moonbit_test_driver_internal_do_execute$test_should_be_skipped$fn$2(
  struct $$mizchi$process_pool$moonbit_test_driver_internal_do_execute$test_should_be_skipped$fn$2$2d$cap* _env$2068,
  moonbit_string_t name$902,
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* attrs$903
);

int32_t $$mizchi$process_pool$moonbit_test_driver_internal_do_execute$handle_result$fn$1(
  struct $$3c$String$2a$String$2a$Bool$3e$$3d$$3e$Unit* _env$2054,
  moonbit_string_t testname$893,
  moonbit_string_t message$894,
  int32_t skipped$895
);

int32_t $mizchi$process_pool$moonbit_test_driver_internal_new_test_arg(
  moonbit_string_t _discard_$891
);

int32_t $mizchi$process_pool$moonbit_test_driver_internal_is_being_cancelled(
  
);

int32_t $mizchi$process_pool$moonbit_test_driver_internal_catch_error(
  struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$* f$889,
  struct $$3c$$3e$$3d$$3e$Unit* on_ok$890,
  struct $$3c$Error$3e$$3d$$3e$Unit* on_err$887
);

int32_t $$moonbitlang$core$builtin$Show$$$moonbitlang$core$builtin$SourceLoc$$output(
  moonbit_string_t self$876,
  struct $$moonbitlang$core$builtin$Logger logger$877
);

int32_t $$moonbitlang$core$builtin$Show$$$moonbitlang$core$builtin$SourceLocRepr$$output(
  struct $$moonbitlang$core$builtin$SourceLocRepr* self$839,
  struct $$moonbitlang$core$builtin$Logger logger$875
);

int32_t $moonbitlang$core$builtin$println$0(moonbit_string_t input$837);

int32_t $$moonbitlang$core$builtin$Hash$$Int$$hash_combine(
  int32_t self$836,
  struct $$moonbitlang$core$builtin$Hasher* hasher$835
);

int32_t $$moonbitlang$core$builtin$Hash$$String$$hash_combine(
  moonbit_string_t self$834,
  struct $$moonbitlang$core$builtin$Hasher* hasher$833
);

int32_t $$moonbitlang$core$builtin$Hasher$$combine_string(
  struct $$moonbitlang$core$builtin$Hasher* self$831,
  moonbit_string_t value$829
);

moonbit_string_t $$moonbitlang$core$builtin$Array$$unsafe_get$1(
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* self$826,
  int32_t idx$827
);

struct $$3c$String$2a$Int$3e$* $$moonbitlang$core$builtin$Array$$unsafe_get$0(
  struct $$moonbitlang$core$builtin$Array$3c$$3c$String$2a$Int$3e$$3e$* self$824,
  int32_t idx$825
);

struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* $$moonbitlang$core$builtin$Map$$get$7(
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$820,
  moonbit_string_t key$816
);

struct $$3c$$3c$Int$2a$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* $$moonbitlang$core$builtin$Map$$get$6(
  struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* self$811,
  int32_t key$807
);

struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* $$moonbitlang$core$builtin$Map$$get$5(
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$802,
  moonbit_string_t key$798
);

struct $$3c$$3c$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* $$moonbitlang$core$builtin$Map$$get$4(
  struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* self$793,
  int32_t key$789
);

struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* $$moonbitlang$core$builtin$Map$$get$3(
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$784,
  moonbit_string_t key$780
);

struct $$3c$$3c$Int$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* $$moonbitlang$core$builtin$Map$$get$2(
  struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* self$775,
  int32_t key$771
);

struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* $$moonbitlang$core$builtin$Map$$get$1(
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$766,
  moonbit_string_t key$762
);

struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* $$moonbitlang$core$builtin$Map$$get$0(
  struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* self$757,
  int32_t key$753
);

struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* $$moonbitlang$core$builtin$Map$$from_array$4(
  struct $$moonbitlang$core$builtin$ArrayView$3c$$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$$3e$ arr$745
);

struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* $$moonbitlang$core$builtin$Map$$from_array$3(
  struct $$moonbitlang$core$builtin$ArrayView$3c$$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$$3e$ arr$737
);

struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* $$moonbitlang$core$builtin$Map$$from_array$2(
  struct $$moonbitlang$core$builtin$ArrayView$3c$$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$$3e$ arr$729
);

struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* $$moonbitlang$core$builtin$Map$$from_array$1(
  struct $$moonbitlang$core$builtin$ArrayView$3c$$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$ arr$721
);

struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* $$moonbitlang$core$builtin$Map$$from_array$0(
  struct $$moonbitlang$core$builtin$ArrayView$3c$$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$$3e$ arr$713
);

int32_t $$moonbitlang$core$builtin$Map$$set$4(
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$709,
  moonbit_string_t key$710,
  struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* value$711
);

int32_t $$moonbitlang$core$builtin$Map$$set$3(
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$706,
  moonbit_string_t key$707,
  struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* value$708
);

int32_t $$moonbitlang$core$builtin$Map$$set$2(
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$703,
  moonbit_string_t key$704,
  struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* value$705
);

int32_t $$moonbitlang$core$builtin$Map$$set$1(
  struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* self$700,
  int32_t key$701,
  struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* value$702
);

int32_t $$moonbitlang$core$builtin$Map$$set$0(
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$697,
  moonbit_string_t key$698,
  struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* value$699
);

int32_t $$moonbitlang$core$builtin$Map$$grow$4(
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$687
);

int32_t $$moonbitlang$core$builtin$Map$$grow$3(
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$676
);

int32_t $$moonbitlang$core$builtin$Map$$grow$2(
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$665
);

int32_t $$moonbitlang$core$builtin$Map$$grow$1(
  struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* self$654
);

int32_t $$moonbitlang$core$builtin$Map$$grow$0(
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$643
);

int32_t $$moonbitlang$core$builtin$Map$$set_with_hash$4(
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$626,
  moonbit_string_t key$635,
  struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* value$636,
  int32_t hash$634
);

int32_t $$moonbitlang$core$builtin$Map$$set_with_hash$3(
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$610,
  moonbit_string_t key$619,
  struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* value$620,
  int32_t hash$618
);

int32_t $$moonbitlang$core$builtin$Map$$set_with_hash$2(
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$594,
  moonbit_string_t key$603,
  struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* value$604,
  int32_t hash$602
);

int32_t $$moonbitlang$core$builtin$Map$$set_with_hash$1(
  struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* self$578,
  int32_t key$587,
  struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* value$588,
  int32_t hash$586
);

int32_t $$moonbitlang$core$builtin$Map$$set_with_hash$0(
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$562,
  moonbit_string_t key$571,
  struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* value$572,
  int32_t hash$570
);

int32_t $$moonbitlang$core$builtin$Map$$push_away$4(
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$556,
  int32_t idx$561,
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* entry$560
);

int32_t $$moonbitlang$core$builtin$Map$$push_away$3(
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$546,
  int32_t idx$551,
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* entry$550
);

int32_t $$moonbitlang$core$builtin$Map$$push_away$2(
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$536,
  int32_t idx$541,
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* entry$540
);

int32_t $$moonbitlang$core$builtin$Map$$push_away$1(
  struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* self$526,
  int32_t idx$531,
  struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* entry$530
);

int32_t $$moonbitlang$core$builtin$Map$$push_away$0(
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$516,
  int32_t idx$521,
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* entry$520
);

int32_t $$moonbitlang$core$builtin$Map$$set_entry$4(
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$506,
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* entry$508,
  int32_t new_idx$507
);

int32_t $$moonbitlang$core$builtin$Map$$set_entry$3(
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$500,
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* entry$502,
  int32_t new_idx$501
);

int32_t $$moonbitlang$core$builtin$Map$$set_entry$2(
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$494,
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* entry$496,
  int32_t new_idx$495
);

int32_t $$moonbitlang$core$builtin$Map$$set_entry$1(
  struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* self$488,
  struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* entry$490,
  int32_t new_idx$489
);

int32_t $$moonbitlang$core$builtin$Map$$set_entry$0(
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$482,
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* entry$484,
  int32_t new_idx$483
);

int32_t $$moonbitlang$core$builtin$Map$$add_entry_to_tail$4(
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$479,
  int32_t idx$481,
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* entry$480
);

int32_t $$moonbitlang$core$builtin$Map$$add_entry_to_tail$3(
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$475,
  int32_t idx$477,
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* entry$476
);

int32_t $$moonbitlang$core$builtin$Map$$add_entry_to_tail$2(
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$471,
  int32_t idx$473,
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* entry$472
);

int32_t $$moonbitlang$core$builtin$Map$$add_entry_to_tail$1(
  struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* self$467,
  int32_t idx$469,
  struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* entry$468
);

int32_t $$moonbitlang$core$builtin$Map$$add_entry_to_tail$0(
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$463,
  int32_t idx$465,
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* entry$464
);

struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* $$moonbitlang$core$builtin$Map$$new$inner$4(
  int32_t capacity$457
);

struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* $$moonbitlang$core$builtin$Map$$new$inner$3(
  int32_t capacity$451
);

struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* $$moonbitlang$core$builtin$Map$$new$inner$2(
  int32_t capacity$445
);

struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* $$moonbitlang$core$builtin$Map$$new$inner$1(
  int32_t capacity$439
);

struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* $$moonbitlang$core$builtin$Map$$new$inner$0(
  int32_t capacity$433
);

int32_t $Int$$next_power_of_two(int32_t self$431);

int32_t $moonbitlang$core$builtin$calc_grow_threshold(int32_t capacity$430);

struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* $Option$$unwrap$4(
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$428
);

struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* $Option$$unwrap$3(
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$426
);

struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* $Option$$unwrap$2(
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$424
);

struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* $Option$$unwrap$1(
  struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* self$422
);

struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* $Option$$unwrap$0(
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$420
);

moonbit_string_t $$moonbitlang$core$builtin$Show$$String$$to_string(
  moonbit_string_t self$419
);

int32_t $$moonbitlang$core$builtin$Show$$Int$$output(
  int32_t self$418,
  struct $$moonbitlang$core$builtin$Logger logger$417
);

int32_t $$moonbitlang$core$builtin$Array$$push$2(
  struct $$moonbitlang$core$builtin$Array$3c$async$3c$$3e$$3d$$3e$Unit$21$Error$3e$* self$414,
  struct $$3c$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit* value$416
);

int32_t $$moonbitlang$core$builtin$Array$$push$1(
  struct $$moonbitlang$core$builtin$Array$3c$$3c$String$2a$Int$3e$$3e$* self$411,
  struct $$3c$String$2a$Int$3e$* value$413
);

int32_t $$moonbitlang$core$builtin$Array$$push$0(
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* self$408,
  moonbit_string_t value$410
);

int32_t $$moonbitlang$core$builtin$Array$$realloc$2(
  struct $$moonbitlang$core$builtin$Array$3c$async$3c$$3e$$3d$$3e$Unit$21$Error$3e$* self$406
);

int32_t $$moonbitlang$core$builtin$Array$$realloc$1(
  struct $$moonbitlang$core$builtin$Array$3c$$3c$String$2a$Int$3e$$3e$* self$403
);

int32_t $$moonbitlang$core$builtin$Array$$realloc$0(
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* self$400
);

int32_t $$moonbitlang$core$builtin$Array$$resize_buffer$2(
  struct $$moonbitlang$core$builtin$Array$3c$async$3c$$3e$$3d$$3e$Unit$21$Error$3e$* self$396,
  int32_t new_capacity$394
);

int32_t $$moonbitlang$core$builtin$Array$$resize_buffer$1(
  struct $$moonbitlang$core$builtin$Array$3c$$3c$String$2a$Int$3e$$3e$* self$390,
  int32_t new_capacity$388
);

int32_t $$moonbitlang$core$builtin$Array$$resize_buffer$0(
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* self$384,
  int32_t new_capacity$382
);

struct $$moonbitlang$core$builtin$Array$3c$String$3e$* $$moonbitlang$core$builtin$Array$$new$inner$0(
  int32_t capacity$380
);

int32_t $$moonbitlang$core$builtin$Logger$$$moonbitlang$core$builtin$StringBuilder$$write_view(
  struct $$moonbitlang$core$builtin$StringBuilder* self$378,
  struct $StringView str$379
);

int32_t $String$$char_length_ge$inner(
  moonbit_string_t self$370,
  int32_t len$373,
  int32_t start_offset$377,
  int64_t end_offset$368
);

int32_t $$moonbitlang$core$builtin$ArrayView$$length$4(
  struct $$moonbitlang$core$builtin$ArrayView$3c$$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$$3e$ self$366
);

int32_t $$moonbitlang$core$builtin$ArrayView$$length$3(
  struct $$moonbitlang$core$builtin$ArrayView$3c$$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$$3e$ self$365
);

int32_t $$moonbitlang$core$builtin$ArrayView$$length$2(
  struct $$moonbitlang$core$builtin$ArrayView$3c$$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$$3e$ self$364
);

int32_t $$moonbitlang$core$builtin$ArrayView$$length$1(
  struct $$moonbitlang$core$builtin$ArrayView$3c$$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$ self$363
);

int32_t $$moonbitlang$core$builtin$ArrayView$$length$0(
  struct $$moonbitlang$core$builtin$ArrayView$3c$$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$$3e$ self$362
);

int32_t $$moonbitlang$core$builtin$Show$$String$$output(
  moonbit_string_t self$354,
  struct $$moonbitlang$core$builtin$Logger logger$352
);

int32_t $moonbitlang$core$builtin$output$flush_segment$7c$3594(
  struct $$3c$String$2a$$moonbitlang$core$builtin$Logger$3e$* _env$348,
  int32_t seg$351,
  int32_t i$350
);

moonbit_string_t $Byte$$to_hex(int32_t b$346);

int32_t $moonbitlang$core$builtin$to_hex$to_hex_digit$7c$3604(int32_t i$344);

int32_t $$moonbitlang$core$builtin$Sub$$Byte$$sub(
  int32_t self$342,
  int32_t that$343
);

int32_t $$moonbitlang$core$builtin$Mod$$Byte$$mod(
  int32_t self$340,
  int32_t that$341
);

int32_t $$moonbitlang$core$builtin$Div$$Byte$$div(
  int32_t self$338,
  int32_t that$339
);

int32_t $$moonbitlang$core$builtin$Add$$Byte$$add(
  int32_t self$336,
  int32_t that$337
);

moonbit_string_t $String$$unsafe_substring(
  moonbit_string_t str$333,
  int32_t start$331,
  int32_t end$332
);

moonbit_string_t $Int$$to_string$inner(int32_t self$315, int32_t radix$314);

int32_t $moonbitlang$core$builtin$radix_count32(
  uint32_t value$308,
  int32_t radix$311
);

int32_t $moonbitlang$core$builtin$hex_count32(uint32_t value$306);

int32_t $moonbitlang$core$builtin$dec_count32(uint32_t value$305);

int32_t $moonbitlang$core$builtin$int_to_string_dec(
  uint16_t* buffer$295,
  uint32_t num$283,
  int32_t digit_start$286,
  int32_t total_len$285
);

int32_t $moonbitlang$core$builtin$int_to_string_generic(
  uint16_t* buffer$277,
  uint32_t num$271,
  int32_t digit_start$269,
  int32_t total_len$268,
  int32_t radix$273
);

int32_t $moonbitlang$core$builtin$int_to_string_hex(
  uint16_t* buffer$264,
  uint32_t num$260,
  int32_t digit_start$258,
  int32_t total_len$257
);

moonbit_string_t $$moonbitlang$core$builtin$Show$$$default_impl$$to_string$3(
  void* self$255
);

moonbit_string_t $$moonbitlang$core$builtin$Show$$$default_impl$$to_string$2(
  void* self$253
);

moonbit_string_t $$moonbitlang$core$builtin$Show$$$default_impl$$to_string$1(
  moonbit_string_t self$251
);

moonbit_string_t $$moonbitlang$core$builtin$Show$$$default_impl$$to_string$0(
  int32_t self$249
);

int32_t $StringView$$start_offset(struct $StringView self$247);

int32_t $StringView$$length(struct $StringView self$246);

moonbit_string_t $StringView$$data(struct $StringView self$245);

int32_t $$moonbitlang$core$builtin$Logger$$$default_impl$$write_substring$0(
  struct $$moonbitlang$core$builtin$StringBuilder* self$239,
  moonbit_string_t value$242,
  int32_t start$243,
  int32_t len$244
);

struct moonbit_result_1 $String$$sub(
  moonbit_string_t self$237,
  int64_t start$opt$235,
  int64_t end$238
);

struct moonbit_result_1 $String$$sub$inner(
  moonbit_string_t self$227,
  int32_t start$233,
  int64_t end$229
);

int32_t $$moonbitlang$core$builtin$Hash$$$default_impl$$hash$1(
  moonbit_string_t self$225
);

int32_t $$moonbitlang$core$builtin$Hash$$$default_impl$$hash$0(
  int32_t self$223
);

struct $$moonbitlang$core$builtin$Hasher* $$moonbitlang$core$builtin$Hasher$$new(
  int64_t seed$opt$220
);

struct $$moonbitlang$core$builtin$Hasher* $$moonbitlang$core$builtin$Hasher$$new$inner(
  int32_t seed$218
);

int32_t $$moonbitlang$core$builtin$Hasher$$finalize(
  struct $$moonbitlang$core$builtin$Hasher* self$217
);

uint32_t $$moonbitlang$core$builtin$Hasher$$avalanche(
  struct $$moonbitlang$core$builtin$Hasher* self$216
);

int32_t $$moonbitlang$core$builtin$Hasher$$combine$1(
  struct $$moonbitlang$core$builtin$Hasher* self$214,
  moonbit_string_t value$213
);

int32_t $$moonbitlang$core$builtin$Hasher$$combine$0(
  struct $$moonbitlang$core$builtin$Hasher* self$212,
  int32_t value$211
);

int32_t $$moonbitlang$core$builtin$Hasher$$combine_int(
  struct $$moonbitlang$core$builtin$Hasher* self$209,
  int32_t value$210
);

int32_t $$moonbitlang$core$builtin$Logger$$$moonbitlang$core$builtin$StringBuilder$$write_string(
  struct $$moonbitlang$core$builtin$StringBuilder* self$207,
  moonbit_string_t str$208
);

int32_t $FixedArray$$blit_from_string(
  moonbit_bytes_t self$199,
  int32_t bytes_offset$194,
  moonbit_string_t str$201,
  int32_t str_offset$197,
  int32_t length$195
);

struct $$moonbitlang$core$builtin$SourceLocRepr* $$moonbitlang$core$builtin$SourceLocRepr$$parse(
  moonbit_string_t repr$116
);

moonbit_string_t $$moonbitlang$core$builtin$Array$$at$0(
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* self$113,
  int32_t index$114
);

int32_t $$moonbitlang$core$builtin$Array$$length$1(
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* self$111
);

int32_t $$moonbitlang$core$builtin$Array$$length$0(
  struct $$moonbitlang$core$builtin$Array$3c$$3c$String$2a$Int$3e$$3e$* self$110
);

struct $$3c$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit** $$moonbitlang$core$builtin$Array$$buffer$2(
  struct $$moonbitlang$core$builtin$Array$3c$async$3c$$3e$$3d$$3e$Unit$21$Error$3e$* self$109
);

moonbit_string_t* $$moonbitlang$core$builtin$Array$$buffer$1(
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* self$108
);

struct $$3c$String$2a$Int$3e$** $$moonbitlang$core$builtin$Array$$buffer$0(
  struct $$moonbitlang$core$builtin$Array$3c$$3c$String$2a$Int$3e$$3e$* self$107
);

moonbit_string_t $String$$escape(moonbit_string_t self$106);

int32_t $String$$unsafe_charcode_at(
  moonbit_string_t self$103,
  int32_t idx$104
);

int32_t $Int$$is_trailing_surrogate(int32_t self$102);

int32_t $Int$$is_leading_surrogate(int32_t self$101);

int32_t $$moonbitlang$core$builtin$Logger$$$moonbitlang$core$builtin$StringBuilder$$write_char(
  struct $$moonbitlang$core$builtin$StringBuilder* self$98,
  int32_t ch$100
);

int32_t $$moonbitlang$core$builtin$StringBuilder$$grow_if_necessary(
  struct $$moonbitlang$core$builtin$StringBuilder* self$93,
  int32_t required$94
);

int32_t $$moonbitlang$core$builtin$Default$$Byte$$default();

int32_t $FixedArray$$set_utf16le_char(
  moonbit_bytes_t self$87,
  int32_t offset$88,
  int32_t value$86
);

int32_t $UInt$$to_byte(uint32_t self$84);

uint32_t $Char$$to_uint(int32_t self$83);

moonbit_string_t $$moonbitlang$core$builtin$StringBuilder$$to_string(
  struct $$moonbitlang$core$builtin$StringBuilder* self$82
);

moonbit_string_t $Bytes$$to_unchecked_string$inner(
  moonbit_bytes_t self$77,
  int32_t offset$81,
  int64_t length$79
);

#define $moonbitlang$core$builtin$unsafe_sub_string moonbit_unsafe_bytes_sub_string

struct $$moonbitlang$core$builtin$StringBuilder* $$moonbitlang$core$builtin$StringBuilder$$new$inner(
  int32_t size_hint$74
);

int32_t $Byte$$to_char(int32_t self$72);

int32_t $$moonbitlang$core$builtin$UninitializedArray$$unsafe_blit$2(
  struct $$3c$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit** dst$67,
  int32_t dst_offset$68,
  struct $$3c$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit** src$69,
  int32_t src_offset$70,
  int32_t len$71
);

int32_t $$moonbitlang$core$builtin$UninitializedArray$$unsafe_blit$1(
  struct $$3c$String$2a$Int$3e$** dst$62,
  int32_t dst_offset$63,
  struct $$3c$String$2a$Int$3e$** src$64,
  int32_t src_offset$65,
  int32_t len$66
);

int32_t $$moonbitlang$core$builtin$UninitializedArray$$unsafe_blit$0(
  moonbit_string_t* dst$57,
  int32_t dst_offset$58,
  moonbit_string_t* src$59,
  int32_t src_offset$60,
  int32_t len$61
);

int32_t $FixedArray$$unsafe_blit$3(
  struct $$3c$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit** dst$48,
  int32_t dst_offset$50,
  struct $$3c$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit** src$49,
  int32_t src_offset$51,
  int32_t len$53
);

int32_t $FixedArray$$unsafe_blit$2(
  struct $$3c$String$2a$Int$3e$** dst$39,
  int32_t dst_offset$41,
  struct $$3c$String$2a$Int$3e$** src$40,
  int32_t src_offset$42,
  int32_t len$44
);

int32_t $FixedArray$$unsafe_blit$1(
  moonbit_string_t* dst$30,
  int32_t dst_offset$32,
  moonbit_string_t* src$31,
  int32_t src_offset$33,
  int32_t len$35
);

int32_t $FixedArray$$unsafe_blit$0(
  moonbit_bytes_t dst$21,
  int32_t dst_offset$23,
  moonbit_bytes_t src$22,
  int32_t src_offset$24,
  int32_t len$26
);

int32_t $moonbitlang$core$builtin$abort$1(
  moonbit_string_t string$19,
  moonbit_string_t loc$20
);

int32_t $moonbitlang$core$builtin$abort$0(
  moonbit_string_t string$17,
  moonbit_string_t loc$18
);

int32_t $$moonbitlang$core$builtin$Hasher$$combine_uint(
  struct $$moonbitlang$core$builtin$Hasher* self$15,
  uint32_t value$16
);

int32_t $$moonbitlang$core$builtin$Hasher$$consume4(
  struct $$moonbitlang$core$builtin$Hasher* self$13,
  uint32_t input$14
);

uint32_t $moonbitlang$core$builtin$rotl(uint32_t x$11, int32_t r$12);

int32_t $$moonbitlang$core$builtin$Show$$$moonbitlang$core$builtin$Failure$$output(
  void* _x_5032$7,
  struct $$moonbitlang$core$builtin$Logger _x_5033$10
);

int32_t $$moonbitlang$core$builtin$Show$$$moonbitlang$core$builtin$CreatingViewError$$output(
  void* _x_5046$5,
  struct $$moonbitlang$core$builtin$Logger _x_5047$6
);

int32_t $$moonbitlang$core$builtin$Logger$$write_object$0(
  struct $$moonbitlang$core$builtin$Logger self$4,
  moonbit_string_t obj$3
);

int32_t $moonbitlang$core$abort$abort$1(moonbit_string_t msg$2);

int32_t $moonbitlang$core$abort$abort$0(moonbit_string_t msg$1);

moonbit_string_t $Error$to_string(void* _e$1093);

int32_t $$moonbitlang$core$builtin$Logger$$$moonbitlang$core$builtin$StringBuilder$$write_char$dyncall_as_$moonbitlang$core$builtin$Logger(
  void* _obj_ptr$1110,
  int32_t _param$1109
);

int32_t $$moonbitlang$core$builtin$Logger$$$moonbitlang$core$builtin$StringBuilder$$write_view$dyncall_as_$moonbitlang$core$builtin$Logger(
  void* _obj_ptr$1107,
  struct $StringView _param$1106
);

int32_t $$moonbitlang$core$builtin$Logger$$$default_impl$$write_substring$dyncall_as_$moonbitlang$core$builtin$Logger$0(
  void* _obj_ptr$1104,
  moonbit_string_t _param$1101,
  int32_t _param$1102,
  int32_t _param$1103
);

int32_t $$moonbitlang$core$builtin$Logger$$$moonbitlang$core$builtin$StringBuilder$$write_string$dyncall_as_$moonbitlang$core$builtin$Logger(
  void* _obj_ptr$1099,
  moonbit_string_t _param$1098
);

struct { int32_t rc; uint32_t meta; uint16_t const data[35]; 
} const moonbit_string_literal_4 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 34), 
    45, 45, 45, 45, 45, 32, 66, 69, 71, 73, 78, 32, 77, 79, 79, 78, 32, 
    84, 69, 83, 84, 32, 82, 69, 83, 85, 76, 84, 32, 45, 45, 45, 45, 45, 
    0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[13]; 
} const moonbit_string_literal_1 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 12), 
    115, 107, 105, 112, 112, 101, 100, 32, 116, 101, 115, 116, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[1]; 
} const moonbit_string_literal_0 =
  { -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 0), 0};

struct { int32_t rc; uint32_t meta; uint16_t const data[8]; 
} const moonbit_string_literal_34 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 7), 
    108, 105, 98, 46, 109, 98, 116, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[15]; 
} const moonbit_string_literal_33 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 14), 
    108, 105, 98, 95, 110, 97, 116, 105, 118, 101, 46, 109, 98, 116, 
    0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[3]; 
} const moonbit_string_literal_16 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 2), 
    92, 116, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[3]; 
} const moonbit_string_literal_14 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 2), 
    92, 114, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[3]; 
} const moonbit_string_literal_3 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 2), 
    58, 32, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[23]; 
} const moonbit_string_literal_11 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 22), 
    105, 110, 118, 97, 108, 105, 100, 32, 115, 117, 114, 114, 111, 103, 
    97, 116, 101, 32, 112, 97, 105, 114, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[53]; 
} const moonbit_string_literal_30 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 52), 
    109, 111, 111, 110, 98, 105, 116, 108, 97, 110, 103, 47, 99, 111, 
    114, 101, 47, 98, 117, 105, 108, 116, 105, 110, 46, 83, 110, 97, 
    112, 115, 104, 111, 116, 69, 114, 114, 111, 114, 46, 83, 110, 97, 
    112, 115, 104, 111, 116, 69, 114, 114, 111, 114, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[7]; 
} const moonbit_string_literal_24 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 6), 
    10, 32, 32, 97, 116, 32, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[3]; 
} const moonbit_string_literal_13 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 2), 
    92, 110, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[31]; 
} const moonbit_string_literal_18 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 30), 
    114, 97, 100, 105, 120, 32, 109, 117, 115, 116, 32, 98, 101, 32, 
    98, 101, 116, 119, 101, 101, 110, 32, 50, 32, 97, 110, 100, 32, 51, 
    54, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[2]; 
} const moonbit_string_literal_20 =
  { -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 1), 48, 0};

struct { int32_t rc; uint32_t meta; uint16_t const data[18]; 
} const moonbit_string_literal_22 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 17), 
    67, 104, 97, 114, 32, 111, 117, 116, 32, 111, 102, 32, 114, 97, 110, 
    103, 101, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[4]; 
} const moonbit_string_literal_17 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 3), 
    92, 117, 123, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[17]; 
} const moonbit_string_literal_28 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 16), 
    73, 110, 100, 101, 120, 79, 117, 116, 79, 102, 66, 111, 117, 110, 
    100, 115, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[21]; 
} const moonbit_string_literal_2 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 20), 
    115, 107, 105, 112, 112, 101, 100, 32, 116, 101, 115, 116, 32, 98, 
    108, 111, 99, 107, 58, 32, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[13]; 
} const moonbit_string_literal_6 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 12), 
    44, 32, 34, 105, 110, 100, 101, 120, 34, 58, 32, 34, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[2]; 
} const moonbit_string_literal_27 =
  { -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 1), 41, 0};

struct { int32_t rc; uint32_t meta; uint16_t const data[49]; 
} const moonbit_string_literal_23 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 48), 
    64, 109, 111, 111, 110, 98, 105, 116, 108, 97, 110, 103, 47, 99, 
    111, 114, 101, 47, 98, 117, 105, 108, 116, 105, 110, 58, 98, 121, 
    116, 101, 115, 46, 109, 98, 116, 58, 50, 57, 56, 58, 53, 45, 50, 
    57, 56, 58, 51, 49, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[53]; 
} const moonbit_string_literal_19 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 52), 
    64, 109, 111, 111, 110, 98, 105, 116, 108, 97, 110, 103, 47, 99, 
    111, 114, 101, 47, 98, 117, 105, 108, 116, 105, 110, 58, 116, 111, 
    95, 115, 116, 114, 105, 110, 103, 46, 109, 98, 116, 58, 50, 51, 57, 
    58, 53, 45, 50, 51, 57, 58, 52, 52, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[50]; 
} const moonbit_string_literal_12 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 49), 
    64, 109, 111, 111, 110, 98, 105, 116, 108, 97, 110, 103, 47, 99, 
    111, 114, 101, 47, 98, 117, 105, 108, 116, 105, 110, 58, 115, 116, 
    114, 105, 110, 103, 46, 109, 98, 116, 58, 52, 53, 52, 58, 57, 45, 
    52, 53, 52, 58, 52, 48, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[37]; 
} const moonbit_string_literal_21 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 36), 
    48, 49, 50, 51, 52, 53, 54, 55, 56, 57, 97, 98, 99, 100, 101, 102, 
    103, 104, 105, 106, 107, 108, 109, 110, 111, 112, 113, 114, 115, 
    116, 117, 118, 119, 120, 121, 122, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[86]; 
} const moonbit_string_literal_31 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 85), 
    109, 105, 122, 99, 104, 105, 47, 112, 114, 111, 99, 101, 115, 115, 
    95, 112, 111, 111, 108, 46, 77, 111, 111, 110, 66, 105, 116, 84, 
    101, 115, 116, 68, 114, 105, 118, 101, 114, 73, 110, 116, 101, 114, 
    110, 97, 108, 74, 115, 69, 114, 114, 111, 114, 46, 77, 111, 111, 
    110, 66, 105, 116, 84, 101, 115, 116, 68, 114, 105, 118, 101, 114, 
    73, 110, 116, 101, 114, 110, 97, 108, 74, 115, 69, 114, 114, 111, 
    114, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[3]; 
} const moonbit_string_literal_15 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 2), 
    92, 98, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[51]; 
} const moonbit_string_literal_32 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 50), 
    109, 111, 111, 110, 98, 105, 116, 108, 97, 110, 103, 47, 99, 111, 
    114, 101, 47, 98, 117, 105, 108, 116, 105, 110, 46, 73, 110, 115, 
    112, 101, 99, 116, 69, 114, 114, 111, 114, 46, 73, 110, 115, 112, 
    101, 99, 116, 69, 114, 114, 111, 114, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[2]; 
} const moonbit_string_literal_25 =
  { -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 1), 10, 0};

struct { int32_t rc; uint32_t meta; uint16_t const data[33]; 
} const moonbit_string_literal_10 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 32), 
    45, 45, 45, 45, 45, 32, 69, 78, 68, 32, 77, 79, 79, 78, 32, 84, 69, 
    83, 84, 32, 82, 69, 83, 85, 76, 84, 32, 45, 45, 45, 45, 45, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[48]; 
} const moonbit_string_literal_5 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 47), 
    123, 34, 112, 97, 99, 107, 97, 103, 101, 34, 58, 32, 34, 109, 105, 
    122, 99, 104, 105, 47, 112, 114, 111, 99, 101, 115, 115, 95, 112, 
    111, 111, 108, 34, 44, 32, 34, 102, 105, 108, 101, 110, 97, 109, 
    101, 34, 58, 32, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[13]; 
} const moonbit_string_literal_29 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 12), 
    73, 110, 118, 97, 108, 105, 100, 73, 110, 100, 101, 120, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[9]; 
} const moonbit_string_literal_26 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 8), 
    70, 97, 105, 108, 117, 114, 101, 40, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[17]; 
} const moonbit_string_literal_7 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 16), 
    34, 44, 32, 34, 116, 101, 115, 116, 95, 110, 97, 109, 101, 34, 58, 
    32, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[2]; 
} const moonbit_string_literal_9 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 1), 125, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[14]; 
} const moonbit_string_literal_8 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 13), 
    44, 32, 34, 109, 101, 115, 115, 97, 103, 101, 34, 58, 32, 0
  };

struct moonbit_object const moonbit_constant_constructor_0 =
  { -1, Moonbit_make_regular_object_header(2, 0, 0)};

struct moonbit_object const moonbit_constant_constructor_1 =
  { -1, Moonbit_make_regular_object_header(2, 0, 1)};

struct {
  int32_t rc;
  uint32_t meta;
  struct $$moonbitlang$core$builtin$Logger$static_method_table data;
  
} $$moonbitlang$core$builtin$StringBuilder$as_$moonbitlang$core$builtin$Logger$static_method_table_id$object =
  {
    -1,
    Moonbit_make_regular_object_header(
      sizeof(
        struct $$moonbitlang$core$builtin$Logger$static_method_table
      )
      >> 2,
        0,
        0
    ),
    {
      .$method_0 = $$moonbitlang$core$builtin$Logger$$$moonbitlang$core$builtin$StringBuilder$$write_string$dyncall_as_$moonbitlang$core$builtin$Logger,
        .$method_1 = $$moonbitlang$core$builtin$Logger$$$default_impl$$write_substring$dyncall_as_$moonbitlang$core$builtin$Logger$0,
        .$method_2 = $$moonbitlang$core$builtin$Logger$$$moonbitlang$core$builtin$StringBuilder$$write_view$dyncall_as_$moonbitlang$core$builtin$Logger,
        .$method_3 = $$moonbitlang$core$builtin$Logger$$$moonbitlang$core$builtin$StringBuilder$$write_char$dyncall_as_$moonbitlang$core$builtin$Logger
    }
  };

struct $$moonbitlang$core$builtin$Logger$static_method_table* $$moonbitlang$core$builtin$StringBuilder$as_$moonbitlang$core$builtin$Logger$static_method_table_id =
  &$$moonbitlang$core$builtin$StringBuilder$as_$moonbitlang$core$builtin$Logger$static_method_table_id$object.data;

int32_t $mizchi$process_pool$moonbit_test_driver_internal_max_concurrent_tests =
  10;

struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* $mizchi$process_pool$moonbit_test_driver_internal_with_args_tests;

struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* $mizchi$process_pool$moonbit_test_driver_internal_async_tests_with_args;

struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* $mizchi$process_pool$moonbit_test_driver_internal_async_tests;

struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* $mizchi$process_pool$moonbit_test_driver_internal_no_args_tests;

int32_t $mizchi$process_pool$moonbit_test_driver_internal_run_async_tests(
  struct $$moonbitlang$core$builtin$Array$3c$async$3c$$3e$$3d$$3e$Unit$21$Error$3e$* _tests$1086
) {
  moonbit_decref(_tests$1086);
  return 0;
}

struct $$moonbitlang$core$builtin$Array$3c$$3c$String$2a$Int$3e$$3e$* $mizchi$process_pool$moonbit_test_driver_internal_native_parse_args(
  
) {
  int32_t moonbit_test_driver_internal_parse_int_$1045 = 0;
  int32_t moonbit_test_driver_internal_utf8_bytes_to_mbt_string$1051 = 0;
  int32_t moonbit_test_driver_internal_get_cli_args_internal$1058 =
    moonbit_test_driver_internal_utf8_bytes_to_mbt_string$1051;
  int32_t moonbit_test_driver_internal_split_mbt_string$1063 = 0;
  struct $$3c$String$2a$Int$3e$** _tmp$2264 =
    (struct $$3c$String$2a$Int$3e$**)moonbit_empty_ref_array;
  struct $$moonbitlang$core$builtin$Array$3c$$3c$String$2a$Int$3e$$3e$* file_and_index$1070 =
    (struct $$moonbitlang$core$builtin$Array$3c$$3c$String$2a$Int$3e$$3e$*)moonbit_malloc(
      sizeof(
        struct $$moonbitlang$core$builtin$Array$3c$$3c$String$2a$Int$3e$$3e$
      )
    );
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* cli_args$1071;
  moonbit_string_t _tmp$2263;
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* test_args$1072;
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* _arr$1073;
  int32_t _len$1074;
  int32_t _i$1075;
  Moonbit_object_header(file_and_index$1070)->meta
  = Moonbit_make_regular_object_header(
    offsetof(
      struct $$moonbitlang$core$builtin$Array$3c$$3c$String$2a$Int$3e$$3e$,
        $0
    )
    >> 2,
      1,
      0
  );
  file_and_index$1070->$0 = _tmp$2264;
  file_and_index$1070->$1 = 0;
  cli_args$1071
  = $$mizchi$process_pool$moonbit_test_driver_internal_native_parse_args$moonbit_test_driver_internal_get_cli_args_internal$fn$19(
    moonbit_test_driver_internal_get_cli_args_internal$1058
  );
  _tmp$2263 = $$moonbitlang$core$builtin$Array$$at$0(cli_args$1071, 1);
  test_args$1072
  = $$mizchi$process_pool$moonbit_test_driver_internal_native_parse_args$moonbit_test_driver_internal_split_mbt_string$fn$20(
    moonbit_test_driver_internal_split_mbt_string$1063, _tmp$2263, 47
  );
  _arr$1073 = test_args$1072;
  moonbit_incref(_arr$1073);
  _len$1074 = $$moonbitlang$core$builtin$Array$$length$1(_arr$1073);
  _i$1075 = 0;
  while (1) {
    if (_i$1075 < _len$1074) {
      moonbit_string_t arg$1076;
      struct $$moonbitlang$core$builtin$Array$3c$String$3e$* file_and_range$1077;
      moonbit_string_t file$1078;
      moonbit_string_t range$1079;
      struct $$moonbitlang$core$builtin$Array$3c$String$3e$* start_and_end$1080;
      moonbit_string_t _tmp$2261;
      int32_t start$1081;
      moonbit_string_t _tmp$2260;
      int32_t end$1082;
      int32_t i$1083;
      int32_t _tmp$2262;
      moonbit_incref(_arr$1073);
      arg$1076
      = $$moonbitlang$core$builtin$Array$$unsafe_get$1(
        _arr$1073, _i$1075
      );
      file_and_range$1077
      = $$mizchi$process_pool$moonbit_test_driver_internal_native_parse_args$moonbit_test_driver_internal_split_mbt_string$fn$20(
        moonbit_test_driver_internal_split_mbt_string$1063, arg$1076, 58
      );
      moonbit_incref(file_and_range$1077);
      file$1078
      = $$moonbitlang$core$builtin$Array$$at$0(
        file_and_range$1077, 0
      );
      range$1079
      = $$moonbitlang$core$builtin$Array$$at$0(
        file_and_range$1077, 1
      );
      start_and_end$1080
      = $$mizchi$process_pool$moonbit_test_driver_internal_native_parse_args$moonbit_test_driver_internal_split_mbt_string$fn$20(
        moonbit_test_driver_internal_split_mbt_string$1063, range$1079, 45
      );
      moonbit_incref(start_and_end$1080);
      _tmp$2261
      = $$moonbitlang$core$builtin$Array$$at$0(
        start_and_end$1080, 0
      );
      start$1081
      = $$mizchi$process_pool$moonbit_test_driver_internal_native_parse_args$moonbit_test_driver_internal_parse_int_$fn$17(
        moonbit_test_driver_internal_parse_int_$1045, _tmp$2261
      );
      _tmp$2260
      = $$moonbitlang$core$builtin$Array$$at$0(
        start_and_end$1080, 1
      );
      end$1082
      = $$mizchi$process_pool$moonbit_test_driver_internal_native_parse_args$moonbit_test_driver_internal_parse_int_$fn$17(
        moonbit_test_driver_internal_parse_int_$1045, _tmp$2260
      );
      i$1083 = start$1081;
      while (1) {
        if (i$1083 < end$1082) {
          struct $$3c$String$2a$Int$3e$* _tuple$2258;
          int32_t _tmp$2259;
          moonbit_incref(file$1078);
          _tuple$2258
          = (struct $$3c$String$2a$Int$3e$*)moonbit_malloc(
              sizeof(struct $$3c$String$2a$Int$3e$)
            );
          Moonbit_object_header(_tuple$2258)->meta
          = Moonbit_make_regular_object_header(
            offsetof(struct $$3c$String$2a$Int$3e$, $0) >> 2, 1, 0
          );
          _tuple$2258->$0 = file$1078;
          _tuple$2258->$1 = i$1083;
          moonbit_incref(file_and_index$1070);
          $$moonbitlang$core$builtin$Array$$push$1(
            file_and_index$1070, _tuple$2258
          );
          _tmp$2259 = i$1083 + 1;
          i$1083 = _tmp$2259;
          continue;
        } else {
          moonbit_decref(file$1078);
        }
        break;
      }
      _tmp$2262 = _i$1075 + 1;
      _i$1075 = _tmp$2262;
      continue;
    } else {
      moonbit_decref(_arr$1073);
    }
    break;
  }
  return file_and_index$1070;
}

struct $$moonbitlang$core$builtin$Array$3c$String$3e$* $$mizchi$process_pool$moonbit_test_driver_internal_native_parse_args$moonbit_test_driver_internal_split_mbt_string$fn$20(
  int32_t _env$2239,
  moonbit_string_t s$1064,
  int32_t sep$1065
) {
  moonbit_string_t* _tmp$2257 = (moonbit_string_t*)moonbit_empty_ref_array;
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* res$1066 =
    (struct $$moonbitlang$core$builtin$Array$3c$String$3e$*)moonbit_malloc(
      sizeof(struct $$moonbitlang$core$builtin$Array$3c$String$3e$)
    );
  struct $Ref$3c$Int$3e$* i$1067;
  struct $Ref$3c$Int$3e$* start$1068;
  int32_t val$2252;
  int32_t _tmp$2253;
  Moonbit_object_header(res$1066)->meta
  = Moonbit_make_regular_object_header(
    offsetof(struct $$moonbitlang$core$builtin$Array$3c$String$3e$, $0) >> 2,
      1,
      0
  );
  res$1066->$0 = _tmp$2257;
  res$1066->$1 = 0;
  i$1067
  = (struct $Ref$3c$Int$3e$*)moonbit_malloc(sizeof(struct $Ref$3c$Int$3e$));
  Moonbit_object_header(i$1067)->meta
  = Moonbit_make_regular_object_header(
    sizeof(struct $Ref$3c$Int$3e$) >> 2, 0, 0
  );
  i$1067->$0 = 0;
  start$1068
  = (struct $Ref$3c$Int$3e$*)moonbit_malloc(sizeof(struct $Ref$3c$Int$3e$));
  Moonbit_object_header(start$1068)->meta
  = Moonbit_make_regular_object_header(
    sizeof(struct $Ref$3c$Int$3e$) >> 2, 0, 0
  );
  start$1068->$0 = 0;
  while (1) {
    int32_t val$2240 = i$1067->$0;
    int32_t _tmp$2241 = Moonbit_array_length(s$1064);
    if (val$2240 < _tmp$2241) {
      int32_t val$2244 = i$1067->$0;
      int32_t _tmp$2243;
      int32_t _tmp$2242;
      int32_t val$2251;
      int32_t _tmp$2250;
      if (val$2244 < 0 || val$2244 >= Moonbit_array_length(s$1064)) {
        moonbit_panic();
      }
      _tmp$2243 = s$1064[val$2244];
      _tmp$2242 = _tmp$2243;
      if (_tmp$2242 == sep$1065) {
        int32_t val$2246 = start$1068->$0;
        int32_t val$2247 = i$1067->$0;
        moonbit_string_t _tmp$2245;
        int32_t val$2249;
        int32_t _tmp$2248;
        moonbit_incref(s$1064);
        _tmp$2245 = $String$$unsafe_substring(s$1064, val$2246, val$2247);
        moonbit_incref(res$1066);
        $$moonbitlang$core$builtin$Array$$push$0(res$1066, _tmp$2245);
        val$2249 = i$1067->$0;
        _tmp$2248 = val$2249 + 1;
        start$1068->$0 = _tmp$2248;
      }
      val$2251 = i$1067->$0;
      _tmp$2250 = val$2251 + 1;
      i$1067->$0 = _tmp$2250;
      continue;
    } else {
      moonbit_decref(i$1067);
    }
    break;
  }
  val$2252 = start$1068->$0;
  _tmp$2253 = Moonbit_array_length(s$1064);
  if (val$2252 < _tmp$2253) {
    int32_t _field$2265 = start$1068->$0;
    int32_t val$2255;
    int32_t _tmp$2256;
    moonbit_string_t _tmp$2254;
    moonbit_decref(start$1068);
    val$2255 = _field$2265;
    _tmp$2256 = Moonbit_array_length(s$1064);
    _tmp$2254 = $String$$unsafe_substring(s$1064, val$2255, _tmp$2256);
    moonbit_incref(res$1066);
    $$moonbitlang$core$builtin$Array$$push$0(res$1066, _tmp$2254);
  } else {
    moonbit_decref(start$1068);
    moonbit_decref(s$1064);
  }
  return res$1066;
}

struct $$moonbitlang$core$builtin$Array$3c$String$3e$* $$mizchi$process_pool$moonbit_test_driver_internal_native_parse_args$moonbit_test_driver_internal_get_cli_args_internal$fn$19(
  int32_t moonbit_test_driver_internal_utf8_bytes_to_mbt_string$1051
) {
  moonbit_bytes_t* tmp$1059 =
    $mizchi$process_pool$moonbit_test_driver_internal_get_cli_args_ffi();
  int32_t _tmp$2238 = Moonbit_array_length(tmp$1059);
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* res$1060 =
    $$moonbitlang$core$builtin$Array$$new$inner$0(_tmp$2238);
  int32_t i$1061 = 0;
  while (1) {
    int32_t _tmp$2234 = Moonbit_array_length(tmp$1059);
    if (i$1061 < _tmp$2234) {
      moonbit_bytes_t _tmp$2266;
      moonbit_bytes_t _tmp$2236;
      moonbit_string_t _tmp$2235;
      int32_t _tmp$2237;
      if (i$1061 < 0 || i$1061 >= Moonbit_array_length(tmp$1059)) {
        moonbit_panic();
      }
      _tmp$2266 = (moonbit_bytes_t)tmp$1059[i$1061];
      _tmp$2236 = _tmp$2266;
      moonbit_incref(_tmp$2236);
      _tmp$2235
      = $$mizchi$process_pool$moonbit_test_driver_internal_native_parse_args$moonbit_test_driver_internal_utf8_bytes_to_mbt_string$fn$18(
        moonbit_test_driver_internal_utf8_bytes_to_mbt_string$1051, _tmp$2236
      );
      moonbit_incref(res$1060);
      $$moonbitlang$core$builtin$Array$$push$0(res$1060, _tmp$2235);
      _tmp$2237 = i$1061 + 1;
      i$1061 = _tmp$2237;
      continue;
    } else {
      moonbit_decref(tmp$1059);
    }
    break;
  }
  return res$1060;
}

moonbit_string_t $$mizchi$process_pool$moonbit_test_driver_internal_native_parse_args$moonbit_test_driver_internal_utf8_bytes_to_mbt_string$fn$18(
  int32_t _env$2148,
  moonbit_bytes_t bytes$1052
) {
  struct $$moonbitlang$core$builtin$StringBuilder* res$1053 =
    $$moonbitlang$core$builtin$StringBuilder$$new$inner(0);
  int32_t len$1054 = Moonbit_array_length(bytes$1052);
  struct $Ref$3c$Int$3e$* i$1055 =
    (struct $Ref$3c$Int$3e$*)moonbit_malloc(sizeof(struct $Ref$3c$Int$3e$));
  Moonbit_object_header(i$1055)->meta
  = Moonbit_make_regular_object_header(
    sizeof(struct $Ref$3c$Int$3e$) >> 2, 0, 0
  );
  i$1055->$0 = 0;
  while (1) {
    int32_t val$2149 = i$1055->$0;
    if (val$2149 < len$1054) {
      int32_t val$2233 = i$1055->$0;
      int32_t _tmp$2232;
      int32_t _tmp$2231;
      struct $Ref$3c$Int$3e$* c$1056;
      int32_t val$2150;
      if (val$2233 < 0 || val$2233 >= Moonbit_array_length(bytes$1052)) {
        moonbit_panic();
      }
      _tmp$2232 = bytes$1052[val$2233];
      _tmp$2231 = (int32_t)_tmp$2232;
      c$1056
      = (struct $Ref$3c$Int$3e$*)moonbit_malloc(
          sizeof(struct $Ref$3c$Int$3e$)
        );
      Moonbit_object_header(c$1056)->meta
      = Moonbit_make_regular_object_header(
        sizeof(struct $Ref$3c$Int$3e$) >> 2, 0, 0
      );
      c$1056->$0 = _tmp$2231;
      val$2150 = c$1056->$0;
      if (val$2150 < 128) {
        int32_t _field$2267 = c$1056->$0;
        int32_t val$2152;
        int32_t _tmp$2151;
        int32_t val$2154;
        int32_t _tmp$2153;
        moonbit_decref(c$1056);
        val$2152 = _field$2267;
        _tmp$2151 = val$2152;
        moonbit_incref(res$1053);
        $$moonbitlang$core$builtin$Logger$$$moonbitlang$core$builtin$StringBuilder$$write_char(
          res$1053, _tmp$2151
        );
        val$2154 = i$1055->$0;
        _tmp$2153 = val$2154 + 1;
        i$1055->$0 = _tmp$2153;
      } else {
        int32_t val$2155 = c$1056->$0;
        if (val$2155 < 224) {
          int32_t val$2157 = i$1055->$0;
          int32_t _tmp$2156 = val$2157 + 1;
          int32_t val$2166;
          int32_t _tmp$2165;
          int32_t _tmp$2159;
          int32_t val$2164;
          int32_t _tmp$2163;
          int32_t _tmp$2162;
          int32_t _tmp$2161;
          int32_t _tmp$2160;
          int32_t _tmp$2158;
          int32_t _field$2268;
          int32_t val$2168;
          int32_t _tmp$2167;
          int32_t val$2170;
          int32_t _tmp$2169;
          if (_tmp$2156 >= len$1054) {
            moonbit_decref(c$1056);
            moonbit_decref(i$1055);
            moonbit_decref(bytes$1052);
            break;
          }
          val$2166 = c$1056->$0;
          _tmp$2165 = val$2166 & 31;
          _tmp$2159 = _tmp$2165 << 6;
          val$2164 = i$1055->$0;
          _tmp$2163 = val$2164 + 1;
          if (_tmp$2163 < 0 || _tmp$2163 >= Moonbit_array_length(bytes$1052)) {
            moonbit_panic();
          }
          _tmp$2162 = bytes$1052[_tmp$2163];
          _tmp$2161 = (int32_t)_tmp$2162;
          _tmp$2160 = _tmp$2161 & 63;
          _tmp$2158 = _tmp$2159 | _tmp$2160;
          c$1056->$0 = _tmp$2158;
          _field$2268 = c$1056->$0;
          moonbit_decref(c$1056);
          val$2168 = _field$2268;
          _tmp$2167 = val$2168;
          moonbit_incref(res$1053);
          $$moonbitlang$core$builtin$Logger$$$moonbitlang$core$builtin$StringBuilder$$write_char(
            res$1053, _tmp$2167
          );
          val$2170 = i$1055->$0;
          _tmp$2169 = val$2170 + 2;
          i$1055->$0 = _tmp$2169;
        } else {
          int32_t val$2171 = c$1056->$0;
          if (val$2171 < 240) {
            int32_t val$2173 = i$1055->$0;
            int32_t _tmp$2172 = val$2173 + 2;
            int32_t val$2189;
            int32_t _tmp$2188;
            int32_t _tmp$2181;
            int32_t val$2187;
            int32_t _tmp$2186;
            int32_t _tmp$2185;
            int32_t _tmp$2184;
            int32_t _tmp$2183;
            int32_t _tmp$2182;
            int32_t _tmp$2175;
            int32_t val$2180;
            int32_t _tmp$2179;
            int32_t _tmp$2178;
            int32_t _tmp$2177;
            int32_t _tmp$2176;
            int32_t _tmp$2174;
            int32_t _field$2269;
            int32_t val$2191;
            int32_t _tmp$2190;
            int32_t val$2193;
            int32_t _tmp$2192;
            if (_tmp$2172 >= len$1054) {
              moonbit_decref(c$1056);
              moonbit_decref(i$1055);
              moonbit_decref(bytes$1052);
              break;
            }
            val$2189 = c$1056->$0;
            _tmp$2188 = val$2189 & 15;
            _tmp$2181 = _tmp$2188 << 12;
            val$2187 = i$1055->$0;
            _tmp$2186 = val$2187 + 1;
            if (
              _tmp$2186 < 0 || _tmp$2186 >= Moonbit_array_length(bytes$1052)
            ) {
              moonbit_panic();
            }
            _tmp$2185 = bytes$1052[_tmp$2186];
            _tmp$2184 = (int32_t)_tmp$2185;
            _tmp$2183 = _tmp$2184 & 63;
            _tmp$2182 = _tmp$2183 << 6;
            _tmp$2175 = _tmp$2181 | _tmp$2182;
            val$2180 = i$1055->$0;
            _tmp$2179 = val$2180 + 2;
            if (
              _tmp$2179 < 0 || _tmp$2179 >= Moonbit_array_length(bytes$1052)
            ) {
              moonbit_panic();
            }
            _tmp$2178 = bytes$1052[_tmp$2179];
            _tmp$2177 = (int32_t)_tmp$2178;
            _tmp$2176 = _tmp$2177 & 63;
            _tmp$2174 = _tmp$2175 | _tmp$2176;
            c$1056->$0 = _tmp$2174;
            _field$2269 = c$1056->$0;
            moonbit_decref(c$1056);
            val$2191 = _field$2269;
            _tmp$2190 = val$2191;
            moonbit_incref(res$1053);
            $$moonbitlang$core$builtin$Logger$$$moonbitlang$core$builtin$StringBuilder$$write_char(
              res$1053, _tmp$2190
            );
            val$2193 = i$1055->$0;
            _tmp$2192 = val$2193 + 3;
            i$1055->$0 = _tmp$2192;
          } else {
            int32_t val$2195 = i$1055->$0;
            int32_t _tmp$2194 = val$2195 + 3;
            int32_t val$2218;
            int32_t _tmp$2217;
            int32_t _tmp$2210;
            int32_t val$2216;
            int32_t _tmp$2215;
            int32_t _tmp$2214;
            int32_t _tmp$2213;
            int32_t _tmp$2212;
            int32_t _tmp$2211;
            int32_t _tmp$2203;
            int32_t val$2209;
            int32_t _tmp$2208;
            int32_t _tmp$2207;
            int32_t _tmp$2206;
            int32_t _tmp$2205;
            int32_t _tmp$2204;
            int32_t _tmp$2197;
            int32_t val$2202;
            int32_t _tmp$2201;
            int32_t _tmp$2200;
            int32_t _tmp$2199;
            int32_t _tmp$2198;
            int32_t _tmp$2196;
            int32_t val$2220;
            int32_t _tmp$2219;
            int32_t val$2224;
            int32_t _tmp$2223;
            int32_t _tmp$2222;
            int32_t _tmp$2221;
            int32_t _field$2270;
            int32_t val$2228;
            int32_t _tmp$2227;
            int32_t _tmp$2226;
            int32_t _tmp$2225;
            int32_t val$2230;
            int32_t _tmp$2229;
            if (_tmp$2194 >= len$1054) {
              moonbit_decref(c$1056);
              moonbit_decref(i$1055);
              moonbit_decref(bytes$1052);
              break;
            }
            val$2218 = c$1056->$0;
            _tmp$2217 = val$2218 & 7;
            _tmp$2210 = _tmp$2217 << 18;
            val$2216 = i$1055->$0;
            _tmp$2215 = val$2216 + 1;
            if (
              _tmp$2215 < 0 || _tmp$2215 >= Moonbit_array_length(bytes$1052)
            ) {
              moonbit_panic();
            }
            _tmp$2214 = bytes$1052[_tmp$2215];
            _tmp$2213 = (int32_t)_tmp$2214;
            _tmp$2212 = _tmp$2213 & 63;
            _tmp$2211 = _tmp$2212 << 12;
            _tmp$2203 = _tmp$2210 | _tmp$2211;
            val$2209 = i$1055->$0;
            _tmp$2208 = val$2209 + 2;
            if (
              _tmp$2208 < 0 || _tmp$2208 >= Moonbit_array_length(bytes$1052)
            ) {
              moonbit_panic();
            }
            _tmp$2207 = bytes$1052[_tmp$2208];
            _tmp$2206 = (int32_t)_tmp$2207;
            _tmp$2205 = _tmp$2206 & 63;
            _tmp$2204 = _tmp$2205 << 6;
            _tmp$2197 = _tmp$2203 | _tmp$2204;
            val$2202 = i$1055->$0;
            _tmp$2201 = val$2202 + 3;
            if (
              _tmp$2201 < 0 || _tmp$2201 >= Moonbit_array_length(bytes$1052)
            ) {
              moonbit_panic();
            }
            _tmp$2200 = bytes$1052[_tmp$2201];
            _tmp$2199 = (int32_t)_tmp$2200;
            _tmp$2198 = _tmp$2199 & 63;
            _tmp$2196 = _tmp$2197 | _tmp$2198;
            c$1056->$0 = _tmp$2196;
            val$2220 = c$1056->$0;
            _tmp$2219 = val$2220 - 65536;
            c$1056->$0 = _tmp$2219;
            val$2224 = c$1056->$0;
            _tmp$2223 = val$2224 >> 10;
            _tmp$2222 = _tmp$2223 + 55296;
            _tmp$2221 = _tmp$2222;
            moonbit_incref(res$1053);
            $$moonbitlang$core$builtin$Logger$$$moonbitlang$core$builtin$StringBuilder$$write_char(
              res$1053, _tmp$2221
            );
            _field$2270 = c$1056->$0;
            moonbit_decref(c$1056);
            val$2228 = _field$2270;
            _tmp$2227 = val$2228 & 1023;
            _tmp$2226 = _tmp$2227 + 56320;
            _tmp$2225 = _tmp$2226;
            moonbit_incref(res$1053);
            $$moonbitlang$core$builtin$Logger$$$moonbitlang$core$builtin$StringBuilder$$write_char(
              res$1053, _tmp$2225
            );
            val$2230 = i$1055->$0;
            _tmp$2229 = val$2230 + 4;
            i$1055->$0 = _tmp$2229;
          }
        }
      }
      continue;
    } else {
      moonbit_decref(i$1055);
      moonbit_decref(bytes$1052);
    }
    break;
  }
  return $$moonbitlang$core$builtin$StringBuilder$$to_string(res$1053);
}

int32_t $$mizchi$process_pool$moonbit_test_driver_internal_native_parse_args$moonbit_test_driver_internal_parse_int_$fn$17(
  int32_t _env$2141,
  moonbit_string_t s$1046
) {
  struct $Ref$3c$Int$3e$* res$1047 =
    (struct $Ref$3c$Int$3e$*)moonbit_malloc(sizeof(struct $Ref$3c$Int$3e$));
  int32_t len$1048;
  int32_t i$1049;
  int32_t _field$2271;
  Moonbit_object_header(res$1047)->meta
  = Moonbit_make_regular_object_header(
    sizeof(struct $Ref$3c$Int$3e$) >> 2, 0, 0
  );
  res$1047->$0 = 0;
  len$1048 = Moonbit_array_length(s$1046);
  i$1049 = 0;
  while (1) {
    if (i$1049 < len$1048) {
      int32_t val$2146 = res$1047->$0;
      int32_t _tmp$2143 = val$2146 * 10;
      int32_t _tmp$2145;
      int32_t _tmp$2144;
      int32_t _tmp$2142;
      int32_t _tmp$2147;
      if (i$1049 < 0 || i$1049 >= Moonbit_array_length(s$1046)) {
        moonbit_panic();
      }
      _tmp$2145 = s$1046[i$1049];
      _tmp$2144 = _tmp$2145 - 48;
      _tmp$2142 = _tmp$2143 + _tmp$2144;
      res$1047->$0 = _tmp$2142;
      _tmp$2147 = i$1049 + 1;
      i$1049 = _tmp$2147;
      continue;
    } else {
      moonbit_decref(s$1046);
    }
    break;
  }
  _field$2271 = res$1047->$0;
  moonbit_decref(res$1047);
  return _field$2271;
}

int32_t $mizchi$process_pool$moonbit_test_driver_internal_do_execute(
  struct $$moonbitlang$core$builtin$Array$3c$async$3c$$3e$$3d$$3e$Unit$21$Error$3e$* async_tests$975,
  moonbit_string_t filename$897,
  int32_t index$900
) {
  struct $$mizchi$process_pool$moonbit_test_driver_internal_do_execute$handle_result$fn$1$2d$cap* _closure$2733;
  struct $$3c$String$2a$String$2a$Bool$3e$$3d$$3e$Unit* handle_result$892;
  struct $$mizchi$process_pool$moonbit_test_driver_internal_do_execute$test_should_be_skipped$fn$2$2d$cap* test_should_be_skipped$901;
  struct $$mizchi$process_pool$moonbit_test_driver_internal_do_execute$on_err$fn$3$2d$cap* _closure$2734;
  struct $$3c$String$2a$Error$3e$$3d$$3e$Unit* on_err$914;
  struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* index_map$931;
  struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _bind$945;
  struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$* f$933;
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* attrs$934;
  struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* _bind$940;
  moonbit_string_t name$937;
  moonbit_string_t name$935;
  int32_t len$2091;
  int32_t _tmp$2084;
  struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* index_map$949;
  struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _bind$963;
  struct $$3c$Int$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$* f$951;
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* attrs$952;
  struct $$3c$$3c$Int$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* _bind$958;
  moonbit_string_t name$955;
  moonbit_string_t name$953;
  int32_t len$2104;
  int32_t _tmp$2093;
  struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* index_map$967;
  struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _bind$1003;
  struct $$3c$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit* f$969;
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* attrs$970;
  struct $$3c$$3c$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* _bind$998;
  moonbit_string_t name$973;
  moonbit_string_t name$971;
  int32_t len$2121;
  int32_t _tmp$2106;
  struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* index_map$1007;
  struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _bind$1042;
  struct $$3c$Int$2a$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit* f$1009;
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* attrs$1010;
  struct $$3c$$3c$Int$2a$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* _bind$1037;
  moonbit_string_t name$1013;
  moonbit_string_t name$1011;
  int32_t len$2139;
  int32_t _tmp$2123;
  moonbit_incref(filename$897);
  _closure$2733
  = (struct $$mizchi$process_pool$moonbit_test_driver_internal_do_execute$handle_result$fn$1$2d$cap*)moonbit_malloc(
      sizeof(
        struct $$mizchi$process_pool$moonbit_test_driver_internal_do_execute$handle_result$fn$1$2d$cap
      )
    );
  Moonbit_object_header(_closure$2733)->meta
  = Moonbit_make_regular_object_header(
    offsetof(
      struct $$mizchi$process_pool$moonbit_test_driver_internal_do_execute$handle_result$fn$1$2d$cap,
        $1
    )
    >> 2,
      1,
      0
  );
  _closure$2733->code
  = &$$mizchi$process_pool$moonbit_test_driver_internal_do_execute$handle_result$fn$1;
  _closure$2733->$0 = index$900;
  _closure$2733->$1 = filename$897;
  handle_result$892
  = (struct $$3c$String$2a$String$2a$Bool$3e$$3d$$3e$Unit*)_closure$2733;
  moonbit_incref(filename$897);
  moonbit_incref(handle_result$892);
  test_should_be_skipped$901
  = (struct $$mizchi$process_pool$moonbit_test_driver_internal_do_execute$test_should_be_skipped$fn$2$2d$cap*)moonbit_malloc(
      sizeof(
        struct $$mizchi$process_pool$moonbit_test_driver_internal_do_execute$test_should_be_skipped$fn$2$2d$cap
      )
    );
  Moonbit_object_header(test_should_be_skipped$901)->meta
  = Moonbit_make_regular_object_header(
    offsetof(
      struct $$mizchi$process_pool$moonbit_test_driver_internal_do_execute$test_should_be_skipped$fn$2$2d$cap,
        $0
    )
    >> 2,
      2,
      0
  );
  test_should_be_skipped$901->$0 = filename$897;
  test_should_be_skipped$901->$1 = handle_result$892;
  moonbit_incref(handle_result$892);
  _closure$2734
  = (struct $$mizchi$process_pool$moonbit_test_driver_internal_do_execute$on_err$fn$3$2d$cap*)moonbit_malloc(
      sizeof(
        struct $$mizchi$process_pool$moonbit_test_driver_internal_do_execute$on_err$fn$3$2d$cap
      )
    );
  Moonbit_object_header(_closure$2734)->meta
  = Moonbit_make_regular_object_header(
    offsetof(
      struct $$mizchi$process_pool$moonbit_test_driver_internal_do_execute$on_err$fn$3$2d$cap,
        $0
    )
    >> 2,
      1,
      0
  );
  _closure$2734->code
  = &$$mizchi$process_pool$moonbit_test_driver_internal_do_execute$on_err$fn$3;
  _closure$2734->$0 = handle_result$892;
  on_err$914 = (struct $$3c$String$2a$Error$3e$$3d$$3e$Unit*)_closure$2734;
  moonbit_incref(filename$897);
  moonbit_incref(
    $mizchi$process_pool$moonbit_test_driver_internal_no_args_tests
  );
  _bind$945
  = $$moonbitlang$core$builtin$Map$$get$1(
    $mizchi$process_pool$moonbit_test_driver_internal_no_args_tests,
      filename$897
  );
  if (_bind$945 == 0) {
    if (_bind$945) {
      moonbit_decref(_bind$945);
    }
  } else {
    struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _Some$946 =
      _bind$945;
    struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _index_map$947 =
      _Some$946;
    index_map$931 = _index_map$947;
    goto $join$930;
  }
  goto $joinlet$2735;
  $join$930:;
  _bind$940 = $$moonbitlang$core$builtin$Map$$get$0(index_map$931, index$900);
  if (_bind$940 == 0) {
    if (_bind$940) {
      moonbit_decref(_bind$940);
    }
  } else {
    struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* _Some$941 =
      _bind$940;
    struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* _x$942 =
      _Some$941;
    struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$* _field$2287 =
      _x$942->$0;
    struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$* _f$943 =
      _field$2287;
    struct $$moonbitlang$core$builtin$Array$3c$String$3e$* _field$2286 =
      _x$942->$1;
    int32_t _cnt$2595 = Moonbit_object_header(_x$942)->rc;
    struct $$moonbitlang$core$builtin$Array$3c$String$3e$* _attrs$944;
    if (_cnt$2595 > 1) {
      int32_t _new_cnt$2596 = _cnt$2595 - 1;
      Moonbit_object_header(_x$942)->rc = _new_cnt$2596;
      moonbit_incref(_field$2286);
      moonbit_incref(_f$943);
    } else if (_cnt$2595 == 1) {
      moonbit_free(_x$942);
    }
    _attrs$944 = _field$2286;
    f$933 = _f$943;
    attrs$934 = _attrs$944;
    goto $join$932;
  }
  goto $joinlet$2736;
  $join$932:;
  len$2091 = attrs$934->$1;
  if (len$2091 >= 1) {
    moonbit_string_t* _field$2285 = attrs$934->$0;
    moonbit_string_t* buf$2092 = _field$2285;
    moonbit_string_t _tmp$2284 = (moonbit_string_t)buf$2092[0];
    moonbit_string_t _name$938 = _tmp$2284;
    moonbit_incref(_name$938);
    name$937 = _name$938;
    goto $join$936;
  } else {
    name$935 = (moonbit_string_t)moonbit_string_literal_0.data;
  }
  goto $joinlet$2737;
  $join$936:;
  name$935 = name$937;
  $joinlet$2737:;
  moonbit_incref(name$935);
  moonbit_incref(test_should_be_skipped$901);
  _tmp$2084
  = $$mizchi$process_pool$moonbit_test_driver_internal_do_execute$test_should_be_skipped$fn$2(
    test_should_be_skipped$901, name$935, attrs$934
  );
  if (!_tmp$2084) {
    struct $$mizchi$process_pool$moonbit_test_driver_internal_do_execute$fn$5$2d$cap* _closure$2738;
    struct $$3c$$3e$$3d$$3e$Unit* _tmp$2085;
    struct $$mizchi$process_pool$moonbit_test_driver_internal_do_execute$fn$4$2d$cap* _closure$2739;
    struct $$3c$Error$3e$$3d$$3e$Unit* _tmp$2086;
    moonbit_decref(async_tests$975);
    moonbit_decref(test_should_be_skipped$901);
    moonbit_decref(filename$897);
    moonbit_incref(name$935);
    _closure$2738
    = (struct $$mizchi$process_pool$moonbit_test_driver_internal_do_execute$fn$5$2d$cap*)moonbit_malloc(
        sizeof(
          struct $$mizchi$process_pool$moonbit_test_driver_internal_do_execute$fn$5$2d$cap
        )
      );
    Moonbit_object_header(_closure$2738)->meta
    = Moonbit_make_regular_object_header(
      offsetof(
        struct $$mizchi$process_pool$moonbit_test_driver_internal_do_execute$fn$5$2d$cap,
          $0
      )
      >> 2,
        2,
        0
    );
    _closure$2738->code
    = &$$mizchi$process_pool$moonbit_test_driver_internal_do_execute$fn$5;
    _closure$2738->$0 = name$935;
    _closure$2738->$1 = handle_result$892;
    _tmp$2085 = (struct $$3c$$3e$$3d$$3e$Unit*)_closure$2738;
    _closure$2739
    = (struct $$mizchi$process_pool$moonbit_test_driver_internal_do_execute$fn$4$2d$cap*)moonbit_malloc(
        sizeof(
          struct $$mizchi$process_pool$moonbit_test_driver_internal_do_execute$fn$4$2d$cap
        )
      );
    Moonbit_object_header(_closure$2739)->meta
    = Moonbit_make_regular_object_header(
      offsetof(
        struct $$mizchi$process_pool$moonbit_test_driver_internal_do_execute$fn$4$2d$cap,
          $0
      )
      >> 2,
        2,
        0
    );
    _closure$2739->code
    = &$$mizchi$process_pool$moonbit_test_driver_internal_do_execute$fn$4;
    _closure$2739->$0 = name$935;
    _closure$2739->$1 = on_err$914;
    _tmp$2086 = (struct $$3c$Error$3e$$3d$$3e$Unit*)_closure$2739;
    $mizchi$process_pool$moonbit_test_driver_internal_catch_error(
      f$933, _tmp$2085, _tmp$2086
    );
    return 0;
  } else {
    moonbit_decref(name$935);
    moonbit_decref(f$933);
  }
  $joinlet$2736:;
  $joinlet$2735:;
  moonbit_incref(filename$897);
  moonbit_incref(
    $mizchi$process_pool$moonbit_test_driver_internal_with_args_tests
  );
  _bind$963
  = $$moonbitlang$core$builtin$Map$$get$3(
    $mizchi$process_pool$moonbit_test_driver_internal_with_args_tests,
      filename$897
  );
  if (_bind$963 == 0) {
    if (_bind$963) {
      moonbit_decref(_bind$963);
    }
  } else {
    struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _Some$964 =
      _bind$963;
    struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _index_map$965 =
      _Some$964;
    index_map$949 = _index_map$965;
    goto $join$948;
  }
  goto $joinlet$2740;
  $join$948:;
  _bind$958 = $$moonbitlang$core$builtin$Map$$get$2(index_map$949, index$900);
  if (_bind$958 == 0) {
    if (_bind$958) {
      moonbit_decref(_bind$958);
    }
  } else {
    struct $$3c$$3c$Int$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* _Some$959 =
      _bind$958;
    struct $$3c$$3c$Int$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* _x$960 =
      _Some$959;
    struct $$3c$Int$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$* _field$2283 =
      _x$960->$0;
    struct $$3c$Int$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$* _f$961 =
      _field$2283;
    struct $$moonbitlang$core$builtin$Array$3c$String$3e$* _field$2282 =
      _x$960->$1;
    int32_t _cnt$2597 = Moonbit_object_header(_x$960)->rc;
    struct $$moonbitlang$core$builtin$Array$3c$String$3e$* _attrs$962;
    if (_cnt$2597 > 1) {
      int32_t _new_cnt$2598 = _cnt$2597 - 1;
      Moonbit_object_header(_x$960)->rc = _new_cnt$2598;
      moonbit_incref(_field$2282);
      moonbit_incref(_f$961);
    } else if (_cnt$2597 == 1) {
      moonbit_free(_x$960);
    }
    _attrs$962 = _field$2282;
    f$951 = _f$961;
    attrs$952 = _attrs$962;
    goto $join$950;
  }
  goto $joinlet$2741;
  $join$950:;
  len$2104 = attrs$952->$1;
  if (len$2104 >= 1) {
    moonbit_string_t* _field$2281 = attrs$952->$0;
    moonbit_string_t* buf$2105 = _field$2281;
    moonbit_string_t _tmp$2280 = (moonbit_string_t)buf$2105[0];
    moonbit_string_t _name$956 = _tmp$2280;
    moonbit_incref(_name$956);
    name$955 = _name$956;
    goto $join$954;
  } else {
    name$953 = (moonbit_string_t)moonbit_string_literal_0.data;
  }
  goto $joinlet$2742;
  $join$954:;
  name$953 = name$955;
  $joinlet$2742:;
  moonbit_incref(name$953);
  moonbit_incref(test_should_be_skipped$901);
  _tmp$2093
  = $$mizchi$process_pool$moonbit_test_driver_internal_do_execute$test_should_be_skipped$fn$2(
    test_should_be_skipped$901, name$953, attrs$952
  );
  if (!_tmp$2093) {
    struct $$mizchi$process_pool$moonbit_test_driver_internal_do_execute$fn$8$2d$cap* _closure$2743;
    struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$* _tmp$2094;
    struct $$mizchi$process_pool$moonbit_test_driver_internal_do_execute$fn$7$2d$cap* _closure$2744;
    struct $$3c$$3e$$3d$$3e$Unit* _tmp$2095;
    struct $$mizchi$process_pool$moonbit_test_driver_internal_do_execute$fn$6$2d$cap* _closure$2745;
    struct $$3c$Error$3e$$3d$$3e$Unit* _tmp$2096;
    moonbit_decref(async_tests$975);
    moonbit_decref(test_should_be_skipped$901);
    moonbit_decref(filename$897);
    moonbit_incref(name$953);
    _closure$2743
    = (struct $$mizchi$process_pool$moonbit_test_driver_internal_do_execute$fn$8$2d$cap*)moonbit_malloc(
        sizeof(
          struct $$mizchi$process_pool$moonbit_test_driver_internal_do_execute$fn$8$2d$cap
        )
      );
    Moonbit_object_header(_closure$2743)->meta
    = Moonbit_make_regular_object_header(
      offsetof(
        struct $$mizchi$process_pool$moonbit_test_driver_internal_do_execute$fn$8$2d$cap,
          $0
      )
      >> 2,
        2,
        0
    );
    _closure$2743->code
    = &$$mizchi$process_pool$moonbit_test_driver_internal_do_execute$fn$8;
    _closure$2743->$0 = name$953;
    _closure$2743->$1 = f$951;
    _tmp$2094
    = (struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$*)_closure$2743;
    moonbit_incref(name$953);
    _closure$2744
    = (struct $$mizchi$process_pool$moonbit_test_driver_internal_do_execute$fn$7$2d$cap*)moonbit_malloc(
        sizeof(
          struct $$mizchi$process_pool$moonbit_test_driver_internal_do_execute$fn$7$2d$cap
        )
      );
    Moonbit_object_header(_closure$2744)->meta
    = Moonbit_make_regular_object_header(
      offsetof(
        struct $$mizchi$process_pool$moonbit_test_driver_internal_do_execute$fn$7$2d$cap,
          $0
      )
      >> 2,
        2,
        0
    );
    _closure$2744->code
    = &$$mizchi$process_pool$moonbit_test_driver_internal_do_execute$fn$7;
    _closure$2744->$0 = name$953;
    _closure$2744->$1 = handle_result$892;
    _tmp$2095 = (struct $$3c$$3e$$3d$$3e$Unit*)_closure$2744;
    _closure$2745
    = (struct $$mizchi$process_pool$moonbit_test_driver_internal_do_execute$fn$6$2d$cap*)moonbit_malloc(
        sizeof(
          struct $$mizchi$process_pool$moonbit_test_driver_internal_do_execute$fn$6$2d$cap
        )
      );
    Moonbit_object_header(_closure$2745)->meta
    = Moonbit_make_regular_object_header(
      offsetof(
        struct $$mizchi$process_pool$moonbit_test_driver_internal_do_execute$fn$6$2d$cap,
          $0
      )
      >> 2,
        2,
        0
    );
    _closure$2745->code
    = &$$mizchi$process_pool$moonbit_test_driver_internal_do_execute$fn$6;
    _closure$2745->$0 = name$953;
    _closure$2745->$1 = on_err$914;
    _tmp$2096 = (struct $$3c$Error$3e$$3d$$3e$Unit*)_closure$2745;
    $mizchi$process_pool$moonbit_test_driver_internal_catch_error(
      _tmp$2094, _tmp$2095, _tmp$2096
    );
    return 0;
  } else {
    moonbit_decref(name$953);
    moonbit_decref(f$951);
  }
  $joinlet$2741:;
  $joinlet$2740:;
  moonbit_incref(filename$897);
  moonbit_incref(
    $mizchi$process_pool$moonbit_test_driver_internal_async_tests
  );
  _bind$1003
  = $$moonbitlang$core$builtin$Map$$get$5(
    $mizchi$process_pool$moonbit_test_driver_internal_async_tests,
      filename$897
  );
  if (_bind$1003 == 0) {
    if (_bind$1003) {
      moonbit_decref(_bind$1003);
    }
  } else {
    struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _Some$1004 =
      _bind$1003;
    struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _index_map$1005 =
      _Some$1004;
    index_map$967 = _index_map$1005;
    goto $join$966;
  }
  goto $joinlet$2746;
  $join$966:;
  _bind$998 = $$moonbitlang$core$builtin$Map$$get$4(index_map$967, index$900);
  if (_bind$998 == 0) {
    if (_bind$998) {
      moonbit_decref(_bind$998);
    }
  } else {
    struct $$3c$$3c$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* _Some$999 =
      _bind$998;
    struct $$3c$$3c$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* _x$1000 =
      _Some$999;
    struct $$3c$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit* _field$2279 =
      _x$1000->$0;
    struct $$3c$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit* _f$1001 =
      _field$2279;
    struct $$moonbitlang$core$builtin$Array$3c$String$3e$* _field$2278 =
      _x$1000->$1;
    int32_t _cnt$2599 = Moonbit_object_header(_x$1000)->rc;
    struct $$moonbitlang$core$builtin$Array$3c$String$3e$* _attrs$1002;
    if (_cnt$2599 > 1) {
      int32_t _new_cnt$2600 = _cnt$2599 - 1;
      Moonbit_object_header(_x$1000)->rc = _new_cnt$2600;
      moonbit_incref(_field$2278);
      moonbit_incref(_f$1001);
    } else if (_cnt$2599 == 1) {
      moonbit_free(_x$1000);
    }
    _attrs$1002 = _field$2278;
    f$969 = _f$1001;
    attrs$970 = _attrs$1002;
    goto $join$968;
  }
  goto $joinlet$2747;
  $join$968:;
  len$2121 = attrs$970->$1;
  if (len$2121 >= 1) {
    moonbit_string_t* _field$2277 = attrs$970->$0;
    moonbit_string_t* buf$2122 = _field$2277;
    moonbit_string_t _tmp$2276 = (moonbit_string_t)buf$2122[0];
    moonbit_string_t _name$974 = _tmp$2276;
    moonbit_incref(_name$974);
    name$973 = _name$974;
    goto $join$972;
  } else {
    name$971 = (moonbit_string_t)moonbit_string_literal_0.data;
  }
  goto $joinlet$2748;
  $join$972:;
  name$971 = name$973;
  $joinlet$2748:;
  moonbit_incref(name$971);
  moonbit_incref(test_should_be_skipped$901);
  _tmp$2106
  = $$mizchi$process_pool$moonbit_test_driver_internal_do_execute$test_should_be_skipped$fn$2(
    test_should_be_skipped$901, name$971, attrs$970
  );
  if (!_tmp$2106) {
    struct $$mizchi$process_pool$moonbit_test_driver_internal_do_execute$fn$9$2d$cap* _closure$2749;
    struct $$3c$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit* _tmp$2107;
    moonbit_decref(test_should_be_skipped$901);
    moonbit_decref(filename$897);
    _closure$2749
    = (struct $$mizchi$process_pool$moonbit_test_driver_internal_do_execute$fn$9$2d$cap*)moonbit_malloc(
        sizeof(
          struct $$mizchi$process_pool$moonbit_test_driver_internal_do_execute$fn$9$2d$cap
        )
      );
    Moonbit_object_header(_closure$2749)->meta
    = Moonbit_make_regular_object_header(
      offsetof(
        struct $$mizchi$process_pool$moonbit_test_driver_internal_do_execute$fn$9$2d$cap,
          $0
      )
      >> 2,
        4,
        0
    );
    _closure$2749->code
    = &$$mizchi$process_pool$moonbit_test_driver_internal_do_execute$fn$9;
    _closure$2749->$0 = name$971;
    _closure$2749->$1 = f$969;
    _closure$2749->$2 = on_err$914;
    _closure$2749->$3 = handle_result$892;
    _tmp$2107
    = (struct $$3c$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit*)_closure$2749;
    $$moonbitlang$core$builtin$Array$$push$2(async_tests$975, _tmp$2107);
    return 0;
  } else {
    moonbit_decref(name$971);
    moonbit_decref(f$969);
  }
  $joinlet$2747:;
  $joinlet$2746:;
  moonbit_incref(
    $mizchi$process_pool$moonbit_test_driver_internal_async_tests_with_args
  );
  _bind$1042
  = $$moonbitlang$core$builtin$Map$$get$7(
    $mizchi$process_pool$moonbit_test_driver_internal_async_tests_with_args,
      filename$897
  );
  if (_bind$1042 == 0) {
    if (_bind$1042) {
      moonbit_decref(_bind$1042);
    }
    moonbit_decref(async_tests$975);
    moonbit_decref(on_err$914);
    moonbit_decref(test_should_be_skipped$901);
  } else {
    struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _Some$1043 =
      _bind$1042;
    struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _index_map$1044 =
      _Some$1043;
    index_map$1007 = _index_map$1044;
    goto $join$1006;
  }
  goto $joinlet$2750;
  $join$1006:;
  _bind$1037
  = $$moonbitlang$core$builtin$Map$$get$6(
    index_map$1007, index$900
  );
  if (_bind$1037 == 0) {
    if (_bind$1037) {
      moonbit_decref(_bind$1037);
    }
    moonbit_decref(async_tests$975);
    moonbit_decref(on_err$914);
    moonbit_decref(test_should_be_skipped$901);
  } else {
    struct $$3c$$3c$Int$2a$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* _Some$1038 =
      _bind$1037;
    struct $$3c$$3c$Int$2a$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* _x$1039 =
      _Some$1038;
    struct $$3c$Int$2a$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit* _field$2275 =
      _x$1039->$0;
    struct $$3c$Int$2a$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit* _f$1040 =
      _field$2275;
    struct $$moonbitlang$core$builtin$Array$3c$String$3e$* _field$2274 =
      _x$1039->$1;
    int32_t _cnt$2601 = Moonbit_object_header(_x$1039)->rc;
    struct $$moonbitlang$core$builtin$Array$3c$String$3e$* _attrs$1041;
    if (_cnt$2601 > 1) {
      int32_t _new_cnt$2602 = _cnt$2601 - 1;
      Moonbit_object_header(_x$1039)->rc = _new_cnt$2602;
      moonbit_incref(_field$2274);
      moonbit_incref(_f$1040);
    } else if (_cnt$2601 == 1) {
      moonbit_free(_x$1039);
    }
    _attrs$1041 = _field$2274;
    f$1009 = _f$1040;
    attrs$1010 = _attrs$1041;
    goto $join$1008;
  }
  goto $joinlet$2751;
  $join$1008:;
  len$2139 = attrs$1010->$1;
  if (len$2139 >= 1) {
    moonbit_string_t* _field$2273 = attrs$1010->$0;
    moonbit_string_t* buf$2140 = _field$2273;
    moonbit_string_t _tmp$2272 = (moonbit_string_t)buf$2140[0];
    moonbit_string_t _name$1014 = _tmp$2272;
    moonbit_incref(_name$1014);
    name$1013 = _name$1014;
    goto $join$1012;
  } else {
    name$1011 = (moonbit_string_t)moonbit_string_literal_0.data;
  }
  goto $joinlet$2752;
  $join$1012:;
  name$1011 = name$1013;
  $joinlet$2752:;
  moonbit_incref(name$1011);
  _tmp$2123
  = $$mizchi$process_pool$moonbit_test_driver_internal_do_execute$test_should_be_skipped$fn$2(
    test_should_be_skipped$901, name$1011, attrs$1010
  );
  if (!_tmp$2123) {
    struct $$mizchi$process_pool$moonbit_test_driver_internal_do_execute$fn$13$2d$cap* _closure$2753 =
      (struct $$mizchi$process_pool$moonbit_test_driver_internal_do_execute$fn$13$2d$cap*)moonbit_malloc(
        sizeof(
          struct $$mizchi$process_pool$moonbit_test_driver_internal_do_execute$fn$13$2d$cap
        )
      );
    struct $$3c$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit* _tmp$2124;
    Moonbit_object_header(_closure$2753)->meta
    = Moonbit_make_regular_object_header(
      offsetof(
        struct $$mizchi$process_pool$moonbit_test_driver_internal_do_execute$fn$13$2d$cap,
          $0
      )
      >> 2,
        4,
        0
    );
    _closure$2753->code
    = &$$mizchi$process_pool$moonbit_test_driver_internal_do_execute$fn$13;
    _closure$2753->$0 = name$1011;
    _closure$2753->$1 = f$1009;
    _closure$2753->$2 = on_err$914;
    _closure$2753->$3 = handle_result$892;
    _tmp$2124
    = (struct $$3c$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit*)_closure$2753;
    $$moonbitlang$core$builtin$Array$$push$2(async_tests$975, _tmp$2124);
    return 0;
  } else {
    moonbit_decref(name$1011);
    moonbit_decref(f$1009);
    moonbit_decref(async_tests$975);
    moonbit_decref(on_err$914);
  }
  $joinlet$2751:;
  $joinlet$2750:;
  $$mizchi$process_pool$moonbit_test_driver_internal_do_execute$handle_result$fn$1(
    handle_result$892,
      (moonbit_string_t)moonbit_string_literal_0.data,
      (moonbit_string_t)moonbit_string_literal_1.data,
      1
  );
  return 0;
}

int32_t $$mizchi$process_pool$moonbit_test_driver_internal_do_execute$fn$13(
  struct $$3c$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit* _env$2125,
  struct $$3c$Unit$3e$$3d$$3e$Unit* _cont$1015,
  struct $$3c$Error$3e$$3d$$3e$Unit* _err_cont$1016
) {
  struct $$mizchi$process_pool$moonbit_test_driver_internal_do_execute$fn$13$2d$cap* _casted_env$2126 =
    (struct $$mizchi$process_pool$moonbit_test_driver_internal_do_execute$fn$13$2d$cap*)_env$2125;
  struct $$3c$String$2a$String$2a$Bool$3e$$3d$$3e$Unit* _field$2291 =
    _casted_env$2126->$3;
  struct $$3c$String$2a$String$2a$Bool$3e$$3d$$3e$Unit* handle_result$892 =
    _field$2291;
  struct $$3c$String$2a$Error$3e$$3d$$3e$Unit* _field$2290 =
    _casted_env$2126->$2;
  struct $$3c$String$2a$Error$3e$$3d$$3e$Unit* on_err$914 = _field$2290;
  struct $$3c$Int$2a$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit* _field$2289 =
    _casted_env$2126->$1;
  struct $$3c$Int$2a$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit* f$1009 =
    _field$2289;
  moonbit_string_t _field$2288 = _casted_env$2126->$0;
  int32_t _cnt$2603 = Moonbit_object_header(_casted_env$2126)->rc;
  moonbit_string_t name$1011;
  int32_t _async_driver$1017;
  int32_t _tmp$2130;
  struct $$mizchi$process_pool$moonbit_test_driver_internal_do_execute$fn$16$2d$cap* _closure$2754;
  struct $$3c$Unit$3e$$3d$$3e$Unit* _tmp$2131;
  struct $$mizchi$process_pool$moonbit_test_driver_internal_do_execute$fn$15$2d$cap* _closure$2755;
  struct $$3c$Error$3e$$3d$$3e$Unit* _tmp$2132;
  if (_cnt$2603 > 1) {
    int32_t _new_cnt$2604 = _cnt$2603 - 1;
    Moonbit_object_header(_casted_env$2126)->rc = _new_cnt$2604;
    moonbit_incref(handle_result$892);
    moonbit_incref(on_err$914);
    moonbit_incref(f$1009);
    moonbit_incref(_field$2288);
  } else if (_cnt$2603 == 1) {
    moonbit_free(_casted_env$2126);
  }
  name$1011 = _field$2288;
  _async_driver$1017 = 0;
  moonbit_incref(name$1011);
  _tmp$2130
  = $mizchi$process_pool$moonbit_test_driver_internal_new_test_arg(
    name$1011
  );
  moonbit_incref(_cont$1015);
  moonbit_incref(name$1011);
  _closure$2754
  = (struct $$mizchi$process_pool$moonbit_test_driver_internal_do_execute$fn$16$2d$cap*)moonbit_malloc(
      sizeof(
        struct $$mizchi$process_pool$moonbit_test_driver_internal_do_execute$fn$16$2d$cap
      )
    );
  Moonbit_object_header(_closure$2754)->meta
  = Moonbit_make_regular_object_header(
    offsetof(
      struct $$mizchi$process_pool$moonbit_test_driver_internal_do_execute$fn$16$2d$cap,
        $1
    )
    >> 2,
      3,
      0
  );
  _closure$2754->code
  = &$$mizchi$process_pool$moonbit_test_driver_internal_do_execute$fn$16;
  _closure$2754->$0 = _async_driver$1017;
  _closure$2754->$1 = _cont$1015;
  _closure$2754->$2 = name$1011;
  _closure$2754->$3 = handle_result$892;
  _tmp$2131 = (struct $$3c$Unit$3e$$3d$$3e$Unit*)_closure$2754;
  _closure$2755
  = (struct $$mizchi$process_pool$moonbit_test_driver_internal_do_execute$fn$15$2d$cap*)moonbit_malloc(
      sizeof(
        struct $$mizchi$process_pool$moonbit_test_driver_internal_do_execute$fn$15$2d$cap
      )
    );
  Moonbit_object_header(_closure$2755)->meta
  = Moonbit_make_regular_object_header(
    offsetof(
      struct $$mizchi$process_pool$moonbit_test_driver_internal_do_execute$fn$15$2d$cap,
        $1
    )
    >> 2,
      4,
      0
  );
  _closure$2755->code
  = &$$mizchi$process_pool$moonbit_test_driver_internal_do_execute$fn$15;
  _closure$2755->$0 = _async_driver$1017;
  _closure$2755->$1 = _err_cont$1016;
  _closure$2755->$2 = _cont$1015;
  _closure$2755->$3 = name$1011;
  _closure$2755->$4 = on_err$914;
  _tmp$2132 = (struct $$3c$Error$3e$$3d$$3e$Unit*)_closure$2755;
  f$1009->code(f$1009, _tmp$2130, _tmp$2131, _tmp$2132);
  return 0;
}

int32_t $$mizchi$process_pool$moonbit_test_driver_internal_do_execute$fn$16(
  struct $$3c$Unit$3e$$3d$$3e$Unit* _env$2136,
  int32_t _cont_param$1035
) {
  struct $$mizchi$process_pool$moonbit_test_driver_internal_do_execute$fn$16$2d$cap* _casted_env$2137 =
    (struct $$mizchi$process_pool$moonbit_test_driver_internal_do_execute$fn$16$2d$cap*)_env$2136;
  struct $$3c$String$2a$String$2a$Bool$3e$$3d$$3e$Unit* _field$2295 =
    _casted_env$2137->$3;
  struct $$3c$String$2a$String$2a$Bool$3e$$3d$$3e$Unit* handle_result$892 =
    _field$2295;
  moonbit_string_t _field$2294 = _casted_env$2137->$2;
  moonbit_string_t name$1011 = _field$2294;
  struct $$3c$Unit$3e$$3d$$3e$Unit* _field$2293 = _casted_env$2137->$1;
  struct $$3c$Unit$3e$$3d$$3e$Unit* _cont$1015 = _field$2293;
  int32_t _field$2292 = _casted_env$2137->$0;
  int32_t _cnt$2605 = Moonbit_object_header(_casted_env$2137)->rc;
  int32_t _async_driver$1017;
  void* State_1$2138;
  if (_cnt$2605 > 1) {
    int32_t _new_cnt$2606 = _cnt$2605 - 1;
    Moonbit_object_header(_casted_env$2137)->rc = _new_cnt$2606;
    moonbit_incref(handle_result$892);
    moonbit_incref(name$1011);
    moonbit_incref(_cont$1015);
  } else if (_cnt$2605 == 1) {
    moonbit_free(_casted_env$2137);
  }
  _async_driver$1017 = _field$2292;
  State_1$2138
  = (void*)moonbit_malloc(
      sizeof(
        struct $$mizchi$process_pool$moonbit_test_driver_internal_do_execute$handle_result$53$test_should_be_skipped$54$on_err$55$$2a$arm$290$$2a$arm$283$lambda$427$State$State_1
      )
    );
  Moonbit_object_header(State_1$2138)->meta
  = Moonbit_make_regular_object_header(
    offsetof(
      struct $$mizchi$process_pool$moonbit_test_driver_internal_do_execute$handle_result$53$test_should_be_skipped$54$on_err$55$$2a$arm$290$$2a$arm$283$lambda$427$State$State_1,
        $1
    )
    >> 2,
      3,
      1
  );
  ((struct $$mizchi$process_pool$moonbit_test_driver_internal_do_execute$handle_result$53$test_should_be_skipped$54$on_err$55$$2a$arm$290$$2a$arm$283$lambda$427$State$State_1*)State_1$2138)->$0
  = _cont_param$1035;
  ((struct $$mizchi$process_pool$moonbit_test_driver_internal_do_execute$handle_result$53$test_should_be_skipped$54$on_err$55$$2a$arm$290$$2a$arm$283$lambda$427$State$State_1*)State_1$2138)->$1
  = handle_result$892;
  ((struct $$mizchi$process_pool$moonbit_test_driver_internal_do_execute$handle_result$53$test_should_be_skipped$54$on_err$55$$2a$arm$290$$2a$arm$283$lambda$427$State$State_1*)State_1$2138)->$2
  = name$1011;
  ((struct $$mizchi$process_pool$moonbit_test_driver_internal_do_execute$handle_result$53$test_should_be_skipped$54$on_err$55$$2a$arm$290$$2a$arm$283$lambda$427$State$State_1*)State_1$2138)->$3
  = _cont$1015;
  $$mizchi$process_pool$moonbit_test_driver_internal_do_execute$$2a$async_driver$fn$14(
    _async_driver$1017, State_1$2138
  );
  return 0;
}

int32_t $$mizchi$process_pool$moonbit_test_driver_internal_do_execute$fn$15(
  struct $$3c$Error$3e$$3d$$3e$Unit* _env$2133,
  void* _cont_param$1036
) {
  struct $$mizchi$process_pool$moonbit_test_driver_internal_do_execute$fn$15$2d$cap* _casted_env$2134 =
    (struct $$mizchi$process_pool$moonbit_test_driver_internal_do_execute$fn$15$2d$cap*)_env$2133;
  struct $$3c$String$2a$Error$3e$$3d$$3e$Unit* _field$2300 =
    _casted_env$2134->$4;
  struct $$3c$String$2a$Error$3e$$3d$$3e$Unit* on_err$914 = _field$2300;
  moonbit_string_t _field$2299 = _casted_env$2134->$3;
  moonbit_string_t name$1011 = _field$2299;
  struct $$3c$Unit$3e$$3d$$3e$Unit* _field$2298 = _casted_env$2134->$2;
  struct $$3c$Unit$3e$$3d$$3e$Unit* _cont$1015 = _field$2298;
  struct $$3c$Error$3e$$3d$$3e$Unit* _field$2297 = _casted_env$2134->$1;
  struct $$3c$Error$3e$$3d$$3e$Unit* _err_cont$1016 = _field$2297;
  int32_t _field$2296 = _casted_env$2134->$0;
  int32_t _cnt$2607 = Moonbit_object_header(_casted_env$2134)->rc;
  int32_t _async_driver$1017;
  void* _try$273$2135;
  if (_cnt$2607 > 1) {
    int32_t _new_cnt$2608 = _cnt$2607 - 1;
    Moonbit_object_header(_casted_env$2134)->rc = _new_cnt$2608;
    moonbit_incref(on_err$914);
    moonbit_incref(name$1011);
    moonbit_incref(_cont$1015);
    moonbit_incref(_err_cont$1016);
  } else if (_cnt$2607 == 1) {
    moonbit_free(_casted_env$2134);
  }
  _async_driver$1017 = _field$2296;
  _try$273$2135
  = (void*)moonbit_malloc(
      sizeof(
        struct $$mizchi$process_pool$moonbit_test_driver_internal_do_execute$handle_result$53$test_should_be_skipped$54$on_err$55$$2a$arm$290$$2a$arm$283$lambda$427$State$$2a$try$273
      )
    );
  Moonbit_object_header(_try$273$2135)->meta
  = Moonbit_make_regular_object_header(
    offsetof(
      struct $$mizchi$process_pool$moonbit_test_driver_internal_do_execute$handle_result$53$test_should_be_skipped$54$on_err$55$$2a$arm$290$$2a$arm$283$lambda$427$State$$2a$try$273,
        $0
    )
    >> 2,
      5,
      0
  );
  ((struct $$mizchi$process_pool$moonbit_test_driver_internal_do_execute$handle_result$53$test_should_be_skipped$54$on_err$55$$2a$arm$290$$2a$arm$283$lambda$427$State$$2a$try$273*)_try$273$2135)->$0
  = _cont_param$1036;
  ((struct $$mizchi$process_pool$moonbit_test_driver_internal_do_execute$handle_result$53$test_should_be_skipped$54$on_err$55$$2a$arm$290$$2a$arm$283$lambda$427$State$$2a$try$273*)_try$273$2135)->$1
  = on_err$914;
  ((struct $$mizchi$process_pool$moonbit_test_driver_internal_do_execute$handle_result$53$test_should_be_skipped$54$on_err$55$$2a$arm$290$$2a$arm$283$lambda$427$State$$2a$try$273*)_try$273$2135)->$2
  = name$1011;
  ((struct $$mizchi$process_pool$moonbit_test_driver_internal_do_execute$handle_result$53$test_should_be_skipped$54$on_err$55$$2a$arm$290$$2a$arm$283$lambda$427$State$$2a$try$273*)_try$273$2135)->$3
  = _cont$1015;
  ((struct $$mizchi$process_pool$moonbit_test_driver_internal_do_execute$handle_result$53$test_should_be_skipped$54$on_err$55$$2a$arm$290$$2a$arm$283$lambda$427$State$$2a$try$273*)_try$273$2135)->$4
  = _err_cont$1016;
  $$mizchi$process_pool$moonbit_test_driver_internal_do_execute$$2a$async_driver$fn$14(
    _async_driver$1017, _try$273$2135
  );
  return 0;
}

int32_t $$mizchi$process_pool$moonbit_test_driver_internal_do_execute$$2a$async_driver$fn$14(
  int32_t _env$2127,
  void* _state$1018
) {
  switch (Moonbit_object_tag(_state$1018)) {
    case 0: {
      struct $$mizchi$process_pool$moonbit_test_driver_internal_do_execute$handle_result$53$test_should_be_skipped$54$on_err$55$$2a$arm$290$$2a$arm$283$lambda$427$State$$2a$try$273* _$2a$try$273$1019 =
        (struct $$mizchi$process_pool$moonbit_test_driver_internal_do_execute$handle_result$53$test_should_be_skipped$54$on_err$55$$2a$arm$290$$2a$arm$283$lambda$427$State$$2a$try$273*)_state$1018;
      struct $$3c$Error$3e$$3d$$3e$Unit* _field$2305 = _$2a$try$273$1019->$4;
      struct $$3c$Error$3e$$3d$$3e$Unit* _err_cont$1020 = _field$2305;
      struct $$3c$Unit$3e$$3d$$3e$Unit* _field$2304 = _$2a$try$273$1019->$3;
      struct $$3c$Unit$3e$$3d$$3e$Unit* _cont$1021 = _field$2304;
      moonbit_string_t _field$2303 = _$2a$try$273$1019->$2;
      moonbit_string_t name$1022 = _field$2303;
      struct $$3c$String$2a$Error$3e$$3d$$3e$Unit* _field$2302 =
        _$2a$try$273$1019->$1;
      struct $$3c$String$2a$Error$3e$$3d$$3e$Unit* on_err$1023 = _field$2302;
      void* _field$2301 = _$2a$try$273$1019->$0;
      int32_t _cnt$2609 = Moonbit_object_header(_$2a$try$273$1019)->rc;
      void* _try_err$1024;
      void* err$1026;
      void* err$1028;
      int32_t _tmp$2129;
      if (_cnt$2609 > 1) {
        int32_t _new_cnt$2610 = _cnt$2609 - 1;
        Moonbit_object_header(_$2a$try$273$1019)->rc = _new_cnt$2610;
        moonbit_incref(_err_cont$1020);
        moonbit_incref(_cont$1021);
        moonbit_incref(name$1022);
        moonbit_incref(on_err$1023);
        moonbit_incref(_field$2301);
      } else if (_cnt$2609 == 1) {
        moonbit_free(_$2a$try$273$1019);
      }
      _try_err$1024 = _field$2301;
      if (
        $mizchi$process_pool$moonbit_test_driver_internal_is_being_cancelled()
      ) {
        moonbit_decref(on_err$1023);
        moonbit_decref(name$1022);
        moonbit_decref(_cont$1021);
        err$1028 = _try_err$1024;
        goto $join$1027;
      } else {
        moonbit_decref(_err_cont$1020);
        err$1026 = _try_err$1024;
        goto $join$1025;
      }
      goto $joinlet$2757;
      $join$1027:;
      return _err_cont$1020->code(_err_cont$1020, err$1028);
      $joinlet$2757:;
      goto $joinlet$2756;
      $join$1025:;
      _tmp$2129 = on_err$1023->code(on_err$1023, name$1022, err$1026);
      _cont$1021->code(_cont$1021, _tmp$2129);
      $joinlet$2756:;
      break;
    }
    default: {
      struct $$mizchi$process_pool$moonbit_test_driver_internal_do_execute$handle_result$53$test_should_be_skipped$54$on_err$55$$2a$arm$290$$2a$arm$283$lambda$427$State$State_1* _State_1$1029 =
        (struct $$mizchi$process_pool$moonbit_test_driver_internal_do_execute$handle_result$53$test_should_be_skipped$54$on_err$55$$2a$arm$290$$2a$arm$283$lambda$427$State$State_1*)_state$1018;
      struct $$3c$Unit$3e$$3d$$3e$Unit* _field$2308 = _State_1$1029->$3;
      struct $$3c$Unit$3e$$3d$$3e$Unit* _cont$1030 = _field$2308;
      moonbit_string_t _field$2307 = _State_1$1029->$2;
      moonbit_string_t name$1031 = _field$2307;
      struct $$3c$String$2a$String$2a$Bool$3e$$3d$$3e$Unit* _field$2306 =
        _State_1$1029->$1;
      int32_t _cnt$2611 = Moonbit_object_header(_State_1$1029)->rc;
      struct $$3c$String$2a$String$2a$Bool$3e$$3d$$3e$Unit* handle_result$1032;
      int32_t _tmp$2128;
      if (_cnt$2611 > 1) {
        int32_t _new_cnt$2612 = _cnt$2611 - 1;
        Moonbit_object_header(_State_1$1029)->rc = _new_cnt$2612;
        moonbit_incref(_cont$1030);
        moonbit_incref(name$1031);
        moonbit_incref(_field$2306);
      } else if (_cnt$2611 == 1) {
        moonbit_free(_State_1$1029);
      }
      handle_result$1032 = _field$2306;
      _tmp$2128
      = handle_result$1032->code(
        handle_result$1032,
          name$1031,
          (moonbit_string_t)moonbit_string_literal_0.data,
          0
      );
      _cont$1030->code(_cont$1030, _tmp$2128);
      break;
    }
  }
  return 0;
}

int32_t $$mizchi$process_pool$moonbit_test_driver_internal_do_execute$fn$9(
  struct $$3c$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit* _env$2108,
  struct $$3c$Unit$3e$$3d$$3e$Unit* _cont$976,
  struct $$3c$Error$3e$$3d$$3e$Unit* _err_cont$977
) {
  struct $$mizchi$process_pool$moonbit_test_driver_internal_do_execute$fn$9$2d$cap* _casted_env$2109 =
    (struct $$mizchi$process_pool$moonbit_test_driver_internal_do_execute$fn$9$2d$cap*)_env$2108;
  struct $$3c$String$2a$String$2a$Bool$3e$$3d$$3e$Unit* _field$2312 =
    _casted_env$2109->$3;
  struct $$3c$String$2a$String$2a$Bool$3e$$3d$$3e$Unit* handle_result$892 =
    _field$2312;
  struct $$3c$String$2a$Error$3e$$3d$$3e$Unit* _field$2311 =
    _casted_env$2109->$2;
  struct $$3c$String$2a$Error$3e$$3d$$3e$Unit* on_err$914 = _field$2311;
  struct $$3c$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit* _field$2310 =
    _casted_env$2109->$1;
  struct $$3c$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit* f$969 =
    _field$2310;
  moonbit_string_t _field$2309 = _casted_env$2109->$0;
  int32_t _cnt$2613 = Moonbit_object_header(_casted_env$2109)->rc;
  moonbit_string_t name$971;
  int32_t _async_driver$978;
  struct $$mizchi$process_pool$moonbit_test_driver_internal_do_execute$fn$12$2d$cap* _closure$2758;
  struct $$3c$Unit$3e$$3d$$3e$Unit* _tmp$2113;
  struct $$mizchi$process_pool$moonbit_test_driver_internal_do_execute$fn$11$2d$cap* _closure$2759;
  struct $$3c$Error$3e$$3d$$3e$Unit* _tmp$2114;
  if (_cnt$2613 > 1) {
    int32_t _new_cnt$2614 = _cnt$2613 - 1;
    Moonbit_object_header(_casted_env$2109)->rc = _new_cnt$2614;
    moonbit_incref(handle_result$892);
    moonbit_incref(on_err$914);
    moonbit_incref(f$969);
    moonbit_incref(_field$2309);
  } else if (_cnt$2613 == 1) {
    moonbit_free(_casted_env$2109);
  }
  name$971 = _field$2309;
  _async_driver$978 = 0;
  moonbit_incref(_cont$976);
  moonbit_incref(name$971);
  _closure$2758
  = (struct $$mizchi$process_pool$moonbit_test_driver_internal_do_execute$fn$12$2d$cap*)moonbit_malloc(
      sizeof(
        struct $$mizchi$process_pool$moonbit_test_driver_internal_do_execute$fn$12$2d$cap
      )
    );
  Moonbit_object_header(_closure$2758)->meta
  = Moonbit_make_regular_object_header(
    offsetof(
      struct $$mizchi$process_pool$moonbit_test_driver_internal_do_execute$fn$12$2d$cap,
        $1
    )
    >> 2,
      3,
      0
  );
  _closure$2758->code
  = &$$mizchi$process_pool$moonbit_test_driver_internal_do_execute$fn$12;
  _closure$2758->$0 = _async_driver$978;
  _closure$2758->$1 = _cont$976;
  _closure$2758->$2 = name$971;
  _closure$2758->$3 = handle_result$892;
  _tmp$2113 = (struct $$3c$Unit$3e$$3d$$3e$Unit*)_closure$2758;
  _closure$2759
  = (struct $$mizchi$process_pool$moonbit_test_driver_internal_do_execute$fn$11$2d$cap*)moonbit_malloc(
      sizeof(
        struct $$mizchi$process_pool$moonbit_test_driver_internal_do_execute$fn$11$2d$cap
      )
    );
  Moonbit_object_header(_closure$2759)->meta
  = Moonbit_make_regular_object_header(
    offsetof(
      struct $$mizchi$process_pool$moonbit_test_driver_internal_do_execute$fn$11$2d$cap,
        $1
    )
    >> 2,
      4,
      0
  );
  _closure$2759->code
  = &$$mizchi$process_pool$moonbit_test_driver_internal_do_execute$fn$11;
  _closure$2759->$0 = _async_driver$978;
  _closure$2759->$1 = _err_cont$977;
  _closure$2759->$2 = _cont$976;
  _closure$2759->$3 = name$971;
  _closure$2759->$4 = on_err$914;
  _tmp$2114 = (struct $$3c$Error$3e$$3d$$3e$Unit*)_closure$2759;
  f$969->code(f$969, _tmp$2113, _tmp$2114);
  return 0;
}

int32_t $$mizchi$process_pool$moonbit_test_driver_internal_do_execute$fn$12(
  struct $$3c$Unit$3e$$3d$$3e$Unit* _env$2118,
  int32_t _cont_param$996
) {
  struct $$mizchi$process_pool$moonbit_test_driver_internal_do_execute$fn$12$2d$cap* _casted_env$2119 =
    (struct $$mizchi$process_pool$moonbit_test_driver_internal_do_execute$fn$12$2d$cap*)_env$2118;
  struct $$3c$String$2a$String$2a$Bool$3e$$3d$$3e$Unit* _field$2316 =
    _casted_env$2119->$3;
  struct $$3c$String$2a$String$2a$Bool$3e$$3d$$3e$Unit* handle_result$892 =
    _field$2316;
  moonbit_string_t _field$2315 = _casted_env$2119->$2;
  moonbit_string_t name$971 = _field$2315;
  struct $$3c$Unit$3e$$3d$$3e$Unit* _field$2314 = _casted_env$2119->$1;
  struct $$3c$Unit$3e$$3d$$3e$Unit* _cont$976 = _field$2314;
  int32_t _field$2313 = _casted_env$2119->$0;
  int32_t _cnt$2615 = Moonbit_object_header(_casted_env$2119)->rc;
  int32_t _async_driver$978;
  void* State_1$2120;
  if (_cnt$2615 > 1) {
    int32_t _new_cnt$2616 = _cnt$2615 - 1;
    Moonbit_object_header(_casted_env$2119)->rc = _new_cnt$2616;
    moonbit_incref(handle_result$892);
    moonbit_incref(name$971);
    moonbit_incref(_cont$976);
  } else if (_cnt$2615 == 1) {
    moonbit_free(_casted_env$2119);
  }
  _async_driver$978 = _field$2313;
  State_1$2120
  = (void*)moonbit_malloc(
      sizeof(
        struct $$mizchi$process_pool$moonbit_test_driver_internal_do_execute$handle_result$53$test_should_be_skipped$54$on_err$55$$2a$arm$268$$2a$arm$261$lambda$410$State$State_1
      )
    );
  Moonbit_object_header(State_1$2120)->meta
  = Moonbit_make_regular_object_header(
    offsetof(
      struct $$mizchi$process_pool$moonbit_test_driver_internal_do_execute$handle_result$53$test_should_be_skipped$54$on_err$55$$2a$arm$268$$2a$arm$261$lambda$410$State$State_1,
        $1
    )
    >> 2,
      3,
      1
  );
  ((struct $$mizchi$process_pool$moonbit_test_driver_internal_do_execute$handle_result$53$test_should_be_skipped$54$on_err$55$$2a$arm$268$$2a$arm$261$lambda$410$State$State_1*)State_1$2120)->$0
  = _cont_param$996;
  ((struct $$mizchi$process_pool$moonbit_test_driver_internal_do_execute$handle_result$53$test_should_be_skipped$54$on_err$55$$2a$arm$268$$2a$arm$261$lambda$410$State$State_1*)State_1$2120)->$1
  = handle_result$892;
  ((struct $$mizchi$process_pool$moonbit_test_driver_internal_do_execute$handle_result$53$test_should_be_skipped$54$on_err$55$$2a$arm$268$$2a$arm$261$lambda$410$State$State_1*)State_1$2120)->$2
  = name$971;
  ((struct $$mizchi$process_pool$moonbit_test_driver_internal_do_execute$handle_result$53$test_should_be_skipped$54$on_err$55$$2a$arm$268$$2a$arm$261$lambda$410$State$State_1*)State_1$2120)->$3
  = _cont$976;
  $$mizchi$process_pool$moonbit_test_driver_internal_do_execute$$2a$async_driver$fn$10(
    _async_driver$978, State_1$2120
  );
  return 0;
}

int32_t $$mizchi$process_pool$moonbit_test_driver_internal_do_execute$fn$11(
  struct $$3c$Error$3e$$3d$$3e$Unit* _env$2115,
  void* _cont_param$997
) {
  struct $$mizchi$process_pool$moonbit_test_driver_internal_do_execute$fn$11$2d$cap* _casted_env$2116 =
    (struct $$mizchi$process_pool$moonbit_test_driver_internal_do_execute$fn$11$2d$cap*)_env$2115;
  struct $$3c$String$2a$Error$3e$$3d$$3e$Unit* _field$2321 =
    _casted_env$2116->$4;
  struct $$3c$String$2a$Error$3e$$3d$$3e$Unit* on_err$914 = _field$2321;
  moonbit_string_t _field$2320 = _casted_env$2116->$3;
  moonbit_string_t name$971 = _field$2320;
  struct $$3c$Unit$3e$$3d$$3e$Unit* _field$2319 = _casted_env$2116->$2;
  struct $$3c$Unit$3e$$3d$$3e$Unit* _cont$976 = _field$2319;
  struct $$3c$Error$3e$$3d$$3e$Unit* _field$2318 = _casted_env$2116->$1;
  struct $$3c$Error$3e$$3d$$3e$Unit* _err_cont$977 = _field$2318;
  int32_t _field$2317 = _casted_env$2116->$0;
  int32_t _cnt$2617 = Moonbit_object_header(_casted_env$2116)->rc;
  int32_t _async_driver$978;
  void* _try$251$2117;
  if (_cnt$2617 > 1) {
    int32_t _new_cnt$2618 = _cnt$2617 - 1;
    Moonbit_object_header(_casted_env$2116)->rc = _new_cnt$2618;
    moonbit_incref(on_err$914);
    moonbit_incref(name$971);
    moonbit_incref(_cont$976);
    moonbit_incref(_err_cont$977);
  } else if (_cnt$2617 == 1) {
    moonbit_free(_casted_env$2116);
  }
  _async_driver$978 = _field$2317;
  _try$251$2117
  = (void*)moonbit_malloc(
      sizeof(
        struct $$mizchi$process_pool$moonbit_test_driver_internal_do_execute$handle_result$53$test_should_be_skipped$54$on_err$55$$2a$arm$268$$2a$arm$261$lambda$410$State$$2a$try$251
      )
    );
  Moonbit_object_header(_try$251$2117)->meta
  = Moonbit_make_regular_object_header(
    offsetof(
      struct $$mizchi$process_pool$moonbit_test_driver_internal_do_execute$handle_result$53$test_should_be_skipped$54$on_err$55$$2a$arm$268$$2a$arm$261$lambda$410$State$$2a$try$251,
        $0
    )
    >> 2,
      5,
      0
  );
  ((struct $$mizchi$process_pool$moonbit_test_driver_internal_do_execute$handle_result$53$test_should_be_skipped$54$on_err$55$$2a$arm$268$$2a$arm$261$lambda$410$State$$2a$try$251*)_try$251$2117)->$0
  = _cont_param$997;
  ((struct $$mizchi$process_pool$moonbit_test_driver_internal_do_execute$handle_result$53$test_should_be_skipped$54$on_err$55$$2a$arm$268$$2a$arm$261$lambda$410$State$$2a$try$251*)_try$251$2117)->$1
  = on_err$914;
  ((struct $$mizchi$process_pool$moonbit_test_driver_internal_do_execute$handle_result$53$test_should_be_skipped$54$on_err$55$$2a$arm$268$$2a$arm$261$lambda$410$State$$2a$try$251*)_try$251$2117)->$2
  = name$971;
  ((struct $$mizchi$process_pool$moonbit_test_driver_internal_do_execute$handle_result$53$test_should_be_skipped$54$on_err$55$$2a$arm$268$$2a$arm$261$lambda$410$State$$2a$try$251*)_try$251$2117)->$3
  = _cont$976;
  ((struct $$mizchi$process_pool$moonbit_test_driver_internal_do_execute$handle_result$53$test_should_be_skipped$54$on_err$55$$2a$arm$268$$2a$arm$261$lambda$410$State$$2a$try$251*)_try$251$2117)->$4
  = _err_cont$977;
  $$mizchi$process_pool$moonbit_test_driver_internal_do_execute$$2a$async_driver$fn$10(
    _async_driver$978, _try$251$2117
  );
  return 0;
}

int32_t $$mizchi$process_pool$moonbit_test_driver_internal_do_execute$$2a$async_driver$fn$10(
  int32_t _env$2110,
  void* _state$979
) {
  switch (Moonbit_object_tag(_state$979)) {
    case 0: {
      struct $$mizchi$process_pool$moonbit_test_driver_internal_do_execute$handle_result$53$test_should_be_skipped$54$on_err$55$$2a$arm$268$$2a$arm$261$lambda$410$State$$2a$try$251* _$2a$try$251$980 =
        (struct $$mizchi$process_pool$moonbit_test_driver_internal_do_execute$handle_result$53$test_should_be_skipped$54$on_err$55$$2a$arm$268$$2a$arm$261$lambda$410$State$$2a$try$251*)_state$979;
      struct $$3c$Error$3e$$3d$$3e$Unit* _field$2326 = _$2a$try$251$980->$4;
      struct $$3c$Error$3e$$3d$$3e$Unit* _err_cont$981 = _field$2326;
      struct $$3c$Unit$3e$$3d$$3e$Unit* _field$2325 = _$2a$try$251$980->$3;
      struct $$3c$Unit$3e$$3d$$3e$Unit* _cont$982 = _field$2325;
      moonbit_string_t _field$2324 = _$2a$try$251$980->$2;
      moonbit_string_t name$983 = _field$2324;
      struct $$3c$String$2a$Error$3e$$3d$$3e$Unit* _field$2323 =
        _$2a$try$251$980->$1;
      struct $$3c$String$2a$Error$3e$$3d$$3e$Unit* on_err$984 = _field$2323;
      void* _field$2322 = _$2a$try$251$980->$0;
      int32_t _cnt$2619 = Moonbit_object_header(_$2a$try$251$980)->rc;
      void* _try_err$985;
      void* err$987;
      void* err$989;
      int32_t _tmp$2112;
      if (_cnt$2619 > 1) {
        int32_t _new_cnt$2620 = _cnt$2619 - 1;
        Moonbit_object_header(_$2a$try$251$980)->rc = _new_cnt$2620;
        moonbit_incref(_err_cont$981);
        moonbit_incref(_cont$982);
        moonbit_incref(name$983);
        moonbit_incref(on_err$984);
        moonbit_incref(_field$2322);
      } else if (_cnt$2619 == 1) {
        moonbit_free(_$2a$try$251$980);
      }
      _try_err$985 = _field$2322;
      if (
        $mizchi$process_pool$moonbit_test_driver_internal_is_being_cancelled()
      ) {
        moonbit_decref(on_err$984);
        moonbit_decref(name$983);
        moonbit_decref(_cont$982);
        err$989 = _try_err$985;
        goto $join$988;
      } else {
        moonbit_decref(_err_cont$981);
        err$987 = _try_err$985;
        goto $join$986;
      }
      goto $joinlet$2761;
      $join$988:;
      return _err_cont$981->code(_err_cont$981, err$989);
      $joinlet$2761:;
      goto $joinlet$2760;
      $join$986:;
      _tmp$2112 = on_err$984->code(on_err$984, name$983, err$987);
      _cont$982->code(_cont$982, _tmp$2112);
      $joinlet$2760:;
      break;
    }
    default: {
      struct $$mizchi$process_pool$moonbit_test_driver_internal_do_execute$handle_result$53$test_should_be_skipped$54$on_err$55$$2a$arm$268$$2a$arm$261$lambda$410$State$State_1* _State_1$990 =
        (struct $$mizchi$process_pool$moonbit_test_driver_internal_do_execute$handle_result$53$test_should_be_skipped$54$on_err$55$$2a$arm$268$$2a$arm$261$lambda$410$State$State_1*)_state$979;
      struct $$3c$Unit$3e$$3d$$3e$Unit* _field$2329 = _State_1$990->$3;
      struct $$3c$Unit$3e$$3d$$3e$Unit* _cont$991 = _field$2329;
      moonbit_string_t _field$2328 = _State_1$990->$2;
      moonbit_string_t name$992 = _field$2328;
      struct $$3c$String$2a$String$2a$Bool$3e$$3d$$3e$Unit* _field$2327 =
        _State_1$990->$1;
      int32_t _cnt$2621 = Moonbit_object_header(_State_1$990)->rc;
      struct $$3c$String$2a$String$2a$Bool$3e$$3d$$3e$Unit* handle_result$993;
      int32_t _tmp$2111;
      if (_cnt$2621 > 1) {
        int32_t _new_cnt$2622 = _cnt$2621 - 1;
        Moonbit_object_header(_State_1$990)->rc = _new_cnt$2622;
        moonbit_incref(_cont$991);
        moonbit_incref(name$992);
        moonbit_incref(_field$2327);
      } else if (_cnt$2621 == 1) {
        moonbit_free(_State_1$990);
      }
      handle_result$993 = _field$2327;
      _tmp$2111
      = handle_result$993->code(
        handle_result$993,
          name$992,
          (moonbit_string_t)moonbit_string_literal_0.data,
          0
      );
      _cont$991->code(_cont$991, _tmp$2111);
      break;
    }
  }
  return 0;
}

struct moonbit_result_0 $$mizchi$process_pool$moonbit_test_driver_internal_do_execute$fn$8(
  struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$* _env$2101
) {
  struct $$mizchi$process_pool$moonbit_test_driver_internal_do_execute$fn$8$2d$cap* _casted_env$2102 =
    (struct $$mizchi$process_pool$moonbit_test_driver_internal_do_execute$fn$8$2d$cap*)_env$2101;
  struct $$3c$Int$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$* _field$2331 =
    _casted_env$2102->$1;
  struct $$3c$Int$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$* f$951 =
    _field$2331;
  moonbit_string_t _field$2330 = _casted_env$2102->$0;
  int32_t _cnt$2623 = Moonbit_object_header(_casted_env$2102)->rc;
  moonbit_string_t name$953;
  int32_t _tmp$2103;
  if (_cnt$2623 > 1) {
    int32_t _new_cnt$2624 = _cnt$2623 - 1;
    Moonbit_object_header(_casted_env$2102)->rc = _new_cnt$2624;
    moonbit_incref(f$951);
    moonbit_incref(_field$2330);
  } else if (_cnt$2623 == 1) {
    moonbit_free(_casted_env$2102);
  }
  name$953 = _field$2330;
  _tmp$2103
  = $mizchi$process_pool$moonbit_test_driver_internal_new_test_arg(
    name$953
  );
  return f$951->code(f$951, _tmp$2103);
}

int32_t $$mizchi$process_pool$moonbit_test_driver_internal_do_execute$fn$7(
  struct $$3c$$3e$$3d$$3e$Unit* _env$2099
) {
  struct $$mizchi$process_pool$moonbit_test_driver_internal_do_execute$fn$7$2d$cap* _casted_env$2100 =
    (struct $$mizchi$process_pool$moonbit_test_driver_internal_do_execute$fn$7$2d$cap*)_env$2099;
  struct $$3c$String$2a$String$2a$Bool$3e$$3d$$3e$Unit* _field$2333 =
    _casted_env$2100->$1;
  struct $$3c$String$2a$String$2a$Bool$3e$$3d$$3e$Unit* handle_result$892 =
    _field$2333;
  moonbit_string_t _field$2332 = _casted_env$2100->$0;
  int32_t _cnt$2625 = Moonbit_object_header(_casted_env$2100)->rc;
  moonbit_string_t name$953;
  if (_cnt$2625 > 1) {
    int32_t _new_cnt$2626 = _cnt$2625 - 1;
    Moonbit_object_header(_casted_env$2100)->rc = _new_cnt$2626;
    moonbit_incref(handle_result$892);
    moonbit_incref(_field$2332);
  } else if (_cnt$2625 == 1) {
    moonbit_free(_casted_env$2100);
  }
  name$953 = _field$2332;
  $$mizchi$process_pool$moonbit_test_driver_internal_do_execute$handle_result$fn$1(
    handle_result$892,
      name$953,
      (moonbit_string_t)moonbit_string_literal_0.data,
      0
  );
  return 0;
}

int32_t $$mizchi$process_pool$moonbit_test_driver_internal_do_execute$fn$6(
  struct $$3c$Error$3e$$3d$$3e$Unit* _env$2097,
  void* err$957
) {
  struct $$mizchi$process_pool$moonbit_test_driver_internal_do_execute$fn$6$2d$cap* _casted_env$2098 =
    (struct $$mizchi$process_pool$moonbit_test_driver_internal_do_execute$fn$6$2d$cap*)_env$2097;
  struct $$3c$String$2a$Error$3e$$3d$$3e$Unit* _field$2335 =
    _casted_env$2098->$1;
  struct $$3c$String$2a$Error$3e$$3d$$3e$Unit* on_err$914 = _field$2335;
  moonbit_string_t _field$2334 = _casted_env$2098->$0;
  int32_t _cnt$2627 = Moonbit_object_header(_casted_env$2098)->rc;
  moonbit_string_t name$953;
  if (_cnt$2627 > 1) {
    int32_t _new_cnt$2628 = _cnt$2627 - 1;
    Moonbit_object_header(_casted_env$2098)->rc = _new_cnt$2628;
    moonbit_incref(on_err$914);
    moonbit_incref(_field$2334);
  } else if (_cnt$2627 == 1) {
    moonbit_free(_casted_env$2098);
  }
  name$953 = _field$2334;
  $$mizchi$process_pool$moonbit_test_driver_internal_do_execute$on_err$fn$3(
    on_err$914, name$953, err$957
  );
  return 0;
}

int32_t $$mizchi$process_pool$moonbit_test_driver_internal_do_execute$fn$5(
  struct $$3c$$3e$$3d$$3e$Unit* _env$2089
) {
  struct $$mizchi$process_pool$moonbit_test_driver_internal_do_execute$fn$5$2d$cap* _casted_env$2090 =
    (struct $$mizchi$process_pool$moonbit_test_driver_internal_do_execute$fn$5$2d$cap*)_env$2089;
  struct $$3c$String$2a$String$2a$Bool$3e$$3d$$3e$Unit* _field$2337 =
    _casted_env$2090->$1;
  struct $$3c$String$2a$String$2a$Bool$3e$$3d$$3e$Unit* handle_result$892 =
    _field$2337;
  moonbit_string_t _field$2336 = _casted_env$2090->$0;
  int32_t _cnt$2629 = Moonbit_object_header(_casted_env$2090)->rc;
  moonbit_string_t name$935;
  if (_cnt$2629 > 1) {
    int32_t _new_cnt$2630 = _cnt$2629 - 1;
    Moonbit_object_header(_casted_env$2090)->rc = _new_cnt$2630;
    moonbit_incref(handle_result$892);
    moonbit_incref(_field$2336);
  } else if (_cnt$2629 == 1) {
    moonbit_free(_casted_env$2090);
  }
  name$935 = _field$2336;
  $$mizchi$process_pool$moonbit_test_driver_internal_do_execute$handle_result$fn$1(
    handle_result$892,
      name$935,
      (moonbit_string_t)moonbit_string_literal_0.data,
      0
  );
  return 0;
}

int32_t $$mizchi$process_pool$moonbit_test_driver_internal_do_execute$fn$4(
  struct $$3c$Error$3e$$3d$$3e$Unit* _env$2087,
  void* err$939
) {
  struct $$mizchi$process_pool$moonbit_test_driver_internal_do_execute$fn$4$2d$cap* _casted_env$2088 =
    (struct $$mizchi$process_pool$moonbit_test_driver_internal_do_execute$fn$4$2d$cap*)_env$2087;
  struct $$3c$String$2a$Error$3e$$3d$$3e$Unit* _field$2339 =
    _casted_env$2088->$1;
  struct $$3c$String$2a$Error$3e$$3d$$3e$Unit* on_err$914 = _field$2339;
  moonbit_string_t _field$2338 = _casted_env$2088->$0;
  int32_t _cnt$2631 = Moonbit_object_header(_casted_env$2088)->rc;
  moonbit_string_t name$935;
  if (_cnt$2631 > 1) {
    int32_t _new_cnt$2632 = _cnt$2631 - 1;
    Moonbit_object_header(_casted_env$2088)->rc = _new_cnt$2632;
    moonbit_incref(on_err$914);
    moonbit_incref(_field$2338);
  } else if (_cnt$2631 == 1) {
    moonbit_free(_casted_env$2088);
  }
  name$935 = _field$2338;
  $$mizchi$process_pool$moonbit_test_driver_internal_do_execute$on_err$fn$3(
    on_err$914, name$935, err$939
  );
  return 0;
}

int32_t $$mizchi$process_pool$moonbit_test_driver_internal_do_execute$on_err$fn$3(
  struct $$3c$String$2a$Error$3e$$3d$$3e$Unit* _env$2082,
  moonbit_string_t name$915,
  void* err$916
) {
  struct $$mizchi$process_pool$moonbit_test_driver_internal_do_execute$on_err$fn$3$2d$cap* _casted_env$2083 =
    (struct $$mizchi$process_pool$moonbit_test_driver_internal_do_execute$on_err$fn$3$2d$cap*)_env$2082;
  struct $$3c$String$2a$String$2a$Bool$3e$$3d$$3e$Unit* _field$2344 =
    _casted_env$2083->$0;
  int32_t _cnt$2633 = Moonbit_object_header(_casted_env$2083)->rc;
  struct $$3c$String$2a$String$2a$Bool$3e$$3d$$3e$Unit* handle_result$892;
  void* e$918;
  moonbit_string_t e$921;
  moonbit_string_t message$919;
  if (_cnt$2633 > 1) {
    int32_t _new_cnt$2634 = _cnt$2633 - 1;
    Moonbit_object_header(_casted_env$2083)->rc = _new_cnt$2634;
    moonbit_incref(_field$2344);
  } else if (_cnt$2633 == 1) {
    moonbit_free(_casted_env$2083);
  }
  handle_result$892 = _field$2344;
  switch (Moonbit_object_tag(err$916)) {
    case 2: {
      struct $Error$moonbitlang$core$builtin$Failure$Failure* _Failure$922 =
        (struct $Error$moonbitlang$core$builtin$Failure$Failure*)err$916;
      moonbit_string_t _field$2340 = _Failure$922->$0;
      int32_t _cnt$2635 = Moonbit_object_header(_Failure$922)->rc;
      moonbit_string_t _e$923;
      if (_cnt$2635 > 1) {
        int32_t _new_cnt$2636 = _cnt$2635 - 1;
        Moonbit_object_header(_Failure$922)->rc = _new_cnt$2636;
        moonbit_incref(_field$2340);
      } else if (_cnt$2635 == 1) {
        moonbit_free(_Failure$922);
      }
      _e$923 = _field$2340;
      e$921 = _e$923;
      goto $join$920;
      break;
    }
    
    case 3: {
      struct $Error$moonbitlang$core$builtin$InspectError$InspectError* _InspectError$924 =
        (struct $Error$moonbitlang$core$builtin$InspectError$InspectError*)err$916;
      moonbit_string_t _field$2341 = _InspectError$924->$0;
      int32_t _cnt$2637 = Moonbit_object_header(_InspectError$924)->rc;
      moonbit_string_t _e$925;
      if (_cnt$2637 > 1) {
        int32_t _new_cnt$2638 = _cnt$2637 - 1;
        Moonbit_object_header(_InspectError$924)->rc = _new_cnt$2638;
        moonbit_incref(_field$2341);
      } else if (_cnt$2637 == 1) {
        moonbit_free(_InspectError$924);
      }
      _e$925 = _field$2341;
      e$921 = _e$925;
      goto $join$920;
      break;
    }
    
    case 4: {
      struct $Error$moonbitlang$core$builtin$SnapshotError$SnapshotError* _SnapshotError$926 =
        (struct $Error$moonbitlang$core$builtin$SnapshotError$SnapshotError*)err$916;
      moonbit_string_t _field$2342 = _SnapshotError$926->$0;
      int32_t _cnt$2639 = Moonbit_object_header(_SnapshotError$926)->rc;
      moonbit_string_t _e$927;
      if (_cnt$2639 > 1) {
        int32_t _new_cnt$2640 = _cnt$2639 - 1;
        Moonbit_object_header(_SnapshotError$926)->rc = _new_cnt$2640;
        moonbit_incref(_field$2342);
      } else if (_cnt$2639 == 1) {
        moonbit_free(_SnapshotError$926);
      }
      _e$927 = _field$2342;
      e$921 = _e$927;
      goto $join$920;
      break;
    }
    
    case 5: {
      struct $Error$mizchi$process_pool$MoonBitTestDriverInternalJsError$MoonBitTestDriverInternalJsError* _MoonBitTestDriverInternalJsError$928 =
        (struct $Error$mizchi$process_pool$MoonBitTestDriverInternalJsError$MoonBitTestDriverInternalJsError*)err$916;
      moonbit_string_t _field$2343 =
        _MoonBitTestDriverInternalJsError$928->$0;
      int32_t _cnt$2641 =
        Moonbit_object_header(_MoonBitTestDriverInternalJsError$928)->rc;
      moonbit_string_t _e$929;
      if (_cnt$2641 > 1) {
        int32_t _new_cnt$2642 = _cnt$2641 - 1;
        Moonbit_object_header(_MoonBitTestDriverInternalJsError$928)->rc
        = _new_cnt$2642;
        moonbit_incref(_field$2343);
      } else if (_cnt$2641 == 1) {
        moonbit_free(_MoonBitTestDriverInternalJsError$928);
      }
      _e$929 = _field$2343;
      e$921 = _e$929;
      goto $join$920;
      break;
    }
    default: {
      e$918 = err$916;
      goto $join$917;
      break;
    }
  }
  goto $joinlet$2763;
  $join$920:;
  $$mizchi$process_pool$moonbit_test_driver_internal_do_execute$handle_result$fn$1(
    handle_result$892, name$915, e$921, 0
  );
  $joinlet$2763:;
  goto $joinlet$2762;
  $join$917:;
  message$919 = $Error$to_string(e$918);
  $$mizchi$process_pool$moonbit_test_driver_internal_do_execute$handle_result$fn$1(
    handle_result$892, name$915, message$919, 0
  );
  $joinlet$2762:;
  return 0;
}

int32_t $$mizchi$process_pool$moonbit_test_driver_internal_do_execute$test_should_be_skipped$fn$2(
  struct $$mizchi$process_pool$moonbit_test_driver_internal_do_execute$test_should_be_skipped$fn$2$2d$cap* _env$2068,
  moonbit_string_t name$902,
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* attrs$903
) {
  struct $$3c$String$2a$String$2a$Bool$3e$$3d$$3e$Unit* _field$2347 =
    _env$2068->$1;
  struct $$3c$String$2a$String$2a$Bool$3e$$3d$$3e$Unit* handle_result$892 =
    _field$2347;
  moonbit_string_t _field$2346 = _env$2068->$0;
  int32_t _cnt$2643 = Moonbit_object_header(_env$2068)->rc;
  moonbit_string_t filename$897;
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* _arr$904;
  int32_t _len$905;
  int32_t _i$906;
  if (_cnt$2643 > 1) {
    int32_t _new_cnt$2644 = _cnt$2643 - 1;
    Moonbit_object_header(_env$2068)->rc = _new_cnt$2644;
    moonbit_incref(handle_result$892);
    moonbit_incref(_field$2346);
  } else if (_cnt$2643 == 1) {
    moonbit_free(_env$2068);
  }
  filename$897 = _field$2346;
  _arr$904 = attrs$903;
  moonbit_incref(_arr$904);
  _len$905 = $$moonbitlang$core$builtin$Array$$length$1(_arr$904);
  _i$906 = 0;
  while (1) {
    if (_i$906 < _len$905) {
      moonbit_string_t attr$907;
      int32_t _tmp$2070;
      int64_t _tmp$2069;
      int32_t _tmp$2081;
      moonbit_incref(_arr$904);
      attr$907
      = $$moonbitlang$core$builtin$Array$$unsafe_get$1(
        _arr$904, _i$906
      );
      _tmp$2070 = Moonbit_array_length(attr$907);
      _tmp$2069 = (int64_t)_tmp$2070;
      moonbit_incref(attr$907);
      if ($String$$char_length_ge$inner(attr$907, 5, 0, _tmp$2069)) {
        int32_t _tmp$2080 = attr$907[0];
        int32_t _x$908 = _tmp$2080;
        if (_x$908 == 112) {
          int32_t _tmp$2079 = attr$907[1];
          int32_t _x$909 = _tmp$2079;
          if (_x$909 == 97) {
            int32_t _tmp$2078 = attr$907[2];
            int32_t _x$910 = _tmp$2078;
            if (_x$910 == 110) {
              int32_t _tmp$2077 = attr$907[3];
              int32_t _x$911 = _tmp$2077;
              if (_x$911 == 105) {
                int32_t _tmp$2345 = attr$907[4];
                int32_t _tmp$2076;
                int32_t _x$912;
                moonbit_decref(attr$907);
                _tmp$2076 = _tmp$2345;
                _x$912 = _tmp$2076;
                if (_x$912 == 99) {
                  moonbit_string_t _tmp$2075;
                  moonbit_string_t _tmp$2074;
                  moonbit_string_t _tmp$2072;
                  moonbit_string_t _tmp$2073;
                  moonbit_string_t _tmp$2071;
                  moonbit_decref(_arr$904);
                  _tmp$2075
                  = $$moonbitlang$core$builtin$Show$$String$$to_string(
                    filename$897
                  );
                  _tmp$2074
                  = moonbit_add_string(
                    (moonbit_string_t)moonbit_string_literal_2.data,
                      _tmp$2075
                  );
                  _tmp$2072
                  = moonbit_add_string(
                    _tmp$2074,
                      (moonbit_string_t)moonbit_string_literal_3.data
                  );
                  moonbit_incref(name$902);
                  _tmp$2073
                  = $$moonbitlang$core$builtin$Show$$String$$to_string(
                    name$902
                  );
                  _tmp$2071 = moonbit_add_string(_tmp$2072, _tmp$2073);
                  $moonbitlang$core$builtin$println$0(_tmp$2071);
                  $$mizchi$process_pool$moonbit_test_driver_internal_do_execute$handle_result$fn$1(
                    handle_result$892,
                      name$902,
                      (moonbit_string_t)moonbit_string_literal_1.data,
                      1
                  );
                  return 1;
                }
              } else {
                moonbit_decref(attr$907);
              }
            } else {
              moonbit_decref(attr$907);
            }
          } else {
            moonbit_decref(attr$907);
          }
        } else {
          moonbit_decref(attr$907);
        }
      } else {
        moonbit_decref(attr$907);
      }
      _tmp$2081 = _i$906 + 1;
      _i$906 = _tmp$2081;
      continue;
    } else {
      moonbit_decref(_arr$904);
      moonbit_decref(name$902);
      moonbit_decref(filename$897);
      moonbit_decref(handle_result$892);
      return 0;
    }
    break;
  }
}

int32_t $$mizchi$process_pool$moonbit_test_driver_internal_do_execute$handle_result$fn$1(
  struct $$3c$String$2a$String$2a$Bool$3e$$3d$$3e$Unit* _env$2054,
  moonbit_string_t testname$893,
  moonbit_string_t message$894,
  int32_t skipped$895
) {
  struct $$mizchi$process_pool$moonbit_test_driver_internal_do_execute$handle_result$fn$1$2d$cap* _casted_env$2055 =
    (struct $$mizchi$process_pool$moonbit_test_driver_internal_do_execute$handle_result$fn$1$2d$cap*)_env$2054;
  moonbit_string_t _field$2349 = _casted_env$2055->$1;
  moonbit_string_t filename$897 = _field$2349;
  int32_t _field$2348 = _casted_env$2055->$0;
  int32_t _cnt$2645 = Moonbit_object_header(_casted_env$2055)->rc;
  int32_t index$900;
  int32_t _if_result$2765;
  moonbit_string_t file_name$896;
  moonbit_string_t test_name$898;
  moonbit_string_t message$899;
  moonbit_string_t _tmp$2067;
  moonbit_string_t _tmp$2066;
  moonbit_string_t _tmp$2064;
  moonbit_string_t _tmp$2065;
  moonbit_string_t _tmp$2063;
  moonbit_string_t _tmp$2061;
  moonbit_string_t _tmp$2062;
  moonbit_string_t _tmp$2060;
  moonbit_string_t _tmp$2058;
  moonbit_string_t _tmp$2059;
  moonbit_string_t _tmp$2057;
  moonbit_string_t _tmp$2056;
  if (_cnt$2645 > 1) {
    int32_t _new_cnt$2646 = _cnt$2645 - 1;
    Moonbit_object_header(_casted_env$2055)->rc = _new_cnt$2646;
    moonbit_incref(filename$897);
  } else if (_cnt$2645 == 1) {
    moonbit_free(_casted_env$2055);
  }
  index$900 = _field$2348;
  if (!skipped$895) {
    _if_result$2765 = 1;
  } else {
    _if_result$2765 = 0;
  }
  if (_if_result$2765) {
    
  }
  file_name$896 = $String$$escape(filename$897);
  test_name$898 = $String$$escape(testname$893);
  message$899 = $String$$escape(message$894);
  $moonbitlang$core$builtin$println$0(
    (moonbit_string_t)moonbit_string_literal_4.data
  );
  _tmp$2067
  = $$moonbitlang$core$builtin$Show$$String$$to_string(
    file_name$896
  );
  _tmp$2066
  = moonbit_add_string(
    (moonbit_string_t)moonbit_string_literal_5.data, _tmp$2067
  );
  _tmp$2064
  = moonbit_add_string(
    _tmp$2066, (moonbit_string_t)moonbit_string_literal_6.data
  );
  _tmp$2065
  = $$moonbitlang$core$builtin$Show$$$default_impl$$to_string$0(
    index$900
  );
  _tmp$2063 = moonbit_add_string(_tmp$2064, _tmp$2065);
  _tmp$2061
  = moonbit_add_string(
    _tmp$2063, (moonbit_string_t)moonbit_string_literal_7.data
  );
  _tmp$2062
  = $$moonbitlang$core$builtin$Show$$String$$to_string(
    test_name$898
  );
  _tmp$2060 = moonbit_add_string(_tmp$2061, _tmp$2062);
  _tmp$2058
  = moonbit_add_string(
    _tmp$2060, (moonbit_string_t)moonbit_string_literal_8.data
  );
  _tmp$2059 = $$moonbitlang$core$builtin$Show$$String$$to_string(message$899);
  _tmp$2057 = moonbit_add_string(_tmp$2058, _tmp$2059);
  _tmp$2056
  = moonbit_add_string(
    _tmp$2057, (moonbit_string_t)moonbit_string_literal_9.data
  );
  $moonbitlang$core$builtin$println$0(_tmp$2056);
  $moonbitlang$core$builtin$println$0(
    (moonbit_string_t)moonbit_string_literal_10.data
  );
  return 0;
}

int32_t $mizchi$process_pool$moonbit_test_driver_internal_new_test_arg(
  moonbit_string_t _discard_$891
) {
  moonbit_decref(_discard_$891);
  return 42;
}

int32_t $mizchi$process_pool$moonbit_test_driver_internal_is_being_cancelled(
  
) {
  return 0;
}

int32_t $mizchi$process_pool$moonbit_test_driver_internal_catch_error(
  struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$* f$889,
  struct $$3c$$3e$$3d$$3e$Unit* on_ok$890,
  struct $$3c$Error$3e$$3d$$3e$Unit* on_err$887
) {
  void* _try_err$885;
  struct moonbit_result_0 _tmp$2767 = f$889->code(f$889);
  void* err$886;
  if (_tmp$2767.tag) {
    int32_t const _ok$2052 = _tmp$2767.data.ok;
    moonbit_decref(on_err$887);
  } else {
    void* const _err$2053 = _tmp$2767.data.err;
    moonbit_decref(on_ok$890);
    _try_err$885 = _err$2053;
    goto $join$884;
  }
  on_ok$890->code(on_ok$890);
  goto $joinlet$2766;
  $join$884:;
  err$886 = _try_err$885;
  on_err$887->code(on_err$887, err$886);
  $joinlet$2766:;
  return 0;
}

int32_t $$moonbitlang$core$builtin$Show$$$moonbitlang$core$builtin$SourceLoc$$output(
  moonbit_string_t self$876,
  struct $$moonbitlang$core$builtin$Logger logger$877
) {
  moonbit_string_t _tmp$2051 = self$876;
  struct $$moonbitlang$core$builtin$SourceLocRepr* _tmp$2050 =
    $$moonbitlang$core$builtin$SourceLocRepr$$parse(_tmp$2051);
  $$moonbitlang$core$builtin$Show$$$moonbitlang$core$builtin$SourceLocRepr$$output(
    _tmp$2050, logger$877
  );
  return 0;
}

int32_t $$moonbitlang$core$builtin$Show$$$moonbitlang$core$builtin$SourceLocRepr$$output(
  struct $$moonbitlang$core$builtin$SourceLocRepr* self$839,
  struct $$moonbitlang$core$builtin$Logger logger$875
) {
  struct $StringView _field$2358 =
    (struct $StringView){self$839->$0_1, self$839->$0_2, self$839->$0_0};
  struct $StringView pkg$838 = _field$2358;
  moonbit_string_t _data$840;
  int32_t _start$841;
  int32_t _tmp$2049;
  int32_t _end$842;
  int32_t _cursor$843;
  int32_t accept_state$844;
  int32_t match_end$845;
  int32_t match_tag_saver_0$846;
  int32_t tag_0$847;
  struct $$3c$StringView$2a$Option$3c$StringView$3e$$3e$* _bind$848;
  struct $StringView _field$2357;
  struct $StringView _module_name$871;
  void* _field$2356;
  int32_t _cnt$2647;
  void* _package_name$872;
  struct $StringView _field$2354;
  struct $StringView filename$2003;
  struct $StringView _field$2353;
  struct $StringView start_line$2004;
  struct $StringView _field$2352;
  struct $StringView start_column$2005;
  struct $StringView _field$2351;
  struct $StringView end_line$2006;
  struct $StringView _field$2350;
  int32_t _cnt$2651;
  struct $StringView end_column$2007;
  struct $$moonbitlang$core$builtin$Logger _bind$2002;
  moonbit_incref(pkg$838.$0);
  moonbit_incref(pkg$838.$0);
  _data$840 = $StringView$$data(pkg$838);
  moonbit_incref(pkg$838.$0);
  _start$841 = $StringView$$start_offset(pkg$838);
  moonbit_incref(pkg$838.$0);
  _tmp$2049 = $StringView$$length(pkg$838);
  _end$842 = _start$841 + _tmp$2049;
  _cursor$843 = _start$841;
  accept_state$844 = -1;
  match_end$845 = -1;
  match_tag_saver_0$846 = -1;
  tag_0$847 = -1;
  while (1) {
    int32_t _tmp$2022 = _cursor$843;
    if (_tmp$2022 < _end$842) {
      int32_t _tmp$2048 = _cursor$843;
      int32_t next_char$858;
      int32_t _tmp$2023;
      moonbit_incref(_data$840);
      next_char$858 = $String$$unsafe_charcode_at(_data$840, _tmp$2048);
      _tmp$2023 = _cursor$843;
      _cursor$843 = _tmp$2023 + 1;
      if (next_char$858 < 55296) {
        if (next_char$858 < 47) {
          goto $join$856;
        } else if (next_char$858 > 47) {
          goto $join$856;
        } else {
          while (1) {
            int32_t _tmp$2024;
            tag_0$847 = _cursor$843;
            _tmp$2024 = _cursor$843;
            if (_tmp$2024 < _end$842) {
              int32_t _tmp$2041 = _cursor$843;
              int32_t next_char$861;
              int32_t _tmp$2025;
              moonbit_incref(_data$840);
              next_char$861
              = $String$$unsafe_charcode_at(
                _data$840, _tmp$2041
              );
              _tmp$2025 = _cursor$843;
              _cursor$843 = _tmp$2025 + 1;
              if (next_char$861 < 55296) {
                if (next_char$861 < 47) {
                  goto $join$859;
                } else if (next_char$861 > 47) {
                  goto $join$859;
                } else {
                  while (1) {
                    int32_t _tmp$2026 = _cursor$843;
                    if (_tmp$2026 < _end$842) {
                      int32_t _tmp$2034 = _cursor$843;
                      int32_t next_char$864;
                      int32_t _tmp$2027;
                      moonbit_incref(_data$840);
                      next_char$864
                      = $String$$unsafe_charcode_at(
                        _data$840, _tmp$2034
                      );
                      _tmp$2027 = _cursor$843;
                      _cursor$843 = _tmp$2027 + 1;
                      if (next_char$864 < 56319) {
                        if (next_char$864 < 55296) {
                          goto $join$862;
                        } else {
                          int32_t _tmp$2028 = _cursor$843;
                          if (_tmp$2028 < _end$842) {
                            int32_t _tmp$2030 = _cursor$843;
                            int32_t next_char$865;
                            int32_t _tmp$2029;
                            moonbit_incref(_data$840);
                            next_char$865
                            = $String$$unsafe_charcode_at(
                              _data$840, _tmp$2030
                            );
                            _tmp$2029 = _cursor$843;
                            _cursor$843 = _tmp$2029 + 1;
                            if (next_char$865 < 56320) {
                              goto $join$849;
                            } else if (next_char$865 > 65535) {
                              goto $join$849;
                            } else {
                              continue;
                            }
                          } else {
                            goto $join$849;
                          }
                        }
                      } else if (next_char$864 > 56319) {
                        if (next_char$864 < 65536) {
                          goto $join$862;
                        } else {
                          goto $join$849;
                        }
                      } else {
                        int32_t _tmp$2031 = _cursor$843;
                        if (_tmp$2031 < _end$842) {
                          int32_t _tmp$2033 = _cursor$843;
                          int32_t next_char$866;
                          int32_t _tmp$2032;
                          moonbit_incref(_data$840);
                          next_char$866
                          = $String$$unsafe_charcode_at(
                            _data$840, _tmp$2033
                          );
                          _tmp$2032 = _cursor$843;
                          _cursor$843 = _tmp$2032 + 1;
                          if (next_char$866 < 56320) {
                            goto $join$849;
                          } else if (next_char$866 > 57343) {
                            goto $join$849;
                          } else {
                            continue;
                          }
                        } else {
                          goto $join$849;
                        }
                      }
                      goto $joinlet$2774;
                      $join$862:;
                      continue;
                      $joinlet$2774:;
                    } else {
                      match_tag_saver_0$846 = tag_0$847;
                      accept_state$844 = 0;
                      match_end$845 = _cursor$843;
                      goto $join$849;
                    }
                    break;
                  }
                }
              } else if (next_char$861 > 56318) {
                if (next_char$861 < 57344) {
                  int32_t _tmp$2035 = _cursor$843;
                  if (_tmp$2035 < _end$842) {
                    int32_t _tmp$2037 = _cursor$843;
                    int32_t next_char$867;
                    int32_t _tmp$2036;
                    moonbit_incref(_data$840);
                    next_char$867
                    = $String$$unsafe_charcode_at(
                      _data$840, _tmp$2037
                    );
                    _tmp$2036 = _cursor$843;
                    _cursor$843 = _tmp$2036 + 1;
                    if (next_char$867 < 56320) {
                      goto $join$849;
                    } else if (next_char$867 > 57343) {
                      goto $join$849;
                    } else {
                      continue;
                    }
                  } else {
                    goto $join$849;
                  }
                } else if (next_char$861 > 65535) {
                  goto $join$849;
                } else {
                  goto $join$859;
                }
              } else {
                int32_t _tmp$2038 = _cursor$843;
                if (_tmp$2038 < _end$842) {
                  int32_t _tmp$2040 = _cursor$843;
                  int32_t next_char$868;
                  int32_t _tmp$2039;
                  moonbit_incref(_data$840);
                  next_char$868
                  = $String$$unsafe_charcode_at(
                    _data$840, _tmp$2040
                  );
                  _tmp$2039 = _cursor$843;
                  _cursor$843 = _tmp$2039 + 1;
                  if (next_char$868 < 56320) {
                    goto $join$849;
                  } else if (next_char$868 > 65535) {
                    goto $join$849;
                  } else {
                    continue;
                  }
                } else {
                  goto $join$849;
                }
              }
              goto $joinlet$2772;
              $join$859:;
              continue;
              $joinlet$2772:;
            } else {
              goto $join$849;
            }
            break;
          }
        }
      } else if (next_char$858 > 56318) {
        if (next_char$858 < 57344) {
          int32_t _tmp$2042 = _cursor$843;
          if (_tmp$2042 < _end$842) {
            int32_t _tmp$2044 = _cursor$843;
            int32_t next_char$869;
            int32_t _tmp$2043;
            moonbit_incref(_data$840);
            next_char$869 = $String$$unsafe_charcode_at(_data$840, _tmp$2044);
            _tmp$2043 = _cursor$843;
            _cursor$843 = _tmp$2043 + 1;
            if (next_char$869 < 56320) {
              goto $join$849;
            } else if (next_char$869 > 57343) {
              goto $join$849;
            } else {
              continue;
            }
          } else {
            goto $join$849;
          }
        } else if (next_char$858 > 65535) {
          goto $join$849;
        } else {
          goto $join$856;
        }
      } else {
        int32_t _tmp$2045 = _cursor$843;
        if (_tmp$2045 < _end$842) {
          int32_t _tmp$2047 = _cursor$843;
          int32_t next_char$870;
          int32_t _tmp$2046;
          moonbit_incref(_data$840);
          next_char$870 = $String$$unsafe_charcode_at(_data$840, _tmp$2047);
          _tmp$2046 = _cursor$843;
          _cursor$843 = _tmp$2046 + 1;
          if (next_char$870 < 56320) {
            goto $join$849;
          } else if (next_char$870 > 65535) {
            goto $join$849;
          } else {
            continue;
          }
        } else {
          goto $join$849;
        }
      }
      goto $joinlet$2770;
      $join$856:;
      continue;
      $joinlet$2770:;
    } else {
      goto $join$849;
    }
    break;
  }
  goto $joinlet$2768;
  $join$849:;
  switch (accept_state$844) {
    case 0: {
      void* _try_err$852;
      struct $StringView package_name$850;
      int32_t _tmp$2018;
      int32_t _tmp$2017;
      int64_t _tmp$2014;
      int32_t _tmp$2016;
      int64_t _tmp$2015;
      struct moonbit_result_1 _tmp$2776;
      void* _try_err$855;
      struct $StringView module_name$853;
      int64_t _tmp$2009;
      int32_t _tmp$2011;
      int64_t _tmp$2010;
      struct moonbit_result_1 _tmp$2778;
      void* Some$2008;
      moonbit_decref(pkg$838.$0);
      _tmp$2018 = match_tag_saver_0$846;
      _tmp$2017 = _tmp$2018 + 1;
      _tmp$2014 = (int64_t)_tmp$2017;
      _tmp$2016 = match_end$845;
      _tmp$2015 = (int64_t)_tmp$2016;
      moonbit_incref(_data$840);
      _tmp$2776 = $String$$sub(_data$840, _tmp$2014, _tmp$2015);
      if (_tmp$2776.tag) {
        struct $StringView const _ok$2019 = _tmp$2776.data.ok;
        package_name$850 = _ok$2019;
      } else {
        void* const _err$2020 = _tmp$2776.data.err;
        _try_err$852 = _err$2020;
        goto $join$851;
      }
      goto $joinlet$2775;
      $join$851:;
      moonbit_decref(_try_err$852);
      moonbit_panic();
      $joinlet$2775:;
      _tmp$2009 = (int64_t)_start$841;
      _tmp$2011 = match_tag_saver_0$846;
      _tmp$2010 = (int64_t)_tmp$2011;
      _tmp$2778 = $String$$sub(_data$840, _tmp$2009, _tmp$2010);
      if (_tmp$2778.tag) {
        struct $StringView const _ok$2012 = _tmp$2778.data.ok;
        module_name$853 = _ok$2012;
      } else {
        void* const _err$2013 = _tmp$2778.data.err;
        _try_err$855 = _err$2013;
        goto $join$854;
      }
      goto $joinlet$2777;
      $join$854:;
      moonbit_decref(_try_err$855);
      moonbit_panic();
      $joinlet$2777:;
      Some$2008
      = (void*)moonbit_malloc(sizeof(struct $Option$3c$StringView$3e$$Some));
      Moonbit_object_header(Some$2008)->meta
      = Moonbit_make_regular_object_header(
        offsetof(struct $Option$3c$StringView$3e$$Some, $0_0) >> 2, 1, 1
      );
      ((struct $Option$3c$StringView$3e$$Some*)Some$2008)->$0_0
      = package_name$850.$0;
      ((struct $Option$3c$StringView$3e$$Some*)Some$2008)->$0_1
      = package_name$850.$1;
      ((struct $Option$3c$StringView$3e$$Some*)Some$2008)->$0_2
      = package_name$850.$2;
      _bind$848
      = (struct $$3c$StringView$2a$Option$3c$StringView$3e$$3e$*)moonbit_malloc(
          sizeof(struct $$3c$StringView$2a$Option$3c$StringView$3e$$3e$)
        );
      Moonbit_object_header(_bind$848)->meta
      = Moonbit_make_regular_object_header(
        offsetof(
          struct $$3c$StringView$2a$Option$3c$StringView$3e$$3e$, $0_0
        )
        >> 2,
          2,
          0
      );
      _bind$848->$0_0 = module_name$853.$0;
      _bind$848->$0_1 = module_name$853.$1;
      _bind$848->$0_2 = module_name$853.$2;
      _bind$848->$1 = Some$2008;
      break;
    }
    default: {
      void* None$2021;
      moonbit_decref(_data$840);
      None$2021 = (struct moonbit_object*)&moonbit_constant_constructor_0 + 1;
      _bind$848
      = (struct $$3c$StringView$2a$Option$3c$StringView$3e$$3e$*)moonbit_malloc(
          sizeof(struct $$3c$StringView$2a$Option$3c$StringView$3e$$3e$)
        );
      Moonbit_object_header(_bind$848)->meta
      = Moonbit_make_regular_object_header(
        offsetof(
          struct $$3c$StringView$2a$Option$3c$StringView$3e$$3e$, $0_0
        )
        >> 2,
          2,
          0
      );
      _bind$848->$0_0 = pkg$838.$0;
      _bind$848->$0_1 = pkg$838.$1;
      _bind$848->$0_2 = pkg$838.$2;
      _bind$848->$1 = None$2021;
      break;
    }
  }
  $joinlet$2768:;
  _field$2357
  = (struct $StringView){
    _bind$848->$0_1, _bind$848->$0_2, _bind$848->$0_0
  };
  _module_name$871 = _field$2357;
  _field$2356 = _bind$848->$1;
  _cnt$2647 = Moonbit_object_header(_bind$848)->rc;
  if (_cnt$2647 > 1) {
    int32_t _new_cnt$2648 = _cnt$2647 - 1;
    Moonbit_object_header(_bind$848)->rc = _new_cnt$2648;
    moonbit_incref(_field$2356);
    moonbit_incref(_module_name$871.$0);
  } else if (_cnt$2647 == 1) {
    moonbit_free(_bind$848);
  }
  _package_name$872 = _field$2356;
  switch (Moonbit_object_tag(_package_name$872)) {
    case 1: {
      struct $Option$3c$StringView$3e$$Some* _Some$873 =
        (struct $Option$3c$StringView$3e$$Some*)_package_name$872;
      struct $StringView _field$2355 =
        (struct $StringView){
          _Some$873->$0_1, _Some$873->$0_2, _Some$873->$0_0
        };
      int32_t _cnt$2649 = Moonbit_object_header(_Some$873)->rc;
      struct $StringView _pkg_name$874;
      struct $$moonbitlang$core$builtin$Logger _bind$2001;
      if (_cnt$2649 > 1) {
        int32_t _new_cnt$2650 = _cnt$2649 - 1;
        Moonbit_object_header(_Some$873)->rc = _new_cnt$2650;
        moonbit_incref(_field$2355.$0);
      } else if (_cnt$2649 == 1) {
        moonbit_free(_Some$873);
      }
      _pkg_name$874 = _field$2355;
      if (logger$875.$1) {
        moonbit_incref(logger$875.$1);
      }
      logger$875.$0->$method_2(logger$875.$1, _pkg_name$874);
      _bind$2001 = logger$875;
      if (_bind$2001.$1) {
        moonbit_incref(_bind$2001.$1);
      }
      _bind$2001.$0->$method_3(_bind$2001.$1, 47);
      break;
    }
    default: {
      moonbit_decref(_package_name$872);
      break;
    }
  }
  _field$2354
  = (struct $StringView){
    self$839->$1_1, self$839->$1_2, self$839->$1_0
  };
  filename$2003 = _field$2354;
  moonbit_incref(filename$2003.$0);
  if (logger$875.$1) {
    moonbit_incref(logger$875.$1);
  }
  logger$875.$0->$method_2(logger$875.$1, filename$2003);
  if (logger$875.$1) {
    moonbit_incref(logger$875.$1);
  }
  logger$875.$0->$method_3(logger$875.$1, 58);
  _field$2353
  = (struct $StringView){
    self$839->$2_1, self$839->$2_2, self$839->$2_0
  };
  start_line$2004 = _field$2353;
  moonbit_incref(start_line$2004.$0);
  if (logger$875.$1) {
    moonbit_incref(logger$875.$1);
  }
  logger$875.$0->$method_2(logger$875.$1, start_line$2004);
  if (logger$875.$1) {
    moonbit_incref(logger$875.$1);
  }
  logger$875.$0->$method_3(logger$875.$1, 58);
  _field$2352
  = (struct $StringView){
    self$839->$3_1, self$839->$3_2, self$839->$3_0
  };
  start_column$2005 = _field$2352;
  moonbit_incref(start_column$2005.$0);
  if (logger$875.$1) {
    moonbit_incref(logger$875.$1);
  }
  logger$875.$0->$method_2(logger$875.$1, start_column$2005);
  if (logger$875.$1) {
    moonbit_incref(logger$875.$1);
  }
  logger$875.$0->$method_3(logger$875.$1, 45);
  _field$2351
  = (struct $StringView){
    self$839->$4_1, self$839->$4_2, self$839->$4_0
  };
  end_line$2006 = _field$2351;
  moonbit_incref(end_line$2006.$0);
  if (logger$875.$1) {
    moonbit_incref(logger$875.$1);
  }
  logger$875.$0->$method_2(logger$875.$1, end_line$2006);
  if (logger$875.$1) {
    moonbit_incref(logger$875.$1);
  }
  logger$875.$0->$method_3(logger$875.$1, 58);
  _field$2350
  = (struct $StringView){
    self$839->$5_1, self$839->$5_2, self$839->$5_0
  };
  _cnt$2651 = Moonbit_object_header(self$839)->rc;
  if (_cnt$2651 > 1) {
    int32_t _new_cnt$2657 = _cnt$2651 - 1;
    Moonbit_object_header(self$839)->rc = _new_cnt$2657;
    moonbit_incref(_field$2350.$0);
  } else if (_cnt$2651 == 1) {
    struct $StringView _field$2656 =
      (struct $StringView){self$839->$4_1, self$839->$4_2, self$839->$4_0};
    struct $StringView _field$2655;
    struct $StringView _field$2654;
    struct $StringView _field$2653;
    struct $StringView _field$2652;
    moonbit_decref(_field$2656.$0);
    _field$2655
    = (struct $StringView){
      self$839->$3_1, self$839->$3_2, self$839->$3_0
    };
    moonbit_decref(_field$2655.$0);
    _field$2654
    = (struct $StringView){
      self$839->$2_1, self$839->$2_2, self$839->$2_0
    };
    moonbit_decref(_field$2654.$0);
    _field$2653
    = (struct $StringView){
      self$839->$1_1, self$839->$1_2, self$839->$1_0
    };
    moonbit_decref(_field$2653.$0);
    _field$2652
    = (struct $StringView){
      self$839->$0_1, self$839->$0_2, self$839->$0_0
    };
    moonbit_decref(_field$2652.$0);
    moonbit_free(self$839);
  }
  end_column$2007 = _field$2350;
  if (logger$875.$1) {
    moonbit_incref(logger$875.$1);
  }
  logger$875.$0->$method_2(logger$875.$1, end_column$2007);
  if (logger$875.$1) {
    moonbit_incref(logger$875.$1);
  }
  logger$875.$0->$method_3(logger$875.$1, 64);
  _bind$2002 = logger$875;
  _bind$2002.$0->$method_2(_bind$2002.$1, _module_name$871);
  return 0;
}

int32_t $moonbitlang$core$builtin$println$0(moonbit_string_t input$837) {
  moonbit_string_t _tmp$2000 =
    $$moonbitlang$core$builtin$Show$$String$$to_string(input$837);
  moonbit_println(_tmp$2000);
  moonbit_decref(_tmp$2000);
  return 0;
}

int32_t $$moonbitlang$core$builtin$Hash$$Int$$hash_combine(
  int32_t self$836,
  struct $$moonbitlang$core$builtin$Hasher* hasher$835
) {
  $$moonbitlang$core$builtin$Hasher$$combine_int(hasher$835, self$836);
  return 0;
}

int32_t $$moonbitlang$core$builtin$Hash$$String$$hash_combine(
  moonbit_string_t self$834,
  struct $$moonbitlang$core$builtin$Hasher* hasher$833
) {
  $$moonbitlang$core$builtin$Hasher$$combine_string(hasher$833, self$834);
  return 0;
}

int32_t $$moonbitlang$core$builtin$Hasher$$combine_string(
  struct $$moonbitlang$core$builtin$Hasher* self$831,
  moonbit_string_t value$829
) {
  int32_t _end2383$828 = Moonbit_array_length(value$829);
  int32_t i$830 = 0;
  while (1) {
    if (i$830 < _end2383$828) {
      int32_t _tmp$1998 = value$829[i$830];
      uint32_t _tmp$1997 = *(uint32_t*)&_tmp$1998;
      int32_t _tmp$1999;
      moonbit_incref(self$831);
      $$moonbitlang$core$builtin$Hasher$$combine_uint(self$831, _tmp$1997);
      _tmp$1999 = i$830 + 1;
      i$830 = _tmp$1999;
      continue;
    } else {
      moonbit_decref(self$831);
      moonbit_decref(value$829);
    }
    break;
  }
  return 0;
}

moonbit_string_t $$moonbitlang$core$builtin$Array$$unsafe_get$1(
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* self$826,
  int32_t idx$827
) {
  moonbit_string_t* _tmp$1996 =
    $$moonbitlang$core$builtin$Array$$buffer$1(self$826);
  moonbit_string_t _tmp$2359;
  if (idx$827 < 0 || idx$827 >= Moonbit_array_length(_tmp$1996)) {
    moonbit_panic();
  }
  _tmp$2359 = (moonbit_string_t)_tmp$1996[idx$827];
  moonbit_incref(_tmp$2359);
  moonbit_decref(_tmp$1996);
  return _tmp$2359;
}

struct $$3c$String$2a$Int$3e$* $$moonbitlang$core$builtin$Array$$unsafe_get$0(
  struct $$moonbitlang$core$builtin$Array$3c$$3c$String$2a$Int$3e$$3e$* self$824,
  int32_t idx$825
) {
  struct $$3c$String$2a$Int$3e$** _tmp$1995 =
    $$moonbitlang$core$builtin$Array$$buffer$0(self$824);
  struct $$3c$String$2a$Int$3e$* _tmp$2360;
  if (idx$825 < 0 || idx$825 >= Moonbit_array_length(_tmp$1995)) {
    moonbit_panic();
  }
  _tmp$2360 = (struct $$3c$String$2a$Int$3e$*)_tmp$1995[idx$825];
  if (_tmp$2360) {
    moonbit_incref(_tmp$2360);
  }
  moonbit_decref(_tmp$1995);
  return _tmp$2360;
}

struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* $$moonbitlang$core$builtin$Map$$get$7(
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$820,
  moonbit_string_t key$816
) {
  int32_t hash$815;
  int32_t capacity_mask$1994;
  int32_t _tmp$1993;
  int32_t i$817;
  int32_t idx$818;
  moonbit_incref(key$816);
  hash$815 = $$moonbitlang$core$builtin$Hash$$$default_impl$$hash$1(key$816);
  capacity_mask$1994 = self$820->$3;
  _tmp$1993 = hash$815 & capacity_mask$1994;
  i$817 = 0;
  idx$818 = _tmp$1993;
  while (1) {
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** _field$2366 =
      self$820->$0;
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** entries$1992 =
      _field$2366;
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$2365;
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _bind$819;
    if (idx$818 < 0 || idx$818 >= Moonbit_array_length(entries$1992)) {
      moonbit_panic();
    }
    _tmp$2365
    = (struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$*)entries$1992[
        idx$818
      ];
    _bind$819 = _tmp$2365;
    if (_bind$819 == 0) {
      struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _tmp$1981;
      if (_bind$819) {
        moonbit_incref(_bind$819);
      }
      moonbit_decref(self$820);
      if (_bind$819) {
        moonbit_decref(_bind$819);
      }
      moonbit_decref(key$816);
      _tmp$1981 = 0;
      return _tmp$1981;
    } else {
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _Some$821 =
        _bind$819;
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _entry$822 =
        _Some$821;
      int32_t hash$1983 = _entry$822->$3;
      int32_t _if_result$2781;
      int32_t _field$2361;
      int32_t psl$1986;
      int32_t _tmp$1988;
      int32_t _tmp$1990;
      int32_t capacity_mask$1991;
      int32_t _tmp$1989;
      if (hash$1983 == hash$815) {
        moonbit_string_t _field$2364 = _entry$822->$4;
        moonbit_string_t key$1982 = _field$2364;
        int32_t _tmp$2363 = moonbit_val_array_equal(key$1982, key$816);
        _if_result$2781 = _tmp$2363;
      } else {
        _if_result$2781 = 0;
      }
      if (_if_result$2781) {
        struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _field$2362;
        int32_t _cnt$2658;
        struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* value$1985;
        struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _tmp$1984;
        moonbit_incref(_entry$822);
        moonbit_decref(self$820);
        moonbit_decref(key$816);
        _field$2362 = _entry$822->$5;
        _cnt$2658 = Moonbit_object_header(_entry$822)->rc;
        if (_cnt$2658 > 1) {
          int32_t _new_cnt$2661 = _cnt$2658 - 1;
          Moonbit_object_header(_entry$822)->rc = _new_cnt$2661;
          moonbit_incref(_field$2362);
        } else if (_cnt$2658 == 1) {
          moonbit_string_t _field$2660 = _entry$822->$4;
          struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _field$2659;
          moonbit_decref(_field$2660);
          _field$2659 = _entry$822->$1;
          if (_field$2659) {
            moonbit_decref(_field$2659);
          }
          moonbit_free(_entry$822);
        }
        value$1985 = _field$2362;
        _tmp$1984 = value$1985;
        return _tmp$1984;
      } else {
        moonbit_incref(_entry$822);
      }
      _field$2361 = _entry$822->$2;
      moonbit_decref(_entry$822);
      psl$1986 = _field$2361;
      if (i$817 > psl$1986) {
        struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _tmp$1987;
        moonbit_decref(self$820);
        moonbit_decref(key$816);
        _tmp$1987 = 0;
        return _tmp$1987;
      }
      _tmp$1988 = i$817 + 1;
      _tmp$1990 = idx$818 + 1;
      capacity_mask$1991 = self$820->$3;
      _tmp$1989 = _tmp$1990 & capacity_mask$1991;
      i$817 = _tmp$1988;
      idx$818 = _tmp$1989;
      continue;
    }
    break;
  }
}

struct $$3c$$3c$Int$2a$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* $$moonbitlang$core$builtin$Map$$get$6(
  struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* self$811,
  int32_t key$807
) {
  int32_t hash$806 =
    $$moonbitlang$core$builtin$Hash$$$default_impl$$hash$0(key$807);
  int32_t capacity_mask$1980 = self$811->$3;
  int32_t _tmp$1979 = hash$806 & capacity_mask$1980;
  int32_t i$808 = 0;
  int32_t idx$809 = _tmp$1979;
  while (1) {
    struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$** _field$2370 =
      self$811->$0;
    struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$** entries$1978 =
      _field$2370;
    struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _tmp$2369;
    struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _bind$810;
    if (idx$809 < 0 || idx$809 >= Moonbit_array_length(entries$1978)) {
      moonbit_panic();
    }
    _tmp$2369
    = (struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$*)entries$1978[
        idx$809
      ];
    _bind$810 = _tmp$2369;
    if (_bind$810 == 0) {
      struct $$3c$$3c$Int$2a$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* _tmp$1967;
      if (_bind$810) {
        moonbit_incref(_bind$810);
      }
      moonbit_decref(self$811);
      if (_bind$810) {
        moonbit_decref(_bind$810);
      }
      _tmp$1967 = 0;
      return _tmp$1967;
    } else {
      struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _Some$812 =
        _bind$810;
      struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _entry$813 =
        _Some$812;
      int32_t hash$1969 = _entry$813->$3;
      int32_t _if_result$2783;
      int32_t _field$2367;
      int32_t psl$1972;
      int32_t _tmp$1974;
      int32_t _tmp$1976;
      int32_t capacity_mask$1977;
      int32_t _tmp$1975;
      if (hash$1969 == hash$806) {
        int32_t key$1968 = _entry$813->$4;
        _if_result$2783 = key$1968 == key$807;
      } else {
        _if_result$2783 = 0;
      }
      if (_if_result$2783) {
        struct $$3c$$3c$Int$2a$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* _field$2368;
        int32_t _cnt$2662;
        struct $$3c$$3c$Int$2a$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* value$1971;
        struct $$3c$$3c$Int$2a$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* _tmp$1970;
        moonbit_incref(_entry$813);
        moonbit_decref(self$811);
        _field$2368 = _entry$813->$5;
        _cnt$2662 = Moonbit_object_header(_entry$813)->rc;
        if (_cnt$2662 > 1) {
          int32_t _new_cnt$2664 = _cnt$2662 - 1;
          Moonbit_object_header(_entry$813)->rc = _new_cnt$2664;
          moonbit_incref(_field$2368);
        } else if (_cnt$2662 == 1) {
          struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _field$2663 =
            _entry$813->$1;
          if (_field$2663) {
            moonbit_decref(_field$2663);
          }
          moonbit_free(_entry$813);
        }
        value$1971 = _field$2368;
        _tmp$1970 = value$1971;
        return _tmp$1970;
      } else {
        moonbit_incref(_entry$813);
      }
      _field$2367 = _entry$813->$2;
      moonbit_decref(_entry$813);
      psl$1972 = _field$2367;
      if (i$808 > psl$1972) {
        struct $$3c$$3c$Int$2a$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* _tmp$1973;
        moonbit_decref(self$811);
        _tmp$1973 = 0;
        return _tmp$1973;
      }
      _tmp$1974 = i$808 + 1;
      _tmp$1976 = idx$809 + 1;
      capacity_mask$1977 = self$811->$3;
      _tmp$1975 = _tmp$1976 & capacity_mask$1977;
      i$808 = _tmp$1974;
      idx$809 = _tmp$1975;
      continue;
    }
    break;
  }
}

struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* $$moonbitlang$core$builtin$Map$$get$5(
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$802,
  moonbit_string_t key$798
) {
  int32_t hash$797;
  int32_t capacity_mask$1966;
  int32_t _tmp$1965;
  int32_t i$799;
  int32_t idx$800;
  moonbit_incref(key$798);
  hash$797 = $$moonbitlang$core$builtin$Hash$$$default_impl$$hash$1(key$798);
  capacity_mask$1966 = self$802->$3;
  _tmp$1965 = hash$797 & capacity_mask$1966;
  i$799 = 0;
  idx$800 = _tmp$1965;
  while (1) {
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** _field$2376 =
      self$802->$0;
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** entries$1964 =
      _field$2376;
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$2375;
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _bind$801;
    if (idx$800 < 0 || idx$800 >= Moonbit_array_length(entries$1964)) {
      moonbit_panic();
    }
    _tmp$2375
    = (struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$*)entries$1964[
        idx$800
      ];
    _bind$801 = _tmp$2375;
    if (_bind$801 == 0) {
      struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _tmp$1953;
      if (_bind$801) {
        moonbit_incref(_bind$801);
      }
      moonbit_decref(self$802);
      if (_bind$801) {
        moonbit_decref(_bind$801);
      }
      moonbit_decref(key$798);
      _tmp$1953 = 0;
      return _tmp$1953;
    } else {
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _Some$803 =
        _bind$801;
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _entry$804 =
        _Some$803;
      int32_t hash$1955 = _entry$804->$3;
      int32_t _if_result$2785;
      int32_t _field$2371;
      int32_t psl$1958;
      int32_t _tmp$1960;
      int32_t _tmp$1962;
      int32_t capacity_mask$1963;
      int32_t _tmp$1961;
      if (hash$1955 == hash$797) {
        moonbit_string_t _field$2374 = _entry$804->$4;
        moonbit_string_t key$1954 = _field$2374;
        int32_t _tmp$2373 = moonbit_val_array_equal(key$1954, key$798);
        _if_result$2785 = _tmp$2373;
      } else {
        _if_result$2785 = 0;
      }
      if (_if_result$2785) {
        struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _field$2372;
        int32_t _cnt$2665;
        struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* value$1957;
        struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _tmp$1956;
        moonbit_incref(_entry$804);
        moonbit_decref(self$802);
        moonbit_decref(key$798);
        _field$2372 = _entry$804->$5;
        _cnt$2665 = Moonbit_object_header(_entry$804)->rc;
        if (_cnt$2665 > 1) {
          int32_t _new_cnt$2668 = _cnt$2665 - 1;
          Moonbit_object_header(_entry$804)->rc = _new_cnt$2668;
          moonbit_incref(_field$2372);
        } else if (_cnt$2665 == 1) {
          moonbit_string_t _field$2667 = _entry$804->$4;
          struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _field$2666;
          moonbit_decref(_field$2667);
          _field$2666 = _entry$804->$1;
          if (_field$2666) {
            moonbit_decref(_field$2666);
          }
          moonbit_free(_entry$804);
        }
        value$1957 = _field$2372;
        _tmp$1956 = value$1957;
        return _tmp$1956;
      } else {
        moonbit_incref(_entry$804);
      }
      _field$2371 = _entry$804->$2;
      moonbit_decref(_entry$804);
      psl$1958 = _field$2371;
      if (i$799 > psl$1958) {
        struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _tmp$1959;
        moonbit_decref(self$802);
        moonbit_decref(key$798);
        _tmp$1959 = 0;
        return _tmp$1959;
      }
      _tmp$1960 = i$799 + 1;
      _tmp$1962 = idx$800 + 1;
      capacity_mask$1963 = self$802->$3;
      _tmp$1961 = _tmp$1962 & capacity_mask$1963;
      i$799 = _tmp$1960;
      idx$800 = _tmp$1961;
      continue;
    }
    break;
  }
}

struct $$3c$$3c$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* $$moonbitlang$core$builtin$Map$$get$4(
  struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* self$793,
  int32_t key$789
) {
  int32_t hash$788 =
    $$moonbitlang$core$builtin$Hash$$$default_impl$$hash$0(key$789);
  int32_t capacity_mask$1952 = self$793->$3;
  int32_t _tmp$1951 = hash$788 & capacity_mask$1952;
  int32_t i$790 = 0;
  int32_t idx$791 = _tmp$1951;
  while (1) {
    struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$async$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$** _field$2380 =
      self$793->$0;
    struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$async$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$** entries$1950 =
      _field$2380;
    struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$async$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _tmp$2379;
    struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$async$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _bind$792;
    if (idx$791 < 0 || idx$791 >= Moonbit_array_length(entries$1950)) {
      moonbit_panic();
    }
    _tmp$2379
    = (struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$async$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$*)entries$1950[
        idx$791
      ];
    _bind$792 = _tmp$2379;
    if (_bind$792 == 0) {
      struct $$3c$$3c$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* _tmp$1939;
      if (_bind$792) {
        moonbit_incref(_bind$792);
      }
      moonbit_decref(self$793);
      if (_bind$792) {
        moonbit_decref(_bind$792);
      }
      _tmp$1939 = 0;
      return _tmp$1939;
    } else {
      struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$async$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _Some$794 =
        _bind$792;
      struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$async$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _entry$795 =
        _Some$794;
      int32_t hash$1941 = _entry$795->$3;
      int32_t _if_result$2787;
      int32_t _field$2377;
      int32_t psl$1944;
      int32_t _tmp$1946;
      int32_t _tmp$1948;
      int32_t capacity_mask$1949;
      int32_t _tmp$1947;
      if (hash$1941 == hash$788) {
        int32_t key$1940 = _entry$795->$4;
        _if_result$2787 = key$1940 == key$789;
      } else {
        _if_result$2787 = 0;
      }
      if (_if_result$2787) {
        struct $$3c$$3c$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* _field$2378;
        int32_t _cnt$2669;
        struct $$3c$$3c$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* value$1943;
        struct $$3c$$3c$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* _tmp$1942;
        moonbit_incref(_entry$795);
        moonbit_decref(self$793);
        _field$2378 = _entry$795->$5;
        _cnt$2669 = Moonbit_object_header(_entry$795)->rc;
        if (_cnt$2669 > 1) {
          int32_t _new_cnt$2671 = _cnt$2669 - 1;
          Moonbit_object_header(_entry$795)->rc = _new_cnt$2671;
          moonbit_incref(_field$2378);
        } else if (_cnt$2669 == 1) {
          struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$async$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _field$2670 =
            _entry$795->$1;
          if (_field$2670) {
            moonbit_decref(_field$2670);
          }
          moonbit_free(_entry$795);
        }
        value$1943 = _field$2378;
        _tmp$1942 = value$1943;
        return _tmp$1942;
      } else {
        moonbit_incref(_entry$795);
      }
      _field$2377 = _entry$795->$2;
      moonbit_decref(_entry$795);
      psl$1944 = _field$2377;
      if (i$790 > psl$1944) {
        struct $$3c$$3c$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* _tmp$1945;
        moonbit_decref(self$793);
        _tmp$1945 = 0;
        return _tmp$1945;
      }
      _tmp$1946 = i$790 + 1;
      _tmp$1948 = idx$791 + 1;
      capacity_mask$1949 = self$793->$3;
      _tmp$1947 = _tmp$1948 & capacity_mask$1949;
      i$790 = _tmp$1946;
      idx$791 = _tmp$1947;
      continue;
    }
    break;
  }
}

struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* $$moonbitlang$core$builtin$Map$$get$3(
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$784,
  moonbit_string_t key$780
) {
  int32_t hash$779;
  int32_t capacity_mask$1938;
  int32_t _tmp$1937;
  int32_t i$781;
  int32_t idx$782;
  moonbit_incref(key$780);
  hash$779 = $$moonbitlang$core$builtin$Hash$$$default_impl$$hash$1(key$780);
  capacity_mask$1938 = self$784->$3;
  _tmp$1937 = hash$779 & capacity_mask$1938;
  i$781 = 0;
  idx$782 = _tmp$1937;
  while (1) {
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** _field$2386 =
      self$784->$0;
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** entries$1936 =
      _field$2386;
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$2385;
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _bind$783;
    if (idx$782 < 0 || idx$782 >= Moonbit_array_length(entries$1936)) {
      moonbit_panic();
    }
    _tmp$2385
    = (struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$*)entries$1936[
        idx$782
      ];
    _bind$783 = _tmp$2385;
    if (_bind$783 == 0) {
      struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _tmp$1925;
      if (_bind$783) {
        moonbit_incref(_bind$783);
      }
      moonbit_decref(self$784);
      if (_bind$783) {
        moonbit_decref(_bind$783);
      }
      moonbit_decref(key$780);
      _tmp$1925 = 0;
      return _tmp$1925;
    } else {
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _Some$785 =
        _bind$783;
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _entry$786 =
        _Some$785;
      int32_t hash$1927 = _entry$786->$3;
      int32_t _if_result$2789;
      int32_t _field$2381;
      int32_t psl$1930;
      int32_t _tmp$1932;
      int32_t _tmp$1934;
      int32_t capacity_mask$1935;
      int32_t _tmp$1933;
      if (hash$1927 == hash$779) {
        moonbit_string_t _field$2384 = _entry$786->$4;
        moonbit_string_t key$1926 = _field$2384;
        int32_t _tmp$2383 = moonbit_val_array_equal(key$1926, key$780);
        _if_result$2789 = _tmp$2383;
      } else {
        _if_result$2789 = 0;
      }
      if (_if_result$2789) {
        struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _field$2382;
        int32_t _cnt$2672;
        struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* value$1929;
        struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _tmp$1928;
        moonbit_incref(_entry$786);
        moonbit_decref(self$784);
        moonbit_decref(key$780);
        _field$2382 = _entry$786->$5;
        _cnt$2672 = Moonbit_object_header(_entry$786)->rc;
        if (_cnt$2672 > 1) {
          int32_t _new_cnt$2675 = _cnt$2672 - 1;
          Moonbit_object_header(_entry$786)->rc = _new_cnt$2675;
          moonbit_incref(_field$2382);
        } else if (_cnt$2672 == 1) {
          moonbit_string_t _field$2674 = _entry$786->$4;
          struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _field$2673;
          moonbit_decref(_field$2674);
          _field$2673 = _entry$786->$1;
          if (_field$2673) {
            moonbit_decref(_field$2673);
          }
          moonbit_free(_entry$786);
        }
        value$1929 = _field$2382;
        _tmp$1928 = value$1929;
        return _tmp$1928;
      } else {
        moonbit_incref(_entry$786);
      }
      _field$2381 = _entry$786->$2;
      moonbit_decref(_entry$786);
      psl$1930 = _field$2381;
      if (i$781 > psl$1930) {
        struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _tmp$1931;
        moonbit_decref(self$784);
        moonbit_decref(key$780);
        _tmp$1931 = 0;
        return _tmp$1931;
      }
      _tmp$1932 = i$781 + 1;
      _tmp$1934 = idx$782 + 1;
      capacity_mask$1935 = self$784->$3;
      _tmp$1933 = _tmp$1934 & capacity_mask$1935;
      i$781 = _tmp$1932;
      idx$782 = _tmp$1933;
      continue;
    }
    break;
  }
}

struct $$3c$$3c$Int$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* $$moonbitlang$core$builtin$Map$$get$2(
  struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* self$775,
  int32_t key$771
) {
  int32_t hash$770 =
    $$moonbitlang$core$builtin$Hash$$$default_impl$$hash$0(key$771);
  int32_t capacity_mask$1924 = self$775->$3;
  int32_t _tmp$1923 = hash$770 & capacity_mask$1924;
  int32_t i$772 = 0;
  int32_t idx$773 = _tmp$1923;
  while (1) {
    struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$** _field$2390 =
      self$775->$0;
    struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$** entries$1922 =
      _field$2390;
    struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _tmp$2389;
    struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _bind$774;
    if (idx$773 < 0 || idx$773 >= Moonbit_array_length(entries$1922)) {
      moonbit_panic();
    }
    _tmp$2389
    = (struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$*)entries$1922[
        idx$773
      ];
    _bind$774 = _tmp$2389;
    if (_bind$774 == 0) {
      struct $$3c$$3c$Int$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* _tmp$1911;
      if (_bind$774) {
        moonbit_incref(_bind$774);
      }
      moonbit_decref(self$775);
      if (_bind$774) {
        moonbit_decref(_bind$774);
      }
      _tmp$1911 = 0;
      return _tmp$1911;
    } else {
      struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _Some$776 =
        _bind$774;
      struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _entry$777 =
        _Some$776;
      int32_t hash$1913 = _entry$777->$3;
      int32_t _if_result$2791;
      int32_t _field$2387;
      int32_t psl$1916;
      int32_t _tmp$1918;
      int32_t _tmp$1920;
      int32_t capacity_mask$1921;
      int32_t _tmp$1919;
      if (hash$1913 == hash$770) {
        int32_t key$1912 = _entry$777->$4;
        _if_result$2791 = key$1912 == key$771;
      } else {
        _if_result$2791 = 0;
      }
      if (_if_result$2791) {
        struct $$3c$$3c$Int$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* _field$2388;
        int32_t _cnt$2676;
        struct $$3c$$3c$Int$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* value$1915;
        struct $$3c$$3c$Int$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* _tmp$1914;
        moonbit_incref(_entry$777);
        moonbit_decref(self$775);
        _field$2388 = _entry$777->$5;
        _cnt$2676 = Moonbit_object_header(_entry$777)->rc;
        if (_cnt$2676 > 1) {
          int32_t _new_cnt$2678 = _cnt$2676 - 1;
          Moonbit_object_header(_entry$777)->rc = _new_cnt$2678;
          moonbit_incref(_field$2388);
        } else if (_cnt$2676 == 1) {
          struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _field$2677 =
            _entry$777->$1;
          if (_field$2677) {
            moonbit_decref(_field$2677);
          }
          moonbit_free(_entry$777);
        }
        value$1915 = _field$2388;
        _tmp$1914 = value$1915;
        return _tmp$1914;
      } else {
        moonbit_incref(_entry$777);
      }
      _field$2387 = _entry$777->$2;
      moonbit_decref(_entry$777);
      psl$1916 = _field$2387;
      if (i$772 > psl$1916) {
        struct $$3c$$3c$Int$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* _tmp$1917;
        moonbit_decref(self$775);
        _tmp$1917 = 0;
        return _tmp$1917;
      }
      _tmp$1918 = i$772 + 1;
      _tmp$1920 = idx$773 + 1;
      capacity_mask$1921 = self$775->$3;
      _tmp$1919 = _tmp$1920 & capacity_mask$1921;
      i$772 = _tmp$1918;
      idx$773 = _tmp$1919;
      continue;
    }
    break;
  }
}

struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* $$moonbitlang$core$builtin$Map$$get$1(
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$766,
  moonbit_string_t key$762
) {
  int32_t hash$761;
  int32_t capacity_mask$1910;
  int32_t _tmp$1909;
  int32_t i$763;
  int32_t idx$764;
  moonbit_incref(key$762);
  hash$761 = $$moonbitlang$core$builtin$Hash$$$default_impl$$hash$1(key$762);
  capacity_mask$1910 = self$766->$3;
  _tmp$1909 = hash$761 & capacity_mask$1910;
  i$763 = 0;
  idx$764 = _tmp$1909;
  while (1) {
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** _field$2396 =
      self$766->$0;
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** entries$1908 =
      _field$2396;
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$2395;
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _bind$765;
    if (idx$764 < 0 || idx$764 >= Moonbit_array_length(entries$1908)) {
      moonbit_panic();
    }
    _tmp$2395
    = (struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$*)entries$1908[
        idx$764
      ];
    _bind$765 = _tmp$2395;
    if (_bind$765 == 0) {
      struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _tmp$1897;
      if (_bind$765) {
        moonbit_incref(_bind$765);
      }
      moonbit_decref(self$766);
      if (_bind$765) {
        moonbit_decref(_bind$765);
      }
      moonbit_decref(key$762);
      _tmp$1897 = 0;
      return _tmp$1897;
    } else {
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _Some$767 =
        _bind$765;
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _entry$768 =
        _Some$767;
      int32_t hash$1899 = _entry$768->$3;
      int32_t _if_result$2793;
      int32_t _field$2391;
      int32_t psl$1902;
      int32_t _tmp$1904;
      int32_t _tmp$1906;
      int32_t capacity_mask$1907;
      int32_t _tmp$1905;
      if (hash$1899 == hash$761) {
        moonbit_string_t _field$2394 = _entry$768->$4;
        moonbit_string_t key$1898 = _field$2394;
        int32_t _tmp$2393 = moonbit_val_array_equal(key$1898, key$762);
        _if_result$2793 = _tmp$2393;
      } else {
        _if_result$2793 = 0;
      }
      if (_if_result$2793) {
        struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _field$2392;
        int32_t _cnt$2679;
        struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* value$1901;
        struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _tmp$1900;
        moonbit_incref(_entry$768);
        moonbit_decref(self$766);
        moonbit_decref(key$762);
        _field$2392 = _entry$768->$5;
        _cnt$2679 = Moonbit_object_header(_entry$768)->rc;
        if (_cnt$2679 > 1) {
          int32_t _new_cnt$2682 = _cnt$2679 - 1;
          Moonbit_object_header(_entry$768)->rc = _new_cnt$2682;
          moonbit_incref(_field$2392);
        } else if (_cnt$2679 == 1) {
          moonbit_string_t _field$2681 = _entry$768->$4;
          struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _field$2680;
          moonbit_decref(_field$2681);
          _field$2680 = _entry$768->$1;
          if (_field$2680) {
            moonbit_decref(_field$2680);
          }
          moonbit_free(_entry$768);
        }
        value$1901 = _field$2392;
        _tmp$1900 = value$1901;
        return _tmp$1900;
      } else {
        moonbit_incref(_entry$768);
      }
      _field$2391 = _entry$768->$2;
      moonbit_decref(_entry$768);
      psl$1902 = _field$2391;
      if (i$763 > psl$1902) {
        struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _tmp$1903;
        moonbit_decref(self$766);
        moonbit_decref(key$762);
        _tmp$1903 = 0;
        return _tmp$1903;
      }
      _tmp$1904 = i$763 + 1;
      _tmp$1906 = idx$764 + 1;
      capacity_mask$1907 = self$766->$3;
      _tmp$1905 = _tmp$1906 & capacity_mask$1907;
      i$763 = _tmp$1904;
      idx$764 = _tmp$1905;
      continue;
    }
    break;
  }
}

struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* $$moonbitlang$core$builtin$Map$$get$0(
  struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* self$757,
  int32_t key$753
) {
  int32_t hash$752 =
    $$moonbitlang$core$builtin$Hash$$$default_impl$$hash$0(key$753);
  int32_t capacity_mask$1896 = self$757->$3;
  int32_t _tmp$1895 = hash$752 & capacity_mask$1896;
  int32_t i$754 = 0;
  int32_t idx$755 = _tmp$1895;
  while (1) {
    struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$** _field$2400 =
      self$757->$0;
    struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$** entries$1894 =
      _field$2400;
    struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _tmp$2399;
    struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _bind$756;
    if (idx$755 < 0 || idx$755 >= Moonbit_array_length(entries$1894)) {
      moonbit_panic();
    }
    _tmp$2399
    = (struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$*)entries$1894[
        idx$755
      ];
    _bind$756 = _tmp$2399;
    if (_bind$756 == 0) {
      struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* _tmp$1883;
      if (_bind$756) {
        moonbit_incref(_bind$756);
      }
      moonbit_decref(self$757);
      if (_bind$756) {
        moonbit_decref(_bind$756);
      }
      _tmp$1883 = 0;
      return _tmp$1883;
    } else {
      struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _Some$758 =
        _bind$756;
      struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _entry$759 =
        _Some$758;
      int32_t hash$1885 = _entry$759->$3;
      int32_t _if_result$2795;
      int32_t _field$2397;
      int32_t psl$1888;
      int32_t _tmp$1890;
      int32_t _tmp$1892;
      int32_t capacity_mask$1893;
      int32_t _tmp$1891;
      if (hash$1885 == hash$752) {
        int32_t key$1884 = _entry$759->$4;
        _if_result$2795 = key$1884 == key$753;
      } else {
        _if_result$2795 = 0;
      }
      if (_if_result$2795) {
        struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* _field$2398;
        int32_t _cnt$2683;
        struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* value$1887;
        struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* _tmp$1886;
        moonbit_incref(_entry$759);
        moonbit_decref(self$757);
        _field$2398 = _entry$759->$5;
        _cnt$2683 = Moonbit_object_header(_entry$759)->rc;
        if (_cnt$2683 > 1) {
          int32_t _new_cnt$2685 = _cnt$2683 - 1;
          Moonbit_object_header(_entry$759)->rc = _new_cnt$2685;
          moonbit_incref(_field$2398);
        } else if (_cnt$2683 == 1) {
          struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _field$2684 =
            _entry$759->$1;
          if (_field$2684) {
            moonbit_decref(_field$2684);
          }
          moonbit_free(_entry$759);
        }
        value$1887 = _field$2398;
        _tmp$1886 = value$1887;
        return _tmp$1886;
      } else {
        moonbit_incref(_entry$759);
      }
      _field$2397 = _entry$759->$2;
      moonbit_decref(_entry$759);
      psl$1888 = _field$2397;
      if (i$754 > psl$1888) {
        struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* _tmp$1889;
        moonbit_decref(self$757);
        _tmp$1889 = 0;
        return _tmp$1889;
      }
      _tmp$1890 = i$754 + 1;
      _tmp$1892 = idx$755 + 1;
      capacity_mask$1893 = self$757->$3;
      _tmp$1891 = _tmp$1892 & capacity_mask$1893;
      i$754 = _tmp$1890;
      idx$755 = _tmp$1891;
      continue;
    }
    break;
  }
}

struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* $$moonbitlang$core$builtin$Map$$from_array$4(
  struct $$moonbitlang$core$builtin$ArrayView$3c$$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$$3e$ arr$745
) {
  int32_t length$744;
  int32_t capacity$746;
  int32_t _tmp$1874;
  int32_t _tmp$1873;
  int32_t _tmp$1882;
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* m$747;
  int32_t _len$748;
  int32_t _i$749;
  moonbit_incref(arr$745.$0);
  length$744 = $$moonbitlang$core$builtin$ArrayView$$length$4(arr$745);
  capacity$746 = $Int$$next_power_of_two(length$744);
  _tmp$1874 = capacity$746;
  _tmp$1873 = $moonbitlang$core$builtin$calc_grow_threshold(_tmp$1874);
  if (length$744 > _tmp$1873) {
    int32_t _tmp$1875 = capacity$746;
    capacity$746 = _tmp$1875 * 2;
  }
  _tmp$1882 = capacity$746;
  m$747 = $$moonbitlang$core$builtin$Map$$new$inner$4(_tmp$1882);
  moonbit_incref(arr$745.$0);
  _len$748 = $$moonbitlang$core$builtin$ArrayView$$length$4(arr$745);
  _i$749 = 0;
  while (1) {
    if (_i$749 < _len$748) {
      struct $$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** _field$2404 =
        arr$745.$0;
      struct $$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** buf$1878 =
        _field$2404;
      int32_t start$1880 = arr$745.$1;
      int32_t _tmp$1879 = start$1880 + _i$749;
      struct $$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$2403 =
        (struct $$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$*)buf$1878[
          _tmp$1879
        ];
      struct $$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* e$750 =
        _tmp$2403;
      moonbit_string_t _field$2402 = e$750->$0;
      moonbit_string_t _tmp$1876 = _field$2402;
      struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _field$2401 =
        e$750->$1;
      struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _tmp$1877 =
        _field$2401;
      int32_t _tmp$1881;
      moonbit_incref(_tmp$1877);
      moonbit_incref(_tmp$1876);
      moonbit_incref(m$747);
      $$moonbitlang$core$builtin$Map$$set$4(m$747, _tmp$1876, _tmp$1877);
      _tmp$1881 = _i$749 + 1;
      _i$749 = _tmp$1881;
      continue;
    } else {
      moonbit_decref(arr$745.$0);
    }
    break;
  }
  return m$747;
}

struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* $$moonbitlang$core$builtin$Map$$from_array$3(
  struct $$moonbitlang$core$builtin$ArrayView$3c$$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$$3e$ arr$737
) {
  int32_t length$736;
  int32_t capacity$738;
  int32_t _tmp$1864;
  int32_t _tmp$1863;
  int32_t _tmp$1872;
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* m$739;
  int32_t _len$740;
  int32_t _i$741;
  moonbit_incref(arr$737.$0);
  length$736 = $$moonbitlang$core$builtin$ArrayView$$length$3(arr$737);
  capacity$738 = $Int$$next_power_of_two(length$736);
  _tmp$1864 = capacity$738;
  _tmp$1863 = $moonbitlang$core$builtin$calc_grow_threshold(_tmp$1864);
  if (length$736 > _tmp$1863) {
    int32_t _tmp$1865 = capacity$738;
    capacity$738 = _tmp$1865 * 2;
  }
  _tmp$1872 = capacity$738;
  m$739 = $$moonbitlang$core$builtin$Map$$new$inner$3(_tmp$1872);
  moonbit_incref(arr$737.$0);
  _len$740 = $$moonbitlang$core$builtin$ArrayView$$length$3(arr$737);
  _i$741 = 0;
  while (1) {
    if (_i$741 < _len$740) {
      struct $$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** _field$2408 =
        arr$737.$0;
      struct $$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** buf$1868 =
        _field$2408;
      int32_t start$1870 = arr$737.$1;
      int32_t _tmp$1869 = start$1870 + _i$741;
      struct $$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$2407 =
        (struct $$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$*)buf$1868[
          _tmp$1869
        ];
      struct $$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* e$742 =
        _tmp$2407;
      moonbit_string_t _field$2406 = e$742->$0;
      moonbit_string_t _tmp$1866 = _field$2406;
      struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _field$2405 =
        e$742->$1;
      struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _tmp$1867 =
        _field$2405;
      int32_t _tmp$1871;
      moonbit_incref(_tmp$1867);
      moonbit_incref(_tmp$1866);
      moonbit_incref(m$739);
      $$moonbitlang$core$builtin$Map$$set$3(m$739, _tmp$1866, _tmp$1867);
      _tmp$1871 = _i$741 + 1;
      _i$741 = _tmp$1871;
      continue;
    } else {
      moonbit_decref(arr$737.$0);
    }
    break;
  }
  return m$739;
}

struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* $$moonbitlang$core$builtin$Map$$from_array$2(
  struct $$moonbitlang$core$builtin$ArrayView$3c$$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$$3e$ arr$729
) {
  int32_t length$728;
  int32_t capacity$730;
  int32_t _tmp$1854;
  int32_t _tmp$1853;
  int32_t _tmp$1862;
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* m$731;
  int32_t _len$732;
  int32_t _i$733;
  moonbit_incref(arr$729.$0);
  length$728 = $$moonbitlang$core$builtin$ArrayView$$length$2(arr$729);
  capacity$730 = $Int$$next_power_of_two(length$728);
  _tmp$1854 = capacity$730;
  _tmp$1853 = $moonbitlang$core$builtin$calc_grow_threshold(_tmp$1854);
  if (length$728 > _tmp$1853) {
    int32_t _tmp$1855 = capacity$730;
    capacity$730 = _tmp$1855 * 2;
  }
  _tmp$1862 = capacity$730;
  m$731 = $$moonbitlang$core$builtin$Map$$new$inner$2(_tmp$1862);
  moonbit_incref(arr$729.$0);
  _len$732 = $$moonbitlang$core$builtin$ArrayView$$length$2(arr$729);
  _i$733 = 0;
  while (1) {
    if (_i$733 < _len$732) {
      struct $$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** _field$2412 =
        arr$729.$0;
      struct $$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** buf$1858 =
        _field$2412;
      int32_t start$1860 = arr$729.$1;
      int32_t _tmp$1859 = start$1860 + _i$733;
      struct $$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$2411 =
        (struct $$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$*)buf$1858[
          _tmp$1859
        ];
      struct $$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* e$734 =
        _tmp$2411;
      moonbit_string_t _field$2410 = e$734->$0;
      moonbit_string_t _tmp$1856 = _field$2410;
      struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _field$2409 =
        e$734->$1;
      struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _tmp$1857 =
        _field$2409;
      int32_t _tmp$1861;
      moonbit_incref(_tmp$1857);
      moonbit_incref(_tmp$1856);
      moonbit_incref(m$731);
      $$moonbitlang$core$builtin$Map$$set$2(m$731, _tmp$1856, _tmp$1857);
      _tmp$1861 = _i$733 + 1;
      _i$733 = _tmp$1861;
      continue;
    } else {
      moonbit_decref(arr$729.$0);
    }
    break;
  }
  return m$731;
}

struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* $$moonbitlang$core$builtin$Map$$from_array$1(
  struct $$moonbitlang$core$builtin$ArrayView$3c$$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$ arr$721
) {
  int32_t length$720;
  int32_t capacity$722;
  int32_t _tmp$1844;
  int32_t _tmp$1843;
  int32_t _tmp$1852;
  struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* m$723;
  int32_t _len$724;
  int32_t _i$725;
  moonbit_incref(arr$721.$0);
  length$720 = $$moonbitlang$core$builtin$ArrayView$$length$1(arr$721);
  capacity$722 = $Int$$next_power_of_two(length$720);
  _tmp$1844 = capacity$722;
  _tmp$1843 = $moonbitlang$core$builtin$calc_grow_threshold(_tmp$1844);
  if (length$720 > _tmp$1843) {
    int32_t _tmp$1845 = capacity$722;
    capacity$722 = _tmp$1845 * 2;
  }
  _tmp$1852 = capacity$722;
  m$723 = $$moonbitlang$core$builtin$Map$$new$inner$1(_tmp$1852);
  moonbit_incref(arr$721.$0);
  _len$724 = $$moonbitlang$core$builtin$ArrayView$$length$1(arr$721);
  _i$725 = 0;
  while (1) {
    if (_i$725 < _len$724) {
      struct $$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$** _field$2415 =
        arr$721.$0;
      struct $$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$** buf$1848 =
        _field$2415;
      int32_t start$1850 = arr$721.$1;
      int32_t _tmp$1849 = start$1850 + _i$725;
      struct $$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _tmp$2414 =
        (struct $$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$*)buf$1848[
          _tmp$1849
        ];
      struct $$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* e$726 =
        _tmp$2414;
      int32_t _tmp$1846 = e$726->$0;
      struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* _field$2413 =
        e$726->$1;
      struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* _tmp$1847 =
        _field$2413;
      int32_t _tmp$1851;
      moonbit_incref(_tmp$1847);
      moonbit_incref(m$723);
      $$moonbitlang$core$builtin$Map$$set$1(m$723, _tmp$1846, _tmp$1847);
      _tmp$1851 = _i$725 + 1;
      _i$725 = _tmp$1851;
      continue;
    } else {
      moonbit_decref(arr$721.$0);
    }
    break;
  }
  return m$723;
}

struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* $$moonbitlang$core$builtin$Map$$from_array$0(
  struct $$moonbitlang$core$builtin$ArrayView$3c$$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$$3e$ arr$713
) {
  int32_t length$712;
  int32_t capacity$714;
  int32_t _tmp$1834;
  int32_t _tmp$1833;
  int32_t _tmp$1842;
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* m$715;
  int32_t _len$716;
  int32_t _i$717;
  moonbit_incref(arr$713.$0);
  length$712 = $$moonbitlang$core$builtin$ArrayView$$length$0(arr$713);
  capacity$714 = $Int$$next_power_of_two(length$712);
  _tmp$1834 = capacity$714;
  _tmp$1833 = $moonbitlang$core$builtin$calc_grow_threshold(_tmp$1834);
  if (length$712 > _tmp$1833) {
    int32_t _tmp$1835 = capacity$714;
    capacity$714 = _tmp$1835 * 2;
  }
  _tmp$1842 = capacity$714;
  m$715 = $$moonbitlang$core$builtin$Map$$new$inner$0(_tmp$1842);
  moonbit_incref(arr$713.$0);
  _len$716 = $$moonbitlang$core$builtin$ArrayView$$length$0(arr$713);
  _i$717 = 0;
  while (1) {
    if (_i$717 < _len$716) {
      struct $$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** _field$2419 =
        arr$713.$0;
      struct $$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** buf$1838 =
        _field$2419;
      int32_t start$1840 = arr$713.$1;
      int32_t _tmp$1839 = start$1840 + _i$717;
      struct $$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$2418 =
        (struct $$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$*)buf$1838[
          _tmp$1839
        ];
      struct $$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* e$718 =
        _tmp$2418;
      moonbit_string_t _field$2417 = e$718->$0;
      moonbit_string_t _tmp$1836 = _field$2417;
      struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _field$2416 =
        e$718->$1;
      struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _tmp$1837 =
        _field$2416;
      int32_t _tmp$1841;
      moonbit_incref(_tmp$1837);
      moonbit_incref(_tmp$1836);
      moonbit_incref(m$715);
      $$moonbitlang$core$builtin$Map$$set$0(m$715, _tmp$1836, _tmp$1837);
      _tmp$1841 = _i$717 + 1;
      _i$717 = _tmp$1841;
      continue;
    } else {
      moonbit_decref(arr$713.$0);
    }
    break;
  }
  return m$715;
}

int32_t $$moonbitlang$core$builtin$Map$$set$4(
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$709,
  moonbit_string_t key$710,
  struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* value$711
) {
  int32_t _tmp$1832;
  moonbit_incref(key$710);
  _tmp$1832 = $$moonbitlang$core$builtin$Hash$$$default_impl$$hash$1(key$710);
  $$moonbitlang$core$builtin$Map$$set_with_hash$4(
    self$709, key$710, value$711, _tmp$1832
  );
  return 0;
}

int32_t $$moonbitlang$core$builtin$Map$$set$3(
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$706,
  moonbit_string_t key$707,
  struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* value$708
) {
  int32_t _tmp$1831;
  moonbit_incref(key$707);
  _tmp$1831 = $$moonbitlang$core$builtin$Hash$$$default_impl$$hash$1(key$707);
  $$moonbitlang$core$builtin$Map$$set_with_hash$3(
    self$706, key$707, value$708, _tmp$1831
  );
  return 0;
}

int32_t $$moonbitlang$core$builtin$Map$$set$2(
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$703,
  moonbit_string_t key$704,
  struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* value$705
) {
  int32_t _tmp$1830;
  moonbit_incref(key$704);
  _tmp$1830 = $$moonbitlang$core$builtin$Hash$$$default_impl$$hash$1(key$704);
  $$moonbitlang$core$builtin$Map$$set_with_hash$2(
    self$703, key$704, value$705, _tmp$1830
  );
  return 0;
}

int32_t $$moonbitlang$core$builtin$Map$$set$1(
  struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* self$700,
  int32_t key$701,
  struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* value$702
) {
  int32_t _tmp$1829 =
    $$moonbitlang$core$builtin$Hash$$$default_impl$$hash$0(key$701);
  $$moonbitlang$core$builtin$Map$$set_with_hash$1(
    self$700, key$701, value$702, _tmp$1829
  );
  return 0;
}

int32_t $$moonbitlang$core$builtin$Map$$set$0(
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$697,
  moonbit_string_t key$698,
  struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* value$699
) {
  int32_t _tmp$1828;
  moonbit_incref(key$698);
  _tmp$1828 = $$moonbitlang$core$builtin$Hash$$$default_impl$$hash$1(key$698);
  $$moonbitlang$core$builtin$Map$$set_with_hash$0(
    self$697, key$698, value$699, _tmp$1828
  );
  return 0;
}

int32_t $$moonbitlang$core$builtin$Map$$grow$4(
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$687
) {
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _field$2426 =
    self$687->$5;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* old_head$686 =
    _field$2426;
  int32_t capacity$1827 = self$687->$2;
  int32_t new_capacity$688 = capacity$1827 << 1;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$1822 =
    0;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** _tmp$1821 =
    (struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$**)moonbit_make_ref_array(
      new_capacity$688, _tmp$1822
    );
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** _old$2425 =
    self$687->$0;
  int32_t _tmp$1823;
  int32_t capacity$1825;
  int32_t _tmp$1824;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$1826;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _old$2424;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _param$689;
  if (old_head$686) {
    moonbit_incref(old_head$686);
  }
  moonbit_decref(_old$2425);
  self$687->$0 = _tmp$1821;
  self$687->$2 = new_capacity$688;
  _tmp$1823 = new_capacity$688 - 1;
  self$687->$3 = _tmp$1823;
  capacity$1825 = self$687->$2;
  _tmp$1824 = $moonbitlang$core$builtin$calc_grow_threshold(capacity$1825);
  self$687->$4 = _tmp$1824;
  self$687->$1 = 0;
  _tmp$1826 = 0;
  _old$2424 = self$687->$5;
  if (_old$2424) {
    moonbit_decref(_old$2424);
  }
  self$687->$5 = _tmp$1826;
  self$687->$6 = -1;
  _param$689 = old_head$686;
  while (1) {
    if (_param$689 == 0) {
      if (_param$689) {
        moonbit_decref(_param$689);
      }
      moonbit_decref(self$687);
    } else {
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _Some$690 =
        _param$689;
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _x$691 =
        _Some$690;
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _field$2423 =
        _x$691->$1;
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _next$692 =
        _field$2423;
      moonbit_string_t _field$2422 = _x$691->$4;
      moonbit_string_t _key$693 = _field$2422;
      struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _field$2421 =
        _x$691->$5;
      struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _value$694 =
        _field$2421;
      int32_t _field$2420 = _x$691->$3;
      int32_t _cnt$2686 = Moonbit_object_header(_x$691)->rc;
      int32_t _hash$695;
      if (_cnt$2686 > 1) {
        int32_t _new_cnt$2687 = _cnt$2686 - 1;
        Moonbit_object_header(_x$691)->rc = _new_cnt$2687;
        moonbit_incref(_value$694);
        moonbit_incref(_key$693);
        if (_next$692) {
          moonbit_incref(_next$692);
        }
      } else if (_cnt$2686 == 1) {
        moonbit_free(_x$691);
      }
      _hash$695 = _field$2420;
      moonbit_incref(self$687);
      $$moonbitlang$core$builtin$Map$$set_with_hash$4(
        self$687, _key$693, _value$694, _hash$695
      );
      _param$689 = _next$692;
      continue;
    }
    break;
  }
  return 0;
}

int32_t $$moonbitlang$core$builtin$Map$$grow$3(
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$676
) {
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _field$2433 =
    self$676->$5;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* old_head$675 =
    _field$2433;
  int32_t capacity$1820 = self$676->$2;
  int32_t new_capacity$677 = capacity$1820 << 1;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$1815 =
    0;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** _tmp$1814 =
    (struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$**)moonbit_make_ref_array(
      new_capacity$677, _tmp$1815
    );
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** _old$2432 =
    self$676->$0;
  int32_t _tmp$1816;
  int32_t capacity$1818;
  int32_t _tmp$1817;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$1819;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _old$2431;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _param$678;
  if (old_head$675) {
    moonbit_incref(old_head$675);
  }
  moonbit_decref(_old$2432);
  self$676->$0 = _tmp$1814;
  self$676->$2 = new_capacity$677;
  _tmp$1816 = new_capacity$677 - 1;
  self$676->$3 = _tmp$1816;
  capacity$1818 = self$676->$2;
  _tmp$1817 = $moonbitlang$core$builtin$calc_grow_threshold(capacity$1818);
  self$676->$4 = _tmp$1817;
  self$676->$1 = 0;
  _tmp$1819 = 0;
  _old$2431 = self$676->$5;
  if (_old$2431) {
    moonbit_decref(_old$2431);
  }
  self$676->$5 = _tmp$1819;
  self$676->$6 = -1;
  _param$678 = old_head$675;
  while (1) {
    if (_param$678 == 0) {
      if (_param$678) {
        moonbit_decref(_param$678);
      }
      moonbit_decref(self$676);
    } else {
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _Some$679 =
        _param$678;
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _x$680 =
        _Some$679;
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _field$2430 =
        _x$680->$1;
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _next$681 =
        _field$2430;
      moonbit_string_t _field$2429 = _x$680->$4;
      moonbit_string_t _key$682 = _field$2429;
      struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _field$2428 =
        _x$680->$5;
      struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _value$683 =
        _field$2428;
      int32_t _field$2427 = _x$680->$3;
      int32_t _cnt$2688 = Moonbit_object_header(_x$680)->rc;
      int32_t _hash$684;
      if (_cnt$2688 > 1) {
        int32_t _new_cnt$2689 = _cnt$2688 - 1;
        Moonbit_object_header(_x$680)->rc = _new_cnt$2689;
        moonbit_incref(_value$683);
        moonbit_incref(_key$682);
        if (_next$681) {
          moonbit_incref(_next$681);
        }
      } else if (_cnt$2688 == 1) {
        moonbit_free(_x$680);
      }
      _hash$684 = _field$2427;
      moonbit_incref(self$676);
      $$moonbitlang$core$builtin$Map$$set_with_hash$3(
        self$676, _key$682, _value$683, _hash$684
      );
      _param$678 = _next$681;
      continue;
    }
    break;
  }
  return 0;
}

int32_t $$moonbitlang$core$builtin$Map$$grow$2(
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$665
) {
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _field$2440 =
    self$665->$5;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* old_head$664 =
    _field$2440;
  int32_t capacity$1813 = self$665->$2;
  int32_t new_capacity$666 = capacity$1813 << 1;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$1808 =
    0;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** _tmp$1807 =
    (struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$**)moonbit_make_ref_array(
      new_capacity$666, _tmp$1808
    );
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** _old$2439 =
    self$665->$0;
  int32_t _tmp$1809;
  int32_t capacity$1811;
  int32_t _tmp$1810;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$1812;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _old$2438;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _param$667;
  if (old_head$664) {
    moonbit_incref(old_head$664);
  }
  moonbit_decref(_old$2439);
  self$665->$0 = _tmp$1807;
  self$665->$2 = new_capacity$666;
  _tmp$1809 = new_capacity$666 - 1;
  self$665->$3 = _tmp$1809;
  capacity$1811 = self$665->$2;
  _tmp$1810 = $moonbitlang$core$builtin$calc_grow_threshold(capacity$1811);
  self$665->$4 = _tmp$1810;
  self$665->$1 = 0;
  _tmp$1812 = 0;
  _old$2438 = self$665->$5;
  if (_old$2438) {
    moonbit_decref(_old$2438);
  }
  self$665->$5 = _tmp$1812;
  self$665->$6 = -1;
  _param$667 = old_head$664;
  while (1) {
    if (_param$667 == 0) {
      if (_param$667) {
        moonbit_decref(_param$667);
      }
      moonbit_decref(self$665);
    } else {
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _Some$668 =
        _param$667;
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _x$669 =
        _Some$668;
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _field$2437 =
        _x$669->$1;
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _next$670 =
        _field$2437;
      moonbit_string_t _field$2436 = _x$669->$4;
      moonbit_string_t _key$671 = _field$2436;
      struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _field$2435 =
        _x$669->$5;
      struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _value$672 =
        _field$2435;
      int32_t _field$2434 = _x$669->$3;
      int32_t _cnt$2690 = Moonbit_object_header(_x$669)->rc;
      int32_t _hash$673;
      if (_cnt$2690 > 1) {
        int32_t _new_cnt$2691 = _cnt$2690 - 1;
        Moonbit_object_header(_x$669)->rc = _new_cnt$2691;
        moonbit_incref(_value$672);
        moonbit_incref(_key$671);
        if (_next$670) {
          moonbit_incref(_next$670);
        }
      } else if (_cnt$2690 == 1) {
        moonbit_free(_x$669);
      }
      _hash$673 = _field$2434;
      moonbit_incref(self$665);
      $$moonbitlang$core$builtin$Map$$set_with_hash$2(
        self$665, _key$671, _value$672, _hash$673
      );
      _param$667 = _next$670;
      continue;
    }
    break;
  }
  return 0;
}

int32_t $$moonbitlang$core$builtin$Map$$grow$1(
  struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* self$654
) {
  struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _field$2446 =
    self$654->$5;
  struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* old_head$653 =
    _field$2446;
  int32_t capacity$1806 = self$654->$2;
  int32_t new_capacity$655 = capacity$1806 << 1;
  struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _tmp$1801 =
    0;
  struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$** _tmp$1800 =
    (struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$**)moonbit_make_ref_array(
      new_capacity$655, _tmp$1801
    );
  struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$** _old$2445 =
    self$654->$0;
  int32_t _tmp$1802;
  int32_t capacity$1804;
  int32_t _tmp$1803;
  struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _tmp$1805;
  struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _old$2444;
  struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _param$656;
  if (old_head$653) {
    moonbit_incref(old_head$653);
  }
  moonbit_decref(_old$2445);
  self$654->$0 = _tmp$1800;
  self$654->$2 = new_capacity$655;
  _tmp$1802 = new_capacity$655 - 1;
  self$654->$3 = _tmp$1802;
  capacity$1804 = self$654->$2;
  _tmp$1803 = $moonbitlang$core$builtin$calc_grow_threshold(capacity$1804);
  self$654->$4 = _tmp$1803;
  self$654->$1 = 0;
  _tmp$1805 = 0;
  _old$2444 = self$654->$5;
  if (_old$2444) {
    moonbit_decref(_old$2444);
  }
  self$654->$5 = _tmp$1805;
  self$654->$6 = -1;
  _param$656 = old_head$653;
  while (1) {
    if (_param$656 == 0) {
      if (_param$656) {
        moonbit_decref(_param$656);
      }
      moonbit_decref(self$654);
    } else {
      struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _Some$657 =
        _param$656;
      struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _x$658 =
        _Some$657;
      struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _field$2443 =
        _x$658->$1;
      struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _next$659 =
        _field$2443;
      int32_t _key$660 = _x$658->$4;
      struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* _field$2442 =
        _x$658->$5;
      struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* _value$661 =
        _field$2442;
      int32_t _field$2441 = _x$658->$3;
      int32_t _cnt$2692 = Moonbit_object_header(_x$658)->rc;
      int32_t _hash$662;
      if (_cnt$2692 > 1) {
        int32_t _new_cnt$2693 = _cnt$2692 - 1;
        Moonbit_object_header(_x$658)->rc = _new_cnt$2693;
        moonbit_incref(_value$661);
        if (_next$659) {
          moonbit_incref(_next$659);
        }
      } else if (_cnt$2692 == 1) {
        moonbit_free(_x$658);
      }
      _hash$662 = _field$2441;
      moonbit_incref(self$654);
      $$moonbitlang$core$builtin$Map$$set_with_hash$1(
        self$654, _key$660, _value$661, _hash$662
      );
      _param$656 = _next$659;
      continue;
    }
    break;
  }
  return 0;
}

int32_t $$moonbitlang$core$builtin$Map$$grow$0(
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$643
) {
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _field$2453 =
    self$643->$5;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* old_head$642 =
    _field$2453;
  int32_t capacity$1799 = self$643->$2;
  int32_t new_capacity$644 = capacity$1799 << 1;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$1794 =
    0;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** _tmp$1793 =
    (struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$**)moonbit_make_ref_array(
      new_capacity$644, _tmp$1794
    );
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** _old$2452 =
    self$643->$0;
  int32_t _tmp$1795;
  int32_t capacity$1797;
  int32_t _tmp$1796;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$1798;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _old$2451;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _param$645;
  if (old_head$642) {
    moonbit_incref(old_head$642);
  }
  moonbit_decref(_old$2452);
  self$643->$0 = _tmp$1793;
  self$643->$2 = new_capacity$644;
  _tmp$1795 = new_capacity$644 - 1;
  self$643->$3 = _tmp$1795;
  capacity$1797 = self$643->$2;
  _tmp$1796 = $moonbitlang$core$builtin$calc_grow_threshold(capacity$1797);
  self$643->$4 = _tmp$1796;
  self$643->$1 = 0;
  _tmp$1798 = 0;
  _old$2451 = self$643->$5;
  if (_old$2451) {
    moonbit_decref(_old$2451);
  }
  self$643->$5 = _tmp$1798;
  self$643->$6 = -1;
  _param$645 = old_head$642;
  while (1) {
    if (_param$645 == 0) {
      if (_param$645) {
        moonbit_decref(_param$645);
      }
      moonbit_decref(self$643);
    } else {
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _Some$646 =
        _param$645;
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _x$647 =
        _Some$646;
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _field$2450 =
        _x$647->$1;
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _next$648 =
        _field$2450;
      moonbit_string_t _field$2449 = _x$647->$4;
      moonbit_string_t _key$649 = _field$2449;
      struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _field$2448 =
        _x$647->$5;
      struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _value$650 =
        _field$2448;
      int32_t _field$2447 = _x$647->$3;
      int32_t _cnt$2694 = Moonbit_object_header(_x$647)->rc;
      int32_t _hash$651;
      if (_cnt$2694 > 1) {
        int32_t _new_cnt$2695 = _cnt$2694 - 1;
        Moonbit_object_header(_x$647)->rc = _new_cnt$2695;
        moonbit_incref(_value$650);
        moonbit_incref(_key$649);
        if (_next$648) {
          moonbit_incref(_next$648);
        }
      } else if (_cnt$2694 == 1) {
        moonbit_free(_x$647);
      }
      _hash$651 = _field$2447;
      moonbit_incref(self$643);
      $$moonbitlang$core$builtin$Map$$set_with_hash$0(
        self$643, _key$649, _value$650, _hash$651
      );
      _param$645 = _next$648;
      continue;
    }
    break;
  }
  return 0;
}

int32_t $$moonbitlang$core$builtin$Map$$set_with_hash$4(
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$626,
  moonbit_string_t key$635,
  struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* value$636,
  int32_t hash$634
) {
  int32_t size$1779 = self$626->$1;
  int32_t grow_at$1780 = self$626->$4;
  int32_t capacity_mask$1792;
  int32_t _tmp$1791;
  struct $$3c$Int$2a$Int$3e$* _bind$627;
  int32_t psl$628;
  int32_t idx$629;
  int32_t _idx$637;
  int32_t _field$2454;
  int32_t _psl$638;
  int32_t _bind$639;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _bind$640;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* entry$641;
  if (size$1779 >= grow_at$1780) {
    moonbit_incref(self$626);
    $$moonbitlang$core$builtin$Map$$grow$4(self$626);
  }
  capacity_mask$1792 = self$626->$3;
  _tmp$1791 = hash$634 & capacity_mask$1792;
  psl$628 = 0;
  idx$629 = _tmp$1791;
  while (1) {
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** _field$2459 =
      self$626->$0;
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** entries$1790 =
      _field$2459;
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$2458;
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _bind$630;
    if (idx$629 < 0 || idx$629 >= Moonbit_array_length(entries$1790)) {
      moonbit_panic();
    }
    _tmp$2458
    = (struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$*)entries$1790[
        idx$629
      ];
    _bind$630 = _tmp$2458;
    if (_bind$630 == 0) {
      struct $$3c$Int$2a$Int$3e$* _tuple$1781 =
        (struct $$3c$Int$2a$Int$3e$*)moonbit_malloc(
          sizeof(struct $$3c$Int$2a$Int$3e$)
        );
      Moonbit_object_header(_tuple$1781)->meta
      = Moonbit_make_regular_object_header(
        sizeof(struct $$3c$Int$2a$Int$3e$) >> 2, 0, 0
      );
      _tuple$1781->$0 = idx$629;
      _tuple$1781->$1 = psl$628;
      _bind$627 = _tuple$1781;
      break;
    } else {
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _Some$632 =
        _bind$630;
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _curr_entry$633 =
        _Some$632;
      int32_t hash$1783 = _curr_entry$633->$3;
      int32_t _if_result$2807;
      int32_t psl$1784;
      int32_t _tmp$1786;
      int32_t _tmp$1788;
      int32_t capacity_mask$1789;
      int32_t _tmp$1787;
      if (hash$1783 == hash$634) {
        moonbit_string_t _field$2457 = _curr_entry$633->$4;
        moonbit_string_t key$1782 = _field$2457;
        int32_t _tmp$2456 = moonbit_val_array_equal(key$1782, key$635);
        _if_result$2807 = _tmp$2456;
      } else {
        _if_result$2807 = 0;
      }
      if (_if_result$2807) {
        struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _old$2455;
        moonbit_incref(_curr_entry$633);
        moonbit_decref(key$635);
        moonbit_decref(self$626);
        _old$2455 = _curr_entry$633->$5;
        moonbit_decref(_old$2455);
        _curr_entry$633->$5 = value$636;
        moonbit_decref(_curr_entry$633);
        return 0;
      } else {
        moonbit_incref(_curr_entry$633);
      }
      psl$1784 = _curr_entry$633->$2;
      if (psl$628 > psl$1784) {
        struct $$3c$Int$2a$Int$3e$* _tuple$1785;
        moonbit_incref(self$626);
        $$moonbitlang$core$builtin$Map$$push_away$4(
          self$626, idx$629, _curr_entry$633
        );
        _tuple$1785
        = (struct $$3c$Int$2a$Int$3e$*)moonbit_malloc(
            sizeof(struct $$3c$Int$2a$Int$3e$)
          );
        Moonbit_object_header(_tuple$1785)->meta
        = Moonbit_make_regular_object_header(
          sizeof(struct $$3c$Int$2a$Int$3e$) >> 2, 0, 0
        );
        _tuple$1785->$0 = idx$629;
        _tuple$1785->$1 = psl$628;
        _bind$627 = _tuple$1785;
        break;
      } else {
        moonbit_decref(_curr_entry$633);
      }
      _tmp$1786 = psl$628 + 1;
      _tmp$1788 = idx$629 + 1;
      capacity_mask$1789 = self$626->$3;
      _tmp$1787 = _tmp$1788 & capacity_mask$1789;
      psl$628 = _tmp$1786;
      idx$629 = _tmp$1787;
      continue;
    }
    break;
  }
  _idx$637 = _bind$627->$0;
  _field$2454 = _bind$627->$1;
  moonbit_decref(_bind$627);
  _psl$638 = _field$2454;
  _bind$639 = self$626->$6;
  _bind$640 = 0;
  entry$641
  = (struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$*)moonbit_malloc(
      sizeof(
        struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$
      )
    );
  Moonbit_object_header(entry$641)->meta
  = Moonbit_make_regular_object_header(
    offsetof(
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$,
        $1
    )
    >> 2,
      3,
      0
  );
  entry$641->$0 = _bind$639;
  entry$641->$1 = _bind$640;
  entry$641->$2 = _psl$638;
  entry$641->$3 = hash$634;
  entry$641->$4 = key$635;
  entry$641->$5 = value$636;
  $$moonbitlang$core$builtin$Map$$add_entry_to_tail$4(
    self$626, _idx$637, entry$641
  );
  return 0;
}

int32_t $$moonbitlang$core$builtin$Map$$set_with_hash$3(
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$610,
  moonbit_string_t key$619,
  struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* value$620,
  int32_t hash$618
) {
  int32_t size$1765 = self$610->$1;
  int32_t grow_at$1766 = self$610->$4;
  int32_t capacity_mask$1778;
  int32_t _tmp$1777;
  struct $$3c$Int$2a$Int$3e$* _bind$611;
  int32_t psl$612;
  int32_t idx$613;
  int32_t _idx$621;
  int32_t _field$2460;
  int32_t _psl$622;
  int32_t _bind$623;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _bind$624;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* entry$625;
  if (size$1765 >= grow_at$1766) {
    moonbit_incref(self$610);
    $$moonbitlang$core$builtin$Map$$grow$3(self$610);
  }
  capacity_mask$1778 = self$610->$3;
  _tmp$1777 = hash$618 & capacity_mask$1778;
  psl$612 = 0;
  idx$613 = _tmp$1777;
  while (1) {
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** _field$2465 =
      self$610->$0;
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** entries$1776 =
      _field$2465;
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$2464;
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _bind$614;
    if (idx$613 < 0 || idx$613 >= Moonbit_array_length(entries$1776)) {
      moonbit_panic();
    }
    _tmp$2464
    = (struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$*)entries$1776[
        idx$613
      ];
    _bind$614 = _tmp$2464;
    if (_bind$614 == 0) {
      struct $$3c$Int$2a$Int$3e$* _tuple$1767 =
        (struct $$3c$Int$2a$Int$3e$*)moonbit_malloc(
          sizeof(struct $$3c$Int$2a$Int$3e$)
        );
      Moonbit_object_header(_tuple$1767)->meta
      = Moonbit_make_regular_object_header(
        sizeof(struct $$3c$Int$2a$Int$3e$) >> 2, 0, 0
      );
      _tuple$1767->$0 = idx$613;
      _tuple$1767->$1 = psl$612;
      _bind$611 = _tuple$1767;
      break;
    } else {
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _Some$616 =
        _bind$614;
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _curr_entry$617 =
        _Some$616;
      int32_t hash$1769 = _curr_entry$617->$3;
      int32_t _if_result$2809;
      int32_t psl$1770;
      int32_t _tmp$1772;
      int32_t _tmp$1774;
      int32_t capacity_mask$1775;
      int32_t _tmp$1773;
      if (hash$1769 == hash$618) {
        moonbit_string_t _field$2463 = _curr_entry$617->$4;
        moonbit_string_t key$1768 = _field$2463;
        int32_t _tmp$2462 = moonbit_val_array_equal(key$1768, key$619);
        _if_result$2809 = _tmp$2462;
      } else {
        _if_result$2809 = 0;
      }
      if (_if_result$2809) {
        struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _old$2461;
        moonbit_incref(_curr_entry$617);
        moonbit_decref(key$619);
        moonbit_decref(self$610);
        _old$2461 = _curr_entry$617->$5;
        moonbit_decref(_old$2461);
        _curr_entry$617->$5 = value$620;
        moonbit_decref(_curr_entry$617);
        return 0;
      } else {
        moonbit_incref(_curr_entry$617);
      }
      psl$1770 = _curr_entry$617->$2;
      if (psl$612 > psl$1770) {
        struct $$3c$Int$2a$Int$3e$* _tuple$1771;
        moonbit_incref(self$610);
        $$moonbitlang$core$builtin$Map$$push_away$3(
          self$610, idx$613, _curr_entry$617
        );
        _tuple$1771
        = (struct $$3c$Int$2a$Int$3e$*)moonbit_malloc(
            sizeof(struct $$3c$Int$2a$Int$3e$)
          );
        Moonbit_object_header(_tuple$1771)->meta
        = Moonbit_make_regular_object_header(
          sizeof(struct $$3c$Int$2a$Int$3e$) >> 2, 0, 0
        );
        _tuple$1771->$0 = idx$613;
        _tuple$1771->$1 = psl$612;
        _bind$611 = _tuple$1771;
        break;
      } else {
        moonbit_decref(_curr_entry$617);
      }
      _tmp$1772 = psl$612 + 1;
      _tmp$1774 = idx$613 + 1;
      capacity_mask$1775 = self$610->$3;
      _tmp$1773 = _tmp$1774 & capacity_mask$1775;
      psl$612 = _tmp$1772;
      idx$613 = _tmp$1773;
      continue;
    }
    break;
  }
  _idx$621 = _bind$611->$0;
  _field$2460 = _bind$611->$1;
  moonbit_decref(_bind$611);
  _psl$622 = _field$2460;
  _bind$623 = self$610->$6;
  _bind$624 = 0;
  entry$625
  = (struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$*)moonbit_malloc(
      sizeof(
        struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$
      )
    );
  Moonbit_object_header(entry$625)->meta
  = Moonbit_make_regular_object_header(
    offsetof(
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$,
        $1
    )
    >> 2,
      3,
      0
  );
  entry$625->$0 = _bind$623;
  entry$625->$1 = _bind$624;
  entry$625->$2 = _psl$622;
  entry$625->$3 = hash$618;
  entry$625->$4 = key$619;
  entry$625->$5 = value$620;
  $$moonbitlang$core$builtin$Map$$add_entry_to_tail$3(
    self$610, _idx$621, entry$625
  );
  return 0;
}

int32_t $$moonbitlang$core$builtin$Map$$set_with_hash$2(
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$594,
  moonbit_string_t key$603,
  struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* value$604,
  int32_t hash$602
) {
  int32_t size$1751 = self$594->$1;
  int32_t grow_at$1752 = self$594->$4;
  int32_t capacity_mask$1764;
  int32_t _tmp$1763;
  struct $$3c$Int$2a$Int$3e$* _bind$595;
  int32_t psl$596;
  int32_t idx$597;
  int32_t _idx$605;
  int32_t _field$2466;
  int32_t _psl$606;
  int32_t _bind$607;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _bind$608;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* entry$609;
  if (size$1751 >= grow_at$1752) {
    moonbit_incref(self$594);
    $$moonbitlang$core$builtin$Map$$grow$2(self$594);
  }
  capacity_mask$1764 = self$594->$3;
  _tmp$1763 = hash$602 & capacity_mask$1764;
  psl$596 = 0;
  idx$597 = _tmp$1763;
  while (1) {
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** _field$2471 =
      self$594->$0;
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** entries$1762 =
      _field$2471;
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$2470;
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _bind$598;
    if (idx$597 < 0 || idx$597 >= Moonbit_array_length(entries$1762)) {
      moonbit_panic();
    }
    _tmp$2470
    = (struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$*)entries$1762[
        idx$597
      ];
    _bind$598 = _tmp$2470;
    if (_bind$598 == 0) {
      struct $$3c$Int$2a$Int$3e$* _tuple$1753 =
        (struct $$3c$Int$2a$Int$3e$*)moonbit_malloc(
          sizeof(struct $$3c$Int$2a$Int$3e$)
        );
      Moonbit_object_header(_tuple$1753)->meta
      = Moonbit_make_regular_object_header(
        sizeof(struct $$3c$Int$2a$Int$3e$) >> 2, 0, 0
      );
      _tuple$1753->$0 = idx$597;
      _tuple$1753->$1 = psl$596;
      _bind$595 = _tuple$1753;
      break;
    } else {
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _Some$600 =
        _bind$598;
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _curr_entry$601 =
        _Some$600;
      int32_t hash$1755 = _curr_entry$601->$3;
      int32_t _if_result$2811;
      int32_t psl$1756;
      int32_t _tmp$1758;
      int32_t _tmp$1760;
      int32_t capacity_mask$1761;
      int32_t _tmp$1759;
      if (hash$1755 == hash$602) {
        moonbit_string_t _field$2469 = _curr_entry$601->$4;
        moonbit_string_t key$1754 = _field$2469;
        int32_t _tmp$2468 = moonbit_val_array_equal(key$1754, key$603);
        _if_result$2811 = _tmp$2468;
      } else {
        _if_result$2811 = 0;
      }
      if (_if_result$2811) {
        struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _old$2467;
        moonbit_incref(_curr_entry$601);
        moonbit_decref(key$603);
        moonbit_decref(self$594);
        _old$2467 = _curr_entry$601->$5;
        moonbit_decref(_old$2467);
        _curr_entry$601->$5 = value$604;
        moonbit_decref(_curr_entry$601);
        return 0;
      } else {
        moonbit_incref(_curr_entry$601);
      }
      psl$1756 = _curr_entry$601->$2;
      if (psl$596 > psl$1756) {
        struct $$3c$Int$2a$Int$3e$* _tuple$1757;
        moonbit_incref(self$594);
        $$moonbitlang$core$builtin$Map$$push_away$2(
          self$594, idx$597, _curr_entry$601
        );
        _tuple$1757
        = (struct $$3c$Int$2a$Int$3e$*)moonbit_malloc(
            sizeof(struct $$3c$Int$2a$Int$3e$)
          );
        Moonbit_object_header(_tuple$1757)->meta
        = Moonbit_make_regular_object_header(
          sizeof(struct $$3c$Int$2a$Int$3e$) >> 2, 0, 0
        );
        _tuple$1757->$0 = idx$597;
        _tuple$1757->$1 = psl$596;
        _bind$595 = _tuple$1757;
        break;
      } else {
        moonbit_decref(_curr_entry$601);
      }
      _tmp$1758 = psl$596 + 1;
      _tmp$1760 = idx$597 + 1;
      capacity_mask$1761 = self$594->$3;
      _tmp$1759 = _tmp$1760 & capacity_mask$1761;
      psl$596 = _tmp$1758;
      idx$597 = _tmp$1759;
      continue;
    }
    break;
  }
  _idx$605 = _bind$595->$0;
  _field$2466 = _bind$595->$1;
  moonbit_decref(_bind$595);
  _psl$606 = _field$2466;
  _bind$607 = self$594->$6;
  _bind$608 = 0;
  entry$609
  = (struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$*)moonbit_malloc(
      sizeof(
        struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$
      )
    );
  Moonbit_object_header(entry$609)->meta
  = Moonbit_make_regular_object_header(
    offsetof(
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$,
        $1
    )
    >> 2,
      3,
      0
  );
  entry$609->$0 = _bind$607;
  entry$609->$1 = _bind$608;
  entry$609->$2 = _psl$606;
  entry$609->$3 = hash$602;
  entry$609->$4 = key$603;
  entry$609->$5 = value$604;
  $$moonbitlang$core$builtin$Map$$add_entry_to_tail$2(
    self$594, _idx$605, entry$609
  );
  return 0;
}

int32_t $$moonbitlang$core$builtin$Map$$set_with_hash$1(
  struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* self$578,
  int32_t key$587,
  struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* value$588,
  int32_t hash$586
) {
  int32_t size$1737 = self$578->$1;
  int32_t grow_at$1738 = self$578->$4;
  int32_t capacity_mask$1750;
  int32_t _tmp$1749;
  struct $$3c$Int$2a$Int$3e$* _bind$579;
  int32_t psl$580;
  int32_t idx$581;
  int32_t _idx$589;
  int32_t _field$2472;
  int32_t _psl$590;
  int32_t _bind$591;
  struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _bind$592;
  struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* entry$593;
  if (size$1737 >= grow_at$1738) {
    moonbit_incref(self$578);
    $$moonbitlang$core$builtin$Map$$grow$1(self$578);
  }
  capacity_mask$1750 = self$578->$3;
  _tmp$1749 = hash$586 & capacity_mask$1750;
  psl$580 = 0;
  idx$581 = _tmp$1749;
  while (1) {
    struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$** _field$2475 =
      self$578->$0;
    struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$** entries$1748 =
      _field$2475;
    struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _tmp$2474;
    struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _bind$582;
    if (idx$581 < 0 || idx$581 >= Moonbit_array_length(entries$1748)) {
      moonbit_panic();
    }
    _tmp$2474
    = (struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$*)entries$1748[
        idx$581
      ];
    _bind$582 = _tmp$2474;
    if (_bind$582 == 0) {
      struct $$3c$Int$2a$Int$3e$* _tuple$1739 =
        (struct $$3c$Int$2a$Int$3e$*)moonbit_malloc(
          sizeof(struct $$3c$Int$2a$Int$3e$)
        );
      Moonbit_object_header(_tuple$1739)->meta
      = Moonbit_make_regular_object_header(
        sizeof(struct $$3c$Int$2a$Int$3e$) >> 2, 0, 0
      );
      _tuple$1739->$0 = idx$581;
      _tuple$1739->$1 = psl$580;
      _bind$579 = _tuple$1739;
      break;
    } else {
      struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _Some$584 =
        _bind$582;
      struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _curr_entry$585 =
        _Some$584;
      int32_t hash$1741 = _curr_entry$585->$3;
      int32_t _if_result$2813;
      int32_t psl$1742;
      int32_t _tmp$1744;
      int32_t _tmp$1746;
      int32_t capacity_mask$1747;
      int32_t _tmp$1745;
      if (hash$1741 == hash$586) {
        int32_t key$1740 = _curr_entry$585->$4;
        _if_result$2813 = key$1740 == key$587;
      } else {
        _if_result$2813 = 0;
      }
      if (_if_result$2813) {
        struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* _old$2473;
        moonbit_incref(_curr_entry$585);
        moonbit_decref(self$578);
        _old$2473 = _curr_entry$585->$5;
        moonbit_decref(_old$2473);
        _curr_entry$585->$5 = value$588;
        moonbit_decref(_curr_entry$585);
        return 0;
      } else {
        moonbit_incref(_curr_entry$585);
      }
      psl$1742 = _curr_entry$585->$2;
      if (psl$580 > psl$1742) {
        struct $$3c$Int$2a$Int$3e$* _tuple$1743;
        moonbit_incref(self$578);
        $$moonbitlang$core$builtin$Map$$push_away$1(
          self$578, idx$581, _curr_entry$585
        );
        _tuple$1743
        = (struct $$3c$Int$2a$Int$3e$*)moonbit_malloc(
            sizeof(struct $$3c$Int$2a$Int$3e$)
          );
        Moonbit_object_header(_tuple$1743)->meta
        = Moonbit_make_regular_object_header(
          sizeof(struct $$3c$Int$2a$Int$3e$) >> 2, 0, 0
        );
        _tuple$1743->$0 = idx$581;
        _tuple$1743->$1 = psl$580;
        _bind$579 = _tuple$1743;
        break;
      } else {
        moonbit_decref(_curr_entry$585);
      }
      _tmp$1744 = psl$580 + 1;
      _tmp$1746 = idx$581 + 1;
      capacity_mask$1747 = self$578->$3;
      _tmp$1745 = _tmp$1746 & capacity_mask$1747;
      psl$580 = _tmp$1744;
      idx$581 = _tmp$1745;
      continue;
    }
    break;
  }
  _idx$589 = _bind$579->$0;
  _field$2472 = _bind$579->$1;
  moonbit_decref(_bind$579);
  _psl$590 = _field$2472;
  _bind$591 = self$578->$6;
  _bind$592 = 0;
  entry$593
  = (struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$*)moonbit_malloc(
      sizeof(
        struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$
      )
    );
  Moonbit_object_header(entry$593)->meta
  = Moonbit_make_regular_object_header(
    offsetof(
      struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$,
        $1
    )
    >> 2,
      2,
      0
  );
  entry$593->$0 = _bind$591;
  entry$593->$1 = _bind$592;
  entry$593->$2 = _psl$590;
  entry$593->$3 = hash$586;
  entry$593->$4 = key$587;
  entry$593->$5 = value$588;
  $$moonbitlang$core$builtin$Map$$add_entry_to_tail$1(
    self$578, _idx$589, entry$593
  );
  return 0;
}

int32_t $$moonbitlang$core$builtin$Map$$set_with_hash$0(
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$562,
  moonbit_string_t key$571,
  struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* value$572,
  int32_t hash$570
) {
  int32_t size$1723 = self$562->$1;
  int32_t grow_at$1724 = self$562->$4;
  int32_t capacity_mask$1736;
  int32_t _tmp$1735;
  struct $$3c$Int$2a$Int$3e$* _bind$563;
  int32_t psl$564;
  int32_t idx$565;
  int32_t _idx$573;
  int32_t _field$2476;
  int32_t _psl$574;
  int32_t _bind$575;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _bind$576;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* entry$577;
  if (size$1723 >= grow_at$1724) {
    moonbit_incref(self$562);
    $$moonbitlang$core$builtin$Map$$grow$0(self$562);
  }
  capacity_mask$1736 = self$562->$3;
  _tmp$1735 = hash$570 & capacity_mask$1736;
  psl$564 = 0;
  idx$565 = _tmp$1735;
  while (1) {
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** _field$2481 =
      self$562->$0;
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** entries$1734 =
      _field$2481;
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$2480;
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _bind$566;
    if (idx$565 < 0 || idx$565 >= Moonbit_array_length(entries$1734)) {
      moonbit_panic();
    }
    _tmp$2480
    = (struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$*)entries$1734[
        idx$565
      ];
    _bind$566 = _tmp$2480;
    if (_bind$566 == 0) {
      struct $$3c$Int$2a$Int$3e$* _tuple$1725 =
        (struct $$3c$Int$2a$Int$3e$*)moonbit_malloc(
          sizeof(struct $$3c$Int$2a$Int$3e$)
        );
      Moonbit_object_header(_tuple$1725)->meta
      = Moonbit_make_regular_object_header(
        sizeof(struct $$3c$Int$2a$Int$3e$) >> 2, 0, 0
      );
      _tuple$1725->$0 = idx$565;
      _tuple$1725->$1 = psl$564;
      _bind$563 = _tuple$1725;
      break;
    } else {
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _Some$568 =
        _bind$566;
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _curr_entry$569 =
        _Some$568;
      int32_t hash$1727 = _curr_entry$569->$3;
      int32_t _if_result$2815;
      int32_t psl$1728;
      int32_t _tmp$1730;
      int32_t _tmp$1732;
      int32_t capacity_mask$1733;
      int32_t _tmp$1731;
      if (hash$1727 == hash$570) {
        moonbit_string_t _field$2479 = _curr_entry$569->$4;
        moonbit_string_t key$1726 = _field$2479;
        int32_t _tmp$2478 = moonbit_val_array_equal(key$1726, key$571);
        _if_result$2815 = _tmp$2478;
      } else {
        _if_result$2815 = 0;
      }
      if (_if_result$2815) {
        struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _old$2477;
        moonbit_incref(_curr_entry$569);
        moonbit_decref(key$571);
        moonbit_decref(self$562);
        _old$2477 = _curr_entry$569->$5;
        moonbit_decref(_old$2477);
        _curr_entry$569->$5 = value$572;
        moonbit_decref(_curr_entry$569);
        return 0;
      } else {
        moonbit_incref(_curr_entry$569);
      }
      psl$1728 = _curr_entry$569->$2;
      if (psl$564 > psl$1728) {
        struct $$3c$Int$2a$Int$3e$* _tuple$1729;
        moonbit_incref(self$562);
        $$moonbitlang$core$builtin$Map$$push_away$0(
          self$562, idx$565, _curr_entry$569
        );
        _tuple$1729
        = (struct $$3c$Int$2a$Int$3e$*)moonbit_malloc(
            sizeof(struct $$3c$Int$2a$Int$3e$)
          );
        Moonbit_object_header(_tuple$1729)->meta
        = Moonbit_make_regular_object_header(
          sizeof(struct $$3c$Int$2a$Int$3e$) >> 2, 0, 0
        );
        _tuple$1729->$0 = idx$565;
        _tuple$1729->$1 = psl$564;
        _bind$563 = _tuple$1729;
        break;
      } else {
        moonbit_decref(_curr_entry$569);
      }
      _tmp$1730 = psl$564 + 1;
      _tmp$1732 = idx$565 + 1;
      capacity_mask$1733 = self$562->$3;
      _tmp$1731 = _tmp$1732 & capacity_mask$1733;
      psl$564 = _tmp$1730;
      idx$565 = _tmp$1731;
      continue;
    }
    break;
  }
  _idx$573 = _bind$563->$0;
  _field$2476 = _bind$563->$1;
  moonbit_decref(_bind$563);
  _psl$574 = _field$2476;
  _bind$575 = self$562->$6;
  _bind$576 = 0;
  entry$577
  = (struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$*)moonbit_malloc(
      sizeof(
        struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$
      )
    );
  Moonbit_object_header(entry$577)->meta
  = Moonbit_make_regular_object_header(
    offsetof(
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$,
        $1
    )
    >> 2,
      3,
      0
  );
  entry$577->$0 = _bind$575;
  entry$577->$1 = _bind$576;
  entry$577->$2 = _psl$574;
  entry$577->$3 = hash$570;
  entry$577->$4 = key$571;
  entry$577->$5 = value$572;
  $$moonbitlang$core$builtin$Map$$add_entry_to_tail$0(
    self$562, _idx$573, entry$577
  );
  return 0;
}

int32_t $$moonbitlang$core$builtin$Map$$push_away$4(
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$556,
  int32_t idx$561,
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* entry$560
) {
  int32_t psl$1722 = entry$560->$2;
  int32_t _tmp$1718 = psl$1722 + 1;
  int32_t _tmp$1720 = idx$561 + 1;
  int32_t capacity_mask$1721 = self$556->$3;
  int32_t _tmp$1719 = _tmp$1720 & capacity_mask$1721;
  int32_t psl$552 = _tmp$1718;
  int32_t idx$553 = _tmp$1719;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* entry$554 =
    entry$560;
  while (1) {
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** _field$2483 =
      self$556->$0;
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** entries$1717 =
      _field$2483;
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$2482;
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _bind$555;
    if (idx$553 < 0 || idx$553 >= Moonbit_array_length(entries$1717)) {
      moonbit_panic();
    }
    _tmp$2482
    = (struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$*)entries$1717[
        idx$553
      ];
    _bind$555 = _tmp$2482;
    if (_bind$555 == 0) {
      entry$554->$2 = psl$552;
      $$moonbitlang$core$builtin$Map$$set_entry$4(
        self$556, entry$554, idx$553
      );
      break;
    } else {
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _Some$558 =
        _bind$555;
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _curr_entry$559 =
        _Some$558;
      int32_t psl$1707 = _curr_entry$559->$2;
      if (psl$552 > psl$1707) {
        int32_t psl$1712;
        int32_t _tmp$1708;
        int32_t _tmp$1710;
        int32_t capacity_mask$1711;
        int32_t _tmp$1709;
        entry$554->$2 = psl$552;
        moonbit_incref(_curr_entry$559);
        moonbit_incref(self$556);
        $$moonbitlang$core$builtin$Map$$set_entry$4(
          self$556, entry$554, idx$553
        );
        psl$1712 = _curr_entry$559->$2;
        _tmp$1708 = psl$1712 + 1;
        _tmp$1710 = idx$553 + 1;
        capacity_mask$1711 = self$556->$3;
        _tmp$1709 = _tmp$1710 & capacity_mask$1711;
        psl$552 = _tmp$1708;
        idx$553 = _tmp$1709;
        entry$554 = _curr_entry$559;
        continue;
      } else {
        int32_t _tmp$1713 = psl$552 + 1;
        int32_t _tmp$1715 = idx$553 + 1;
        int32_t capacity_mask$1716 = self$556->$3;
        int32_t _tmp$1714 = _tmp$1715 & capacity_mask$1716;
        struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$2817 =
          entry$554;
        psl$552 = _tmp$1713;
        idx$553 = _tmp$1714;
        entry$554 = _tmp$2817;
        continue;
      }
    }
    break;
  }
  return 0;
}

int32_t $$moonbitlang$core$builtin$Map$$push_away$3(
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$546,
  int32_t idx$551,
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* entry$550
) {
  int32_t psl$1706 = entry$550->$2;
  int32_t _tmp$1702 = psl$1706 + 1;
  int32_t _tmp$1704 = idx$551 + 1;
  int32_t capacity_mask$1705 = self$546->$3;
  int32_t _tmp$1703 = _tmp$1704 & capacity_mask$1705;
  int32_t psl$542 = _tmp$1702;
  int32_t idx$543 = _tmp$1703;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* entry$544 =
    entry$550;
  while (1) {
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** _field$2485 =
      self$546->$0;
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** entries$1701 =
      _field$2485;
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$2484;
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _bind$545;
    if (idx$543 < 0 || idx$543 >= Moonbit_array_length(entries$1701)) {
      moonbit_panic();
    }
    _tmp$2484
    = (struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$*)entries$1701[
        idx$543
      ];
    _bind$545 = _tmp$2484;
    if (_bind$545 == 0) {
      entry$544->$2 = psl$542;
      $$moonbitlang$core$builtin$Map$$set_entry$3(
        self$546, entry$544, idx$543
      );
      break;
    } else {
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _Some$548 =
        _bind$545;
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _curr_entry$549 =
        _Some$548;
      int32_t psl$1691 = _curr_entry$549->$2;
      if (psl$542 > psl$1691) {
        int32_t psl$1696;
        int32_t _tmp$1692;
        int32_t _tmp$1694;
        int32_t capacity_mask$1695;
        int32_t _tmp$1693;
        entry$544->$2 = psl$542;
        moonbit_incref(_curr_entry$549);
        moonbit_incref(self$546);
        $$moonbitlang$core$builtin$Map$$set_entry$3(
          self$546, entry$544, idx$543
        );
        psl$1696 = _curr_entry$549->$2;
        _tmp$1692 = psl$1696 + 1;
        _tmp$1694 = idx$543 + 1;
        capacity_mask$1695 = self$546->$3;
        _tmp$1693 = _tmp$1694 & capacity_mask$1695;
        psl$542 = _tmp$1692;
        idx$543 = _tmp$1693;
        entry$544 = _curr_entry$549;
        continue;
      } else {
        int32_t _tmp$1697 = psl$542 + 1;
        int32_t _tmp$1699 = idx$543 + 1;
        int32_t capacity_mask$1700 = self$546->$3;
        int32_t _tmp$1698 = _tmp$1699 & capacity_mask$1700;
        struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$2819 =
          entry$544;
        psl$542 = _tmp$1697;
        idx$543 = _tmp$1698;
        entry$544 = _tmp$2819;
        continue;
      }
    }
    break;
  }
  return 0;
}

int32_t $$moonbitlang$core$builtin$Map$$push_away$2(
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$536,
  int32_t idx$541,
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* entry$540
) {
  int32_t psl$1690 = entry$540->$2;
  int32_t _tmp$1686 = psl$1690 + 1;
  int32_t _tmp$1688 = idx$541 + 1;
  int32_t capacity_mask$1689 = self$536->$3;
  int32_t _tmp$1687 = _tmp$1688 & capacity_mask$1689;
  int32_t psl$532 = _tmp$1686;
  int32_t idx$533 = _tmp$1687;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* entry$534 =
    entry$540;
  while (1) {
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** _field$2487 =
      self$536->$0;
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** entries$1685 =
      _field$2487;
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$2486;
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _bind$535;
    if (idx$533 < 0 || idx$533 >= Moonbit_array_length(entries$1685)) {
      moonbit_panic();
    }
    _tmp$2486
    = (struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$*)entries$1685[
        idx$533
      ];
    _bind$535 = _tmp$2486;
    if (_bind$535 == 0) {
      entry$534->$2 = psl$532;
      $$moonbitlang$core$builtin$Map$$set_entry$2(
        self$536, entry$534, idx$533
      );
      break;
    } else {
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _Some$538 =
        _bind$535;
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _curr_entry$539 =
        _Some$538;
      int32_t psl$1675 = _curr_entry$539->$2;
      if (psl$532 > psl$1675) {
        int32_t psl$1680;
        int32_t _tmp$1676;
        int32_t _tmp$1678;
        int32_t capacity_mask$1679;
        int32_t _tmp$1677;
        entry$534->$2 = psl$532;
        moonbit_incref(_curr_entry$539);
        moonbit_incref(self$536);
        $$moonbitlang$core$builtin$Map$$set_entry$2(
          self$536, entry$534, idx$533
        );
        psl$1680 = _curr_entry$539->$2;
        _tmp$1676 = psl$1680 + 1;
        _tmp$1678 = idx$533 + 1;
        capacity_mask$1679 = self$536->$3;
        _tmp$1677 = _tmp$1678 & capacity_mask$1679;
        psl$532 = _tmp$1676;
        idx$533 = _tmp$1677;
        entry$534 = _curr_entry$539;
        continue;
      } else {
        int32_t _tmp$1681 = psl$532 + 1;
        int32_t _tmp$1683 = idx$533 + 1;
        int32_t capacity_mask$1684 = self$536->$3;
        int32_t _tmp$1682 = _tmp$1683 & capacity_mask$1684;
        struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$2821 =
          entry$534;
        psl$532 = _tmp$1681;
        idx$533 = _tmp$1682;
        entry$534 = _tmp$2821;
        continue;
      }
    }
    break;
  }
  return 0;
}

int32_t $$moonbitlang$core$builtin$Map$$push_away$1(
  struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* self$526,
  int32_t idx$531,
  struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* entry$530
) {
  int32_t psl$1674 = entry$530->$2;
  int32_t _tmp$1670 = psl$1674 + 1;
  int32_t _tmp$1672 = idx$531 + 1;
  int32_t capacity_mask$1673 = self$526->$3;
  int32_t _tmp$1671 = _tmp$1672 & capacity_mask$1673;
  int32_t psl$522 = _tmp$1670;
  int32_t idx$523 = _tmp$1671;
  struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* entry$524 =
    entry$530;
  while (1) {
    struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$** _field$2489 =
      self$526->$0;
    struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$** entries$1669 =
      _field$2489;
    struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _tmp$2488;
    struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _bind$525;
    if (idx$523 < 0 || idx$523 >= Moonbit_array_length(entries$1669)) {
      moonbit_panic();
    }
    _tmp$2488
    = (struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$*)entries$1669[
        idx$523
      ];
    _bind$525 = _tmp$2488;
    if (_bind$525 == 0) {
      entry$524->$2 = psl$522;
      $$moonbitlang$core$builtin$Map$$set_entry$1(
        self$526, entry$524, idx$523
      );
      break;
    } else {
      struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _Some$528 =
        _bind$525;
      struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _curr_entry$529 =
        _Some$528;
      int32_t psl$1659 = _curr_entry$529->$2;
      if (psl$522 > psl$1659) {
        int32_t psl$1664;
        int32_t _tmp$1660;
        int32_t _tmp$1662;
        int32_t capacity_mask$1663;
        int32_t _tmp$1661;
        entry$524->$2 = psl$522;
        moonbit_incref(_curr_entry$529);
        moonbit_incref(self$526);
        $$moonbitlang$core$builtin$Map$$set_entry$1(
          self$526, entry$524, idx$523
        );
        psl$1664 = _curr_entry$529->$2;
        _tmp$1660 = psl$1664 + 1;
        _tmp$1662 = idx$523 + 1;
        capacity_mask$1663 = self$526->$3;
        _tmp$1661 = _tmp$1662 & capacity_mask$1663;
        psl$522 = _tmp$1660;
        idx$523 = _tmp$1661;
        entry$524 = _curr_entry$529;
        continue;
      } else {
        int32_t _tmp$1665 = psl$522 + 1;
        int32_t _tmp$1667 = idx$523 + 1;
        int32_t capacity_mask$1668 = self$526->$3;
        int32_t _tmp$1666 = _tmp$1667 & capacity_mask$1668;
        struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _tmp$2823 =
          entry$524;
        psl$522 = _tmp$1665;
        idx$523 = _tmp$1666;
        entry$524 = _tmp$2823;
        continue;
      }
    }
    break;
  }
  return 0;
}

int32_t $$moonbitlang$core$builtin$Map$$push_away$0(
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$516,
  int32_t idx$521,
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* entry$520
) {
  int32_t psl$1658 = entry$520->$2;
  int32_t _tmp$1654 = psl$1658 + 1;
  int32_t _tmp$1656 = idx$521 + 1;
  int32_t capacity_mask$1657 = self$516->$3;
  int32_t _tmp$1655 = _tmp$1656 & capacity_mask$1657;
  int32_t psl$512 = _tmp$1654;
  int32_t idx$513 = _tmp$1655;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* entry$514 =
    entry$520;
  while (1) {
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** _field$2491 =
      self$516->$0;
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** entries$1653 =
      _field$2491;
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$2490;
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _bind$515;
    if (idx$513 < 0 || idx$513 >= Moonbit_array_length(entries$1653)) {
      moonbit_panic();
    }
    _tmp$2490
    = (struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$*)entries$1653[
        idx$513
      ];
    _bind$515 = _tmp$2490;
    if (_bind$515 == 0) {
      entry$514->$2 = psl$512;
      $$moonbitlang$core$builtin$Map$$set_entry$0(
        self$516, entry$514, idx$513
      );
      break;
    } else {
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _Some$518 =
        _bind$515;
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _curr_entry$519 =
        _Some$518;
      int32_t psl$1643 = _curr_entry$519->$2;
      if (psl$512 > psl$1643) {
        int32_t psl$1648;
        int32_t _tmp$1644;
        int32_t _tmp$1646;
        int32_t capacity_mask$1647;
        int32_t _tmp$1645;
        entry$514->$2 = psl$512;
        moonbit_incref(_curr_entry$519);
        moonbit_incref(self$516);
        $$moonbitlang$core$builtin$Map$$set_entry$0(
          self$516, entry$514, idx$513
        );
        psl$1648 = _curr_entry$519->$2;
        _tmp$1644 = psl$1648 + 1;
        _tmp$1646 = idx$513 + 1;
        capacity_mask$1647 = self$516->$3;
        _tmp$1645 = _tmp$1646 & capacity_mask$1647;
        psl$512 = _tmp$1644;
        idx$513 = _tmp$1645;
        entry$514 = _curr_entry$519;
        continue;
      } else {
        int32_t _tmp$1649 = psl$512 + 1;
        int32_t _tmp$1651 = idx$513 + 1;
        int32_t capacity_mask$1652 = self$516->$3;
        int32_t _tmp$1650 = _tmp$1651 & capacity_mask$1652;
        struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$2825 =
          entry$514;
        psl$512 = _tmp$1649;
        idx$513 = _tmp$1650;
        entry$514 = _tmp$2825;
        continue;
      }
    }
    break;
  }
  return 0;
}

int32_t $$moonbitlang$core$builtin$Map$$set_entry$4(
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$506,
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* entry$508,
  int32_t new_idx$507
) {
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** _field$2494 =
    self$506->$0;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** entries$1641 =
    _field$2494;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$1642;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _old$2493;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _field$2492;
  int32_t _cnt$2696;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _bind$509;
  moonbit_incref(entry$508);
  _tmp$1642 = entry$508;
  if (new_idx$507 < 0 || new_idx$507 >= Moonbit_array_length(entries$1641)) {
    moonbit_panic();
  }
  _old$2493
  = (struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$*)entries$1641[
      new_idx$507
    ];
  if (_old$2493) {
    moonbit_decref(_old$2493);
  }
  entries$1641[new_idx$507] = _tmp$1642;
  _field$2492 = entry$508->$1;
  _cnt$2696 = Moonbit_object_header(entry$508)->rc;
  if (_cnt$2696 > 1) {
    int32_t _new_cnt$2699 = _cnt$2696 - 1;
    Moonbit_object_header(entry$508)->rc = _new_cnt$2699;
    if (_field$2492) {
      moonbit_incref(_field$2492);
    }
  } else if (_cnt$2696 == 1) {
    struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _field$2698 =
      entry$508->$5;
    moonbit_string_t _field$2697;
    moonbit_decref(_field$2698);
    _field$2697 = entry$508->$4;
    moonbit_decref(_field$2697);
    moonbit_free(entry$508);
  }
  _bind$509 = _field$2492;
  if (_bind$509 == 0) {
    if (_bind$509) {
      moonbit_decref(_bind$509);
    }
    self$506->$6 = new_idx$507;
    moonbit_decref(self$506);
  } else {
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _Some$510;
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _next$511;
    moonbit_decref(self$506);
    _Some$510 = _bind$509;
    _next$511 = _Some$510;
    _next$511->$0 = new_idx$507;
    moonbit_decref(_next$511);
  }
  return 0;
}

int32_t $$moonbitlang$core$builtin$Map$$set_entry$3(
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$500,
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* entry$502,
  int32_t new_idx$501
) {
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** _field$2497 =
    self$500->$0;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** entries$1639 =
    _field$2497;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$1640;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _old$2496;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _field$2495;
  int32_t _cnt$2700;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _bind$503;
  moonbit_incref(entry$502);
  _tmp$1640 = entry$502;
  if (new_idx$501 < 0 || new_idx$501 >= Moonbit_array_length(entries$1639)) {
    moonbit_panic();
  }
  _old$2496
  = (struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$*)entries$1639[
      new_idx$501
    ];
  if (_old$2496) {
    moonbit_decref(_old$2496);
  }
  entries$1639[new_idx$501] = _tmp$1640;
  _field$2495 = entry$502->$1;
  _cnt$2700 = Moonbit_object_header(entry$502)->rc;
  if (_cnt$2700 > 1) {
    int32_t _new_cnt$2703 = _cnt$2700 - 1;
    Moonbit_object_header(entry$502)->rc = _new_cnt$2703;
    if (_field$2495) {
      moonbit_incref(_field$2495);
    }
  } else if (_cnt$2700 == 1) {
    struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _field$2702 =
      entry$502->$5;
    moonbit_string_t _field$2701;
    moonbit_decref(_field$2702);
    _field$2701 = entry$502->$4;
    moonbit_decref(_field$2701);
    moonbit_free(entry$502);
  }
  _bind$503 = _field$2495;
  if (_bind$503 == 0) {
    if (_bind$503) {
      moonbit_decref(_bind$503);
    }
    self$500->$6 = new_idx$501;
    moonbit_decref(self$500);
  } else {
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _Some$504;
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _next$505;
    moonbit_decref(self$500);
    _Some$504 = _bind$503;
    _next$505 = _Some$504;
    _next$505->$0 = new_idx$501;
    moonbit_decref(_next$505);
  }
  return 0;
}

int32_t $$moonbitlang$core$builtin$Map$$set_entry$2(
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$494,
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* entry$496,
  int32_t new_idx$495
) {
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** _field$2500 =
    self$494->$0;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** entries$1637 =
    _field$2500;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$1638;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _old$2499;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _field$2498;
  int32_t _cnt$2704;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _bind$497;
  moonbit_incref(entry$496);
  _tmp$1638 = entry$496;
  if (new_idx$495 < 0 || new_idx$495 >= Moonbit_array_length(entries$1637)) {
    moonbit_panic();
  }
  _old$2499
  = (struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$*)entries$1637[
      new_idx$495
    ];
  if (_old$2499) {
    moonbit_decref(_old$2499);
  }
  entries$1637[new_idx$495] = _tmp$1638;
  _field$2498 = entry$496->$1;
  _cnt$2704 = Moonbit_object_header(entry$496)->rc;
  if (_cnt$2704 > 1) {
    int32_t _new_cnt$2707 = _cnt$2704 - 1;
    Moonbit_object_header(entry$496)->rc = _new_cnt$2707;
    if (_field$2498) {
      moonbit_incref(_field$2498);
    }
  } else if (_cnt$2704 == 1) {
    struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _field$2706 =
      entry$496->$5;
    moonbit_string_t _field$2705;
    moonbit_decref(_field$2706);
    _field$2705 = entry$496->$4;
    moonbit_decref(_field$2705);
    moonbit_free(entry$496);
  }
  _bind$497 = _field$2498;
  if (_bind$497 == 0) {
    if (_bind$497) {
      moonbit_decref(_bind$497);
    }
    self$494->$6 = new_idx$495;
    moonbit_decref(self$494);
  } else {
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _Some$498;
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _next$499;
    moonbit_decref(self$494);
    _Some$498 = _bind$497;
    _next$499 = _Some$498;
    _next$499->$0 = new_idx$495;
    moonbit_decref(_next$499);
  }
  return 0;
}

int32_t $$moonbitlang$core$builtin$Map$$set_entry$1(
  struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* self$488,
  struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* entry$490,
  int32_t new_idx$489
) {
  struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$** _field$2503 =
    self$488->$0;
  struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$** entries$1635 =
    _field$2503;
  struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _tmp$1636;
  struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _old$2502;
  struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _field$2501;
  int32_t _cnt$2708;
  struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _bind$491;
  moonbit_incref(entry$490);
  _tmp$1636 = entry$490;
  if (new_idx$489 < 0 || new_idx$489 >= Moonbit_array_length(entries$1635)) {
    moonbit_panic();
  }
  _old$2502
  = (struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$*)entries$1635[
      new_idx$489
    ];
  if (_old$2502) {
    moonbit_decref(_old$2502);
  }
  entries$1635[new_idx$489] = _tmp$1636;
  _field$2501 = entry$490->$1;
  _cnt$2708 = Moonbit_object_header(entry$490)->rc;
  if (_cnt$2708 > 1) {
    int32_t _new_cnt$2710 = _cnt$2708 - 1;
    Moonbit_object_header(entry$490)->rc = _new_cnt$2710;
    if (_field$2501) {
      moonbit_incref(_field$2501);
    }
  } else if (_cnt$2708 == 1) {
    struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* _field$2709 =
      entry$490->$5;
    moonbit_decref(_field$2709);
    moonbit_free(entry$490);
  }
  _bind$491 = _field$2501;
  if (_bind$491 == 0) {
    if (_bind$491) {
      moonbit_decref(_bind$491);
    }
    self$488->$6 = new_idx$489;
    moonbit_decref(self$488);
  } else {
    struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _Some$492;
    struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _next$493;
    moonbit_decref(self$488);
    _Some$492 = _bind$491;
    _next$493 = _Some$492;
    _next$493->$0 = new_idx$489;
    moonbit_decref(_next$493);
  }
  return 0;
}

int32_t $$moonbitlang$core$builtin$Map$$set_entry$0(
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$482,
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* entry$484,
  int32_t new_idx$483
) {
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** _field$2506 =
    self$482->$0;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** entries$1633 =
    _field$2506;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$1634;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _old$2505;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _field$2504;
  int32_t _cnt$2711;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _bind$485;
  moonbit_incref(entry$484);
  _tmp$1634 = entry$484;
  if (new_idx$483 < 0 || new_idx$483 >= Moonbit_array_length(entries$1633)) {
    moonbit_panic();
  }
  _old$2505
  = (struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$*)entries$1633[
      new_idx$483
    ];
  if (_old$2505) {
    moonbit_decref(_old$2505);
  }
  entries$1633[new_idx$483] = _tmp$1634;
  _field$2504 = entry$484->$1;
  _cnt$2711 = Moonbit_object_header(entry$484)->rc;
  if (_cnt$2711 > 1) {
    int32_t _new_cnt$2714 = _cnt$2711 - 1;
    Moonbit_object_header(entry$484)->rc = _new_cnt$2714;
    if (_field$2504) {
      moonbit_incref(_field$2504);
    }
  } else if (_cnt$2711 == 1) {
    struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _field$2713 =
      entry$484->$5;
    moonbit_string_t _field$2712;
    moonbit_decref(_field$2713);
    _field$2712 = entry$484->$4;
    moonbit_decref(_field$2712);
    moonbit_free(entry$484);
  }
  _bind$485 = _field$2504;
  if (_bind$485 == 0) {
    if (_bind$485) {
      moonbit_decref(_bind$485);
    }
    self$482->$6 = new_idx$483;
    moonbit_decref(self$482);
  } else {
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _Some$486;
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _next$487;
    moonbit_decref(self$482);
    _Some$486 = _bind$485;
    _next$487 = _Some$486;
    _next$487->$0 = new_idx$483;
    moonbit_decref(_next$487);
  }
  return 0;
}

int32_t $$moonbitlang$core$builtin$Map$$add_entry_to_tail$4(
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$479,
  int32_t idx$481,
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* entry$480
) {
  int32_t _bind$478 = self$479->$6;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** _field$2508;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** entries$1629;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$1630;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _old$2507;
  int32_t size$1632;
  int32_t _tmp$1631;
  switch (_bind$478) {
    case -1: {
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$1624;
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _old$2509;
      moonbit_incref(entry$480);
      _tmp$1624 = entry$480;
      _old$2509 = self$479->$5;
      if (_old$2509) {
        moonbit_decref(_old$2509);
      }
      self$479->$5 = _tmp$1624;
      break;
    }
    default: {
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** _field$2512 =
        self$479->$0;
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** entries$1628 =
        _field$2512;
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$2511;
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$1627;
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$1625;
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$1626;
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _old$2510;
      if (_bind$478 < 0 || _bind$478 >= Moonbit_array_length(entries$1628)) {
        moonbit_panic();
      }
      _tmp$2511
      = (struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$*)entries$1628[
          _bind$478
        ];
      _tmp$1627 = _tmp$2511;
      if (_tmp$1627) {
        moonbit_incref(_tmp$1627);
      }
      _tmp$1625 = $Option$$unwrap$4(_tmp$1627);
      moonbit_incref(entry$480);
      _tmp$1626 = entry$480;
      _old$2510 = _tmp$1625->$1;
      if (_old$2510) {
        moonbit_decref(_old$2510);
      }
      _tmp$1625->$1 = _tmp$1626;
      moonbit_decref(_tmp$1625);
      break;
    }
  }
  self$479->$6 = idx$481;
  _field$2508 = self$479->$0;
  entries$1629 = _field$2508;
  _tmp$1630 = entry$480;
  if (idx$481 < 0 || idx$481 >= Moonbit_array_length(entries$1629)) {
    moonbit_panic();
  }
  _old$2507
  = (struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$*)entries$1629[
      idx$481
    ];
  if (_old$2507) {
    moonbit_decref(_old$2507);
  }
  entries$1629[idx$481] = _tmp$1630;
  size$1632 = self$479->$1;
  _tmp$1631 = size$1632 + 1;
  self$479->$1 = _tmp$1631;
  moonbit_decref(self$479);
  return 0;
}

int32_t $$moonbitlang$core$builtin$Map$$add_entry_to_tail$3(
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$475,
  int32_t idx$477,
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* entry$476
) {
  int32_t _bind$474 = self$475->$6;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** _field$2514;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** entries$1620;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$1621;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _old$2513;
  int32_t size$1623;
  int32_t _tmp$1622;
  switch (_bind$474) {
    case -1: {
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$1615;
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _old$2515;
      moonbit_incref(entry$476);
      _tmp$1615 = entry$476;
      _old$2515 = self$475->$5;
      if (_old$2515) {
        moonbit_decref(_old$2515);
      }
      self$475->$5 = _tmp$1615;
      break;
    }
    default: {
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** _field$2518 =
        self$475->$0;
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** entries$1619 =
        _field$2518;
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$2517;
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$1618;
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$1616;
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$1617;
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _old$2516;
      if (_bind$474 < 0 || _bind$474 >= Moonbit_array_length(entries$1619)) {
        moonbit_panic();
      }
      _tmp$2517
      = (struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$*)entries$1619[
          _bind$474
        ];
      _tmp$1618 = _tmp$2517;
      if (_tmp$1618) {
        moonbit_incref(_tmp$1618);
      }
      _tmp$1616 = $Option$$unwrap$3(_tmp$1618);
      moonbit_incref(entry$476);
      _tmp$1617 = entry$476;
      _old$2516 = _tmp$1616->$1;
      if (_old$2516) {
        moonbit_decref(_old$2516);
      }
      _tmp$1616->$1 = _tmp$1617;
      moonbit_decref(_tmp$1616);
      break;
    }
  }
  self$475->$6 = idx$477;
  _field$2514 = self$475->$0;
  entries$1620 = _field$2514;
  _tmp$1621 = entry$476;
  if (idx$477 < 0 || idx$477 >= Moonbit_array_length(entries$1620)) {
    moonbit_panic();
  }
  _old$2513
  = (struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$*)entries$1620[
      idx$477
    ];
  if (_old$2513) {
    moonbit_decref(_old$2513);
  }
  entries$1620[idx$477] = _tmp$1621;
  size$1623 = self$475->$1;
  _tmp$1622 = size$1623 + 1;
  self$475->$1 = _tmp$1622;
  moonbit_decref(self$475);
  return 0;
}

int32_t $$moonbitlang$core$builtin$Map$$add_entry_to_tail$2(
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$471,
  int32_t idx$473,
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* entry$472
) {
  int32_t _bind$470 = self$471->$6;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** _field$2520;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** entries$1611;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$1612;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _old$2519;
  int32_t size$1614;
  int32_t _tmp$1613;
  switch (_bind$470) {
    case -1: {
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$1606;
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _old$2521;
      moonbit_incref(entry$472);
      _tmp$1606 = entry$472;
      _old$2521 = self$471->$5;
      if (_old$2521) {
        moonbit_decref(_old$2521);
      }
      self$471->$5 = _tmp$1606;
      break;
    }
    default: {
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** _field$2524 =
        self$471->$0;
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** entries$1610 =
        _field$2524;
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$2523;
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$1609;
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$1607;
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$1608;
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _old$2522;
      if (_bind$470 < 0 || _bind$470 >= Moonbit_array_length(entries$1610)) {
        moonbit_panic();
      }
      _tmp$2523
      = (struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$*)entries$1610[
          _bind$470
        ];
      _tmp$1609 = _tmp$2523;
      if (_tmp$1609) {
        moonbit_incref(_tmp$1609);
      }
      _tmp$1607 = $Option$$unwrap$2(_tmp$1609);
      moonbit_incref(entry$472);
      _tmp$1608 = entry$472;
      _old$2522 = _tmp$1607->$1;
      if (_old$2522) {
        moonbit_decref(_old$2522);
      }
      _tmp$1607->$1 = _tmp$1608;
      moonbit_decref(_tmp$1607);
      break;
    }
  }
  self$471->$6 = idx$473;
  _field$2520 = self$471->$0;
  entries$1611 = _field$2520;
  _tmp$1612 = entry$472;
  if (idx$473 < 0 || idx$473 >= Moonbit_array_length(entries$1611)) {
    moonbit_panic();
  }
  _old$2519
  = (struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$*)entries$1611[
      idx$473
    ];
  if (_old$2519) {
    moonbit_decref(_old$2519);
  }
  entries$1611[idx$473] = _tmp$1612;
  size$1614 = self$471->$1;
  _tmp$1613 = size$1614 + 1;
  self$471->$1 = _tmp$1613;
  moonbit_decref(self$471);
  return 0;
}

int32_t $$moonbitlang$core$builtin$Map$$add_entry_to_tail$1(
  struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* self$467,
  int32_t idx$469,
  struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* entry$468
) {
  int32_t _bind$466 = self$467->$6;
  struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$** _field$2526;
  struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$** entries$1602;
  struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _tmp$1603;
  struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _old$2525;
  int32_t size$1605;
  int32_t _tmp$1604;
  switch (_bind$466) {
    case -1: {
      struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _tmp$1597;
      struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _old$2527;
      moonbit_incref(entry$468);
      _tmp$1597 = entry$468;
      _old$2527 = self$467->$5;
      if (_old$2527) {
        moonbit_decref(_old$2527);
      }
      self$467->$5 = _tmp$1597;
      break;
    }
    default: {
      struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$** _field$2530 =
        self$467->$0;
      struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$** entries$1601 =
        _field$2530;
      struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _tmp$2529;
      struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _tmp$1600;
      struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _tmp$1598;
      struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _tmp$1599;
      struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _old$2528;
      if (_bind$466 < 0 || _bind$466 >= Moonbit_array_length(entries$1601)) {
        moonbit_panic();
      }
      _tmp$2529
      = (struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$*)entries$1601[
          _bind$466
        ];
      _tmp$1600 = _tmp$2529;
      if (_tmp$1600) {
        moonbit_incref(_tmp$1600);
      }
      _tmp$1598 = $Option$$unwrap$1(_tmp$1600);
      moonbit_incref(entry$468);
      _tmp$1599 = entry$468;
      _old$2528 = _tmp$1598->$1;
      if (_old$2528) {
        moonbit_decref(_old$2528);
      }
      _tmp$1598->$1 = _tmp$1599;
      moonbit_decref(_tmp$1598);
      break;
    }
  }
  self$467->$6 = idx$469;
  _field$2526 = self$467->$0;
  entries$1602 = _field$2526;
  _tmp$1603 = entry$468;
  if (idx$469 < 0 || idx$469 >= Moonbit_array_length(entries$1602)) {
    moonbit_panic();
  }
  _old$2525
  = (struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$*)entries$1602[
      idx$469
    ];
  if (_old$2525) {
    moonbit_decref(_old$2525);
  }
  entries$1602[idx$469] = _tmp$1603;
  size$1605 = self$467->$1;
  _tmp$1604 = size$1605 + 1;
  self$467->$1 = _tmp$1604;
  moonbit_decref(self$467);
  return 0;
}

int32_t $$moonbitlang$core$builtin$Map$$add_entry_to_tail$0(
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$463,
  int32_t idx$465,
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* entry$464
) {
  int32_t _bind$462 = self$463->$6;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** _field$2532;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** entries$1593;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$1594;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _old$2531;
  int32_t size$1596;
  int32_t _tmp$1595;
  switch (_bind$462) {
    case -1: {
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$1588;
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _old$2533;
      moonbit_incref(entry$464);
      _tmp$1588 = entry$464;
      _old$2533 = self$463->$5;
      if (_old$2533) {
        moonbit_decref(_old$2533);
      }
      self$463->$5 = _tmp$1588;
      break;
    }
    default: {
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** _field$2536 =
        self$463->$0;
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** entries$1592 =
        _field$2536;
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$2535;
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$1591;
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$1589;
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$1590;
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _old$2534;
      if (_bind$462 < 0 || _bind$462 >= Moonbit_array_length(entries$1592)) {
        moonbit_panic();
      }
      _tmp$2535
      = (struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$*)entries$1592[
          _bind$462
        ];
      _tmp$1591 = _tmp$2535;
      if (_tmp$1591) {
        moonbit_incref(_tmp$1591);
      }
      _tmp$1589 = $Option$$unwrap$0(_tmp$1591);
      moonbit_incref(entry$464);
      _tmp$1590 = entry$464;
      _old$2534 = _tmp$1589->$1;
      if (_old$2534) {
        moonbit_decref(_old$2534);
      }
      _tmp$1589->$1 = _tmp$1590;
      moonbit_decref(_tmp$1589);
      break;
    }
  }
  self$463->$6 = idx$465;
  _field$2532 = self$463->$0;
  entries$1593 = _field$2532;
  _tmp$1594 = entry$464;
  if (idx$465 < 0 || idx$465 >= Moonbit_array_length(entries$1593)) {
    moonbit_panic();
  }
  _old$2531
  = (struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$*)entries$1593[
      idx$465
    ];
  if (_old$2531) {
    moonbit_decref(_old$2531);
  }
  entries$1593[idx$465] = _tmp$1594;
  size$1596 = self$463->$1;
  _tmp$1595 = size$1596 + 1;
  self$463->$1 = _tmp$1595;
  moonbit_decref(self$463);
  return 0;
}

struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* $$moonbitlang$core$builtin$Map$$new$inner$4(
  int32_t capacity$457
) {
  int32_t capacity$456 = $Int$$next_power_of_two(capacity$457);
  int32_t _bind$458 = capacity$456 - 1;
  int32_t _bind$459 =
    $moonbitlang$core$builtin$calc_grow_threshold(capacity$456);
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$1587 =
    0;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** _bind$460 =
    (struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$**)moonbit_make_ref_array(
      capacity$456, _tmp$1587
    );
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _bind$461 =
    0;
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _block$2826 =
    (struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$*)moonbit_malloc(
      sizeof(
        struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$
      )
    );
  Moonbit_object_header(_block$2826)->meta
  = Moonbit_make_regular_object_header(
    offsetof(
      struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$,
        $0
    )
    >> 2,
      2,
      0
  );
  _block$2826->$0 = _bind$460;
  _block$2826->$1 = 0;
  _block$2826->$2 = capacity$456;
  _block$2826->$3 = _bind$458;
  _block$2826->$4 = _bind$459;
  _block$2826->$5 = _bind$461;
  _block$2826->$6 = -1;
  return _block$2826;
}

struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* $$moonbitlang$core$builtin$Map$$new$inner$3(
  int32_t capacity$451
) {
  int32_t capacity$450 = $Int$$next_power_of_two(capacity$451);
  int32_t _bind$452 = capacity$450 - 1;
  int32_t _bind$453 =
    $moonbitlang$core$builtin$calc_grow_threshold(capacity$450);
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$1586 =
    0;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** _bind$454 =
    (struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$**)moonbit_make_ref_array(
      capacity$450, _tmp$1586
    );
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _bind$455 =
    0;
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _block$2827 =
    (struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$*)moonbit_malloc(
      sizeof(
        struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$
      )
    );
  Moonbit_object_header(_block$2827)->meta
  = Moonbit_make_regular_object_header(
    offsetof(
      struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$,
        $0
    )
    >> 2,
      2,
      0
  );
  _block$2827->$0 = _bind$454;
  _block$2827->$1 = 0;
  _block$2827->$2 = capacity$450;
  _block$2827->$3 = _bind$452;
  _block$2827->$4 = _bind$453;
  _block$2827->$5 = _bind$455;
  _block$2827->$6 = -1;
  return _block$2827;
}

struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* $$moonbitlang$core$builtin$Map$$new$inner$2(
  int32_t capacity$445
) {
  int32_t capacity$444 = $Int$$next_power_of_two(capacity$445);
  int32_t _bind$446 = capacity$444 - 1;
  int32_t _bind$447 =
    $moonbitlang$core$builtin$calc_grow_threshold(capacity$444);
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$1585 =
    0;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** _bind$448 =
    (struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$**)moonbit_make_ref_array(
      capacity$444, _tmp$1585
    );
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _bind$449 =
    0;
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _block$2828 =
    (struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$*)moonbit_malloc(
      sizeof(
        struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$
      )
    );
  Moonbit_object_header(_block$2828)->meta
  = Moonbit_make_regular_object_header(
    offsetof(
      struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$,
        $0
    )
    >> 2,
      2,
      0
  );
  _block$2828->$0 = _bind$448;
  _block$2828->$1 = 0;
  _block$2828->$2 = capacity$444;
  _block$2828->$3 = _bind$446;
  _block$2828->$4 = _bind$447;
  _block$2828->$5 = _bind$449;
  _block$2828->$6 = -1;
  return _block$2828;
}

struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* $$moonbitlang$core$builtin$Map$$new$inner$1(
  int32_t capacity$439
) {
  int32_t capacity$438 = $Int$$next_power_of_two(capacity$439);
  int32_t _bind$440 = capacity$438 - 1;
  int32_t _bind$441 =
    $moonbitlang$core$builtin$calc_grow_threshold(capacity$438);
  struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _tmp$1584 =
    0;
  struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$** _bind$442 =
    (struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$**)moonbit_make_ref_array(
      capacity$438, _tmp$1584
    );
  struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _bind$443 =
    0;
  struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _block$2829 =
    (struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$*)moonbit_malloc(
      sizeof(
        struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$
      )
    );
  Moonbit_object_header(_block$2829)->meta
  = Moonbit_make_regular_object_header(
    offsetof(
      struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$,
        $0
    )
    >> 2,
      2,
      0
  );
  _block$2829->$0 = _bind$442;
  _block$2829->$1 = 0;
  _block$2829->$2 = capacity$438;
  _block$2829->$3 = _bind$440;
  _block$2829->$4 = _bind$441;
  _block$2829->$5 = _bind$443;
  _block$2829->$6 = -1;
  return _block$2829;
}

struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* $$moonbitlang$core$builtin$Map$$new$inner$0(
  int32_t capacity$433
) {
  int32_t capacity$432 = $Int$$next_power_of_two(capacity$433);
  int32_t _bind$434 = capacity$432 - 1;
  int32_t _bind$435 =
    $moonbitlang$core$builtin$calc_grow_threshold(capacity$432);
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$1583 =
    0;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** _bind$436 =
    (struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$**)moonbit_make_ref_array(
      capacity$432, _tmp$1583
    );
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _bind$437 =
    0;
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _block$2830 =
    (struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$*)moonbit_malloc(
      sizeof(
        struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$
      )
    );
  Moonbit_object_header(_block$2830)->meta
  = Moonbit_make_regular_object_header(
    offsetof(
      struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$,
        $0
    )
    >> 2,
      2,
      0
  );
  _block$2830->$0 = _bind$436;
  _block$2830->$1 = 0;
  _block$2830->$2 = capacity$432;
  _block$2830->$3 = _bind$434;
  _block$2830->$4 = _bind$435;
  _block$2830->$5 = _bind$437;
  _block$2830->$6 = -1;
  return _block$2830;
}

int32_t $Int$$next_power_of_two(int32_t self$431) {
  if (self$431 >= 0) {
    int32_t _tmp$1582;
    int32_t _tmp$1581;
    int32_t _tmp$1580;
    int32_t _tmp$1579;
    if (self$431 <= 1) {
      return 1;
    }
    if (self$431 > 1073741824) {
      return 1073741824;
    }
    _tmp$1582 = self$431 - 1;
    _tmp$1581 = moonbit_clz32(_tmp$1582);
    _tmp$1580 = _tmp$1581 - 1;
    _tmp$1579 = 2147483647 >> (_tmp$1580 & 31);
    return _tmp$1579 + 1;
  } else {
    moonbit_panic();
  }
}

int32_t $moonbitlang$core$builtin$calc_grow_threshold(int32_t capacity$430) {
  int32_t _tmp$1578 = capacity$430 * 13;
  return _tmp$1578 / 16;
}

struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* $Option$$unwrap$4(
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$428
) {
  if (self$428 == 0) {
    if (self$428) {
      moonbit_decref(self$428);
    }
    moonbit_panic();
  } else {
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _Some$429 =
      self$428;
    return _Some$429;
  }
}

struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* $Option$$unwrap$3(
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$426
) {
  if (self$426 == 0) {
    if (self$426) {
      moonbit_decref(self$426);
    }
    moonbit_panic();
  } else {
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _Some$427 =
      self$426;
    return _Some$427;
  }
}

struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* $Option$$unwrap$2(
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$424
) {
  if (self$424 == 0) {
    if (self$424) {
      moonbit_decref(self$424);
    }
    moonbit_panic();
  } else {
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _Some$425 =
      self$424;
    return _Some$425;
  }
}

struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* $Option$$unwrap$1(
  struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* self$422
) {
  if (self$422 == 0) {
    if (self$422) {
      moonbit_decref(self$422);
    }
    moonbit_panic();
  } else {
    struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _Some$423 =
      self$422;
    return _Some$423;
  }
}

struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* $Option$$unwrap$0(
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$420
) {
  if (self$420 == 0) {
    if (self$420) {
      moonbit_decref(self$420);
    }
    moonbit_panic();
  } else {
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _Some$421 =
      self$420;
    return _Some$421;
  }
}

moonbit_string_t $$moonbitlang$core$builtin$Show$$String$$to_string(
  moonbit_string_t self$419
) {
  return self$419;
}

int32_t $$moonbitlang$core$builtin$Show$$Int$$output(
  int32_t self$418,
  struct $$moonbitlang$core$builtin$Logger logger$417
) {
  moonbit_string_t _tmp$1577 = $Int$$to_string$inner(self$418, 10);
  logger$417.$0->$method_0(logger$417.$1, _tmp$1577);
  return 0;
}

int32_t $$moonbitlang$core$builtin$Array$$push$2(
  struct $$moonbitlang$core$builtin$Array$3c$async$3c$$3e$$3d$$3e$Unit$21$Error$3e$* self$414,
  struct $$3c$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit* value$416
) {
  int32_t len$1572 = self$414->$1;
  struct $$3c$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit** _tmp$1574;
  int32_t _tmp$2539;
  int32_t _tmp$1573;
  int32_t length$415;
  struct $$3c$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit** _field$2538;
  struct $$3c$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit** buf$1575;
  struct $$3c$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit* _old$2537;
  int32_t _tmp$1576;
  moonbit_incref(self$414);
  _tmp$1574 = $$moonbitlang$core$builtin$Array$$buffer$2(self$414);
  _tmp$2539 = Moonbit_array_length(_tmp$1574);
  moonbit_decref(_tmp$1574);
  _tmp$1573 = _tmp$2539;
  if (len$1572 == _tmp$1573) {
    moonbit_incref(self$414);
    $$moonbitlang$core$builtin$Array$$realloc$2(self$414);
  }
  length$415 = self$414->$1;
  _field$2538 = self$414->$0;
  buf$1575 = _field$2538;
  _old$2537
  = (struct $$3c$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit*)buf$1575[
      length$415
    ];
  if (_old$2537) {
    moonbit_decref(_old$2537);
  }
  buf$1575[length$415] = value$416;
  _tmp$1576 = length$415 + 1;
  self$414->$1 = _tmp$1576;
  moonbit_decref(self$414);
  return 0;
}

int32_t $$moonbitlang$core$builtin$Array$$push$1(
  struct $$moonbitlang$core$builtin$Array$3c$$3c$String$2a$Int$3e$$3e$* self$411,
  struct $$3c$String$2a$Int$3e$* value$413
) {
  int32_t len$1567 = self$411->$1;
  struct $$3c$String$2a$Int$3e$** _tmp$1569;
  int32_t _tmp$2542;
  int32_t _tmp$1568;
  int32_t length$412;
  struct $$3c$String$2a$Int$3e$** _field$2541;
  struct $$3c$String$2a$Int$3e$** buf$1570;
  struct $$3c$String$2a$Int$3e$* _old$2540;
  int32_t _tmp$1571;
  moonbit_incref(self$411);
  _tmp$1569 = $$moonbitlang$core$builtin$Array$$buffer$0(self$411);
  _tmp$2542 = Moonbit_array_length(_tmp$1569);
  moonbit_decref(_tmp$1569);
  _tmp$1568 = _tmp$2542;
  if (len$1567 == _tmp$1568) {
    moonbit_incref(self$411);
    $$moonbitlang$core$builtin$Array$$realloc$1(self$411);
  }
  length$412 = self$411->$1;
  _field$2541 = self$411->$0;
  buf$1570 = _field$2541;
  _old$2540 = (struct $$3c$String$2a$Int$3e$*)buf$1570[length$412];
  if (_old$2540) {
    moonbit_decref(_old$2540);
  }
  buf$1570[length$412] = value$413;
  _tmp$1571 = length$412 + 1;
  self$411->$1 = _tmp$1571;
  moonbit_decref(self$411);
  return 0;
}

int32_t $$moonbitlang$core$builtin$Array$$push$0(
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* self$408,
  moonbit_string_t value$410
) {
  int32_t len$1562 = self$408->$1;
  moonbit_string_t* _tmp$1564;
  int32_t _tmp$2545;
  int32_t _tmp$1563;
  int32_t length$409;
  moonbit_string_t* _field$2544;
  moonbit_string_t* buf$1565;
  moonbit_string_t _old$2543;
  int32_t _tmp$1566;
  moonbit_incref(self$408);
  _tmp$1564 = $$moonbitlang$core$builtin$Array$$buffer$1(self$408);
  _tmp$2545 = Moonbit_array_length(_tmp$1564);
  moonbit_decref(_tmp$1564);
  _tmp$1563 = _tmp$2545;
  if (len$1562 == _tmp$1563) {
    moonbit_incref(self$408);
    $$moonbitlang$core$builtin$Array$$realloc$0(self$408);
  }
  length$409 = self$408->$1;
  _field$2544 = self$408->$0;
  buf$1565 = _field$2544;
  _old$2543 = (moonbit_string_t)buf$1565[length$409];
  moonbit_decref(_old$2543);
  buf$1565[length$409] = value$410;
  _tmp$1566 = length$409 + 1;
  self$408->$1 = _tmp$1566;
  moonbit_decref(self$408);
  return 0;
}

int32_t $$moonbitlang$core$builtin$Array$$realloc$2(
  struct $$moonbitlang$core$builtin$Array$3c$async$3c$$3e$$3d$$3e$Unit$21$Error$3e$* self$406
) {
  int32_t old_cap$405 = self$406->$1;
  int32_t new_cap$407;
  if (old_cap$405 == 0) {
    new_cap$407 = 8;
  } else {
    new_cap$407 = old_cap$405 * 2;
  }
  $$moonbitlang$core$builtin$Array$$resize_buffer$2(self$406, new_cap$407);
  return 0;
}

int32_t $$moonbitlang$core$builtin$Array$$realloc$1(
  struct $$moonbitlang$core$builtin$Array$3c$$3c$String$2a$Int$3e$$3e$* self$403
) {
  int32_t old_cap$402 = self$403->$1;
  int32_t new_cap$404;
  if (old_cap$402 == 0) {
    new_cap$404 = 8;
  } else {
    new_cap$404 = old_cap$402 * 2;
  }
  $$moonbitlang$core$builtin$Array$$resize_buffer$1(self$403, new_cap$404);
  return 0;
}

int32_t $$moonbitlang$core$builtin$Array$$realloc$0(
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* self$400
) {
  int32_t old_cap$399 = self$400->$1;
  int32_t new_cap$401;
  if (old_cap$399 == 0) {
    new_cap$401 = 8;
  } else {
    new_cap$401 = old_cap$399 * 2;
  }
  $$moonbitlang$core$builtin$Array$$resize_buffer$0(self$400, new_cap$401);
  return 0;
}

int32_t $$moonbitlang$core$builtin$Array$$resize_buffer$2(
  struct $$moonbitlang$core$builtin$Array$3c$async$3c$$3e$$3d$$3e$Unit$21$Error$3e$* self$396,
  int32_t new_capacity$394
) {
  struct $$3c$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit** new_buf$393 =
    (struct $$3c$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit**)moonbit_make_ref_array(
      new_capacity$394, 0
    );
  struct $$3c$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit** _field$2547 =
    self$396->$0;
  struct $$3c$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit** old_buf$395 =
    _field$2547;
  int32_t old_cap$397 = Moonbit_array_length(old_buf$395);
  int32_t copy_len$398;
  struct $$3c$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit** _old$2546;
  if (old_cap$397 < new_capacity$394) {
    copy_len$398 = old_cap$397;
  } else {
    copy_len$398 = new_capacity$394;
  }
  moonbit_incref(old_buf$395);
  moonbit_incref(new_buf$393);
  $$moonbitlang$core$builtin$UninitializedArray$$unsafe_blit$2(
    new_buf$393, 0, old_buf$395, 0, copy_len$398
  );
  _old$2546 = self$396->$0;
  moonbit_decref(_old$2546);
  self$396->$0 = new_buf$393;
  moonbit_decref(self$396);
  return 0;
}

int32_t $$moonbitlang$core$builtin$Array$$resize_buffer$1(
  struct $$moonbitlang$core$builtin$Array$3c$$3c$String$2a$Int$3e$$3e$* self$390,
  int32_t new_capacity$388
) {
  struct $$3c$String$2a$Int$3e$** new_buf$387 =
    (struct $$3c$String$2a$Int$3e$**)moonbit_make_ref_array(
      new_capacity$388, 0
    );
  struct $$3c$String$2a$Int$3e$** _field$2549 = self$390->$0;
  struct $$3c$String$2a$Int$3e$** old_buf$389 = _field$2549;
  int32_t old_cap$391 = Moonbit_array_length(old_buf$389);
  int32_t copy_len$392;
  struct $$3c$String$2a$Int$3e$** _old$2548;
  if (old_cap$391 < new_capacity$388) {
    copy_len$392 = old_cap$391;
  } else {
    copy_len$392 = new_capacity$388;
  }
  moonbit_incref(old_buf$389);
  moonbit_incref(new_buf$387);
  $$moonbitlang$core$builtin$UninitializedArray$$unsafe_blit$1(
    new_buf$387, 0, old_buf$389, 0, copy_len$392
  );
  _old$2548 = self$390->$0;
  moonbit_decref(_old$2548);
  self$390->$0 = new_buf$387;
  moonbit_decref(self$390);
  return 0;
}

int32_t $$moonbitlang$core$builtin$Array$$resize_buffer$0(
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* self$384,
  int32_t new_capacity$382
) {
  moonbit_string_t* new_buf$381 =
    (moonbit_string_t*)moonbit_make_ref_array(
      new_capacity$382, (moonbit_string_t)moonbit_string_literal_0.data
    );
  moonbit_string_t* _field$2551 = self$384->$0;
  moonbit_string_t* old_buf$383 = _field$2551;
  int32_t old_cap$385 = Moonbit_array_length(old_buf$383);
  int32_t copy_len$386;
  moonbit_string_t* _old$2550;
  if (old_cap$385 < new_capacity$382) {
    copy_len$386 = old_cap$385;
  } else {
    copy_len$386 = new_capacity$382;
  }
  moonbit_incref(old_buf$383);
  moonbit_incref(new_buf$381);
  $$moonbitlang$core$builtin$UninitializedArray$$unsafe_blit$0(
    new_buf$381, 0, old_buf$383, 0, copy_len$386
  );
  _old$2550 = self$384->$0;
  moonbit_decref(_old$2550);
  self$384->$0 = new_buf$381;
  moonbit_decref(self$384);
  return 0;
}

struct $$moonbitlang$core$builtin$Array$3c$String$3e$* $$moonbitlang$core$builtin$Array$$new$inner$0(
  int32_t capacity$380
) {
  if (capacity$380 == 0) {
    moonbit_string_t* _tmp$1560 = (moonbit_string_t*)moonbit_empty_ref_array;
    struct $$moonbitlang$core$builtin$Array$3c$String$3e$* _block$2831 =
      (struct $$moonbitlang$core$builtin$Array$3c$String$3e$*)moonbit_malloc(
        sizeof(struct $$moonbitlang$core$builtin$Array$3c$String$3e$)
      );
    Moonbit_object_header(_block$2831)->meta
    = Moonbit_make_regular_object_header(
      offsetof(
        struct $$moonbitlang$core$builtin$Array$3c$String$3e$, $0
      )
      >> 2,
        1,
        0
    );
    _block$2831->$0 = _tmp$1560;
    _block$2831->$1 = 0;
    return _block$2831;
  } else {
    moonbit_string_t* _tmp$1561 =
      (moonbit_string_t*)moonbit_make_ref_array(
        capacity$380, (moonbit_string_t)moonbit_string_literal_0.data
      );
    struct $$moonbitlang$core$builtin$Array$3c$String$3e$* _block$2832 =
      (struct $$moonbitlang$core$builtin$Array$3c$String$3e$*)moonbit_malloc(
        sizeof(struct $$moonbitlang$core$builtin$Array$3c$String$3e$)
      );
    Moonbit_object_header(_block$2832)->meta
    = Moonbit_make_regular_object_header(
      offsetof(
        struct $$moonbitlang$core$builtin$Array$3c$String$3e$, $0
      )
      >> 2,
        1,
        0
    );
    _block$2832->$0 = _tmp$1561;
    _block$2832->$1 = 0;
    return _block$2832;
  }
}

int32_t $$moonbitlang$core$builtin$Logger$$$moonbitlang$core$builtin$StringBuilder$$write_view(
  struct $$moonbitlang$core$builtin$StringBuilder* self$378,
  struct $StringView str$379
) {
  int32_t len$1548 = self$378->$1;
  int32_t _tmp$1550;
  int32_t _tmp$1549;
  int32_t _tmp$1547;
  moonbit_bytes_t _field$2552;
  moonbit_bytes_t data$1551;
  int32_t len$1552;
  moonbit_string_t _tmp$1553;
  int32_t _tmp$1554;
  int32_t _tmp$1555;
  int32_t len$1557;
  int32_t _tmp$1559;
  int32_t _tmp$1558;
  int32_t _tmp$1556;
  moonbit_incref(str$379.$0);
  _tmp$1550 = $StringView$$length(str$379);
  _tmp$1549 = _tmp$1550 * 2;
  _tmp$1547 = len$1548 + _tmp$1549;
  moonbit_incref(self$378);
  $$moonbitlang$core$builtin$StringBuilder$$grow_if_necessary(
    self$378, _tmp$1547
  );
  _field$2552 = self$378->$0;
  data$1551 = _field$2552;
  len$1552 = self$378->$1;
  moonbit_incref(data$1551);
  moonbit_incref(str$379.$0);
  _tmp$1553 = $StringView$$data(str$379);
  moonbit_incref(str$379.$0);
  _tmp$1554 = $StringView$$start_offset(str$379);
  moonbit_incref(str$379.$0);
  _tmp$1555 = $StringView$$length(str$379);
  $FixedArray$$blit_from_string(
    data$1551, len$1552, _tmp$1553, _tmp$1554, _tmp$1555
  );
  len$1557 = self$378->$1;
  _tmp$1559 = $StringView$$length(str$379);
  _tmp$1558 = _tmp$1559 * 2;
  _tmp$1556 = len$1557 + _tmp$1558;
  self$378->$1 = _tmp$1556;
  moonbit_decref(self$378);
  return 0;
}

int32_t $String$$char_length_ge$inner(
  moonbit_string_t self$370,
  int32_t len$373,
  int32_t start_offset$377,
  int64_t end_offset$368
) {
  int32_t end_offset$367;
  int32_t index$371;
  int32_t count$372;
  if (end_offset$368 == 4294967296ll) {
    end_offset$367 = Moonbit_array_length(self$370);
  } else {
    int64_t _Some$369 = end_offset$368;
    end_offset$367 = (int32_t)_Some$369;
  }
  index$371 = start_offset$377;
  count$372 = 0;
  while (1) {
    int32_t _if_result$2834;
    if (index$371 < end_offset$367) {
      _if_result$2834 = count$372 < len$373;
    } else {
      _if_result$2834 = 0;
    }
    if (_if_result$2834) {
      int32_t c1$374 = self$370[index$371];
      int32_t _if_result$2835;
      int32_t _tmp$1545;
      int32_t _tmp$1546;
      if ($Int$$is_leading_surrogate(c1$374)) {
        int32_t _tmp$1541 = index$371 + 1;
        _if_result$2835 = _tmp$1541 < end_offset$367;
      } else {
        _if_result$2835 = 0;
      }
      if (_if_result$2835) {
        int32_t _tmp$1544 = index$371 + 1;
        int32_t c2$375 = self$370[_tmp$1544];
        if ($Int$$is_trailing_surrogate(c2$375)) {
          int32_t _tmp$1542 = index$371 + 2;
          int32_t _tmp$1543 = count$372 + 1;
          index$371 = _tmp$1542;
          count$372 = _tmp$1543;
          continue;
        } else {
          $moonbitlang$core$builtin$abort$0(
            (moonbit_string_t)moonbit_string_literal_11.data,
              (moonbit_string_t)moonbit_string_literal_12.data
          );
        }
      }
      _tmp$1545 = index$371 + 1;
      _tmp$1546 = count$372 + 1;
      index$371 = _tmp$1545;
      count$372 = _tmp$1546;
      continue;
    } else {
      moonbit_decref(self$370);
      return count$372 >= len$373;
    }
    break;
  }
}

int32_t $$moonbitlang$core$builtin$ArrayView$$length$4(
  struct $$moonbitlang$core$builtin$ArrayView$3c$$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$$3e$ self$366
) {
  int32_t end$1539 = self$366.$2;
  int32_t _field$2553 = self$366.$1;
  int32_t start$1540;
  moonbit_decref(self$366.$0);
  start$1540 = _field$2553;
  return end$1539 - start$1540;
}

int32_t $$moonbitlang$core$builtin$ArrayView$$length$3(
  struct $$moonbitlang$core$builtin$ArrayView$3c$$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$$3e$ self$365
) {
  int32_t end$1537 = self$365.$2;
  int32_t _field$2554 = self$365.$1;
  int32_t start$1538;
  moonbit_decref(self$365.$0);
  start$1538 = _field$2554;
  return end$1537 - start$1538;
}

int32_t $$moonbitlang$core$builtin$ArrayView$$length$2(
  struct $$moonbitlang$core$builtin$ArrayView$3c$$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$$3e$ self$364
) {
  int32_t end$1535 = self$364.$2;
  int32_t _field$2555 = self$364.$1;
  int32_t start$1536;
  moonbit_decref(self$364.$0);
  start$1536 = _field$2555;
  return end$1535 - start$1536;
}

int32_t $$moonbitlang$core$builtin$ArrayView$$length$1(
  struct $$moonbitlang$core$builtin$ArrayView$3c$$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$ self$363
) {
  int32_t end$1533 = self$363.$2;
  int32_t _field$2556 = self$363.$1;
  int32_t start$1534;
  moonbit_decref(self$363.$0);
  start$1534 = _field$2556;
  return end$1533 - start$1534;
}

int32_t $$moonbitlang$core$builtin$ArrayView$$length$0(
  struct $$moonbitlang$core$builtin$ArrayView$3c$$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$$3e$ self$362
) {
  int32_t end$1531 = self$362.$2;
  int32_t _field$2557 = self$362.$1;
  int32_t start$1532;
  moonbit_decref(self$362.$0);
  start$1532 = _field$2557;
  return end$1531 - start$1532;
}

int32_t $$moonbitlang$core$builtin$Show$$String$$output(
  moonbit_string_t self$354,
  struct $$moonbitlang$core$builtin$Logger logger$352
) {
  struct $$3c$String$2a$$moonbitlang$core$builtin$Logger$3e$* _env$353;
  int32_t len$355;
  int32_t i$356;
  int32_t seg$357;
  if (logger$352.$1) {
    moonbit_incref(logger$352.$1);
  }
  logger$352.$0->$method_3(logger$352.$1, 34);
  moonbit_incref(self$354);
  if (logger$352.$1) {
    moonbit_incref(logger$352.$1);
  }
  _env$353
  = (struct $$3c$String$2a$$moonbitlang$core$builtin$Logger$3e$*)moonbit_malloc(
      sizeof(struct $$3c$String$2a$$moonbitlang$core$builtin$Logger$3e$)
    );
  Moonbit_object_header(_env$353)->meta
  = Moonbit_make_regular_object_header(
    offsetof(
      struct $$3c$String$2a$$moonbitlang$core$builtin$Logger$3e$, $0
    )
    >> 2,
      3,
      0
  );
  _env$353->$0 = self$354;
  _env$353->$1_0 = logger$352.$0;
  _env$353->$1_1 = logger$352.$1;
  len$355 = Moonbit_array_length(self$354);
  i$356 = 0;
  seg$357 = 0;
  $$2a$for$358:;
  while (1) {
    int32_t code$359;
    int32_t c$361;
    struct $$moonbitlang$core$builtin$Logger _bind$1513;
    int32_t _tmp$1514;
    int32_t _tmp$1515;
    int32_t _tmp$1516;
    int32_t _tmp$2839;
    int32_t _tmp$2840;
    if (i$356 >= len$355) {
      moonbit_decref(self$354);
      $moonbitlang$core$builtin$output$flush_segment$7c$3594(
        _env$353, seg$357, i$356
      );
      break;
    }
    code$359 = self$354[i$356];
    switch (code$359) {
      case 34: {
        c$361 = code$359;
        goto $join$360;
        break;
      }
      
      case 92: {
        c$361 = code$359;
        goto $join$360;
        break;
      }
      
      case 10: {
        int32_t _tmp$1517;
        int32_t _tmp$1518;
        moonbit_incref(_env$353);
        $moonbitlang$core$builtin$output$flush_segment$7c$3594(
          _env$353, seg$357, i$356
        );
        if (logger$352.$1) {
          moonbit_incref(logger$352.$1);
        }
        logger$352.$0->$method_0(
          logger$352.$1, (moonbit_string_t)moonbit_string_literal_13.data
        );
        _tmp$1517 = i$356 + 1;
        _tmp$1518 = i$356 + 1;
        i$356 = _tmp$1517;
        seg$357 = _tmp$1518;
        goto $$2a$for$358;
        break;
      }
      
      case 13: {
        int32_t _tmp$1519;
        int32_t _tmp$1520;
        moonbit_incref(_env$353);
        $moonbitlang$core$builtin$output$flush_segment$7c$3594(
          _env$353, seg$357, i$356
        );
        if (logger$352.$1) {
          moonbit_incref(logger$352.$1);
        }
        logger$352.$0->$method_0(
          logger$352.$1, (moonbit_string_t)moonbit_string_literal_14.data
        );
        _tmp$1519 = i$356 + 1;
        _tmp$1520 = i$356 + 1;
        i$356 = _tmp$1519;
        seg$357 = _tmp$1520;
        goto $$2a$for$358;
        break;
      }
      
      case 8: {
        int32_t _tmp$1521;
        int32_t _tmp$1522;
        moonbit_incref(_env$353);
        $moonbitlang$core$builtin$output$flush_segment$7c$3594(
          _env$353, seg$357, i$356
        );
        if (logger$352.$1) {
          moonbit_incref(logger$352.$1);
        }
        logger$352.$0->$method_0(
          logger$352.$1, (moonbit_string_t)moonbit_string_literal_15.data
        );
        _tmp$1521 = i$356 + 1;
        _tmp$1522 = i$356 + 1;
        i$356 = _tmp$1521;
        seg$357 = _tmp$1522;
        goto $$2a$for$358;
        break;
      }
      
      case 9: {
        int32_t _tmp$1523;
        int32_t _tmp$1524;
        moonbit_incref(_env$353);
        $moonbitlang$core$builtin$output$flush_segment$7c$3594(
          _env$353, seg$357, i$356
        );
        if (logger$352.$1) {
          moonbit_incref(logger$352.$1);
        }
        logger$352.$0->$method_0(
          logger$352.$1, (moonbit_string_t)moonbit_string_literal_16.data
        );
        _tmp$1523 = i$356 + 1;
        _tmp$1524 = i$356 + 1;
        i$356 = _tmp$1523;
        seg$357 = _tmp$1524;
        goto $$2a$for$358;
        break;
      }
      default: {
        if (code$359 < 32) {
          int32_t _tmp$1527;
          moonbit_string_t _tmp$1526;
          struct $$moonbitlang$core$builtin$Logger _bind$1525;
          int32_t _tmp$1528;
          int32_t _tmp$1529;
          moonbit_incref(_env$353);
          $moonbitlang$core$builtin$output$flush_segment$7c$3594(
            _env$353, seg$357, i$356
          );
          if (logger$352.$1) {
            moonbit_incref(logger$352.$1);
          }
          logger$352.$0->$method_0(
            logger$352.$1, (moonbit_string_t)moonbit_string_literal_17.data
          );
          _tmp$1527 = code$359 & 0xff;
          _tmp$1526 = $Byte$$to_hex(_tmp$1527);
          if (logger$352.$1) {
            moonbit_incref(logger$352.$1);
          }
          logger$352.$0->$method_0(logger$352.$1, _tmp$1526);
          _bind$1525 = logger$352;
          if (_bind$1525.$1) {
            moonbit_incref(_bind$1525.$1);
          }
          _bind$1525.$0->$method_3(_bind$1525.$1, 125);
          _tmp$1528 = i$356 + 1;
          _tmp$1529 = i$356 + 1;
          i$356 = _tmp$1528;
          seg$357 = _tmp$1529;
          goto $$2a$for$358;
        } else {
          int32_t _tmp$1530 = i$356 + 1;
          int32_t _tmp$2838 = seg$357;
          i$356 = _tmp$1530;
          seg$357 = _tmp$2838;
          goto $$2a$for$358;
        }
        break;
      }
    }
    goto $joinlet$2837;
    $join$360:;
    moonbit_incref(_env$353);
    $moonbitlang$core$builtin$output$flush_segment$7c$3594(
      _env$353, seg$357, i$356
    );
    if (logger$352.$1) {
      moonbit_incref(logger$352.$1);
    }
    logger$352.$0->$method_3(logger$352.$1, 92);
    _bind$1513 = logger$352;
    _tmp$1514 = c$361;
    if (_bind$1513.$1) {
      moonbit_incref(_bind$1513.$1);
    }
    _bind$1513.$0->$method_3(_bind$1513.$1, _tmp$1514);
    _tmp$1515 = i$356 + 1;
    _tmp$1516 = i$356 + 1;
    i$356 = _tmp$1515;
    seg$357 = _tmp$1516;
    continue;
    $joinlet$2837:;
    _tmp$2839 = i$356;
    _tmp$2840 = seg$357;
    i$356 = _tmp$2839;
    seg$357 = _tmp$2840;
    continue;
    break;
  }
  logger$352.$0->$method_3(logger$352.$1, 34);
  return 0;
}

int32_t $moonbitlang$core$builtin$output$flush_segment$7c$3594(
  struct $$3c$String$2a$$moonbitlang$core$builtin$Logger$3e$* _env$348,
  int32_t seg$351,
  int32_t i$350
) {
  struct $$moonbitlang$core$builtin$Logger _field$2559 =
    (struct $$moonbitlang$core$builtin$Logger){
      _env$348->$1_0, _env$348->$1_1
    };
  struct $$moonbitlang$core$builtin$Logger logger$347 = _field$2559;
  moonbit_string_t _field$2558 = _env$348->$0;
  int32_t _cnt$2715 = Moonbit_object_header(_env$348)->rc;
  moonbit_string_t self$349;
  if (_cnt$2715 > 1) {
    int32_t _new_cnt$2716 = _cnt$2715 - 1;
    Moonbit_object_header(_env$348)->rc = _new_cnt$2716;
    if (logger$347.$1) {
      moonbit_incref(logger$347.$1);
    }
    moonbit_incref(_field$2558);
  } else if (_cnt$2715 == 1) {
    moonbit_free(_env$348);
  }
  self$349 = _field$2558;
  if (i$350 > seg$351) {
    int32_t _tmp$1512 = i$350 - seg$351;
    logger$347.$0->$method_1(logger$347.$1, self$349, seg$351, _tmp$1512);
  } else {
    moonbit_decref(self$349);
    if (logger$347.$1) {
      moonbit_decref(logger$347.$1);
    }
  }
  return 0;
}

moonbit_string_t $Byte$$to_hex(int32_t b$346) {
  struct $$moonbitlang$core$builtin$StringBuilder* _self$345 =
    $$moonbitlang$core$builtin$StringBuilder$$new$inner(0);
  int32_t _tmp$1509 = $$moonbitlang$core$builtin$Div$$Byte$$div(b$346, 16);
  int32_t _tmp$1508 =
    $moonbitlang$core$builtin$to_hex$to_hex_digit$7c$3604(_tmp$1509);
  int32_t _tmp$1511;
  int32_t _tmp$1510;
  struct $$moonbitlang$core$builtin$StringBuilder* _tmp$1507;
  moonbit_incref(_self$345);
  $$moonbitlang$core$builtin$Logger$$$moonbitlang$core$builtin$StringBuilder$$write_char(
    _self$345, _tmp$1508
  );
  _tmp$1511 = $$moonbitlang$core$builtin$Mod$$Byte$$mod(b$346, 16);
  _tmp$1510
  = $moonbitlang$core$builtin$to_hex$to_hex_digit$7c$3604(
    _tmp$1511
  );
  moonbit_incref(_self$345);
  $$moonbitlang$core$builtin$Logger$$$moonbitlang$core$builtin$StringBuilder$$write_char(
    _self$345, _tmp$1510
  );
  _tmp$1507 = _self$345;
  return $$moonbitlang$core$builtin$StringBuilder$$to_string(_tmp$1507);
}

int32_t $moonbitlang$core$builtin$to_hex$to_hex_digit$7c$3604(int32_t i$344) {
  if (i$344 < 10) {
    int32_t _tmp$1504 = $$moonbitlang$core$builtin$Add$$Byte$$add(i$344, 48);
    return $Byte$$to_char(_tmp$1504);
  } else {
    int32_t _tmp$1506 = $$moonbitlang$core$builtin$Add$$Byte$$add(i$344, 97);
    int32_t _tmp$1505 =
      $$moonbitlang$core$builtin$Sub$$Byte$$sub(_tmp$1506, 10);
    return $Byte$$to_char(_tmp$1505);
  }
}

int32_t $$moonbitlang$core$builtin$Sub$$Byte$$sub(
  int32_t self$342,
  int32_t that$343
) {
  int32_t _tmp$1502 = (int32_t)self$342;
  int32_t _tmp$1503 = (int32_t)that$343;
  int32_t _tmp$1501 = _tmp$1502 - _tmp$1503;
  return _tmp$1501 & 0xff;
}

int32_t $$moonbitlang$core$builtin$Mod$$Byte$$mod(
  int32_t self$340,
  int32_t that$341
) {
  int32_t _tmp$1499 = (int32_t)self$340;
  int32_t _tmp$1500 = (int32_t)that$341;
  int32_t _tmp$1498 = _tmp$1499 % _tmp$1500;
  return _tmp$1498 & 0xff;
}

int32_t $$moonbitlang$core$builtin$Div$$Byte$$div(
  int32_t self$338,
  int32_t that$339
) {
  int32_t _tmp$1496 = (int32_t)self$338;
  int32_t _tmp$1497 = (int32_t)that$339;
  int32_t _tmp$1495 = _tmp$1496 / _tmp$1497;
  return _tmp$1495 & 0xff;
}

int32_t $$moonbitlang$core$builtin$Add$$Byte$$add(
  int32_t self$336,
  int32_t that$337
) {
  int32_t _tmp$1493 = (int32_t)self$336;
  int32_t _tmp$1494 = (int32_t)that$337;
  int32_t _tmp$1492 = _tmp$1493 + _tmp$1494;
  return _tmp$1492 & 0xff;
}

moonbit_string_t $String$$unsafe_substring(
  moonbit_string_t str$333,
  int32_t start$331,
  int32_t end$332
) {
  int32_t _if_result$2841;
  int32_t len$334;
  int32_t _tmp$1490;
  int32_t _tmp$1491;
  moonbit_bytes_t bytes$335;
  moonbit_bytes_t _tmp$1489;
  if (start$331 == 0) {
    int32_t _tmp$1488 = Moonbit_array_length(str$333);
    _if_result$2841 = end$332 == _tmp$1488;
  } else {
    _if_result$2841 = 0;
  }
  if (_if_result$2841) {
    return str$333;
  }
  len$334 = end$332 - start$331;
  _tmp$1490 = len$334 * 2;
  _tmp$1491 = $$moonbitlang$core$builtin$Default$$Byte$$default();
  bytes$335 = (moonbit_bytes_t)moonbit_make_bytes(_tmp$1490, _tmp$1491);
  moonbit_incref(bytes$335);
  $FixedArray$$blit_from_string(bytes$335, 0, str$333, start$331, len$334);
  _tmp$1489 = bytes$335;
  return $Bytes$$to_unchecked_string$inner(_tmp$1489, 0, 4294967296ll);
}

moonbit_string_t $Int$$to_string$inner(int32_t self$315, int32_t radix$314) {
  int32_t _if_result$2842;
  int32_t is_negative$316;
  uint32_t num$317;
  uint16_t* buffer$318;
  if (radix$314 < 2) {
    _if_result$2842 = 1;
  } else {
    _if_result$2842 = radix$314 > 36;
  }
  if (_if_result$2842) {
    $moonbitlang$core$builtin$abort$0(
      (moonbit_string_t)moonbit_string_literal_18.data,
        (moonbit_string_t)moonbit_string_literal_19.data
    );
  }
  if (self$315 == 0) {
    return (moonbit_string_t)moonbit_string_literal_20.data;
  }
  is_negative$316 = self$315 < 0;
  if (is_negative$316) {
    int32_t _tmp$1487 = -self$315;
    num$317 = *(uint32_t*)&_tmp$1487;
  } else {
    num$317 = *(uint32_t*)&self$315;
  }
  switch (radix$314) {
    case 10: {
      int32_t digit_len$319 = $moonbitlang$core$builtin$dec_count32(num$317);
      int32_t _tmp$1484;
      int32_t total_len$320;
      uint16_t* buffer$321;
      int32_t digit_start$322;
      if (is_negative$316) {
        _tmp$1484 = 1;
      } else {
        _tmp$1484 = 0;
      }
      total_len$320 = digit_len$319 + _tmp$1484;
      buffer$321 = (uint16_t*)moonbit_make_string(total_len$320, 0);
      if (is_negative$316) {
        digit_start$322 = 1;
      } else {
        digit_start$322 = 0;
      }
      moonbit_incref(buffer$321);
      $moonbitlang$core$builtin$int_to_string_dec(
        buffer$321, num$317, digit_start$322, total_len$320
      );
      buffer$318 = buffer$321;
      break;
    }
    
    case 16: {
      int32_t digit_len$323 = $moonbitlang$core$builtin$hex_count32(num$317);
      int32_t _tmp$1485;
      int32_t total_len$324;
      uint16_t* buffer$325;
      int32_t digit_start$326;
      if (is_negative$316) {
        _tmp$1485 = 1;
      } else {
        _tmp$1485 = 0;
      }
      total_len$324 = digit_len$323 + _tmp$1485;
      buffer$325 = (uint16_t*)moonbit_make_string(total_len$324, 0);
      if (is_negative$316) {
        digit_start$326 = 1;
      } else {
        digit_start$326 = 0;
      }
      moonbit_incref(buffer$325);
      $moonbitlang$core$builtin$int_to_string_hex(
        buffer$325, num$317, digit_start$326, total_len$324
      );
      buffer$318 = buffer$325;
      break;
    }
    default: {
      int32_t digit_len$327 =
        $moonbitlang$core$builtin$radix_count32(num$317, radix$314);
      int32_t _tmp$1486;
      int32_t total_len$328;
      uint16_t* buffer$329;
      int32_t digit_start$330;
      if (is_negative$316) {
        _tmp$1486 = 1;
      } else {
        _tmp$1486 = 0;
      }
      total_len$328 = digit_len$327 + _tmp$1486;
      buffer$329 = (uint16_t*)moonbit_make_string(total_len$328, 0);
      if (is_negative$316) {
        digit_start$330 = 1;
      } else {
        digit_start$330 = 0;
      }
      moonbit_incref(buffer$329);
      $moonbitlang$core$builtin$int_to_string_generic(
        buffer$329, num$317, digit_start$330, total_len$328, radix$314
      );
      buffer$318 = buffer$329;
      break;
    }
  }
  if (is_negative$316) {
    buffer$318[0] = 45;
  }
  return buffer$318;
}

int32_t $moonbitlang$core$builtin$radix_count32(
  uint32_t value$308,
  int32_t radix$311
) {
  uint32_t num$309;
  uint32_t base$310;
  int32_t count$312;
  if (value$308 == 0u) {
    return 1;
  }
  num$309 = value$308;
  base$310 = *(uint32_t*)&radix$311;
  count$312 = 0;
  while (1) {
    uint32_t _tmp$1481 = num$309;
    if (_tmp$1481 > 0u) {
      int32_t _tmp$1482 = count$312;
      uint32_t _tmp$1483;
      count$312 = _tmp$1482 + 1;
      _tmp$1483 = num$309;
      num$309 = _tmp$1483 / base$310;
      continue;
    }
    break;
  }
  return count$312;
}

int32_t $moonbitlang$core$builtin$hex_count32(uint32_t value$306) {
  if (value$306 == 0u) {
    return 1;
  } else {
    int32_t leading_zeros$307 = moonbit_clz32(value$306);
    int32_t _tmp$1480 = 31 - leading_zeros$307;
    int32_t _tmp$1479 = _tmp$1480 / 4;
    return _tmp$1479 + 1;
  }
}

int32_t $moonbitlang$core$builtin$dec_count32(uint32_t value$305) {
  if (value$305 >= 100000u) {
    if (value$305 >= 10000000u) {
      if (value$305 >= 1000000000u) {
        return 10;
      } else if (value$305 >= 100000000u) {
        return 9;
      } else {
        return 8;
      }
    } else if (value$305 >= 1000000u) {
      return 7;
    } else {
      return 6;
    }
  } else if (value$305 >= 1000u) {
    if (value$305 >= 10000u) {
      return 5;
    } else {
      return 4;
    }
  } else if (value$305 >= 100u) {
    return 3;
  } else if (value$305 >= 10u) {
    return 2;
  } else {
    return 1;
  }
}

int32_t $moonbitlang$core$builtin$int_to_string_dec(
  uint16_t* buffer$295,
  uint32_t num$283,
  int32_t digit_start$286,
  int32_t total_len$285
) {
  uint32_t num$282 = num$283;
  int32_t offset$284 = total_len$285 - digit_start$286;
  uint32_t _tmp$1478;
  int32_t remaining$297;
  int32_t _tmp$1459;
  while (1) {
    uint32_t _tmp$1422 = num$282;
    if (_tmp$1422 >= 10000u) {
      uint32_t _tmp$1445 = num$282;
      uint32_t t$287 = _tmp$1445 / 10000u;
      uint32_t _tmp$1444 = num$282;
      uint32_t _tmp$1443 = _tmp$1444 % 10000u;
      int32_t r$288 = *(int32_t*)&_tmp$1443;
      int32_t d1$289;
      int32_t d2$290;
      int32_t _tmp$1423;
      int32_t _tmp$1442;
      int32_t _tmp$1441;
      int32_t d1_hi$291;
      int32_t _tmp$1440;
      int32_t _tmp$1439;
      int32_t d1_lo$292;
      int32_t _tmp$1438;
      int32_t _tmp$1437;
      int32_t d2_hi$293;
      int32_t _tmp$1436;
      int32_t _tmp$1435;
      int32_t d2_lo$294;
      int32_t _tmp$1425;
      int32_t _tmp$1424;
      int32_t _tmp$1428;
      int32_t _tmp$1427;
      int32_t _tmp$1426;
      int32_t _tmp$1431;
      int32_t _tmp$1430;
      int32_t _tmp$1429;
      int32_t _tmp$1434;
      int32_t _tmp$1433;
      int32_t _tmp$1432;
      num$282 = t$287;
      d1$289 = r$288 / 100;
      d2$290 = r$288 % 100;
      _tmp$1423 = offset$284;
      offset$284 = _tmp$1423 - 4;
      _tmp$1442 = d1$289 / 10;
      _tmp$1441 = 48 + _tmp$1442;
      d1_hi$291 = (uint16_t)_tmp$1441;
      _tmp$1440 = d1$289 % 10;
      _tmp$1439 = 48 + _tmp$1440;
      d1_lo$292 = (uint16_t)_tmp$1439;
      _tmp$1438 = d2$290 / 10;
      _tmp$1437 = 48 + _tmp$1438;
      d2_hi$293 = (uint16_t)_tmp$1437;
      _tmp$1436 = d2$290 % 10;
      _tmp$1435 = 48 + _tmp$1436;
      d2_lo$294 = (uint16_t)_tmp$1435;
      _tmp$1425 = offset$284;
      _tmp$1424 = digit_start$286 + _tmp$1425;
      buffer$295[_tmp$1424] = d1_hi$291;
      _tmp$1428 = offset$284;
      _tmp$1427 = digit_start$286 + _tmp$1428;
      _tmp$1426 = _tmp$1427 + 1;
      buffer$295[_tmp$1426] = d1_lo$292;
      _tmp$1431 = offset$284;
      _tmp$1430 = digit_start$286 + _tmp$1431;
      _tmp$1429 = _tmp$1430 + 2;
      buffer$295[_tmp$1429] = d2_hi$293;
      _tmp$1434 = offset$284;
      _tmp$1433 = digit_start$286 + _tmp$1434;
      _tmp$1432 = _tmp$1433 + 3;
      buffer$295[_tmp$1432] = d2_lo$294;
      continue;
    }
    break;
  }
  _tmp$1478 = num$282;
  remaining$297 = *(int32_t*)&_tmp$1478;
  while (1) {
    int32_t _tmp$1446 = remaining$297;
    if (_tmp$1446 >= 100) {
      int32_t _tmp$1458 = remaining$297;
      int32_t t$298 = _tmp$1458 / 100;
      int32_t _tmp$1457 = remaining$297;
      int32_t d$299 = _tmp$1457 % 100;
      int32_t _tmp$1447;
      int32_t _tmp$1456;
      int32_t _tmp$1455;
      int32_t d_hi$300;
      int32_t _tmp$1454;
      int32_t _tmp$1453;
      int32_t d_lo$301;
      int32_t _tmp$1449;
      int32_t _tmp$1448;
      int32_t _tmp$1452;
      int32_t _tmp$1451;
      int32_t _tmp$1450;
      remaining$297 = t$298;
      _tmp$1447 = offset$284;
      offset$284 = _tmp$1447 - 2;
      _tmp$1456 = d$299 / 10;
      _tmp$1455 = 48 + _tmp$1456;
      d_hi$300 = (uint16_t)_tmp$1455;
      _tmp$1454 = d$299 % 10;
      _tmp$1453 = 48 + _tmp$1454;
      d_lo$301 = (uint16_t)_tmp$1453;
      _tmp$1449 = offset$284;
      _tmp$1448 = digit_start$286 + _tmp$1449;
      buffer$295[_tmp$1448] = d_hi$300;
      _tmp$1452 = offset$284;
      _tmp$1451 = digit_start$286 + _tmp$1452;
      _tmp$1450 = _tmp$1451 + 1;
      buffer$295[_tmp$1450] = d_lo$301;
      continue;
    }
    break;
  }
  _tmp$1459 = remaining$297;
  if (_tmp$1459 >= 10) {
    int32_t _tmp$1460 = offset$284;
    int32_t _tmp$1471;
    int32_t _tmp$1470;
    int32_t _tmp$1469;
    int32_t d_hi$303;
    int32_t _tmp$1468;
    int32_t _tmp$1467;
    int32_t _tmp$1466;
    int32_t d_lo$304;
    int32_t _tmp$1462;
    int32_t _tmp$1461;
    int32_t _tmp$1465;
    int32_t _tmp$1464;
    int32_t _tmp$1463;
    offset$284 = _tmp$1460 - 2;
    _tmp$1471 = remaining$297;
    _tmp$1470 = _tmp$1471 / 10;
    _tmp$1469 = 48 + _tmp$1470;
    d_hi$303 = (uint16_t)_tmp$1469;
    _tmp$1468 = remaining$297;
    _tmp$1467 = _tmp$1468 % 10;
    _tmp$1466 = 48 + _tmp$1467;
    d_lo$304 = (uint16_t)_tmp$1466;
    _tmp$1462 = offset$284;
    _tmp$1461 = digit_start$286 + _tmp$1462;
    buffer$295[_tmp$1461] = d_hi$303;
    _tmp$1465 = offset$284;
    _tmp$1464 = digit_start$286 + _tmp$1465;
    _tmp$1463 = _tmp$1464 + 1;
    buffer$295[_tmp$1463] = d_lo$304;
    moonbit_decref(buffer$295);
  } else {
    int32_t _tmp$1472 = offset$284;
    int32_t _tmp$1477;
    int32_t _tmp$1473;
    int32_t _tmp$1476;
    int32_t _tmp$1475;
    int32_t _tmp$1474;
    offset$284 = _tmp$1472 - 1;
    _tmp$1477 = offset$284;
    _tmp$1473 = digit_start$286 + _tmp$1477;
    _tmp$1476 = remaining$297;
    _tmp$1475 = 48 + _tmp$1476;
    _tmp$1474 = (uint16_t)_tmp$1475;
    buffer$295[_tmp$1473] = _tmp$1474;
    moonbit_decref(buffer$295);
  }
  return 0;
}

int32_t $moonbitlang$core$builtin$int_to_string_generic(
  uint16_t* buffer$277,
  uint32_t num$271,
  int32_t digit_start$269,
  int32_t total_len$268,
  int32_t radix$273
) {
  int32_t offset$267 = total_len$268 - digit_start$269;
  uint32_t n$270 = num$271;
  uint32_t base$272 = *(uint32_t*)&radix$273;
  int32_t _tmp$1402 = radix$273 - 1;
  int32_t _tmp$1401 = radix$273 & _tmp$1402;
  if (_tmp$1401 == 0) {
    int32_t shift$274 = moonbit_ctz32(radix$273);
    uint32_t mask$275 = base$272 - 1u;
    while (1) {
      uint32_t _tmp$1403 = n$270;
      if (_tmp$1403 > 0u) {
        int32_t _tmp$1404 = offset$267;
        uint32_t _tmp$1411;
        uint32_t _tmp$1410;
        int32_t digit$276;
        int32_t _tmp$1408;
        int32_t _tmp$1405;
        int32_t _tmp$1407;
        int32_t _tmp$1406;
        uint32_t _tmp$1409;
        offset$267 = _tmp$1404 - 1;
        _tmp$1411 = n$270;
        _tmp$1410 = _tmp$1411 & mask$275;
        digit$276 = *(int32_t*)&_tmp$1410;
        _tmp$1408 = offset$267;
        _tmp$1405 = digit_start$269 + _tmp$1408;
        _tmp$1407
        = ((moonbit_string_t)moonbit_string_literal_21.data)[
          digit$276
        ];
        _tmp$1406 = (uint16_t)_tmp$1407;
        buffer$277[_tmp$1405] = _tmp$1406;
        _tmp$1409 = n$270;
        n$270 = _tmp$1409 >> (shift$274 & 31);
        continue;
      } else {
        moonbit_decref(buffer$277);
      }
      break;
    }
  } else {
    while (1) {
      uint32_t _tmp$1412 = n$270;
      if (_tmp$1412 > 0u) {
        int32_t _tmp$1413 = offset$267;
        uint32_t _tmp$1421;
        uint32_t q$279;
        uint32_t _tmp$1419;
        uint32_t _tmp$1420;
        uint32_t _tmp$1418;
        int32_t digit$280;
        int32_t _tmp$1417;
        int32_t _tmp$1414;
        int32_t _tmp$1416;
        int32_t _tmp$1415;
        offset$267 = _tmp$1413 - 1;
        _tmp$1421 = n$270;
        q$279 = _tmp$1421 / base$272;
        _tmp$1419 = n$270;
        _tmp$1420 = q$279 * base$272;
        _tmp$1418 = _tmp$1419 - _tmp$1420;
        digit$280 = *(int32_t*)&_tmp$1418;
        _tmp$1417 = offset$267;
        _tmp$1414 = digit_start$269 + _tmp$1417;
        _tmp$1416
        = ((moonbit_string_t)moonbit_string_literal_21.data)[
          digit$280
        ];
        _tmp$1415 = (uint16_t)_tmp$1416;
        buffer$277[_tmp$1414] = _tmp$1415;
        n$270 = q$279;
        continue;
      } else {
        moonbit_decref(buffer$277);
      }
      break;
    }
  }
  return 0;
}

int32_t $moonbitlang$core$builtin$int_to_string_hex(
  uint16_t* buffer$264,
  uint32_t num$260,
  int32_t digit_start$258,
  int32_t total_len$257
) {
  int32_t offset$256 = total_len$257 - digit_start$258;
  uint32_t n$259 = num$260;
  int32_t _tmp$1396;
  while (1) {
    int32_t _tmp$1382 = offset$256;
    if (_tmp$1382 >= 2) {
      int32_t _tmp$1383 = offset$256;
      uint32_t _tmp$1395;
      uint32_t _tmp$1394;
      int32_t byte_val$261;
      int32_t hi$262;
      int32_t lo$263;
      int32_t _tmp$1387;
      int32_t _tmp$1384;
      int32_t _tmp$1386;
      int32_t _tmp$1385;
      int32_t _tmp$1392;
      int32_t _tmp$1391;
      int32_t _tmp$1388;
      int32_t _tmp$1390;
      int32_t _tmp$1389;
      uint32_t _tmp$1393;
      offset$256 = _tmp$1383 - 2;
      _tmp$1395 = n$259;
      _tmp$1394 = _tmp$1395 & 255u;
      byte_val$261 = *(int32_t*)&_tmp$1394;
      hi$262 = byte_val$261 / 16;
      lo$263 = byte_val$261 % 16;
      _tmp$1387 = offset$256;
      _tmp$1384 = digit_start$258 + _tmp$1387;
      _tmp$1386 = ((moonbit_string_t)moonbit_string_literal_21.data)[hi$262];
      _tmp$1385 = (uint16_t)_tmp$1386;
      buffer$264[_tmp$1384] = _tmp$1385;
      _tmp$1392 = offset$256;
      _tmp$1391 = digit_start$258 + _tmp$1392;
      _tmp$1388 = _tmp$1391 + 1;
      _tmp$1390 = ((moonbit_string_t)moonbit_string_literal_21.data)[lo$263];
      _tmp$1389 = (uint16_t)_tmp$1390;
      buffer$264[_tmp$1388] = _tmp$1389;
      _tmp$1393 = n$259;
      n$259 = _tmp$1393 >> 8;
      continue;
    }
    break;
  }
  _tmp$1396 = offset$256;
  if (_tmp$1396 == 1) {
    uint32_t _tmp$1400 = n$259;
    uint32_t _tmp$1399 = _tmp$1400 & 15u;
    int32_t nibble$266 = *(int32_t*)&_tmp$1399;
    int32_t _tmp$1398 =
      ((moonbit_string_t)moonbit_string_literal_21.data)[nibble$266];
    int32_t _tmp$1397 = (uint16_t)_tmp$1398;
    buffer$264[digit_start$258] = _tmp$1397;
    moonbit_decref(buffer$264);
  } else {
    moonbit_decref(buffer$264);
  }
  return 0;
}

moonbit_string_t $$moonbitlang$core$builtin$Show$$$default_impl$$to_string$3(
  void* self$255
) {
  struct $$moonbitlang$core$builtin$StringBuilder* logger$254 =
    $$moonbitlang$core$builtin$StringBuilder$$new$inner(0);
  struct $$moonbitlang$core$builtin$Logger _tmp$1381;
  moonbit_incref(logger$254);
  _tmp$1381
  = (struct $$moonbitlang$core$builtin$Logger){
    $$moonbitlang$core$builtin$StringBuilder$as_$moonbitlang$core$builtin$Logger$static_method_table_id,
      logger$254
  };
  $$moonbitlang$core$builtin$Show$$$moonbitlang$core$builtin$CreatingViewError$$output(
    self$255, _tmp$1381
  );
  return $$moonbitlang$core$builtin$StringBuilder$$to_string(logger$254);
}

moonbit_string_t $$moonbitlang$core$builtin$Show$$$default_impl$$to_string$2(
  void* self$253
) {
  struct $$moonbitlang$core$builtin$StringBuilder* logger$252 =
    $$moonbitlang$core$builtin$StringBuilder$$new$inner(0);
  struct $$moonbitlang$core$builtin$Logger _tmp$1380;
  moonbit_incref(logger$252);
  _tmp$1380
  = (struct $$moonbitlang$core$builtin$Logger){
    $$moonbitlang$core$builtin$StringBuilder$as_$moonbitlang$core$builtin$Logger$static_method_table_id,
      logger$252
  };
  $$moonbitlang$core$builtin$Show$$$moonbitlang$core$builtin$Failure$$output(
    self$253, _tmp$1380
  );
  return $$moonbitlang$core$builtin$StringBuilder$$to_string(logger$252);
}

moonbit_string_t $$moonbitlang$core$builtin$Show$$$default_impl$$to_string$1(
  moonbit_string_t self$251
) {
  struct $$moonbitlang$core$builtin$StringBuilder* logger$250 =
    $$moonbitlang$core$builtin$StringBuilder$$new$inner(0);
  struct $$moonbitlang$core$builtin$Logger _tmp$1379;
  moonbit_incref(logger$250);
  _tmp$1379
  = (struct $$moonbitlang$core$builtin$Logger){
    $$moonbitlang$core$builtin$StringBuilder$as_$moonbitlang$core$builtin$Logger$static_method_table_id,
      logger$250
  };
  $$moonbitlang$core$builtin$Show$$$moonbitlang$core$builtin$SourceLoc$$output(
    self$251, _tmp$1379
  );
  return $$moonbitlang$core$builtin$StringBuilder$$to_string(logger$250);
}

moonbit_string_t $$moonbitlang$core$builtin$Show$$$default_impl$$to_string$0(
  int32_t self$249
) {
  struct $$moonbitlang$core$builtin$StringBuilder* logger$248 =
    $$moonbitlang$core$builtin$StringBuilder$$new$inner(0);
  struct $$moonbitlang$core$builtin$Logger _tmp$1378;
  moonbit_incref(logger$248);
  _tmp$1378
  = (struct $$moonbitlang$core$builtin$Logger){
    $$moonbitlang$core$builtin$StringBuilder$as_$moonbitlang$core$builtin$Logger$static_method_table_id,
      logger$248
  };
  $$moonbitlang$core$builtin$Show$$Int$$output(self$249, _tmp$1378);
  return $$moonbitlang$core$builtin$StringBuilder$$to_string(logger$248);
}

int32_t $StringView$$start_offset(struct $StringView self$247) {
  int32_t _field$2560 = self$247.$1;
  moonbit_decref(self$247.$0);
  return _field$2560;
}

int32_t $StringView$$length(struct $StringView self$246) {
  int32_t end$1376 = self$246.$2;
  int32_t _field$2561 = self$246.$1;
  int32_t start$1377;
  moonbit_decref(self$246.$0);
  start$1377 = _field$2561;
  return end$1376 - start$1377;
}

moonbit_string_t $StringView$$data(struct $StringView self$245) {
  moonbit_string_t _field$2562 = self$245.$0;
  return _field$2562;
}

int32_t $$moonbitlang$core$builtin$Logger$$$default_impl$$write_substring$0(
  struct $$moonbitlang$core$builtin$StringBuilder* self$239,
  moonbit_string_t value$242,
  int32_t start$243,
  int32_t len$244
) {
  void* _try_err$241;
  struct $StringView _tmp$1371;
  int32_t _tmp$1373 = start$243 + len$244;
  int64_t _tmp$1372 = (int64_t)_tmp$1373;
  struct moonbit_result_1 _tmp$2850 =
    $String$$sub$inner(value$242, start$243, _tmp$1372);
  if (_tmp$2850.tag) {
    struct $StringView const _ok$1374 = _tmp$2850.data.ok;
    _tmp$1371 = _ok$1374;
  } else {
    void* const _err$1375 = _tmp$2850.data.err;
    _try_err$241 = _err$1375;
    goto $join$240;
  }
  goto $joinlet$2849;
  $join$240:;
  moonbit_decref(_try_err$241);
  moonbit_panic();
  $joinlet$2849:;
  $$moonbitlang$core$builtin$Logger$$$moonbitlang$core$builtin$StringBuilder$$write_view(
    self$239, _tmp$1371
  );
  return 0;
}

struct moonbit_result_1 $String$$sub(
  moonbit_string_t self$237,
  int64_t start$opt$235,
  int64_t end$238
) {
  int32_t start$234;
  if (start$opt$235 == 4294967296ll) {
    start$234 = 0;
  } else {
    int64_t _Some$236 = start$opt$235;
    start$234 = (int32_t)_Some$236;
  }
  return $String$$sub$inner(self$237, start$234, end$238);
}

struct moonbit_result_1 $String$$sub$inner(
  moonbit_string_t self$227,
  int32_t start$233,
  int64_t end$229
) {
  int32_t len$226 = Moonbit_array_length(self$227);
  int32_t end$228;
  int32_t start$232;
  int32_t _if_result$2851;
  if (end$229 == 4294967296ll) {
    end$228 = len$226;
  } else {
    int64_t _Some$230 = end$229;
    int32_t _end$231 = (int32_t)_Some$230;
    if (_end$231 < 0) {
      end$228 = len$226 + _end$231;
    } else {
      end$228 = _end$231;
    }
  }
  if (start$233 < 0) {
    start$232 = len$226 + start$233;
  } else {
    start$232 = start$233;
  }
  if (start$232 >= 0) {
    if (start$232 <= end$228) {
      _if_result$2851 = end$228 <= len$226;
    } else {
      _if_result$2851 = 0;
    }
  } else {
    _if_result$2851 = 0;
  }
  if (_if_result$2851) {
    int32_t _if_result$2852;
    int32_t _if_result$2854;
    struct $StringView _tmp$1369;
    struct moonbit_result_1 _result$2856;
    if (start$232 < len$226) {
      int32_t _tmp$1365 = self$227[start$232];
      _if_result$2852 = $Int$$is_trailing_surrogate(_tmp$1365);
    } else {
      _if_result$2852 = 0;
    }
    if (_if_result$2852) {
      void* moonbitlang$core$builtin$CreatingViewError$InvalidIndex$1366;
      struct moonbit_result_1 _result$2853;
      moonbit_decref(self$227);
      moonbitlang$core$builtin$CreatingViewError$InvalidIndex$1366
      = (struct moonbit_object*)&moonbit_constant_constructor_0 + 1;
      _result$2853.tag = 0;
      _result$2853.data.err
      = moonbitlang$core$builtin$CreatingViewError$InvalidIndex$1366;
      return _result$2853;
    }
    if (end$228 < len$226) {
      int32_t _tmp$1367 = self$227[end$228];
      _if_result$2854 = $Int$$is_trailing_surrogate(_tmp$1367);
    } else {
      _if_result$2854 = 0;
    }
    if (_if_result$2854) {
      void* moonbitlang$core$builtin$CreatingViewError$InvalidIndex$1368;
      struct moonbit_result_1 _result$2855;
      moonbit_decref(self$227);
      moonbitlang$core$builtin$CreatingViewError$InvalidIndex$1368
      = (struct moonbit_object*)&moonbit_constant_constructor_0 + 1;
      _result$2855.tag = 0;
      _result$2855.data.err
      = moonbitlang$core$builtin$CreatingViewError$InvalidIndex$1368;
      return _result$2855;
    }
    _tmp$1369 = (struct $StringView){start$232, end$228, self$227};
    _result$2856.tag = 1;
    _result$2856.data.ok = _tmp$1369;
    return _result$2856;
  } else {
    void* moonbitlang$core$builtin$CreatingViewError$IndexOutOfBounds$1370;
    struct moonbit_result_1 _result$2857;
    moonbit_decref(self$227);
    moonbitlang$core$builtin$CreatingViewError$IndexOutOfBounds$1370
    = (struct moonbit_object*)&moonbit_constant_constructor_1 + 1;
    _result$2857.tag = 0;
    _result$2857.data.err
    = moonbitlang$core$builtin$CreatingViewError$IndexOutOfBounds$1370;
    return _result$2857;
  }
}

int32_t $$moonbitlang$core$builtin$Hash$$$default_impl$$hash$1(
  moonbit_string_t self$225
) {
  struct $$moonbitlang$core$builtin$Hasher* _self$224 =
    $$moonbitlang$core$builtin$Hasher$$new(4294967296ll);
  struct $$moonbitlang$core$builtin$Hasher* _tmp$1364;
  moonbit_incref(_self$224);
  $$moonbitlang$core$builtin$Hasher$$combine$1(_self$224, self$225);
  _tmp$1364 = _self$224;
  return $$moonbitlang$core$builtin$Hasher$$finalize(_tmp$1364);
}

int32_t $$moonbitlang$core$builtin$Hash$$$default_impl$$hash$0(
  int32_t self$223
) {
  struct $$moonbitlang$core$builtin$Hasher* _self$222 =
    $$moonbitlang$core$builtin$Hasher$$new(4294967296ll);
  struct $$moonbitlang$core$builtin$Hasher* _tmp$1363;
  moonbit_incref(_self$222);
  $$moonbitlang$core$builtin$Hasher$$combine$0(_self$222, self$223);
  _tmp$1363 = _self$222;
  return $$moonbitlang$core$builtin$Hasher$$finalize(_tmp$1363);
}

struct $$moonbitlang$core$builtin$Hasher* $$moonbitlang$core$builtin$Hasher$$new(
  int64_t seed$opt$220
) {
  int32_t seed$219;
  if (seed$opt$220 == 4294967296ll) {
    seed$219 = 0;
  } else {
    int64_t _Some$221 = seed$opt$220;
    seed$219 = (int32_t)_Some$221;
  }
  return $$moonbitlang$core$builtin$Hasher$$new$inner(seed$219);
}

struct $$moonbitlang$core$builtin$Hasher* $$moonbitlang$core$builtin$Hasher$$new$inner(
  int32_t seed$218
) {
  uint32_t _tmp$1362 = *(uint32_t*)&seed$218;
  uint32_t _tmp$1361 = _tmp$1362 + 374761393u;
  struct $$moonbitlang$core$builtin$Hasher* _block$2858 =
    (struct $$moonbitlang$core$builtin$Hasher*)moonbit_malloc(
      sizeof(struct $$moonbitlang$core$builtin$Hasher)
    );
  Moonbit_object_header(_block$2858)->meta
  = Moonbit_make_regular_object_header(
    sizeof(struct $$moonbitlang$core$builtin$Hasher) >> 2, 0, 0
  );
  _block$2858->$0 = _tmp$1361;
  return _block$2858;
}

int32_t $$moonbitlang$core$builtin$Hasher$$finalize(
  struct $$moonbitlang$core$builtin$Hasher* self$217
) {
  uint32_t _tmp$1360 = $$moonbitlang$core$builtin$Hasher$$avalanche(self$217);
  return *(int32_t*)&_tmp$1360;
}

uint32_t $$moonbitlang$core$builtin$Hasher$$avalanche(
  struct $$moonbitlang$core$builtin$Hasher* self$216
) {
  uint32_t _field$2563 = self$216->$0;
  uint32_t acc$215;
  uint32_t _tmp$1349;
  uint32_t _tmp$1351;
  uint32_t _tmp$1350;
  uint32_t _tmp$1352;
  uint32_t _tmp$1353;
  uint32_t _tmp$1355;
  uint32_t _tmp$1354;
  uint32_t _tmp$1356;
  uint32_t _tmp$1357;
  uint32_t _tmp$1359;
  uint32_t _tmp$1358;
  moonbit_decref(self$216);
  acc$215 = _field$2563;
  _tmp$1349 = acc$215;
  _tmp$1351 = acc$215;
  _tmp$1350 = _tmp$1351 >> 15;
  acc$215 = _tmp$1349 ^ _tmp$1350;
  _tmp$1352 = acc$215;
  acc$215 = _tmp$1352 * 2246822519u;
  _tmp$1353 = acc$215;
  _tmp$1355 = acc$215;
  _tmp$1354 = _tmp$1355 >> 13;
  acc$215 = _tmp$1353 ^ _tmp$1354;
  _tmp$1356 = acc$215;
  acc$215 = _tmp$1356 * 3266489917u;
  _tmp$1357 = acc$215;
  _tmp$1359 = acc$215;
  _tmp$1358 = _tmp$1359 >> 16;
  acc$215 = _tmp$1357 ^ _tmp$1358;
  return acc$215;
}

int32_t $$moonbitlang$core$builtin$Hasher$$combine$1(
  struct $$moonbitlang$core$builtin$Hasher* self$214,
  moonbit_string_t value$213
) {
  $$moonbitlang$core$builtin$Hash$$String$$hash_combine(value$213, self$214);
  return 0;
}

int32_t $$moonbitlang$core$builtin$Hasher$$combine$0(
  struct $$moonbitlang$core$builtin$Hasher* self$212,
  int32_t value$211
) {
  $$moonbitlang$core$builtin$Hash$$Int$$hash_combine(value$211, self$212);
  return 0;
}

int32_t $$moonbitlang$core$builtin$Hasher$$combine_int(
  struct $$moonbitlang$core$builtin$Hasher* self$209,
  int32_t value$210
) {
  uint32_t _tmp$1348 = *(uint32_t*)&value$210;
  $$moonbitlang$core$builtin$Hasher$$combine_uint(self$209, _tmp$1348);
  return 0;
}

int32_t $$moonbitlang$core$builtin$Logger$$$moonbitlang$core$builtin$StringBuilder$$write_string(
  struct $$moonbitlang$core$builtin$StringBuilder* self$207,
  moonbit_string_t str$208
) {
  int32_t len$1338 = self$207->$1;
  int32_t _tmp$1340 = Moonbit_array_length(str$208);
  int32_t _tmp$1339 = _tmp$1340 * 2;
  int32_t _tmp$1337 = len$1338 + _tmp$1339;
  moonbit_bytes_t _field$2565;
  moonbit_bytes_t data$1341;
  int32_t len$1342;
  int32_t _tmp$1343;
  int32_t len$1345;
  int32_t _tmp$2564;
  int32_t _tmp$1347;
  int32_t _tmp$1346;
  int32_t _tmp$1344;
  moonbit_incref(self$207);
  $$moonbitlang$core$builtin$StringBuilder$$grow_if_necessary(
    self$207, _tmp$1337
  );
  _field$2565 = self$207->$0;
  data$1341 = _field$2565;
  len$1342 = self$207->$1;
  _tmp$1343 = Moonbit_array_length(str$208);
  moonbit_incref(data$1341);
  moonbit_incref(str$208);
  $FixedArray$$blit_from_string(data$1341, len$1342, str$208, 0, _tmp$1343);
  len$1345 = self$207->$1;
  _tmp$2564 = Moonbit_array_length(str$208);
  moonbit_decref(str$208);
  _tmp$1347 = _tmp$2564;
  _tmp$1346 = _tmp$1347 * 2;
  _tmp$1344 = len$1345 + _tmp$1346;
  self$207->$1 = _tmp$1344;
  moonbit_decref(self$207);
  return 0;
}

int32_t $FixedArray$$blit_from_string(
  moonbit_bytes_t self$199,
  int32_t bytes_offset$194,
  moonbit_string_t str$201,
  int32_t str_offset$197,
  int32_t length$195
) {
  int32_t _tmp$1336 = length$195 * 2;
  int32_t _tmp$1335 = bytes_offset$194 + _tmp$1336;
  int32_t e1$193 = _tmp$1335 - 1;
  int32_t _tmp$1334 = str_offset$197 + length$195;
  int32_t e2$196 = _tmp$1334 - 1;
  int32_t len1$198 = Moonbit_array_length(self$199);
  int32_t len2$200 = Moonbit_array_length(str$201);
  int32_t _if_result$2859;
  if (length$195 >= 0) {
    if (bytes_offset$194 >= 0) {
      if (e1$193 < len1$198) {
        if (str_offset$197 >= 0) {
          _if_result$2859 = e2$196 < len2$200;
        } else {
          _if_result$2859 = 0;
        }
      } else {
        _if_result$2859 = 0;
      }
    } else {
      _if_result$2859 = 0;
    }
  } else {
    _if_result$2859 = 0;
  }
  if (_if_result$2859) {
    int32_t end_str_offset$202 = str_offset$197 + length$195;
    int32_t i$203 = str_offset$197;
    int32_t j$204 = bytes_offset$194;
    while (1) {
      if (i$203 < end_str_offset$202) {
        int32_t _tmp$1331 = str$201[i$203];
        uint32_t c$205 = *(uint32_t*)&_tmp$1331;
        uint32_t _tmp$1327 = c$205 & 255u;
        int32_t _tmp$1326 = $UInt$$to_byte(_tmp$1327);
        int32_t _tmp$1328;
        uint32_t _tmp$1330;
        int32_t _tmp$1329;
        int32_t _tmp$1332;
        int32_t _tmp$1333;
        if (j$204 < 0 || j$204 >= Moonbit_array_length(self$199)) {
          moonbit_panic();
        }
        self$199[j$204] = _tmp$1326;
        _tmp$1328 = j$204 + 1;
        _tmp$1330 = c$205 >> 8;
        _tmp$1329 = $UInt$$to_byte(_tmp$1330);
        if (_tmp$1328 < 0 || _tmp$1328 >= Moonbit_array_length(self$199)) {
          moonbit_panic();
        }
        self$199[_tmp$1328] = _tmp$1329;
        _tmp$1332 = i$203 + 1;
        _tmp$1333 = j$204 + 2;
        i$203 = _tmp$1332;
        j$204 = _tmp$1333;
        continue;
      } else {
        moonbit_decref(str$201);
        moonbit_decref(self$199);
      }
      break;
    }
  } else {
    moonbit_decref(str$201);
    moonbit_decref(self$199);
    moonbit_panic();
  }
  return 0;
}

struct $$moonbitlang$core$builtin$SourceLocRepr* $$moonbitlang$core$builtin$SourceLocRepr$$parse(
  moonbit_string_t repr$116
) {
  int32_t _tmp$1325 = Moonbit_array_length(repr$116);
  struct $StringView _bind$115 = (struct $StringView){0, _tmp$1325, repr$116};
  moonbit_string_t _data$117;
  int32_t _start$118;
  int32_t _tmp$1324;
  int32_t _end$119;
  int32_t _cursor$120;
  int32_t accept_state$121;
  int32_t match_end$122;
  int32_t match_tag_saver_0$123;
  int32_t match_tag_saver_1$124;
  int32_t match_tag_saver_2$125;
  int32_t match_tag_saver_3$126;
  int32_t match_tag_saver_4$127;
  int32_t tag_0$128;
  int32_t tag_1$129;
  int32_t tag_1_1$130;
  int32_t tag_1_2$131;
  int32_t tag_3$132;
  int32_t tag_2$133;
  int32_t tag_2_1$134;
  int32_t tag_4$135;
  int32_t join_dispatch_19$156;
  int32_t _tmp$1309;
  int32_t dispatch_19$157;
  moonbit_incref(_bind$115.$0);
  _data$117 = $StringView$$data(_bind$115);
  moonbit_incref(_bind$115.$0);
  _start$118 = $StringView$$start_offset(_bind$115);
  _tmp$1324 = $StringView$$length(_bind$115);
  _end$119 = _start$118 + _tmp$1324;
  _cursor$120 = _start$118;
  accept_state$121 = -1;
  match_end$122 = -1;
  match_tag_saver_0$123 = -1;
  match_tag_saver_1$124 = -1;
  match_tag_saver_2$125 = -1;
  match_tag_saver_3$126 = -1;
  match_tag_saver_4$127 = -1;
  tag_0$128 = -1;
  tag_1$129 = -1;
  tag_1_1$130 = -1;
  tag_1_2$131 = -1;
  tag_3$132 = -1;
  tag_2$133 = -1;
  tag_2_1$134 = -1;
  tag_4$135 = -1;
  _tmp$1309 = _cursor$120;
  if (_tmp$1309 < _end$119) {
    int32_t _tmp$1323 = _cursor$120;
    int32_t next_char$185;
    int32_t _tmp$1310;
    moonbit_incref(_data$117);
    next_char$185 = $String$$unsafe_charcode_at(_data$117, _tmp$1323);
    _tmp$1310 = _cursor$120;
    _cursor$120 = _tmp$1310 + 1;
    if (next_char$185 < 65) {
      if (next_char$185 < 64) {
        goto $join$136;
      } else {
        while (1) {
          int32_t _tmp$1311;
          tag_0$128 = _cursor$120;
          _tmp$1311 = _cursor$120;
          if (_tmp$1311 < _end$119) {
            int32_t _tmp$1322 = _cursor$120;
            int32_t next_char$188;
            int32_t _tmp$1312;
            moonbit_incref(_data$117);
            next_char$188 = $String$$unsafe_charcode_at(_data$117, _tmp$1322);
            _tmp$1312 = _cursor$120;
            _cursor$120 = _tmp$1312 + 1;
            if (next_char$188 < 55296) {
              if (next_char$188 < 58) {
                goto $join$186;
              } else if (next_char$188 > 58) {
                goto $join$186;
              } else {
                int32_t _tmp$1313 = _cursor$120;
                if (_tmp$1313 < _end$119) {
                  int32_t _tmp$1315 = _cursor$120;
                  int32_t next_char$190;
                  int32_t _tmp$1314;
                  moonbit_incref(_data$117);
                  next_char$190
                  = $String$$unsafe_charcode_at(
                    _data$117, _tmp$1315
                  );
                  _tmp$1314 = _cursor$120;
                  _cursor$120 = _tmp$1314 + 1;
                  if (next_char$190 < 56319) {
                    if (next_char$190 < 55296) {
                      goto $join$189;
                    } else {
                      join_dispatch_19$156 = 7;
                      goto $join$155;
                    }
                  } else if (next_char$190 > 56319) {
                    if (next_char$190 < 65536) {
                      goto $join$189;
                    } else {
                      goto $join$136;
                    }
                  } else {
                    join_dispatch_19$156 = 8;
                    goto $join$155;
                  }
                  $join$189:;
                  join_dispatch_19$156 = 0;
                  goto $join$155;
                } else {
                  goto $join$136;
                }
              }
            } else if (next_char$188 > 56318) {
              if (next_char$188 < 57344) {
                int32_t _tmp$1316 = _cursor$120;
                if (_tmp$1316 < _end$119) {
                  int32_t _tmp$1318 = _cursor$120;
                  int32_t next_char$191;
                  int32_t _tmp$1317;
                  moonbit_incref(_data$117);
                  next_char$191
                  = $String$$unsafe_charcode_at(
                    _data$117, _tmp$1318
                  );
                  _tmp$1317 = _cursor$120;
                  _cursor$120 = _tmp$1317 + 1;
                  if (next_char$191 < 56320) {
                    goto $join$136;
                  } else if (next_char$191 > 57343) {
                    goto $join$136;
                  } else {
                    continue;
                  }
                } else {
                  goto $join$136;
                }
              } else if (next_char$188 > 65535) {
                goto $join$136;
              } else {
                goto $join$186;
              }
            } else {
              int32_t _tmp$1319 = _cursor$120;
              if (_tmp$1319 < _end$119) {
                int32_t _tmp$1321 = _cursor$120;
                int32_t next_char$192;
                int32_t _tmp$1320;
                moonbit_incref(_data$117);
                next_char$192
                = $String$$unsafe_charcode_at(
                  _data$117, _tmp$1321
                );
                _tmp$1320 = _cursor$120;
                _cursor$120 = _tmp$1320 + 1;
                if (next_char$192 < 56320) {
                  goto $join$136;
                } else if (next_char$192 > 65535) {
                  goto $join$136;
                } else {
                  continue;
                }
              } else {
                goto $join$136;
              }
            }
            $join$186:;
            continue;
          } else {
            goto $join$136;
          }
          break;
        }
      }
    } else {
      goto $join$136;
    }
  } else {
    goto $join$136;
  }
  $join$155:;
  dispatch_19$157 = join_dispatch_19$156;
  $loop_label_19$160:;
  while (1) {
    int32_t _tmp$1270;
    switch (dispatch_19$157) {
      case 3: {
        int32_t _tmp$1273;
        tag_1_2$131 = tag_1_1$130;
        tag_1_1$130 = tag_1$129;
        tag_1$129 = _cursor$120;
        _tmp$1273 = _cursor$120;
        if (_tmp$1273 < _end$119) {
          int32_t _tmp$1278 = _cursor$120;
          int32_t next_char$164;
          int32_t _tmp$1274;
          moonbit_incref(_data$117);
          next_char$164 = $String$$unsafe_charcode_at(_data$117, _tmp$1278);
          _tmp$1274 = _cursor$120;
          _cursor$120 = _tmp$1274 + 1;
          if (next_char$164 < 55296) {
            if (next_char$164 < 58) {
              if (next_char$164 < 48) {
                goto $join$163;
              } else {
                int32_t _tmp$1275;
                tag_1$129 = _cursor$120;
                tag_2_1$134 = tag_2$133;
                tag_2$133 = _cursor$120;
                tag_3$132 = _cursor$120;
                _tmp$1275 = _cursor$120;
                if (_tmp$1275 < _end$119) {
                  int32_t _tmp$1277 = _cursor$120;
                  int32_t next_char$166;
                  int32_t _tmp$1276;
                  moonbit_incref(_data$117);
                  next_char$166
                  = $String$$unsafe_charcode_at(
                    _data$117, _tmp$1277
                  );
                  _tmp$1276 = _cursor$120;
                  _cursor$120 = _tmp$1276 + 1;
                  if (next_char$166 < 59) {
                    if (next_char$166 < 46) {
                      if (next_char$166 < 45) {
                        goto $join$165;
                      } else {
                        goto $join$158;
                      }
                    } else if (next_char$166 > 47) {
                      if (next_char$166 < 58) {
                        dispatch_19$157 = 6;
                        goto $loop_label_19$160;
                      } else {
                        dispatch_19$157 = 3;
                        goto $loop_label_19$160;
                      }
                    } else {
                      goto $join$165;
                    }
                  } else if (next_char$166 > 55295) {
                    if (next_char$166 < 57344) {
                      if (next_char$166 < 56319) {
                        dispatch_19$157 = 7;
                        goto $loop_label_19$160;
                      } else {
                        dispatch_19$157 = 8;
                        goto $loop_label_19$160;
                      }
                    } else if (next_char$166 > 65535) {
                      goto $join$136;
                    } else {
                      goto $join$165;
                    }
                  } else {
                    goto $join$165;
                  }
                  $join$165:;
                  dispatch_19$157 = 0;
                  goto $loop_label_19$160;
                } else {
                  goto $join$136;
                }
              }
            } else if (next_char$164 > 58) {
              goto $join$163;
            } else {
              dispatch_19$157 = 1;
              goto $loop_label_19$160;
            }
          } else if (next_char$164 > 56318) {
            if (next_char$164 < 57344) {
              dispatch_19$157 = 8;
              goto $loop_label_19$160;
            } else if (next_char$164 > 65535) {
              goto $join$136;
            } else {
              goto $join$163;
            }
          } else {
            dispatch_19$157 = 7;
            goto $loop_label_19$160;
          }
          $join$163:;
          dispatch_19$157 = 0;
          goto $loop_label_19$160;
        } else {
          goto $join$136;
        }
        break;
      }
      
      case 2: {
        int32_t _tmp$1279;
        tag_1$129 = _cursor$120;
        tag_2$133 = _cursor$120;
        _tmp$1279 = _cursor$120;
        if (_tmp$1279 < _end$119) {
          int32_t _tmp$1281 = _cursor$120;
          int32_t next_char$168;
          int32_t _tmp$1280;
          moonbit_incref(_data$117);
          next_char$168 = $String$$unsafe_charcode_at(_data$117, _tmp$1281);
          _tmp$1280 = _cursor$120;
          _cursor$120 = _tmp$1280 + 1;
          if (next_char$168 < 55296) {
            if (next_char$168 < 58) {
              if (next_char$168 < 48) {
                goto $join$167;
              } else {
                dispatch_19$157 = 2;
                goto $loop_label_19$160;
              }
            } else if (next_char$168 > 58) {
              goto $join$167;
            } else {
              dispatch_19$157 = 3;
              goto $loop_label_19$160;
            }
          } else if (next_char$168 > 56318) {
            if (next_char$168 < 57344) {
              dispatch_19$157 = 8;
              goto $loop_label_19$160;
            } else if (next_char$168 > 65535) {
              goto $join$136;
            } else {
              goto $join$167;
            }
          } else {
            dispatch_19$157 = 7;
            goto $loop_label_19$160;
          }
          $join$167:;
          dispatch_19$157 = 0;
          goto $loop_label_19$160;
        } else {
          goto $join$136;
        }
        break;
      }
      
      case 0: {
        int32_t _tmp$1282;
        tag_1$129 = _cursor$120;
        _tmp$1282 = _cursor$120;
        if (_tmp$1282 < _end$119) {
          int32_t _tmp$1284 = _cursor$120;
          int32_t next_char$170;
          int32_t _tmp$1283;
          moonbit_incref(_data$117);
          next_char$170 = $String$$unsafe_charcode_at(_data$117, _tmp$1284);
          _tmp$1283 = _cursor$120;
          _cursor$120 = _tmp$1283 + 1;
          if (next_char$170 < 55296) {
            if (next_char$170 < 58) {
              goto $join$169;
            } else if (next_char$170 > 58) {
              goto $join$169;
            } else {
              dispatch_19$157 = 1;
              goto $loop_label_19$160;
            }
          } else if (next_char$170 > 56318) {
            if (next_char$170 < 57344) {
              dispatch_19$157 = 8;
              goto $loop_label_19$160;
            } else if (next_char$170 > 65535) {
              goto $join$136;
            } else {
              goto $join$169;
            }
          } else {
            dispatch_19$157 = 7;
            goto $loop_label_19$160;
          }
          $join$169:;
          dispatch_19$157 = 0;
          goto $loop_label_19$160;
        } else {
          goto $join$136;
        }
        break;
      }
      
      case 8: {
        int32_t _tmp$1285 = _cursor$120;
        if (_tmp$1285 < _end$119) {
          int32_t _tmp$1287 = _cursor$120;
          int32_t next_char$171;
          int32_t _tmp$1286;
          moonbit_incref(_data$117);
          next_char$171 = $String$$unsafe_charcode_at(_data$117, _tmp$1287);
          _tmp$1286 = _cursor$120;
          _cursor$120 = _tmp$1286 + 1;
          if (next_char$171 < 56320) {
            goto $join$136;
          } else if (next_char$171 > 57343) {
            goto $join$136;
          } else {
            dispatch_19$157 = 0;
            goto $loop_label_19$160;
          }
        } else {
          goto $join$136;
        }
        break;
      }
      
      case 4: {
        int32_t _tmp$1288;
        tag_1$129 = _cursor$120;
        tag_4$135 = _cursor$120;
        _tmp$1288 = _cursor$120;
        if (_tmp$1288 < _end$119) {
          int32_t _tmp$1296 = _cursor$120;
          int32_t next_char$173;
          int32_t _tmp$1289;
          moonbit_incref(_data$117);
          next_char$173 = $String$$unsafe_charcode_at(_data$117, _tmp$1296);
          _tmp$1289 = _cursor$120;
          _cursor$120 = _tmp$1289 + 1;
          if (next_char$173 < 55296) {
            if (next_char$173 < 58) {
              if (next_char$173 < 48) {
                goto $join$172;
              } else {
                dispatch_19$157 = 4;
                goto $loop_label_19$160;
              }
            } else if (next_char$173 > 58) {
              goto $join$172;
            } else {
              int32_t _tmp$1290;
              tag_1_2$131 = tag_1_1$130;
              tag_1_1$130 = tag_1$129;
              tag_1$129 = _cursor$120;
              _tmp$1290 = _cursor$120;
              if (_tmp$1290 < _end$119) {
                int32_t _tmp$1295 = _cursor$120;
                int32_t next_char$175;
                int32_t _tmp$1291;
                moonbit_incref(_data$117);
                next_char$175
                = $String$$unsafe_charcode_at(
                  _data$117, _tmp$1295
                );
                _tmp$1291 = _cursor$120;
                _cursor$120 = _tmp$1291 + 1;
                if (next_char$175 < 55296) {
                  if (next_char$175 < 58) {
                    if (next_char$175 < 48) {
                      goto $join$174;
                    } else {
                      int32_t _tmp$1292;
                      tag_1$129 = _cursor$120;
                      tag_2_1$134 = tag_2$133;
                      tag_2$133 = _cursor$120;
                      _tmp$1292 = _cursor$120;
                      if (_tmp$1292 < _end$119) {
                        int32_t _tmp$1294 = _cursor$120;
                        int32_t next_char$177;
                        int32_t _tmp$1293;
                        moonbit_incref(_data$117);
                        next_char$177
                        = $String$$unsafe_charcode_at(
                          _data$117, _tmp$1294
                        );
                        _tmp$1293 = _cursor$120;
                        _cursor$120 = _tmp$1293 + 1;
                        if (next_char$177 < 55296) {
                          if (next_char$177 < 58) {
                            if (next_char$177 < 48) {
                              goto $join$176;
                            } else {
                              dispatch_19$157 = 5;
                              goto $loop_label_19$160;
                            }
                          } else if (next_char$177 > 58) {
                            goto $join$176;
                          } else {
                            dispatch_19$157 = 3;
                            goto $loop_label_19$160;
                          }
                        } else if (next_char$177 > 56318) {
                          if (next_char$177 < 57344) {
                            dispatch_19$157 = 8;
                            goto $loop_label_19$160;
                          } else if (next_char$177 > 65535) {
                            goto $join$136;
                          } else {
                            goto $join$176;
                          }
                        } else {
                          dispatch_19$157 = 7;
                          goto $loop_label_19$160;
                        }
                        $join$176:;
                        dispatch_19$157 = 0;
                        goto $loop_label_19$160;
                      } else {
                        goto $join$162;
                      }
                    }
                  } else if (next_char$175 > 58) {
                    goto $join$174;
                  } else {
                    dispatch_19$157 = 1;
                    goto $loop_label_19$160;
                  }
                } else if (next_char$175 > 56318) {
                  if (next_char$175 < 57344) {
                    dispatch_19$157 = 8;
                    goto $loop_label_19$160;
                  } else if (next_char$175 > 65535) {
                    goto $join$136;
                  } else {
                    goto $join$174;
                  }
                } else {
                  dispatch_19$157 = 7;
                  goto $loop_label_19$160;
                }
                $join$174:;
                dispatch_19$157 = 0;
                goto $loop_label_19$160;
              } else {
                goto $join$136;
              }
            }
          } else if (next_char$173 > 56318) {
            if (next_char$173 < 57344) {
              dispatch_19$157 = 8;
              goto $loop_label_19$160;
            } else if (next_char$173 > 65535) {
              goto $join$136;
            } else {
              goto $join$172;
            }
          } else {
            dispatch_19$157 = 7;
            goto $loop_label_19$160;
          }
          $join$172:;
          dispatch_19$157 = 0;
          goto $loop_label_19$160;
        } else {
          goto $join$136;
        }
        break;
      }
      
      case 5: {
        int32_t _tmp$1297;
        tag_1$129 = _cursor$120;
        tag_2$133 = _cursor$120;
        _tmp$1297 = _cursor$120;
        if (_tmp$1297 < _end$119) {
          int32_t _tmp$1299 = _cursor$120;
          int32_t next_char$179;
          int32_t _tmp$1298;
          moonbit_incref(_data$117);
          next_char$179 = $String$$unsafe_charcode_at(_data$117, _tmp$1299);
          _tmp$1298 = _cursor$120;
          _cursor$120 = _tmp$1298 + 1;
          if (next_char$179 < 55296) {
            if (next_char$179 < 58) {
              if (next_char$179 < 48) {
                goto $join$178;
              } else {
                dispatch_19$157 = 5;
                goto $loop_label_19$160;
              }
            } else if (next_char$179 > 58) {
              goto $join$178;
            } else {
              dispatch_19$157 = 3;
              goto $loop_label_19$160;
            }
          } else if (next_char$179 > 56318) {
            if (next_char$179 < 57344) {
              dispatch_19$157 = 8;
              goto $loop_label_19$160;
            } else if (next_char$179 > 65535) {
              goto $join$136;
            } else {
              goto $join$178;
            }
          } else {
            dispatch_19$157 = 7;
            goto $loop_label_19$160;
          }
          $join$178:;
          dispatch_19$157 = 0;
          goto $loop_label_19$160;
        } else {
          goto $join$162;
        }
        break;
      }
      
      case 6: {
        int32_t _tmp$1300;
        tag_1$129 = _cursor$120;
        tag_2$133 = _cursor$120;
        tag_3$132 = _cursor$120;
        _tmp$1300 = _cursor$120;
        if (_tmp$1300 < _end$119) {
          int32_t _tmp$1302 = _cursor$120;
          int32_t next_char$181;
          int32_t _tmp$1301;
          moonbit_incref(_data$117);
          next_char$181 = $String$$unsafe_charcode_at(_data$117, _tmp$1302);
          _tmp$1301 = _cursor$120;
          _cursor$120 = _tmp$1301 + 1;
          if (next_char$181 < 59) {
            if (next_char$181 < 46) {
              if (next_char$181 < 45) {
                goto $join$180;
              } else {
                goto $join$158;
              }
            } else if (next_char$181 > 47) {
              if (next_char$181 < 58) {
                dispatch_19$157 = 6;
                goto $loop_label_19$160;
              } else {
                dispatch_19$157 = 3;
                goto $loop_label_19$160;
              }
            } else {
              goto $join$180;
            }
          } else if (next_char$181 > 55295) {
            if (next_char$181 < 57344) {
              if (next_char$181 < 56319) {
                dispatch_19$157 = 7;
                goto $loop_label_19$160;
              } else {
                dispatch_19$157 = 8;
                goto $loop_label_19$160;
              }
            } else if (next_char$181 > 65535) {
              goto $join$136;
            } else {
              goto $join$180;
            }
          } else {
            goto $join$180;
          }
          $join$180:;
          dispatch_19$157 = 0;
          goto $loop_label_19$160;
        } else {
          goto $join$136;
        }
        break;
      }
      
      case 7: {
        int32_t _tmp$1303 = _cursor$120;
        if (_tmp$1303 < _end$119) {
          int32_t _tmp$1305 = _cursor$120;
          int32_t next_char$182;
          int32_t _tmp$1304;
          moonbit_incref(_data$117);
          next_char$182 = $String$$unsafe_charcode_at(_data$117, _tmp$1305);
          _tmp$1304 = _cursor$120;
          _cursor$120 = _tmp$1304 + 1;
          if (next_char$182 < 56320) {
            goto $join$136;
          } else if (next_char$182 > 65535) {
            goto $join$136;
          } else {
            dispatch_19$157 = 0;
            goto $loop_label_19$160;
          }
        } else {
          goto $join$136;
        }
        break;
      }
      
      case 1: {
        int32_t _tmp$1306;
        tag_1_1$130 = tag_1$129;
        tag_1$129 = _cursor$120;
        _tmp$1306 = _cursor$120;
        if (_tmp$1306 < _end$119) {
          int32_t _tmp$1308 = _cursor$120;
          int32_t next_char$184;
          int32_t _tmp$1307;
          moonbit_incref(_data$117);
          next_char$184 = $String$$unsafe_charcode_at(_data$117, _tmp$1308);
          _tmp$1307 = _cursor$120;
          _cursor$120 = _tmp$1307 + 1;
          if (next_char$184 < 55296) {
            if (next_char$184 < 58) {
              if (next_char$184 < 48) {
                goto $join$183;
              } else {
                dispatch_19$157 = 2;
                goto $loop_label_19$160;
              }
            } else if (next_char$184 > 58) {
              goto $join$183;
            } else {
              dispatch_19$157 = 1;
              goto $loop_label_19$160;
            }
          } else if (next_char$184 > 56318) {
            if (next_char$184 < 57344) {
              dispatch_19$157 = 8;
              goto $loop_label_19$160;
            } else if (next_char$184 > 65535) {
              goto $join$136;
            } else {
              goto $join$183;
            }
          } else {
            dispatch_19$157 = 7;
            goto $loop_label_19$160;
          }
          $join$183:;
          dispatch_19$157 = 0;
          goto $loop_label_19$160;
        } else {
          goto $join$136;
        }
        break;
      }
      default: {
        goto $join$136;
        break;
      }
    }
    $join$162:;
    tag_1$129 = tag_1_2$131;
    tag_2$133 = tag_2_1$134;
    match_tag_saver_0$123 = tag_0$128;
    match_tag_saver_1$124 = tag_1$129;
    match_tag_saver_2$125 = tag_2$133;
    match_tag_saver_3$126 = tag_3$132;
    match_tag_saver_4$127 = tag_4$135;
    accept_state$121 = 0;
    match_end$122 = _cursor$120;
    goto $join$136;
    $join$158:;
    tag_1_1$130 = tag_1_2$131;
    tag_1$129 = _cursor$120;
    tag_2$133 = tag_2_1$134;
    _tmp$1270 = _cursor$120;
    if (_tmp$1270 < _end$119) {
      int32_t _tmp$1272 = _cursor$120;
      int32_t next_char$161;
      int32_t _tmp$1271;
      moonbit_incref(_data$117);
      next_char$161 = $String$$unsafe_charcode_at(_data$117, _tmp$1272);
      _tmp$1271 = _cursor$120;
      _cursor$120 = _tmp$1271 + 1;
      if (next_char$161 < 55296) {
        if (next_char$161 < 58) {
          if (next_char$161 < 48) {
            goto $join$159;
          } else {
            dispatch_19$157 = 4;
            continue;
          }
        } else if (next_char$161 > 58) {
          goto $join$159;
        } else {
          dispatch_19$157 = 1;
          continue;
        }
      } else if (next_char$161 > 56318) {
        if (next_char$161 < 57344) {
          dispatch_19$157 = 8;
          continue;
        } else if (next_char$161 > 65535) {
          goto $join$136;
        } else {
          goto $join$159;
        }
      } else {
        dispatch_19$157 = 7;
        continue;
      }
      $join$159:;
      dispatch_19$157 = 0;
      continue;
    } else {
      goto $join$136;
    }
    break;
  }
  $join$136:;
  switch (accept_state$121) {
    case 0: {
      void* _try_err$139;
      struct $StringView start_line$137;
      int32_t _tmp$1267 = match_tag_saver_1$124;
      int32_t _tmp$1266 = _tmp$1267 + 1;
      int64_t _tmp$1263 = (int64_t)_tmp$1266;
      int32_t _tmp$1265 = match_tag_saver_2$125;
      int64_t _tmp$1264 = (int64_t)_tmp$1265;
      struct moonbit_result_1 _tmp$2881;
      void* _try_err$142;
      struct $StringView start_column$140;
      int32_t _tmp$1260;
      int32_t _tmp$1259;
      int64_t _tmp$1256;
      int32_t _tmp$1258;
      int64_t _tmp$1257;
      struct moonbit_result_1 _tmp$2883;
      void* _try_err$145;
      struct $StringView pkg$143;
      int32_t _tmp$1253;
      int64_t _tmp$1250;
      int32_t _tmp$1252;
      int64_t _tmp$1251;
      struct moonbit_result_1 _tmp$2885;
      void* _try_err$148;
      struct $StringView filename$146;
      int32_t _tmp$1247;
      int32_t _tmp$1246;
      int64_t _tmp$1243;
      int32_t _tmp$1245;
      int64_t _tmp$1244;
      struct moonbit_result_1 _tmp$2887;
      void* _try_err$151;
      struct $StringView end_line$149;
      int32_t _tmp$1240;
      int32_t _tmp$1239;
      int64_t _tmp$1236;
      int32_t _tmp$1238;
      int64_t _tmp$1237;
      struct moonbit_result_1 _tmp$2889;
      void* _try_err$154;
      struct $StringView end_column$152;
      int32_t _tmp$1233;
      int32_t _tmp$1232;
      int64_t _tmp$1229;
      int32_t _tmp$1231;
      int64_t _tmp$1230;
      struct moonbit_result_1 _tmp$2891;
      struct $$moonbitlang$core$builtin$SourceLocRepr* _block$2892;
      moonbit_incref(_data$117);
      _tmp$2881 = $String$$sub(_data$117, _tmp$1263, _tmp$1264);
      if (_tmp$2881.tag) {
        struct $StringView const _ok$1268 = _tmp$2881.data.ok;
        start_line$137 = _ok$1268;
      } else {
        void* const _err$1269 = _tmp$2881.data.err;
        _try_err$139 = _err$1269;
        goto $join$138;
      }
      goto $joinlet$2880;
      $join$138:;
      moonbit_decref(_try_err$139);
      moonbit_panic();
      $joinlet$2880:;
      _tmp$1260 = match_tag_saver_2$125;
      _tmp$1259 = _tmp$1260 + 1;
      _tmp$1256 = (int64_t)_tmp$1259;
      _tmp$1258 = match_tag_saver_3$126;
      _tmp$1257 = (int64_t)_tmp$1258;
      moonbit_incref(_data$117);
      _tmp$2883 = $String$$sub(_data$117, _tmp$1256, _tmp$1257);
      if (_tmp$2883.tag) {
        struct $StringView const _ok$1261 = _tmp$2883.data.ok;
        start_column$140 = _ok$1261;
      } else {
        void* const _err$1262 = _tmp$2883.data.err;
        _try_err$142 = _err$1262;
        goto $join$141;
      }
      goto $joinlet$2882;
      $join$141:;
      moonbit_decref(_try_err$142);
      moonbit_panic();
      $joinlet$2882:;
      _tmp$1253 = _start$118 + 1;
      _tmp$1250 = (int64_t)_tmp$1253;
      _tmp$1252 = match_tag_saver_0$123;
      _tmp$1251 = (int64_t)_tmp$1252;
      moonbit_incref(_data$117);
      _tmp$2885 = $String$$sub(_data$117, _tmp$1250, _tmp$1251);
      if (_tmp$2885.tag) {
        struct $StringView const _ok$1254 = _tmp$2885.data.ok;
        pkg$143 = _ok$1254;
      } else {
        void* const _err$1255 = _tmp$2885.data.err;
        _try_err$145 = _err$1255;
        goto $join$144;
      }
      goto $joinlet$2884;
      $join$144:;
      moonbit_decref(_try_err$145);
      moonbit_panic();
      $joinlet$2884:;
      _tmp$1247 = match_tag_saver_0$123;
      _tmp$1246 = _tmp$1247 + 1;
      _tmp$1243 = (int64_t)_tmp$1246;
      _tmp$1245 = match_tag_saver_1$124;
      _tmp$1244 = (int64_t)_tmp$1245;
      moonbit_incref(_data$117);
      _tmp$2887 = $String$$sub(_data$117, _tmp$1243, _tmp$1244);
      if (_tmp$2887.tag) {
        struct $StringView const _ok$1248 = _tmp$2887.data.ok;
        filename$146 = _ok$1248;
      } else {
        void* const _err$1249 = _tmp$2887.data.err;
        _try_err$148 = _err$1249;
        goto $join$147;
      }
      goto $joinlet$2886;
      $join$147:;
      moonbit_decref(_try_err$148);
      moonbit_panic();
      $joinlet$2886:;
      _tmp$1240 = match_tag_saver_3$126;
      _tmp$1239 = _tmp$1240 + 1;
      _tmp$1236 = (int64_t)_tmp$1239;
      _tmp$1238 = match_tag_saver_4$127;
      _tmp$1237 = (int64_t)_tmp$1238;
      moonbit_incref(_data$117);
      _tmp$2889 = $String$$sub(_data$117, _tmp$1236, _tmp$1237);
      if (_tmp$2889.tag) {
        struct $StringView const _ok$1241 = _tmp$2889.data.ok;
        end_line$149 = _ok$1241;
      } else {
        void* const _err$1242 = _tmp$2889.data.err;
        _try_err$151 = _err$1242;
        goto $join$150;
      }
      goto $joinlet$2888;
      $join$150:;
      moonbit_decref(_try_err$151);
      moonbit_panic();
      $joinlet$2888:;
      _tmp$1233 = match_tag_saver_4$127;
      _tmp$1232 = _tmp$1233 + 1;
      _tmp$1229 = (int64_t)_tmp$1232;
      _tmp$1231 = match_end$122;
      _tmp$1230 = (int64_t)_tmp$1231;
      _tmp$2891 = $String$$sub(_data$117, _tmp$1229, _tmp$1230);
      if (_tmp$2891.tag) {
        struct $StringView const _ok$1234 = _tmp$2891.data.ok;
        end_column$152 = _ok$1234;
      } else {
        void* const _err$1235 = _tmp$2891.data.err;
        _try_err$154 = _err$1235;
        goto $join$153;
      }
      goto $joinlet$2890;
      $join$153:;
      moonbit_decref(_try_err$154);
      moonbit_panic();
      $joinlet$2890:;
      _block$2892
      = (struct $$moonbitlang$core$builtin$SourceLocRepr*)moonbit_malloc(
          sizeof(struct $$moonbitlang$core$builtin$SourceLocRepr)
        );
      Moonbit_object_header(_block$2892)->meta
      = Moonbit_make_regular_object_header(
        offsetof(struct $$moonbitlang$core$builtin$SourceLocRepr, $0_0) >> 2,
          6,
          0
      );
      _block$2892->$0_0 = pkg$143.$0;
      _block$2892->$0_1 = pkg$143.$1;
      _block$2892->$0_2 = pkg$143.$2;
      _block$2892->$1_0 = filename$146.$0;
      _block$2892->$1_1 = filename$146.$1;
      _block$2892->$1_2 = filename$146.$2;
      _block$2892->$2_0 = start_line$137.$0;
      _block$2892->$2_1 = start_line$137.$1;
      _block$2892->$2_2 = start_line$137.$2;
      _block$2892->$3_0 = start_column$140.$0;
      _block$2892->$3_1 = start_column$140.$1;
      _block$2892->$3_2 = start_column$140.$2;
      _block$2892->$4_0 = end_line$149.$0;
      _block$2892->$4_1 = end_line$149.$1;
      _block$2892->$4_2 = end_line$149.$2;
      _block$2892->$5_0 = end_column$152.$0;
      _block$2892->$5_1 = end_column$152.$1;
      _block$2892->$5_2 = end_column$152.$2;
      return _block$2892;
      break;
    }
    default: {
      moonbit_decref(_data$117);
      moonbit_panic();
      break;
    }
  }
}

moonbit_string_t $$moonbitlang$core$builtin$Array$$at$0(
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* self$113,
  int32_t index$114
) {
  int32_t len$112 = self$113->$1;
  int32_t _if_result$2893;
  if (index$114 >= 0) {
    _if_result$2893 = index$114 < len$112;
  } else {
    _if_result$2893 = 0;
  }
  if (_if_result$2893) {
    moonbit_string_t* _tmp$1228 =
      $$moonbitlang$core$builtin$Array$$buffer$1(self$113);
    moonbit_string_t _tmp$2566;
    if (index$114 < 0 || index$114 >= Moonbit_array_length(_tmp$1228)) {
      moonbit_panic();
    }
    _tmp$2566 = (moonbit_string_t)_tmp$1228[index$114];
    moonbit_incref(_tmp$2566);
    moonbit_decref(_tmp$1228);
    return _tmp$2566;
  } else {
    moonbit_decref(self$113);
    moonbit_panic();
  }
}

int32_t $$moonbitlang$core$builtin$Array$$length$1(
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* self$111
) {
  int32_t _field$2567 = self$111->$1;
  moonbit_decref(self$111);
  return _field$2567;
}

int32_t $$moonbitlang$core$builtin$Array$$length$0(
  struct $$moonbitlang$core$builtin$Array$3c$$3c$String$2a$Int$3e$$3e$* self$110
) {
  int32_t _field$2568 = self$110->$1;
  moonbit_decref(self$110);
  return _field$2568;
}

struct $$3c$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit** $$moonbitlang$core$builtin$Array$$buffer$2(
  struct $$moonbitlang$core$builtin$Array$3c$async$3c$$3e$$3d$$3e$Unit$21$Error$3e$* self$109
) {
  struct $$3c$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit** _field$2569 =
    self$109->$0;
  int32_t _cnt$2717 = Moonbit_object_header(self$109)->rc;
  if (_cnt$2717 > 1) {
    int32_t _new_cnt$2718 = _cnt$2717 - 1;
    Moonbit_object_header(self$109)->rc = _new_cnt$2718;
    moonbit_incref(_field$2569);
  } else if (_cnt$2717 == 1) {
    moonbit_free(self$109);
  }
  return _field$2569;
}

moonbit_string_t* $$moonbitlang$core$builtin$Array$$buffer$1(
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* self$108
) {
  moonbit_string_t* _field$2570 = self$108->$0;
  int32_t _cnt$2719 = Moonbit_object_header(self$108)->rc;
  if (_cnt$2719 > 1) {
    int32_t _new_cnt$2720 = _cnt$2719 - 1;
    Moonbit_object_header(self$108)->rc = _new_cnt$2720;
    moonbit_incref(_field$2570);
  } else if (_cnt$2719 == 1) {
    moonbit_free(self$108);
  }
  return _field$2570;
}

struct $$3c$String$2a$Int$3e$** $$moonbitlang$core$builtin$Array$$buffer$0(
  struct $$moonbitlang$core$builtin$Array$3c$$3c$String$2a$Int$3e$$3e$* self$107
) {
  struct $$3c$String$2a$Int$3e$** _field$2571 = self$107->$0;
  int32_t _cnt$2721 = Moonbit_object_header(self$107)->rc;
  if (_cnt$2721 > 1) {
    int32_t _new_cnt$2722 = _cnt$2721 - 1;
    Moonbit_object_header(self$107)->rc = _new_cnt$2722;
    moonbit_incref(_field$2571);
  } else if (_cnt$2721 == 1) {
    moonbit_free(self$107);
  }
  return _field$2571;
}

moonbit_string_t $String$$escape(moonbit_string_t self$106) {
  struct $$moonbitlang$core$builtin$StringBuilder* buf$105 =
    $$moonbitlang$core$builtin$StringBuilder$$new$inner(0);
  struct $$moonbitlang$core$builtin$Logger _tmp$1227;
  moonbit_incref(buf$105);
  _tmp$1227
  = (struct $$moonbitlang$core$builtin$Logger){
    $$moonbitlang$core$builtin$StringBuilder$as_$moonbitlang$core$builtin$Logger$static_method_table_id,
      buf$105
  };
  $$moonbitlang$core$builtin$Show$$String$$output(self$106, _tmp$1227);
  return $$moonbitlang$core$builtin$StringBuilder$$to_string(buf$105);
}

int32_t $String$$unsafe_charcode_at(
  moonbit_string_t self$103,
  int32_t idx$104
) {
  int32_t _tmp$2572 = self$103[idx$104];
  moonbit_decref(self$103);
  return _tmp$2572;
}

int32_t $Int$$is_trailing_surrogate(int32_t self$102) {
  if (56320 <= self$102) {
    return self$102 <= 57343;
  } else {
    return 0;
  }
}

int32_t $Int$$is_leading_surrogate(int32_t self$101) {
  if (55296 <= self$101) {
    return self$101 <= 56319;
  } else {
    return 0;
  }
}

int32_t $$moonbitlang$core$builtin$Logger$$$moonbitlang$core$builtin$StringBuilder$$write_char(
  struct $$moonbitlang$core$builtin$StringBuilder* self$98,
  int32_t ch$100
) {
  int32_t len$1222 = self$98->$1;
  int32_t _tmp$1221 = len$1222 + 4;
  moonbit_bytes_t _field$2573;
  moonbit_bytes_t data$1225;
  int32_t len$1226;
  int32_t inc$99;
  int32_t len$1224;
  int32_t _tmp$1223;
  moonbit_incref(self$98);
  $$moonbitlang$core$builtin$StringBuilder$$grow_if_necessary(
    self$98, _tmp$1221
  );
  _field$2573 = self$98->$0;
  data$1225 = _field$2573;
  len$1226 = self$98->$1;
  moonbit_incref(data$1225);
  inc$99 = $FixedArray$$set_utf16le_char(data$1225, len$1226, ch$100);
  len$1224 = self$98->$1;
  _tmp$1223 = len$1224 + inc$99;
  self$98->$1 = _tmp$1223;
  moonbit_decref(self$98);
  return 0;
}

int32_t $$moonbitlang$core$builtin$StringBuilder$$grow_if_necessary(
  struct $$moonbitlang$core$builtin$StringBuilder* self$93,
  int32_t required$94
) {
  moonbit_bytes_t _field$2577 = self$93->$0;
  moonbit_bytes_t data$1220 = _field$2577;
  int32_t _tmp$2576 = Moonbit_array_length(data$1220);
  int32_t current_len$92 = _tmp$2576;
  int32_t enough_space$95;
  int32_t _tmp$1218;
  int32_t _tmp$1219;
  moonbit_bytes_t new_data$97;
  moonbit_bytes_t _field$2575;
  moonbit_bytes_t data$1216;
  int32_t len$1217;
  moonbit_bytes_t _old$2574;
  if (required$94 <= current_len$92) {
    moonbit_decref(self$93);
    return 0;
  }
  enough_space$95 = current_len$92;
  while (1) {
    int32_t _tmp$1214 = enough_space$95;
    if (_tmp$1214 < required$94) {
      int32_t _tmp$1215 = enough_space$95;
      enough_space$95 = _tmp$1215 * 2;
      continue;
    }
    break;
  }
  _tmp$1218 = enough_space$95;
  _tmp$1219 = $$moonbitlang$core$builtin$Default$$Byte$$default();
  new_data$97 = (moonbit_bytes_t)moonbit_make_bytes(_tmp$1218, _tmp$1219);
  _field$2575 = self$93->$0;
  data$1216 = _field$2575;
  len$1217 = self$93->$1;
  moonbit_incref(data$1216);
  moonbit_incref(new_data$97);
  $FixedArray$$unsafe_blit$0(new_data$97, 0, data$1216, 0, len$1217);
  _old$2574 = self$93->$0;
  moonbit_decref(_old$2574);
  self$93->$0 = new_data$97;
  moonbit_decref(self$93);
  return 0;
}

int32_t $$moonbitlang$core$builtin$Default$$Byte$$default() {
  return 0;
}

int32_t $FixedArray$$set_utf16le_char(
  moonbit_bytes_t self$87,
  int32_t offset$88,
  int32_t value$86
) {
  uint32_t code$85 = $Char$$to_uint(value$86);
  if (code$85 < 65536u) {
    uint32_t _tmp$1197 = code$85 & 255u;
    int32_t _tmp$1196 = $UInt$$to_byte(_tmp$1197);
    int32_t _tmp$1198;
    uint32_t _tmp$1200;
    int32_t _tmp$1199;
    if (offset$88 < 0 || offset$88 >= Moonbit_array_length(self$87)) {
      moonbit_panic();
    }
    self$87[offset$88] = _tmp$1196;
    _tmp$1198 = offset$88 + 1;
    _tmp$1200 = code$85 >> 8;
    _tmp$1199 = $UInt$$to_byte(_tmp$1200);
    if (_tmp$1198 < 0 || _tmp$1198 >= Moonbit_array_length(self$87)) {
      moonbit_panic();
    }
    self$87[_tmp$1198] = _tmp$1199;
    moonbit_decref(self$87);
    return 2;
  } else if (code$85 < 1114112u) {
    uint32_t hi$89 = code$85 - 65536u;
    uint32_t _tmp$1213 = hi$89 >> 10;
    uint32_t lo$90 = _tmp$1213 | 55296u;
    uint32_t _tmp$1212 = hi$89 & 1023u;
    uint32_t hi$91 = _tmp$1212 | 56320u;
    uint32_t _tmp$1202 = lo$90 & 255u;
    int32_t _tmp$1201 = $UInt$$to_byte(_tmp$1202);
    int32_t _tmp$1203;
    uint32_t _tmp$1205;
    int32_t _tmp$1204;
    int32_t _tmp$1206;
    uint32_t _tmp$1208;
    int32_t _tmp$1207;
    int32_t _tmp$1209;
    uint32_t _tmp$1211;
    int32_t _tmp$1210;
    if (offset$88 < 0 || offset$88 >= Moonbit_array_length(self$87)) {
      moonbit_panic();
    }
    self$87[offset$88] = _tmp$1201;
    _tmp$1203 = offset$88 + 1;
    _tmp$1205 = lo$90 >> 8;
    _tmp$1204 = $UInt$$to_byte(_tmp$1205);
    if (_tmp$1203 < 0 || _tmp$1203 >= Moonbit_array_length(self$87)) {
      moonbit_panic();
    }
    self$87[_tmp$1203] = _tmp$1204;
    _tmp$1206 = offset$88 + 2;
    _tmp$1208 = hi$91 & 255u;
    _tmp$1207 = $UInt$$to_byte(_tmp$1208);
    if (_tmp$1206 < 0 || _tmp$1206 >= Moonbit_array_length(self$87)) {
      moonbit_panic();
    }
    self$87[_tmp$1206] = _tmp$1207;
    _tmp$1209 = offset$88 + 3;
    _tmp$1211 = hi$91 >> 8;
    _tmp$1210 = $UInt$$to_byte(_tmp$1211);
    if (_tmp$1209 < 0 || _tmp$1209 >= Moonbit_array_length(self$87)) {
      moonbit_panic();
    }
    self$87[_tmp$1209] = _tmp$1210;
    moonbit_decref(self$87);
    return 4;
  } else {
    moonbit_decref(self$87);
    return $moonbitlang$core$builtin$abort$1(
             (moonbit_string_t)moonbit_string_literal_22.data,
               (moonbit_string_t)moonbit_string_literal_23.data
           );
  }
}

int32_t $UInt$$to_byte(uint32_t self$84) {
  int32_t _tmp$1195 = *(int32_t*)&self$84;
  return _tmp$1195 & 0xff;
}

uint32_t $Char$$to_uint(int32_t self$83) {
  int32_t _tmp$1194 = self$83;
  return *(uint32_t*)&_tmp$1194;
}

moonbit_string_t $$moonbitlang$core$builtin$StringBuilder$$to_string(
  struct $$moonbitlang$core$builtin$StringBuilder* self$82
) {
  moonbit_bytes_t _field$2579 = self$82->$0;
  moonbit_bytes_t data$1193 = _field$2579;
  moonbit_bytes_t _tmp$1190;
  int32_t _field$2578;
  int32_t len$1192;
  int64_t _tmp$1191;
  moonbit_incref(data$1193);
  _tmp$1190 = data$1193;
  _field$2578 = self$82->$1;
  moonbit_decref(self$82);
  len$1192 = _field$2578;
  _tmp$1191 = (int64_t)len$1192;
  return $Bytes$$to_unchecked_string$inner(_tmp$1190, 0, _tmp$1191);
}

moonbit_string_t $Bytes$$to_unchecked_string$inner(
  moonbit_bytes_t self$77,
  int32_t offset$81,
  int64_t length$79
) {
  int32_t len$76 = Moonbit_array_length(self$77);
  int32_t length$78;
  int32_t _if_result$2895;
  if (length$79 == 4294967296ll) {
    length$78 = len$76 - offset$81;
  } else {
    int64_t _Some$80 = length$79;
    length$78 = (int32_t)_Some$80;
  }
  if (offset$81 >= 0) {
    if (length$78 >= 0) {
      int32_t _tmp$1189 = offset$81 + length$78;
      _if_result$2895 = _tmp$1189 <= len$76;
    } else {
      _if_result$2895 = 0;
    }
  } else {
    _if_result$2895 = 0;
  }
  if (_if_result$2895) {
    return $moonbitlang$core$builtin$unsafe_sub_string(
             self$77, offset$81, length$78
           );
  } else {
    moonbit_decref(self$77);
    moonbit_panic();
  }
}

struct $$moonbitlang$core$builtin$StringBuilder* $$moonbitlang$core$builtin$StringBuilder$$new$inner(
  int32_t size_hint$74
) {
  int32_t initial$73;
  moonbit_bytes_t data$75;
  struct $$moonbitlang$core$builtin$StringBuilder* _block$2896;
  if (size_hint$74 < 1) {
    initial$73 = 1;
  } else {
    initial$73 = size_hint$74;
  }
  data$75 = (moonbit_bytes_t)moonbit_make_bytes(initial$73, 0);
  _block$2896
  = (struct $$moonbitlang$core$builtin$StringBuilder*)moonbit_malloc(
      sizeof(struct $$moonbitlang$core$builtin$StringBuilder)
    );
  Moonbit_object_header(_block$2896)->meta
  = Moonbit_make_regular_object_header(
    offsetof(struct $$moonbitlang$core$builtin$StringBuilder, $0) >> 2, 1, 0
  );
  _block$2896->$0 = data$75;
  _block$2896->$1 = 0;
  return _block$2896;
}

int32_t $Byte$$to_char(int32_t self$72) {
  int32_t _tmp$1188 = (int32_t)self$72;
  return _tmp$1188;
}

int32_t $$moonbitlang$core$builtin$UninitializedArray$$unsafe_blit$2(
  struct $$3c$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit** dst$67,
  int32_t dst_offset$68,
  struct $$3c$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit** src$69,
  int32_t src_offset$70,
  int32_t len$71
) {
  $FixedArray$$unsafe_blit$3(
    dst$67, dst_offset$68, src$69, src_offset$70, len$71
  );
  return 0;
}

int32_t $$moonbitlang$core$builtin$UninitializedArray$$unsafe_blit$1(
  struct $$3c$String$2a$Int$3e$** dst$62,
  int32_t dst_offset$63,
  struct $$3c$String$2a$Int$3e$** src$64,
  int32_t src_offset$65,
  int32_t len$66
) {
  $FixedArray$$unsafe_blit$2(
    dst$62, dst_offset$63, src$64, src_offset$65, len$66
  );
  return 0;
}

int32_t $$moonbitlang$core$builtin$UninitializedArray$$unsafe_blit$0(
  moonbit_string_t* dst$57,
  int32_t dst_offset$58,
  moonbit_string_t* src$59,
  int32_t src_offset$60,
  int32_t len$61
) {
  $FixedArray$$unsafe_blit$1(
    dst$57, dst_offset$58, src$59, src_offset$60, len$61
  );
  return 0;
}

int32_t $FixedArray$$unsafe_blit$3(
  struct $$3c$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit** dst$48,
  int32_t dst_offset$50,
  struct $$3c$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit** src$49,
  int32_t src_offset$51,
  int32_t len$53
) {
  int32_t _if_result$2897;
  if (dst$48 == src$49) {
    _if_result$2897 = dst_offset$50 < src_offset$51;
  } else {
    _if_result$2897 = 0;
  }
  if (_if_result$2897) {
    int32_t i$52 = 0;
    while (1) {
      if (i$52 < len$53) {
        int32_t _tmp$1179 = dst_offset$50 + i$52;
        int32_t _tmp$1181 = src_offset$51 + i$52;
        struct $$3c$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit* _tmp$2581;
        struct $$3c$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit* _tmp$1180;
        struct $$3c$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit* _old$2580;
        int32_t _tmp$1182;
        if (_tmp$1181 < 0 || _tmp$1181 >= Moonbit_array_length(src$49)) {
          moonbit_panic();
        }
        _tmp$2581
        = (struct $$3c$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit*)src$49[
            _tmp$1181
          ];
        _tmp$1180 = _tmp$2581;
        if (_tmp$1179 < 0 || _tmp$1179 >= Moonbit_array_length(dst$48)) {
          moonbit_panic();
        }
        _old$2580
        = (struct $$3c$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit*)dst$48[
            _tmp$1179
          ];
        if (_tmp$1180) {
          moonbit_incref(_tmp$1180);
        }
        if (_old$2580) {
          moonbit_decref(_old$2580);
        }
        dst$48[_tmp$1179] = _tmp$1180;
        _tmp$1182 = i$52 + 1;
        i$52 = _tmp$1182;
        continue;
      } else {
        moonbit_decref(src$49);
        moonbit_decref(dst$48);
      }
      break;
    }
  } else {
    int32_t _tmp$1187 = len$53 - 1;
    int32_t i$55 = _tmp$1187;
    while (1) {
      if (i$55 >= 0) {
        int32_t _tmp$1183 = dst_offset$50 + i$55;
        int32_t _tmp$1185 = src_offset$51 + i$55;
        struct $$3c$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit* _tmp$2583;
        struct $$3c$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit* _tmp$1184;
        struct $$3c$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit* _old$2582;
        int32_t _tmp$1186;
        if (_tmp$1185 < 0 || _tmp$1185 >= Moonbit_array_length(src$49)) {
          moonbit_panic();
        }
        _tmp$2583
        = (struct $$3c$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit*)src$49[
            _tmp$1185
          ];
        _tmp$1184 = _tmp$2583;
        if (_tmp$1183 < 0 || _tmp$1183 >= Moonbit_array_length(dst$48)) {
          moonbit_panic();
        }
        _old$2582
        = (struct $$3c$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit*)dst$48[
            _tmp$1183
          ];
        if (_tmp$1184) {
          moonbit_incref(_tmp$1184);
        }
        if (_old$2582) {
          moonbit_decref(_old$2582);
        }
        dst$48[_tmp$1183] = _tmp$1184;
        _tmp$1186 = i$55 - 1;
        i$55 = _tmp$1186;
        continue;
      } else {
        moonbit_decref(src$49);
        moonbit_decref(dst$48);
      }
      break;
    }
  }
  return 0;
}

int32_t $FixedArray$$unsafe_blit$2(
  struct $$3c$String$2a$Int$3e$** dst$39,
  int32_t dst_offset$41,
  struct $$3c$String$2a$Int$3e$** src$40,
  int32_t src_offset$42,
  int32_t len$44
) {
  int32_t _if_result$2900;
  if (dst$39 == src$40) {
    _if_result$2900 = dst_offset$41 < src_offset$42;
  } else {
    _if_result$2900 = 0;
  }
  if (_if_result$2900) {
    int32_t i$43 = 0;
    while (1) {
      if (i$43 < len$44) {
        int32_t _tmp$1170 = dst_offset$41 + i$43;
        int32_t _tmp$1172 = src_offset$42 + i$43;
        struct $$3c$String$2a$Int$3e$* _tmp$2585;
        struct $$3c$String$2a$Int$3e$* _tmp$1171;
        struct $$3c$String$2a$Int$3e$* _old$2584;
        int32_t _tmp$1173;
        if (_tmp$1172 < 0 || _tmp$1172 >= Moonbit_array_length(src$40)) {
          moonbit_panic();
        }
        _tmp$2585 = (struct $$3c$String$2a$Int$3e$*)src$40[_tmp$1172];
        _tmp$1171 = _tmp$2585;
        if (_tmp$1170 < 0 || _tmp$1170 >= Moonbit_array_length(dst$39)) {
          moonbit_panic();
        }
        _old$2584 = (struct $$3c$String$2a$Int$3e$*)dst$39[_tmp$1170];
        if (_tmp$1171) {
          moonbit_incref(_tmp$1171);
        }
        if (_old$2584) {
          moonbit_decref(_old$2584);
        }
        dst$39[_tmp$1170] = _tmp$1171;
        _tmp$1173 = i$43 + 1;
        i$43 = _tmp$1173;
        continue;
      } else {
        moonbit_decref(src$40);
        moonbit_decref(dst$39);
      }
      break;
    }
  } else {
    int32_t _tmp$1178 = len$44 - 1;
    int32_t i$46 = _tmp$1178;
    while (1) {
      if (i$46 >= 0) {
        int32_t _tmp$1174 = dst_offset$41 + i$46;
        int32_t _tmp$1176 = src_offset$42 + i$46;
        struct $$3c$String$2a$Int$3e$* _tmp$2587;
        struct $$3c$String$2a$Int$3e$* _tmp$1175;
        struct $$3c$String$2a$Int$3e$* _old$2586;
        int32_t _tmp$1177;
        if (_tmp$1176 < 0 || _tmp$1176 >= Moonbit_array_length(src$40)) {
          moonbit_panic();
        }
        _tmp$2587 = (struct $$3c$String$2a$Int$3e$*)src$40[_tmp$1176];
        _tmp$1175 = _tmp$2587;
        if (_tmp$1174 < 0 || _tmp$1174 >= Moonbit_array_length(dst$39)) {
          moonbit_panic();
        }
        _old$2586 = (struct $$3c$String$2a$Int$3e$*)dst$39[_tmp$1174];
        if (_tmp$1175) {
          moonbit_incref(_tmp$1175);
        }
        if (_old$2586) {
          moonbit_decref(_old$2586);
        }
        dst$39[_tmp$1174] = _tmp$1175;
        _tmp$1177 = i$46 - 1;
        i$46 = _tmp$1177;
        continue;
      } else {
        moonbit_decref(src$40);
        moonbit_decref(dst$39);
      }
      break;
    }
  }
  return 0;
}

int32_t $FixedArray$$unsafe_blit$1(
  moonbit_string_t* dst$30,
  int32_t dst_offset$32,
  moonbit_string_t* src$31,
  int32_t src_offset$33,
  int32_t len$35
) {
  int32_t _if_result$2903;
  if (dst$30 == src$31) {
    _if_result$2903 = dst_offset$32 < src_offset$33;
  } else {
    _if_result$2903 = 0;
  }
  if (_if_result$2903) {
    int32_t i$34 = 0;
    while (1) {
      if (i$34 < len$35) {
        int32_t _tmp$1161 = dst_offset$32 + i$34;
        int32_t _tmp$1163 = src_offset$33 + i$34;
        moonbit_string_t _tmp$2589;
        moonbit_string_t _tmp$1162;
        moonbit_string_t _old$2588;
        int32_t _tmp$1164;
        if (_tmp$1163 < 0 || _tmp$1163 >= Moonbit_array_length(src$31)) {
          moonbit_panic();
        }
        _tmp$2589 = (moonbit_string_t)src$31[_tmp$1163];
        _tmp$1162 = _tmp$2589;
        if (_tmp$1161 < 0 || _tmp$1161 >= Moonbit_array_length(dst$30)) {
          moonbit_panic();
        }
        _old$2588 = (moonbit_string_t)dst$30[_tmp$1161];
        moonbit_incref(_tmp$1162);
        moonbit_decref(_old$2588);
        dst$30[_tmp$1161] = _tmp$1162;
        _tmp$1164 = i$34 + 1;
        i$34 = _tmp$1164;
        continue;
      } else {
        moonbit_decref(src$31);
        moonbit_decref(dst$30);
      }
      break;
    }
  } else {
    int32_t _tmp$1169 = len$35 - 1;
    int32_t i$37 = _tmp$1169;
    while (1) {
      if (i$37 >= 0) {
        int32_t _tmp$1165 = dst_offset$32 + i$37;
        int32_t _tmp$1167 = src_offset$33 + i$37;
        moonbit_string_t _tmp$2591;
        moonbit_string_t _tmp$1166;
        moonbit_string_t _old$2590;
        int32_t _tmp$1168;
        if (_tmp$1167 < 0 || _tmp$1167 >= Moonbit_array_length(src$31)) {
          moonbit_panic();
        }
        _tmp$2591 = (moonbit_string_t)src$31[_tmp$1167];
        _tmp$1166 = _tmp$2591;
        if (_tmp$1165 < 0 || _tmp$1165 >= Moonbit_array_length(dst$30)) {
          moonbit_panic();
        }
        _old$2590 = (moonbit_string_t)dst$30[_tmp$1165];
        moonbit_incref(_tmp$1166);
        moonbit_decref(_old$2590);
        dst$30[_tmp$1165] = _tmp$1166;
        _tmp$1168 = i$37 - 1;
        i$37 = _tmp$1168;
        continue;
      } else {
        moonbit_decref(src$31);
        moonbit_decref(dst$30);
      }
      break;
    }
  }
  return 0;
}

int32_t $FixedArray$$unsafe_blit$0(
  moonbit_bytes_t dst$21,
  int32_t dst_offset$23,
  moonbit_bytes_t src$22,
  int32_t src_offset$24,
  int32_t len$26
) {
  int32_t _if_result$2906;
  if (dst$21 == src$22) {
    _if_result$2906 = dst_offset$23 < src_offset$24;
  } else {
    _if_result$2906 = 0;
  }
  if (_if_result$2906) {
    int32_t i$25 = 0;
    while (1) {
      if (i$25 < len$26) {
        int32_t _tmp$1152 = dst_offset$23 + i$25;
        int32_t _tmp$1154 = src_offset$24 + i$25;
        int32_t _tmp$1153;
        int32_t _tmp$1155;
        if (_tmp$1154 < 0 || _tmp$1154 >= Moonbit_array_length(src$22)) {
          moonbit_panic();
        }
        _tmp$1153 = (int32_t)src$22[_tmp$1154];
        if (_tmp$1152 < 0 || _tmp$1152 >= Moonbit_array_length(dst$21)) {
          moonbit_panic();
        }
        dst$21[_tmp$1152] = _tmp$1153;
        _tmp$1155 = i$25 + 1;
        i$25 = _tmp$1155;
        continue;
      } else {
        moonbit_decref(src$22);
        moonbit_decref(dst$21);
      }
      break;
    }
  } else {
    int32_t _tmp$1160 = len$26 - 1;
    int32_t i$28 = _tmp$1160;
    while (1) {
      if (i$28 >= 0) {
        int32_t _tmp$1156 = dst_offset$23 + i$28;
        int32_t _tmp$1158 = src_offset$24 + i$28;
        int32_t _tmp$1157;
        int32_t _tmp$1159;
        if (_tmp$1158 < 0 || _tmp$1158 >= Moonbit_array_length(src$22)) {
          moonbit_panic();
        }
        _tmp$1157 = (int32_t)src$22[_tmp$1158];
        if (_tmp$1156 < 0 || _tmp$1156 >= Moonbit_array_length(dst$21)) {
          moonbit_panic();
        }
        dst$21[_tmp$1156] = _tmp$1157;
        _tmp$1159 = i$28 - 1;
        i$28 = _tmp$1159;
        continue;
      } else {
        moonbit_decref(src$22);
        moonbit_decref(dst$21);
      }
      break;
    }
  }
  return 0;
}

int32_t $moonbitlang$core$builtin$abort$1(
  moonbit_string_t string$19,
  moonbit_string_t loc$20
) {
  moonbit_string_t _tmp$1151 =
    $$moonbitlang$core$builtin$Show$$String$$to_string(string$19);
  moonbit_string_t _tmp$1149 =
    moonbit_add_string(
      _tmp$1151, (moonbit_string_t)moonbit_string_literal_24.data
    );
  moonbit_string_t _tmp$1150 =
    $$moonbitlang$core$builtin$Show$$$default_impl$$to_string$1(loc$20);
  moonbit_string_t _tmp$1148 = moonbit_add_string(_tmp$1149, _tmp$1150);
  moonbit_string_t _tmp$1147 =
    moonbit_add_string(
      _tmp$1148, (moonbit_string_t)moonbit_string_literal_25.data
    );
  return $moonbitlang$core$abort$abort$1(_tmp$1147);
}

int32_t $moonbitlang$core$builtin$abort$0(
  moonbit_string_t string$17,
  moonbit_string_t loc$18
) {
  moonbit_string_t _tmp$1146 =
    $$moonbitlang$core$builtin$Show$$String$$to_string(string$17);
  moonbit_string_t _tmp$1144 =
    moonbit_add_string(
      _tmp$1146, (moonbit_string_t)moonbit_string_literal_24.data
    );
  moonbit_string_t _tmp$1145 =
    $$moonbitlang$core$builtin$Show$$$default_impl$$to_string$1(loc$18);
  moonbit_string_t _tmp$1143 = moonbit_add_string(_tmp$1144, _tmp$1145);
  moonbit_string_t _tmp$1142 =
    moonbit_add_string(
      _tmp$1143, (moonbit_string_t)moonbit_string_literal_25.data
    );
  $moonbitlang$core$abort$abort$0(_tmp$1142);
  return 0;
}

int32_t $$moonbitlang$core$builtin$Hasher$$combine_uint(
  struct $$moonbitlang$core$builtin$Hasher* self$15,
  uint32_t value$16
) {
  uint32_t acc$1141 = self$15->$0;
  uint32_t _tmp$1140 = acc$1141 + 4u;
  self$15->$0 = _tmp$1140;
  $$moonbitlang$core$builtin$Hasher$$consume4(self$15, value$16);
  return 0;
}

int32_t $$moonbitlang$core$builtin$Hasher$$consume4(
  struct $$moonbitlang$core$builtin$Hasher* self$13,
  uint32_t input$14
) {
  uint32_t acc$1138 = self$13->$0;
  uint32_t _tmp$1139 = input$14 * 3266489917u;
  uint32_t _tmp$1137 = acc$1138 + _tmp$1139;
  uint32_t _tmp$1136 = $moonbitlang$core$builtin$rotl(_tmp$1137, 17);
  uint32_t _tmp$1135 = _tmp$1136 * 668265263u;
  self$13->$0 = _tmp$1135;
  moonbit_decref(self$13);
  return 0;
}

uint32_t $moonbitlang$core$builtin$rotl(uint32_t x$11, int32_t r$12) {
  uint32_t _tmp$1132 = x$11 << (r$12 & 31);
  int32_t _tmp$1134 = 32 - r$12;
  uint32_t _tmp$1133 = x$11 >> (_tmp$1134 & 31);
  return _tmp$1132 | _tmp$1133;
}

int32_t $$moonbitlang$core$builtin$Show$$$moonbitlang$core$builtin$Failure$$output(
  void* _x_5032$7,
  struct $$moonbitlang$core$builtin$Logger _x_5033$10
) {
  struct $Error$moonbitlang$core$builtin$Failure$Failure* _Failure$8 =
    (struct $Error$moonbitlang$core$builtin$Failure$Failure*)_x_5032$7;
  moonbit_string_t _field$2592 = _Failure$8->$0;
  int32_t _cnt$2723 = Moonbit_object_header(_Failure$8)->rc;
  moonbit_string_t _$2a$err_payload_5034$9;
  struct $$moonbitlang$core$builtin$Logger _bind$1131;
  if (_cnt$2723 > 1) {
    int32_t _new_cnt$2724 = _cnt$2723 - 1;
    Moonbit_object_header(_Failure$8)->rc = _new_cnt$2724;
    moonbit_incref(_field$2592);
  } else if (_cnt$2723 == 1) {
    moonbit_free(_Failure$8);
  }
  _$2a$err_payload_5034$9 = _field$2592;
  if (_x_5033$10.$1) {
    moonbit_incref(_x_5033$10.$1);
  }
  _x_5033$10.$0->$method_0(
    _x_5033$10.$1, (moonbit_string_t)moonbit_string_literal_26.data
  );
  if (_x_5033$10.$1) {
    moonbit_incref(_x_5033$10.$1);
  }
  $$moonbitlang$core$builtin$Logger$$write_object$0(
    _x_5033$10, _$2a$err_payload_5034$9
  );
  _bind$1131 = _x_5033$10;
  _bind$1131.$0->$method_0(
    _bind$1131.$1, (moonbit_string_t)moonbit_string_literal_27.data
  );
  return 0;
}

int32_t $$moonbitlang$core$builtin$Show$$$moonbitlang$core$builtin$CreatingViewError$$output(
  void* _x_5046$5,
  struct $$moonbitlang$core$builtin$Logger _x_5047$6
) {
  switch (Moonbit_object_tag(_x_5046$5)) {
    case 1: {
      _x_5047$6.$0->$method_0(
        _x_5047$6.$1, (moonbit_string_t)moonbit_string_literal_28.data
      );
      break;
    }
    default: {
      moonbit_decref(_x_5046$5);
      _x_5047$6.$0->$method_0(
        _x_5047$6.$1, (moonbit_string_t)moonbit_string_literal_29.data
      );
      break;
    }
  }
  return 0;
}

int32_t $$moonbitlang$core$builtin$Logger$$write_object$0(
  struct $$moonbitlang$core$builtin$Logger self$4,
  moonbit_string_t obj$3
) {
  $$moonbitlang$core$builtin$Show$$String$$output(obj$3, self$4);
  return 0;
}

int32_t $moonbitlang$core$abort$abort$1(moonbit_string_t msg$2) {
  moonbit_println(msg$2);
  moonbit_decref(msg$2);
  moonbit_panic();
}

int32_t $moonbitlang$core$abort$abort$0(moonbit_string_t msg$1) {
  moonbit_println(msg$1);
  moonbit_decref(msg$1);
  moonbit_panic();
  return 0;
}

moonbit_string_t $Error$to_string(void* _e$1093) {
  switch (Moonbit_object_tag(_e$1093)) {
    case 0: {
      return $$moonbitlang$core$builtin$Show$$$default_impl$$to_string$3(
               _e$1093
             );
      break;
    }
    
    case 1: {
      return $$moonbitlang$core$builtin$Show$$$default_impl$$to_string$3(
               _e$1093
             );
      break;
    }
    
    case 4: {
      moonbit_decref(_e$1093);
      return (moonbit_string_t)moonbit_string_literal_30.data;
      break;
    }
    
    case 2: {
      return $$moonbitlang$core$builtin$Show$$$default_impl$$to_string$2(
               _e$1093
             );
      break;
    }
    
    case 5: {
      moonbit_decref(_e$1093);
      return (moonbit_string_t)moonbit_string_literal_31.data;
      break;
    }
    default: {
      moonbit_decref(_e$1093);
      return (moonbit_string_t)moonbit_string_literal_32.data;
      break;
    }
  }
}

int32_t $$moonbitlang$core$builtin$Logger$$$moonbitlang$core$builtin$StringBuilder$$write_char$dyncall_as_$moonbitlang$core$builtin$Logger(
  void* _obj_ptr$1110,
  int32_t _param$1109
) {
  struct $$moonbitlang$core$builtin$StringBuilder* _self$1108 =
    (struct $$moonbitlang$core$builtin$StringBuilder*)_obj_ptr$1110;
  $$moonbitlang$core$builtin$Logger$$$moonbitlang$core$builtin$StringBuilder$$write_char(
    _self$1108, _param$1109
  );
  return 0;
}

int32_t $$moonbitlang$core$builtin$Logger$$$moonbitlang$core$builtin$StringBuilder$$write_view$dyncall_as_$moonbitlang$core$builtin$Logger(
  void* _obj_ptr$1107,
  struct $StringView _param$1106
) {
  struct $$moonbitlang$core$builtin$StringBuilder* _self$1105 =
    (struct $$moonbitlang$core$builtin$StringBuilder*)_obj_ptr$1107;
  $$moonbitlang$core$builtin$Logger$$$moonbitlang$core$builtin$StringBuilder$$write_view(
    _self$1105, _param$1106
  );
  return 0;
}

int32_t $$moonbitlang$core$builtin$Logger$$$default_impl$$write_substring$dyncall_as_$moonbitlang$core$builtin$Logger$0(
  void* _obj_ptr$1104,
  moonbit_string_t _param$1101,
  int32_t _param$1102,
  int32_t _param$1103
) {
  struct $$moonbitlang$core$builtin$StringBuilder* _self$1100 =
    (struct $$moonbitlang$core$builtin$StringBuilder*)_obj_ptr$1104;
  $$moonbitlang$core$builtin$Logger$$$default_impl$$write_substring$0(
    _self$1100, _param$1101, _param$1102, _param$1103
  );
  return 0;
}

int32_t $$moonbitlang$core$builtin$Logger$$$moonbitlang$core$builtin$StringBuilder$$write_string$dyncall_as_$moonbitlang$core$builtin$Logger(
  void* _obj_ptr$1099,
  moonbit_string_t _param$1098
) {
  struct $$moonbitlang$core$builtin$StringBuilder* _self$1097 =
    (struct $$moonbitlang$core$builtin$StringBuilder*)_obj_ptr$1099;
  $$moonbitlang$core$builtin$Logger$$$moonbitlang$core$builtin$StringBuilder$$write_string(
    _self$1097, _param$1098
  );
  return 0;
}

void moonbit_init() {
  struct $$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** _bind$881 =
    (struct $$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$**)moonbit_empty_ref_array;
  struct $$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** _tmp$1116 =
    _bind$881;
  struct $$moonbitlang$core$builtin$ArrayView$3c$$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$$3e$ _tmp$1115 =
    (struct $$moonbitlang$core$builtin$ArrayView$3c$$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$$3e$){
      0, 0, _tmp$1116
    };
  struct $$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** _bind$883;
  struct $$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** _tmp$1118;
  struct $$moonbitlang$core$builtin$ArrayView$3c$$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$$3e$ _tmp$1117;
  struct $$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** _bind$882;
  struct $$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** _tmp$1120;
  struct $$moonbitlang$core$builtin$ArrayView$3c$$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$$3e$ _tmp$1119;
  struct $$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$** _bind$879;
  struct $$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$** _tmp$1130;
  struct $$moonbitlang$core$builtin$ArrayView$3c$$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$ _tmp$1129;
  struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _tmp$1128;
  struct $$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tuple$1123;
  struct $$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$** _bind$880;
  struct $$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$** _tmp$1127;
  struct $$moonbitlang$core$builtin$ArrayView$3c$$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$ _tmp$1126;
  struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _tmp$1125;
  struct $$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tuple$1124;
  struct $$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** _bind$878;
  struct $$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** _tmp$1122;
  struct $$moonbitlang$core$builtin$ArrayView$3c$$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$$3e$ _tmp$1121;
  $mizchi$process_pool$moonbit_test_driver_internal_with_args_tests
  = $$moonbitlang$core$builtin$Map$$from_array$2(
    _tmp$1115
  );
  _bind$883
  = (struct $$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$**)moonbit_empty_ref_array;
  _tmp$1118 = _bind$883;
  _tmp$1117
  = (struct $$moonbitlang$core$builtin$ArrayView$3c$$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$$3e$){
    0, 0, _tmp$1118
  };
  $mizchi$process_pool$moonbit_test_driver_internal_async_tests_with_args
  = $$moonbitlang$core$builtin$Map$$from_array$4(
    _tmp$1117
  );
  _bind$882
  = (struct $$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$**)moonbit_empty_ref_array;
  _tmp$1120 = _bind$882;
  _tmp$1119
  = (struct $$moonbitlang$core$builtin$ArrayView$3c$$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$$3e$){
    0, 0, _tmp$1120
  };
  $mizchi$process_pool$moonbit_test_driver_internal_async_tests
  = $$moonbitlang$core$builtin$Map$$from_array$3(
    _tmp$1119
  );
  _bind$879
  = (struct $$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$**)moonbit_empty_ref_array;
  _tmp$1130 = _bind$879;
  _tmp$1129
  = (struct $$moonbitlang$core$builtin$ArrayView$3c$$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$){
    0, 0, _tmp$1130
  };
  _tmp$1128 = $$moonbitlang$core$builtin$Map$$from_array$1(_tmp$1129);
  _tuple$1123
  = (struct $$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$*)moonbit_malloc(
      sizeof(
        struct $$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$
      )
    );
  Moonbit_object_header(_tuple$1123)->meta
  = Moonbit_make_regular_object_header(
    offsetof(
      struct $$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$,
        $0
    )
    >> 2,
      2,
      0
  );
  _tuple$1123->$0 = (moonbit_string_t)moonbit_string_literal_33.data;
  _tuple$1123->$1 = _tmp$1128;
  _bind$880
  = (struct $$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$**)moonbit_empty_ref_array;
  _tmp$1127 = _bind$880;
  _tmp$1126
  = (struct $$moonbitlang$core$builtin$ArrayView$3c$$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$){
    0, 0, _tmp$1127
  };
  _tmp$1125 = $$moonbitlang$core$builtin$Map$$from_array$1(_tmp$1126);
  _tuple$1124
  = (struct $$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$*)moonbit_malloc(
      sizeof(
        struct $$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$
      )
    );
  Moonbit_object_header(_tuple$1124)->meta
  = Moonbit_make_regular_object_header(
    offsetof(
      struct $$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$,
        $0
    )
    >> 2,
      2,
      0
  );
  _tuple$1124->$0 = (moonbit_string_t)moonbit_string_literal_34.data;
  _tuple$1124->$1 = _tmp$1125;
  _bind$878
  = (struct $$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$**)moonbit_make_ref_array_raw(
      2
    );
  _bind$878[0] = _tuple$1123;
  _bind$878[1] = _tuple$1124;
  _tmp$1122 = _bind$878;
  _tmp$1121
  = (struct $$moonbitlang$core$builtin$ArrayView$3c$$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$$3e$){
    0, 2, _tmp$1122
  };
  $mizchi$process_pool$moonbit_test_driver_internal_no_args_tests
  = $$moonbitlang$core$builtin$Map$$from_array$0(
    _tmp$1121
  );
}

int main(int argc, char** argv) {
  struct $$3c$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit** _tmp$1114;
  struct $$moonbitlang$core$builtin$Array$3c$async$3c$$3e$$3d$$3e$Unit$21$Error$3e$* async_tests$1087;
  struct $$moonbitlang$core$builtin$Array$3c$$3c$String$2a$Int$3e$$3e$* _arr$1088;
  int32_t _len$1089;
  int32_t _i$1090;
  moonbit_runtime_init(argc, argv);
  moonbit_init();
  _tmp$1114
  = (struct $$3c$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit**)moonbit_empty_ref_array;
  async_tests$1087
  = (struct $$moonbitlang$core$builtin$Array$3c$async$3c$$3e$$3d$$3e$Unit$21$Error$3e$*)moonbit_malloc(
      sizeof(
        struct $$moonbitlang$core$builtin$Array$3c$async$3c$$3e$$3d$$3e$Unit$21$Error$3e$
      )
    );
  Moonbit_object_header(async_tests$1087)->meta
  = Moonbit_make_regular_object_header(
    offsetof(
      struct $$moonbitlang$core$builtin$Array$3c$async$3c$$3e$$3d$$3e$Unit$21$Error$3e$,
        $0
    )
    >> 2,
      1,
      0
  );
  async_tests$1087->$0 = _tmp$1114;
  async_tests$1087->$1 = 0;
  _arr$1088
  = $mizchi$process_pool$moonbit_test_driver_internal_native_parse_args();
  moonbit_incref(_arr$1088);
  _len$1089 = $$moonbitlang$core$builtin$Array$$length$0(_arr$1088);
  _i$1090 = 0;
  while (1) {
    if (_i$1090 < _len$1089) {
      struct $$3c$String$2a$Int$3e$* arg$1091;
      moonbit_string_t _field$2594;
      moonbit_string_t _tmp$1111;
      int32_t _field$2593;
      int32_t _cnt$2725;
      int32_t _tmp$1112;
      int32_t _tmp$1113;
      moonbit_incref(_arr$1088);
      arg$1091
      = $$moonbitlang$core$builtin$Array$$unsafe_get$0(
        _arr$1088, _i$1090
      );
      _field$2594 = arg$1091->$0;
      _tmp$1111 = _field$2594;
      _field$2593 = arg$1091->$1;
      _cnt$2725 = Moonbit_object_header(arg$1091)->rc;
      if (_cnt$2725 > 1) {
        int32_t _new_cnt$2726 = _cnt$2725 - 1;
        Moonbit_object_header(arg$1091)->rc = _new_cnt$2726;
        moonbit_incref(_tmp$1111);
      } else if (_cnt$2725 == 1) {
        moonbit_free(arg$1091);
      }
      _tmp$1112 = _field$2593;
      moonbit_incref(async_tests$1087);
      $mizchi$process_pool$moonbit_test_driver_internal_do_execute(
        async_tests$1087, _tmp$1111, _tmp$1112
      );
      _tmp$1113 = _i$1090 + 1;
      _i$1090 = _tmp$1113;
      continue;
    } else {
      moonbit_decref(_arr$1088);
    }
    break;
  }
  $mizchi$process_pool$moonbit_test_driver_internal_run_async_tests(
    async_tests$1087
  );
  return 0;
}