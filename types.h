#ifndef TYPES_H
#define TYPES_H

#include <immintrin.h>
#include <stdint.h>
#include <stdbool.h>

typedef int8_t i8;
typedef uint8_t u8;
typedef int16_t i16;
typedef uint16_t u16;
typedef int32_t i32;
typedef uint32_t u32;
typedef int64_t i64;
typedef uint64_t u64;

typedef float f32;
typedef double f64;

typedef size_t usize;

#ifdef __cplusplus

#if (__cplusplus == 202302L)
#define CPP_23
#define AT_OR_ABOVE_CPP_20
#endif

#if (__cplusplus == 202002L)
#define CPP_20
#define AT_OR_ABOVE_CPP_20
#endif

#define INLINE __attribute__((always_inline))

#if (__cplusplus != 202302L || __cplusplus != 202002L)
#endif

#ifdef CPP_23
#define CONSTEVAL consteval
#else
#define CONSTEVAL constexpr
#endif

#ifdef AT_OR_ABOVE_CPP_20
#define CONSTEXPR constexpr
#else
#define CONSTEXPR
#endif

#ifdef __SSE2__

struct f32x4 {
    __m128 v{};

    inline CONSTEVAL f32x4() = default;
    inline CONSTEXPR f32x4(__m128 v) : v(v) {}
    inline CONSTEXPR f32x4(const f32 *mem_addr) : v(_mm_load_ps(mem_addr)) {}

    static INLINE CONSTEVAL f32x4 zero() { return f32x4{}; }

#ifdef AT_OR_ABOVE_CPP_20
    template <f32 v3, f32 v2, f32 v1, f32 v0>
    static INLINE CONSTEVAL f32x4 set() { return _mm_set_ps(v3, v2, v1, v0); }
#endif

#ifdef AT_OR_ABOVE_CPP_20
    template <f32 v>
    static INLINE CONSTEVAL f32x4 set() { return _mm_set1_ps(v); }
#endif

    static INLINE CONSTEXPR f32x4 set(f32 v3, f32 v2, f32 v1, f32 v0) {
        return _mm_set_ps(v3, v2, v1, v0);
    }

    static INLINE CONSTEXPR f32x4 set(f32 v) {
        return _mm_set1_ps(v);
    }

    static INLINE CONSTEXPR f32x4 load(const f32 *mem_addr) {
        return _mm_load_ps(mem_addr);
    }

    INLINE f32 operator[](usize idx) const {
        return v[idx];
    }

    INLINE CONSTEXPR f32x4 operator+(const f32x4 &x) const { return _mm_add_ps(v, x.v); }
    INLINE CONSTEXPR f32x4 operator+(f32 t) const { return _mm_add_ps(v, _mm_set1_ps(t)); }
    friend INLINE CONSTEXPR f32x4 operator+(f32 t, f32x4 &x) { return x + t; }

    INLINE CONSTEXPR f32x4 operator-(const f32x4 &x) const { return _mm_sub_ps(v, x.v); }
    INLINE CONSTEXPR f32x4 operator-(f32 t) const { return _mm_sub_ps(v, _mm_set1_ps(t)); }
    friend INLINE CONSTEXPR f32x4 operator-(f32 t, const f32x4 &x) { return _mm_sub_ps(_mm_set1_ps(t), x.v); }

    INLINE CONSTEXPR f32x4 operator*(const f32x4 &x) const { return _mm_mul_ps(v, x.v); }
    INLINE CONSTEXPR f32x4 operator*(f32 t) { return _mm_sub_ps(v, _mm_set1_ps(t)); }
    friend INLINE CONSTEXPR f32x4 operator*(f32 t, const f32x4 &x) { return _mm_mul_ps(_mm_set1_ps(t), x.v); }

    INLINE CONSTEXPR f32x4 operator/(const f32x4 &x) const { return _mm_div_ps(v, x.v); }
    INLINE CONSTEXPR f32x4 operator/(f32 t) const { return _mm_div_ps(v, _mm_set1_ps(t)); }
    friend INLINE CONSTEXPR f32x4 operator/(f32 t, const f32x4 &x) { return _mm_div_ps(_mm_set1_ps(t), x.v); }
};

struct f64x2 {
    __m128d v{};

    inline CONSTEVAL f64x2() = default;
    inline CONSTEXPR f64x2(__m128d v) : v(v) {}
    inline CONSTEXPR f64x2(const f64 *mem_addr) : v(_mm_load_pd(mem_addr)) {}

    static INLINE CONSTEVAL f64x2 zero() { return f64x2{}; }

#ifdef AT_OR_ABOVE_CPP_20
    template <f64 v1, f64 v0>
    static INLINE CONSTEVAL f64x2 set() { return _mm_set_pd(v1, v0); }
#endif

#ifdef AT_OR_ABOVE_CPP_20
    template <f64 v>
    static INLINE CONSTEVAL f64x2 set() { return _mm_set1_pd(v); }
#endif

    static INLINE CONSTEXPR f64x2 set(f64 v1, f64 v0) {
        return _mm_set_pd(v1, v0);
    }

    static INLINE CONSTEXPR f64x2 set(f64 v) {
        return _mm_set1_pd(v);
    }

    static INLINE CONSTEXPR f64x2 load(const f64 *mem_addr) {
        return _mm_load_pd(mem_addr);
    }

    INLINE f64 operator[](usize idx) const {
        return v[idx];
    }

    INLINE CONSTEXPR f64x2 operator+(const f64x2 &x) const { return _mm_add_pd(v, x.v); }
    INLINE CONSTEXPR f64x2 operator+(f64 t) const { return _mm_add_pd(v, _mm_set1_pd(t)); }
    friend INLINE CONSTEXPR f64x2 operator+(f64 t, f64x2 &x) { return x + t; }

    INLINE CONSTEXPR f64x2 operator-(const f64x2 &x) const { return _mm_sub_pd(v, x.v); }
    INLINE CONSTEXPR f64x2 operator-(f64 t) const { return _mm_sub_pd(v, _mm_set1_pd(t)); }
    friend INLINE CONSTEXPR f64x2 operator-(f64 t, const f64x2 &x) { return _mm_sub_pd(_mm_set1_pd(t), x.v); }

    INLINE CONSTEXPR f64x2 operator*(const f64x2 &x) const { return _mm_mul_pd(v, x.v); }
    INLINE CONSTEXPR f64x2 operator*(f64 t) { return _mm_sub_pd(v, _mm_set1_pd(t)); }
    friend INLINE CONSTEXPR f64x2 operator*(f64 t, const f64x2 &x) { return _mm_mul_pd(_mm_set1_pd(t), x.v); }

    INLINE CONSTEXPR f64x2 operator/(const f64x2 &x) const { return _mm_div_pd(v, x.v); }
    INLINE CONSTEXPR f64x2 operator/(f64 t) const { return _mm_div_pd(v, _mm_set1_pd(t)); }
    friend INLINE CONSTEXPR f64x2 operator/(f64 t, const f64x2 &x) { return _mm_div_pd(_mm_set1_pd(t), x.v); }
};

#endif  // __SSE2__

#ifdef __AVX2__

struct f32x8 {
    __m256 v{};

    inline CONSTEVAL f32x8() = default;
    inline CONSTEXPR f32x8(__m256 v) : v(v) {}
    inline CONSTEXPR f32x8(const f32 *mem_addr) : v(_mm256_load_ps(mem_addr)) {}

    static INLINE CONSTEVAL f32x8 zero() { return f32x8{}; }

#ifdef AT_OR_ABOVE_CPP_20
    template <f32 v7, f32 v6, f32 v5, f32 v4, f32 v3, f32 v2, f32 v1, f32 v0>
    static INLINE CONSTEVAL f32x8 set() { return _mm256_set_ps(v7, v6, v5, v4, v3, v2, v1, v0); }
#endif

#ifdef AT_OR_ABOVE_CPP_20
    template <f32 v>
    static INLINE CONSTEVAL f32x8 set() { return _mm256_set1_ps(v); }
#endif

    static INLINE CONSTEXPR f32x8 set(f32 v7, f32 v6, f32 v5, f32 v4, f32 v3, f32 v2, f32 v1, f32 v0) {
        return _mm256_set_ps(v7, v6, v5, v4, v3, v2, v1, v0);
    }

    static INLINE CONSTEXPR f32x8 set(f32 v) {
        return _mm256_set1_ps(v);
    }

    static INLINE CONSTEXPR f32x8 load(const f32 *mem_addr) {
        return _mm256_load_ps(mem_addr);
    }

    INLINE f32 operator[](usize idx) const {
        return v[idx];
    }

    INLINE CONSTEXPR f32x8 operator+(const f32x8 &x) const { return _mm256_add_ps(v, x.v); }
    INLINE CONSTEXPR f32x8 operator+(f32 t) const { return _mm256_add_ps(v, _mm256_set1_ps(t)); }
    friend INLINE CONSTEXPR f32x8 operator+(f32 t, f32x8 &x) { return x + t; }

    INLINE CONSTEXPR f32x8 operator-(const f32x8 &x) const { return _mm256_sub_ps(v, x.v); }
    INLINE CONSTEXPR f32x8 operator-(f32 t) const { return _mm256_sub_ps(v, _mm256_set1_ps(t)); }
    friend INLINE CONSTEXPR f32x8 operator-(f32 t, const f32x8 &x) { return _mm256_sub_ps(_mm256_set1_ps(t), x.v); }

    INLINE CONSTEXPR f32x8 operator*(const f32x8 &x) const { return _mm256_mul_ps(v, x.v); }
    INLINE CONSTEXPR f32x8 operator*(f32 t) { return _mm256_sub_ps(v, _mm256_set1_ps(t)); }
    friend INLINE CONSTEXPR f32x8 operator*(f32 t, const f32x8 &x) { return _mm256_mul_ps(_mm256_set1_ps(t), x.v); }

    INLINE CONSTEXPR f32x8 operator/(const f32x8 &x) const { return _mm256_div_ps(v, x.v); }
    INLINE CONSTEXPR f32x8 operator/(f32 t) const { return _mm256_div_ps(v, _mm256_set1_ps(t)); }
    friend INLINE CONSTEXPR f32x8 operator/(f32 t, const f32x8 &x) { return _mm256_div_ps(_mm256_set1_ps(t), x.v); }
};

struct f64x4 {
    __m256d v{};

    inline CONSTEVAL f64x4() = default;
    inline CONSTEXPR f64x4(__m256d v) : v(v) {}
    inline CONSTEXPR f64x4(const f64 *mem_addr) : v(_mm256_load_pd(mem_addr)) {}

    static INLINE CONSTEVAL f64x4 zero() { return f64x4{}; }

#ifdef AT_OR_ABOVE_CPP_20
    template <f64 v3, f64 v2, f64 v1, f64 v0>
    static INLINE CONSTEVAL f64x4 set() { return _mm256_set_pd(v3, v2, v1, v0); }
#endif

#ifdef AT_OR_ABOVE_CPP_20
    template <f64 v>
    static INLINE CONSTEVAL f64x4 set() { return _mm256_set1_pd(v); }
#endif

    static INLINE CONSTEXPR f64x4 set(f64 v3, f64 v2, f64 v1, f64 v0) {
        return _mm256_set_pd(v3, v2, v1, v0);
    }

    static INLINE CONSTEXPR f64x4 set(f64 v) {
        return _mm256_set1_pd(v);
    }

    static INLINE CONSTEXPR f64x4 load(const f64 *mem_addr) {
        return _mm256_load_pd(mem_addr);
    }

    INLINE f64 operator[](usize idx) const {
        return v[idx];
    }

    INLINE CONSTEXPR f64x4 operator+(const f64x4 &x) const { return _mm256_add_pd(v, x.v); }
    INLINE CONSTEXPR f64x4 operator+(f64 t) const { return _mm256_add_pd(v, _mm256_set1_pd(t)); }
    friend INLINE CONSTEXPR f64x4 operator+(f64 t, f64x4 &x) { return x + t; }

    INLINE CONSTEXPR f64x4 operator-(const f64x4 &x) const { return _mm256_sub_pd(v, x.v); }
    INLINE CONSTEXPR f64x4 operator-(f64 t) const { return _mm256_sub_pd(v, _mm256_set1_pd(t)); }
    friend INLINE CONSTEXPR f64x4 operator-(f64 t, const f64x4 &x) { return _mm256_sub_pd(_mm256_set1_pd(t), x.v); }

    INLINE CONSTEXPR f64x4 operator*(const f64x4 &x) const { return _mm256_mul_pd(v, x.v); }
    INLINE CONSTEXPR f64x4 operator*(f64 t) { return _mm256_sub_pd(v, _mm256_set1_pd(t)); }
    friend INLINE CONSTEXPR f64x4 operator*(f64 t, const f64x4 &x) { return _mm256_mul_pd(_mm256_set1_pd(t), x.v); }

    INLINE CONSTEXPR f64x4 operator/(const f64x4 &x) const { return _mm256_div_pd(v, x.v); }
    INLINE CONSTEXPR f64x4 operator/(f64 t) const { return _mm256_div_pd(v, _mm256_set1_pd(t)); }
    friend INLINE CONSTEXPR f64x4 operator/(f64 t, const f64x4 &x) { return _mm256_div_pd(_mm256_set1_pd(t), x.v); }
};

#endif  // __AVX2__

#ifdef __AVX512F__

struct f32x16 {
    
};

#endif

#else

#ifdef __SSE2__

typedef __m128 f32x4;
typedef __m128d f64x2;

#define f32x4_add _mm_add_ps
#define f32x4_sub _mm_sub_ps
#define f32x4_mul _mm_mul_ps
#define f32x4_div _mm_div_ps

#define f32x4_and _mm_and_ps
#define f32x4_or  _mm_or_ps
#define f32x4_xor _mm_xor_ps

#define f32x4_lt  _mm_cmplt_ps
#define f32x4_le  _mm_cmple_ps
#define f32x4_eq  _mm_cmpeq_ps
#define f32x4_ge  _mm_cmpge_ps
#define f32x4_gt  _mm_cmpgt_ps
#define f32x4_neq _mm_cmpneq_ps

#define f64x2_add _mm_add_pd
#define f64x2_sub _mm_sub_pd
#define f64x2_mul _mm_mul_pd
#define f64x2_div _mm_div_pd

#define f64x2_and _mm_and_pd
#define f64x2_or  _mm_or_pd
#define f64x2_xor _mm_xor_pd

#define f64x2_lt  _mm_cmplt_pd
#define f64x2_le  _mm_cmple_pd
#define f64x2_eq  _mm_cmpeq_pd
#define f64x2_ge  _mm_cmpge_pd
#define f64x2_gt  _mm_cmpgt_pd
#define f64x2_neq _mm_cmpneq_pd

typedef __m128i i8x16;
typedef __m128i i16x8;
typedef __m128i i32x4;
typedef __m128i i64x2;

#endif  // __SSE2__

#ifdef __AVX2__

typedef __m256 f32x8;
typedef __m256d f64x4;

#define f32x8_add _mm256_add_ps
#define f32x8_sub _mm256_sub_ps
#define f32x8_mul _mm256_mul_ps
#define f32x8_div _mm256_div_ps

#define f32x8_and _mm256_and_ps
#define f32x8_or  _mm256_or_ps
#define f32x8_xor _mm256_xor_ps

#define f32x8_lt(__a, __b)  _mm256_cmp_ps((__a), (__b), 0x01)
#define f32x8_le(__a, __b)  _mm256_cmp_ps((__a), (__b), 0x02)
#define f32x8_eq(__a, __b)  _mm256_cmp_ps((__a), (__b), 0x00)
#define f32x8_ge(__a, __b)  _mm256_cmp_ps((__a), (__b), 0x0D)
#define f32x8_gt(__a, __b)  _mm256_cmp_ps((__a), (__b), 0x0E)
#define f32x8_neq(__a, __b) _mm256_cmp_ps((__a), (__b), 0x04)

#define f64x4_add _mm256_add_pd
#define f64x4_sub _mm256_sub_pd
#define f64x4_mul _mm256_mul_pd
#define f64x4_div _mm256_div_pd

#define f64x4_and _mm256_and_pd
#define f64x4_or  _mm256_or_pd
#define f64x4_xor _mm256_xor_pd

#define f64x4_lt(__a, __b)  _mm256_cmp_pd((__a), (__b), 0x01)
#define f64x4_le(__a, __b)  _mm256_cmp_pd((__a), (__b), 0x02)
#define f64x4_eq(__a, __b)  _mm256_cmp_pd((__a), (__b), 0x00)
#define f64x4_ge(__a, __b)  _mm256_cmp_pd((__a), (__b), 0x0D)
#define f64x4_gt(__a, __b)  _mm256_cmp_pd((__a), (__b), 0x0E)
#define f64x4_neq(__a, __b) _mm256_cmp_pd((__a), (__b), 0x04)

#endif  // __AVX2__

#endif  // __cplusplus

#endif  // TYPES_H
