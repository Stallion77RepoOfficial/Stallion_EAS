#ifndef _STDENDIAN_H_
#define _STDENDIAN_H_
 
 
#if defined (__cplusplus)
#include <cstdint>
#elif !defined (__OPENCL_VERSION__)
#include <stdint.h>
#endif

 
#if defined(__linux__) || defined(__GLIBC__) || defined(__CYGWIN__)
#include <endian.h>
#include <byteswap.h>
#define __ENDIAN_DEFINED        1
#define __BSWAP_DEFINED         1
#define __HOSTSWAP_DEFINED      1
 
#ifndef _BYTE_ORDER
#define _BYTE_ORDER             __BYTE_ORDER
#define _LITTLE_ENDIAN          __LITTLE_ENDIAN
#define _BIG_ENDIAN             __BIG_ENDIAN
#endif
#define bswap16(x)              bswap_16(x)
#define bswap32(x)              bswap_32(x)
#define bswap64(x)              bswap_64(x)
#endif  

 
#if defined(__FreeBSD__) || defined(__NetBSD__) || \
  defined(__DragonFly__) || defined(__OpenBSD__)
#include <sys/endian.h>
#define __ENDIAN_DEFINED        1
#define __BSWAP_DEFINED         1
#define __HOSTSWAP_DEFINED      1
#endif  

 
#if defined (sun)
#include <sys/isa_defs.h>
 
#if defined(_LITTLE_ENDIAN)
#undef _LITTLE_ENDIAN
#define _LITTLE_ENDIAN          1234
#define _BIG_ENDIAN             4321
#define _BYTE_ORDER             _LITTLE_ENDIAN
#elif defined(_BIG_ENDIAN)
#undef _BIG_ENDIAN
#define _LITTLE_ENDIAN          1234
#define _BIG_ENDIAN             4321
#define _BYTE_ORDER             _BIG_ENDIAN
#endif
#define __ENDIAN_DEFINED        1
#endif  

 
#if defined(_WIN32) || defined(_MSC_VER) || defined(__EMSCRIPTEN__)
 
 
#define _LITTLE_ENDIAN          1234
#define _BIG_ENDIAN             4321
#define _BYTE_ORDER             _LITTLE_ENDIAN
#define __ENDIAN_DEFINED        1
#endif  

 
#if defined(__APPLE__)
#include <machine/endian.h>
#define _BYTE_ORDER             BYTE_ORDER
#define _LITTLE_ENDIAN          LITTLE_ENDIAN
#define _BIG_ENDIAN             BIG_ENDIAN
#define __ENDIAN_DEFINED        1
#endif  

 
#if defined (__OPENCL_VERSION__)
#define _LITTLE_ENDIAN          1234
#define __BIG_ENDIAN            4321
#if defined (__ENDIAN_LITTLE__)
#define _BYTE_ORDER             _LITTLE_ENDIAN
#else
#define _BYTE_ORDER             _BIG_ENDIAN
#endif
#define bswap16(x)              as_ushort(as_uchar2(ushort(x)).s1s0)
#define bswap32(x)              as_uint(as_uchar4(uint(x)).s3s2s1s0)
#define bswap64(x)              as_ulong(as_uchar8(ulong(x)).s7s6s5s4s3s2s1s0)
#define __ENDIAN_DEFINED        1
#define __BSWAP_DEFINED         1
#endif

 
#if !__ENDIAN_DEFINED
#error Could not determine CPU byte order
#endif

 
#ifndef BYTE_ORDER
#define BYTE_ORDER _BYTE_ORDER
#endif
#ifndef LITTLE_ENDIAN
#define LITTLE_ENDIAN _LITTLE_ENDIAN
#endif
#ifndef BIG_ENDIAN
#define BIG_ENDIAN _BIG_ENDIAN
#endif

 
#if _BYTE_ORDER == _LITTLE_ENDIAN
#if !defined (__ENDIAN_LITTLE__)
#define __ENDIAN_LITTLE__   1
#endif
#endif

 
#if !__BSWAP_DEFINED

#ifndef bswap16
 
#if defined __GNUC__
#define bswap16(x) __builtin_bswap16(x)
#else
inline uint16_t bswap16(uint16_t x) {
  return (uint16_t)((((uint16_t) (x) & 0xff00) >> 8) | \
                    (((uint16_t) (x) & 0x00ff) << 8));
}
#endif
#endif

#ifndef bswap32
#if defined __GNUC__
#define bswap32(x) __builtin_bswap32(x)
#else
inline uint32_t bswap32(uint32_t x) {
  return (( x & 0xff000000) >> 24) | \
  (( x & 0x00ff0000) >> 8) | \
  (( x & 0x0000ff00) << 8) | \
  (( x & 0x000000ff) << 24);
}
#endif
#endif

#ifndef bswap64
#if defined __GNUC__
#define bswap64(x) __builtin_bswap64(x)
#else
inline uint64_t bswap64(uint64_t x) {
    return (( x & 0xff00000000000000ull) >> 56) | \
  (( x & 0x00ff000000000000ull) >> 40) | \
  (( x & 0x0000ff0000000000ull) >> 24) | \
  (( x & 0x000000ff00000000ull) >> 8) | \
  (( x & 0x00000000ff000000ull) << 8) | \
  (( x & 0x0000000000ff0000ull) << 24) | \
  (( x & 0x000000000000ff00ull) << 40) | \
  (( x & 0x00000000000000ffull) << 56);
}
#endif
#endif

#endif

 
#ifndef __HOSTSWAP_DEFINED
#if __BYTE_ORDER == __LITTLE_ENDIAN
#define htobe16(x)              bswap16((x))
#define htole16(x)              ((uint16_t)(x))
#define be16toh(x)              bswap16((x))
#define le16toh(x)              ((uint16_t)(x))

#define htobe32(x)              bswap32((x))
#define htole32(x)              ((uint32_t((x))
#define be32toh(x)              bswap32((x))
#define le32toh(x)              ((uint32_t)(x))

#define htobe64(x)              bswap64((x))
#define htole64(x)              ((uint64_t)(x))
#define be64toh(x)              bswap64((x))
#define le64toh(x)              ((uint64_t)(x))
#elif __BYTE_ORDER == __BIG_ENDIAN
#define htobe16(x)              ((uint16_t)(x))
#define htole16(x)              bswap16((x))
#define be16toh(x)              ((uint16_t)(x))
#define le16toh(x)              bswap16((x))

#define htobe32(x)              ((uint32_t)(x))
#define htole32(x)              bswap32((x))
#define be32toh(x)              ((uint32_t)(x))
#define le64toh(x)              bswap64((x))

#define htobe64(x)              ((uint64_t)(x))
#define htole64(x)              bswap64((x))
#define be64toh(x)              ((uint64_t)(x))
#define le32toh(x)              bswap32((x))
#endif
#endif

 
#endif
