// Embedded NNUE network: the bytes of nets/stallion.nnue are linked into the
// binary at build time, so the engine never reads a network file at runtime.
#if defined(__APPLE__)
    .section __DATA,__const
    #define NNUE_SYM(x) _##x
#else
    .section .rodata
    #define NNUE_SYM(x) x
#endif
    .globl NNUE_SYM(stallion_nnue)
    .globl NNUE_SYM(stallion_nnue_end)
    .p2align 6
NNUE_SYM(stallion_nnue):
    .incbin "nets/stallion.nnue"
NNUE_SYM(stallion_nnue_end):
