; ___mulint32 calls are generated by VBCC when working with the long type in C
; 
; implemented using the EAE of QNICE
;
; based on a stripped down and optimized version of MTH$MULU32
; please have a look at monitor/math_library.asm to see how it works, as
; this one here is barely commented
;
; performs a 32bit x 32bit = 32bit multiplication, therefore it can be used
; equally for signed and for unsigned
;
; expects lo(param1)/hi(param1) = R8/R9
;         lo(param2)/hi(param2) = stack, little endian
; outputs lo(result)/hi(result) = R8/R9
;
; done by sy2002 in November 2016

    .text
    .global ___mulint32

    .include "qnice-conv.vasm"
    .include "sysdef.vasm"

___mulint32:

    INCRB

    ; save arguments as in R1|R0 * R3|R2
    MOVE    R8, R0                  ; args by VBCC via registers
    MOVE    R9, R1

    MOVE    R13, R3                 ; args by VBCC on stack
    ADD     1, R3
    MOVE    @R3++, R2
    MOVE    @R3, R3

    ; algorithm:
    ;       R1R0
    ; x     R3R2
    ; ----------
    ;       R2R0
    ; +   R2R1
    ; +   R3R0
    ; ----------

    MOVE    IO$EAE_OPERAND_0, R4
    MOVE    IO$EAE_CSR, R5
    MOVE    EAE$MULU, R6
    MOVE    IO$EAE_RESULT_LO, R11

    MOVE    R4, R7
    MOVE    R0, @R7++
    MOVE    R2, @R7
    MOVE    R6, @R5
    MOVE    R11, R7
    MOVE    @R7++, R8
    MOVE    @R7, R9

    MOVE    R4, R7
    MOVE    R1, @R7++
    MOVE    R2, @R7
    MOVE    R6, @R5
    ADD     @R11, R9

    MOVE    R4, R7
    MOVE    R0, @R7++
    MOVE    R3, @R7
    MOVE    R6, @R5
    ADD     @R11, R9

    DECRB
    RET

    .type   ___mulint32, @function
    .size   ___mulint32, $-___mulint32
    .global ___mulint32
