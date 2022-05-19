.linecont +

; This file incorporates modified code from Damian Yerrick's bcd.s, which
; includes the following notices:
;
; Binary to decimal conversion for 8-bit and 16-bit numbers
; Copyright 2012 Damian Yerrick
;
; Copying and distribution of this file, with or without
; modification, are permitted in any medium without royalty provided
; the copyright notice and this notice are preserved in all source
; code copies.  This file is offered as-is, without any warranty.

.include "ascii.inc"
.include "locals.inc"
.include "math_macros.inc"

.segment "CODE"

.macro bcd8bit_iter value
    .local skip
    cmp value
    bcc skip
    sbc value
skip:
    rol highDigits
.endmacro

;;
; Converts an unsigned 8-bit binary number to two or three BCD digits in no
; more than 80 cycles.
; @param A The number to convert
; @return A: low digit; local_1: upper digits as nibbles
; No other memory or register is touched.
Ascii::bcd8 = bcd8
.proc bcd8
    highDigits = local_1

    ; First clear out two bits of highDigits.  (The conversion will
    ; fill in the other six.)
    asl highDigits
    asl highDigits

    ; Each iteration takes 11 if subtraction occurs or 10 if not.
    ; But if 80 is subtracted, 40 and 20 aren't, and if 200 is
    ; subtracted, 80 is not, and at least one of 40 and 20 is not.
    ; So this part takes up to 6*11-2 cycles.
    bcd8bit_iter #200
    bcd8bit_iter #100
    bcd8bit_iter #80
    bcd8bit_iter #40
    bcd8bit_iter #20
    bcd8bit_iter #10
    rts
.endproc

Ascii::ui8toa = ui8toa
.proc ui8toa
    ; AY = address of string buffer
    sta addr_0
    sty addr_0+1
    ; X = the integer
    txa
    do_bcd:
    jsr bcd8
    tax ; X <- low digit
    ldy #0 ; Y = buffer index
    lda local_1
    lsr
    lsr
    lsr
    lsr
    ; A = high digit
    beq skip_high
        clc
        adc #'0'
        sta (addr_0), Y
        iny
    skip_high:
    lda local_1
    and #%00001111
    ; A = middle digit
    bne write_middle
    cpy #0
    beq skip_middle
    write_middle:
        clc
        adc #'0'
        sta (addr_0), Y
        iny
    skip_middle:
    txa ; A <- low digit
    clc
    adc #'0'
    sta (addr_0), Y
    iny
    lda #Ascii::NUL
    sta (addr_0), Y
    rts
.endproc

Ascii::si8toa = si8toa
.proc si8toa
    ; AY = address of string buffer
    ; X = the integer
    cpx #0
    bpl notneg
        ; X is negative (signed)
        sta addr_0
        sty addr_0+1
        ldy #0
        lda #'-'
        sta (addr_0), Y
        mathmac_inc16 addr_0
        ; negate X
        txa
        eor #$ff
        sec
        adc #0
        ; A = -X, Y = 0
        jmp ui8toa::do_bcd
    notneg:
    jmp ui8toa
    ; no rts
.endproc
