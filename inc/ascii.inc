.ifndef Ascii_included
Ascii_included = 1

.scope Ascii

    ; Low-level 8-bit binary coded decimal conversion routine. Converts an
    ; unsigned 8-bit binary number to two or three BCD digits in no
    ; more than 80 cycles.
    ; @jsr
    ; @param A The number to convert
    ; @returns A: low digit; local_1: upper digits as nibbles
    ; No other memory or register is touched.
    .global bcd8

    ; Unsigned 8bit integer to ascii buffer
    ; @jsr
    ; @param AY Pointer to destination buffer. Must have enough space for the
    ; largest possible string result, i.e. 4 bytes in the general case
    ; (e.g., "123\0").
    ; @param X The integer value.
    ; @clobbers A, X, Y, local_1, addr_0
    ; @returns Y Length of string + 1
    .global ui8toa

    ; Unsigned 8bit integer to ascii with callback
    ; @jsr
    ; @param AY address of callback subroutine. Will be called on each output
    ; char, with the char in A (X and Y arbitrary).
    ; @param X The integer value.
    ; @clobbers A, X, Y
    ;.global ui8toa_cb
    
    ; Signed 8bit integer to ascii buffer
    ; @jsr
    ; @param AY Pointer to destination buffer. Must have enough space for the
    ; largest possible string result, i.e. 5 bytes in the general case
    ; (e.g., "-123\0").
    ; @param X The signed integer value.
    ; @clobbers A, X, Y, local_1, addr_0
    ; @returns Y Length of string (not plus 1, unlike ui8toa)
    .global si8toa

    ; Signed 8bit integer to ascii callback
    ;.global si8toa_cb

    ; TODO: 16bit versions

    ; Unsigned 16bit integer to ascii buffer
    .global ui16toa

    ; Unsigned 16bit integer to ascii callback
    .global ui16toa_cb

    ; Signed 16bit integer to ascii buffer
    .global si16toa

    ; Signed 16bit integer to ascii callback
    .global si16toa_cb

    ; special character constants
                ; Char                         Dec   Oct   Hex   
                ; ──────────────────────────────────────────────
    NUL   = 0   ; NUL '\0' (null character)    0     000   00    
    SOH   = 1   ; SOH (start of heading)       1     001   01    
    ; Use STXT instead of STX to avoid collision with assembly instruction
    STXT  = 2   ; STX (start of text)          2     002   02    
    ETX   = 3   ; ETX (end of text)            3     003   03    
    EOT   = 4   ; EOT (end of transmission)    4     004   04    
    ENQ   = 5   ; ENQ (enquiry)                5     005   05    
    ACK   = 6   ; ACK (acknowledge)            6     006   06    
    BEL   = 7   ; BEL '\a' (bell)              7     007   07    
    BS    = 8   ; BS  '\b' (backspace)         8     010   08    
    HT    = 9   ; HT  '\t' (horizontal tab)    9     011   09    
    LF    = 10  ; LF  '\n' (new line)          10    012   0A    
    VT    = 11  ; VT  '\v' (vertical tab)      11    013   0B    
    FF    = 12  ; FF  '\f' (form feed)         12    014   0C    
    CR    = 13  ; CR  '\r' (carriage ret)      13    015   0D    
    SO    = 14  ; SO  (shift out)              14    016   0E    
    SI    = 15  ; SI  (shift in)               15    017   0F    
    DLE   = 16  ; DLE (data link escape)       16    020   10    
    DC1   = 17  ; DC1 (device control 1)       17    021   11    
    DC2   = 18  ; DC2 (device control 2)       18    022   12    
    DC3   = 19  ; DC3 (device control 3)       19    023   13    
    DC4   = 20  ; DC4 (device control 4)       20    024   14    
    NAK   = 21  ; NAK (negative ack.)          21    025   15    
    SYN   = 22  ; SYN (synchronous idle)       22    026   16    
    ETB   = 23  ; ETB (end of trans. blk)      23    027   17    
    CAN   = 24  ; CAN (cancel)                 24    030   18    
    EM    = 25  ; EM  (end of medium)          25    031   19    
    SUB   = 26  ; SUB (substitute)             26    032   1A    
    ESC   = 27  ; ESC (escape)                 27    033   1B    
    FS    = 28  ; FS  (file separator)         28    034   1C    
    GS    = 29  ; GS  (group separator)        29    035   1D    
    RS    = 30  ; RS  (record separator)       30    036   1E    
    US    = 31  ; US  (unit separator)         31    037   1F    
    SPACE = 32  ; SPACE                        32    040   20    
    DEL   = 127 ; DEL                          127   177   7F    


.endscope

.endif ; Ascii_included
