.linecont +

.include "console.inc"
.include "math_macros.inc"
.include "locals.inc"
.include "ppu.inc"
.include "ascii.inc"

.segment "RAM"
Console::window1:	.tag Console::Window
frame_counter:      .res 1

.segment "RODATA"
Console::text_palette: .byte $1D,$11,$10,$30 

.segment "CODE"

; Adjust Y to name a nametable tile such that 0 < Y < 30 is in nametable $2000
; and 30 <= Y < 60 is in nametable $2800. Assumes Y < 60.
; @clobbers A, Y
.macro y_row_to_nt_tile 
    cpy #30
    bcc :+
        ; Y <- Y + 34
        tya
        clc
        adc #34
        tay
    ; FIXME this will fuck up caller's nesting of anonymous labels?
    :
.endmacro

Console::init_console = init_console
.proc init_console
    ; install text_palette as bg palette number PALETTE_NUM
    ldx #0
    :
        lda Console::text_palette, X
        sta PPU::palette_buffer + Console::PALETTE_NUM*4, X
        inx
        cpx #4
        bcc :-

    ; set up cursor sprite, initially off screen
    PPU_update_known_oam_entry Console::CURSOR_SPRITE_NUM, #$ff, \
        #Console::CURSOR_CHAR, #0, #0

    rts
.endproc

Console::update_frame = update_frame
.proc update_frame
    inc frame_counter
    ldx frame_counter
    cpx #1
    bne :+
        jsr draw_cursor
        rts
    :
    cpx #30
    bne :+
        jsr hide_cursor
        rts
    :
    cpx #60
    bne :+
        ldx #0
        stx frame_counter
    :
    rts
.endproc

Console::putch_render_on = putch_render_on
.proc putch_render_on
    ; A = char
    tax
    ldy Console::window1 + Console::Window::CURSOR_Y
    y_row_to_nt_tile
    txa
    ldx Console::window1 + Console::Window::CURSOR_X
    jsr PPU::update_tile_at_xy
    rts
.endproc

Console::putstr_render_on = putstr_render_on
.proc putstr_render_on
    ; AAYY = address of string
    sta addr_0
    sty addr_0+1
    ; TODO.

    rts
.endproc

Console::putch_render_off = putch_render_off
.proc putch_render_off
    ; A = char
    pha
    ldy Console::window1 + Console::Window::CURSOR_Y
    y_row_to_nt_tile
    ldx Console::window1 + Console::Window::CURSOR_X
    jsr PPU::address_tile
    pla
    sta PPU::REG_DATA
    rts
.endproc

Console::putstr_render_off = putstr_render_off
.proc putstr_render_off
    ; AAYY = address of string
    sta addr_0
    sty addr_0+1

    ; TODO: text scrolling. OFFSET_Y
    ; address tile under cursor
    ldx Console::window1 + Console::Window::CURSOR_X
    ldy Console::window1 + Console::Window::CURSOR_Y
    y_row_to_nt_tile
    jsr PPU::address_tile

    ; throughout the char-drawing loop, X tracks value of CURSOR_X, to be saved
    ; at end
    ldx Console::window1 + Console::Window::CURSOR_X
    ldy #0 ; char index
    @each_char:
        lda (addr_0), Y
        beq @done ; A == 0, i.e., we found the null string terminator
        cmp #Ascii::LF
        bne @not_newline
            ; newline
            ; save Y = char index
            tya
            pha
            ; increment CURSOR_Y, reset CURSOR_X to zero, address new tile
            inc Console::window1 + Console::Window::CURSOR_Y
            lda Console::window1 + Console::Window::CURSOR_Y
            cmp #60
            bne :+
                lda #0
                sta Console::window1 + Console::Window::CURSOR_Y
            :
            ldx #0
            ldy Console::window1 + Console::Window::CURSOR_Y
            y_row_to_nt_tile
            jsr PPU::address_tile
            ; restore Y <- char index
            pla
            tay
            ;ldx #0
            iny
            bne @each_char 
            jmp @next_256_chars
        @not_newline:
        sta PPU::REG_DATA
        inx
        cpx #Console::SCREEN_COLS
        bne @char_done
            ; filled a line. increase CURSOR_Y, reset CURSOR_X to 0
            ldx #0
            inc Console::window1 + Console::Window::CURSOR_Y
            lda Console::window1 + Console::Window::CURSOR_Y
            cmp #0
            beq @new_nt
            cmp #60
            bne :+
                lda #0
                sta Console::window1 + Console::Window::CURSOR_Y
                beq @new_nt
            :
            cmp #30
            bne @char_done
            @new_nt:
                tya
                pha
                ldy Console::window1 + Console::Window::CURSOR_Y
                y_row_to_nt_tile
                jsr PPU::address_tile
                pla
                tay
        @char_done:
        iny
        bne @each_char ; Note: won't draw more than 255 chars
    @next_256_chars:
        ; we got through 256 chars (as many as we can index with Y) without
        ; encountering a null. Increment the page (i.e., MSB) of our string
        ; pointer and continue
        ldy addr_0+1
        iny
        sty addr_0+1
        ldy #0
        jmp @each_char
    @done: ; done drawing chars
    ; X = new CURSOR_X
    stx Console::window1 + Console::Window::CURSOR_X
    jsr draw_cursor
    rts
.endproc

Console::hide_cursor = hide_cursor
.proc hide_cursor
    lda #$ff
    PPU_sta_oam_byte Console::CURSOR_SPRITE_NUM, PPU::OAMEntry::y_coord
    rts
.endproc

.segment "RAM"
temp16:     .res 2
temp16_2:   .res 2

.segment "CODE"

Console::draw_cursor = draw_cursor
.proc draw_cursor
    ; cursor is a sprite with pixel-space coordinates = 8 * cursor's tile-space
    ; coordinates
    ; CURSOR_X
    lda Console::window1 + Console::Window::CURSOR_X
    ; clamp cursor location within bounds of window
    cmp #Console::SCREEN_COLS
    bmi :+
        ; cursor x can be at most SCREEN_COLS-1
        lda #Console::SCREEN_COLS-1
        sta Console::window1 + Console::Window::CURSOR_X
    :
    cmp #0
    bpl :+
        ; cursor x must be at least 0
        lda #0
        sta Console::window1 + Console::Window::CURSOR_X
    :
    asl ; A <- A * 8
    asl
    asl
    PPU_sta_oam_byte Console::CURSOR_SPRITE_NUM, PPU::OAMEntry::x_coord

    ; CURSOR_Y
    lda Console::window1 + Console::Window::CURSOR_Y
    ; clamp cursor location within bounds of window text
    cmp Console::window1 + Console::Window::TEXT_ROWS
    bmi :+
        ; cursor y can be at most the window's TEXT_ROWS-1
        lda #0 ; wrap to top
        sta Console::window1 + Console::Window::CURSOR_Y
    :
    tay
    cmp #0
    bpl :+
        ; cursor y must be at least 0
        ldy Console::window1 + Console::Window::TEXT_ROWS
        dey ; wrap to bottom
        sty Console::window1 + Console::Window::CURSOR_Y
    :
    ; Y = cursor_y

    ; we'll compute a 16-bit pixelspace y position for cursor
    ypos16 = temp16
    scroll16 = temp16_2
    ldx #0          ; X will = 0 if top nametable, 1 if bottom nametable
    stx ypos16+1    ; init MSB of ypos to 0
    stx scroll16 + 0
    stx scroll16 + 1
    lda #%00000010
    bit PPU::ctrl
    beq :+
        inx ; bottom nametable
    :

    cpx #0
    beq @no_adjust ; branch if top nametable
    ; bottom nametable
    cpy #Console::SCREEN_ROWS+1
    bcc :+
        ; bottom nametable && CURSOR_Y > SCREEN_ROWS
        ; adjust: A <- CURSOR_Y - SCREEN_ROWS
        tya 
        sec
        sbc #Console::SCREEN_ROWS
        tay
        ; unconditional branch since we know the above subtraction cannot have
        ; given a negative result
        bcs @no_adjust
        ;jmp @no_adjust
    :
    ; bottom nametable && CURSOR_Y <= SCREEN_ROWS. adjust scroll
    lda PPU::scroll_y
    sec
    sbc #240
    sta scroll16
    lda #0
    sbc #0
    sta scroll16 + 1

    bcc @compute_ypos ; unconditional
    ;jmp @compute_ypos

    @no_adjust:
    lda PPU::scroll_y
    sta scroll16

    @compute_ypos:
    ; Y = adjusted CURSOR_Y; store to LSB of ypos16
    ; ypos16 <- Y * 8 (16bit)
    sty ypos16
    .repeat 3
        asl ypos16 + 0
        rol ypos16 + 1
    .endrepeat

    ; scroll16 = adjusted scroll_y
    ; ypos16 <- ypos16 - scroll16 (16bit)
    sec
    lda ypos16
    sbc scroll16
    sta ypos16
    lda ypos16   + 1
    sbc scroll16 + 1
    ; sta ypos16   + 1 ; no need to store
    bne @offscreen

    ldy ypos16 + 0
    cpy #0
    beq :+
        ; sprite gets displayed at pixel y+1, so decrement y UNLESS we are at
        ; y=0, in which case we just have to live with it
        dey
    :
    PPU_sty_oam_byte Console::CURSOR_SPRITE_NUM, PPU::OAMEntry::y_coord
    rts

    @offscreen:
    ldy #$ff
    PPU_sty_oam_byte Console::CURSOR_SPRITE_NUM, PPU::OAMEntry::y_coord
    rts
.endproc

.segment "RAM"
; line buffer for word wrapping. long enough for a line of characters plus a
; newline and a terminating null
wordwrap_buffer: .res Console::SCREEN_COLS + 2

.segment "CODE"

; clobbers A, X, Y, addr_1, local_1
Console::putstr_wordwrapped_render_off = putstr_wordwrapped_render_off
.proc putstr_wordwrapped_render_off
    ; use addr_1 so not to clobber putstr's addr_0
    string              = addr_1
    chars_since_space   = local_1
    buflen              = Console::SCREEN_COLS ; NOT + 2
    ; AAYY = address of string
    sta string 
    tya
    sta string+1

    @start:
    lda #0
    sta chars_since_space
    ldy #0 ; Y = index in input string
    ; X = index in buffer. We start filling buffer from index equal to cursor's
    ; X position, so that "buffer is full" when "line is full"
    ldx Console::window1 + Console::Window::CURSOR_X 
    @each_char:
        inc chars_since_space
        ; copy char to buffer
        lda (string), Y
        iny
        sta wordwrap_buffer, X
        inx
        cmp #0
        ; char was = null ?
        beq @input_null
        ; char was = \n ?
        cmp #Ascii::LF
        beq @input_newline
        ; char was = ' ' ?
        cmp #Ascii::SPACE
        bne :+
            lda #0
            sta chars_since_space
        :
        ; buffer full?
        cpx #buflen
        bne @each_char ; not full, continue to next char
        ; buffer is full
        ; If next char in input string is space, newline, or null, we
        ; can just consume it and output the line
        lda (string), Y
        bne :+
            ; null -- don't increment Y
            jmp @input_newline
        :
        cmp #Ascii::LF
        bne :+
            iny
            jmp @input_newline
        :
        cmp #Ascii::SPACE
        bne :+
            iny
            jmp @input_newline
        :

        ; otherwise, rewind X back to index of last space we wrote
        txa
        sec
        sbc chars_since_space
        bne :+
            ; we had to rewind all the way to the beginning of the buffer,
            ; which means there was no previous space char. no choice but to
            ; character-wrap
            ldx #buflen
            jmp @input_newline
        :
        tax
        ; X = index in buffer of last space we wrote. replace it with a
        ; newline, and rewind Y, our index into the source string, by the same
        ; amount
        cpx #buflen
        beq @input_newline  ; no need to write newline, this line ends with a
                            ; space
        lda #Ascii::LF
        sta wordwrap_buffer, X
        inx
        tya
        sec
        sbc chars_since_space
        tay
        ; continue on to @input_newline
    
    @input_newline:
        ; copied a newline to buffer before filling buffer. render the buffer,
        ; advance the string pointer by number of chars read, and continue.
        ; null-terminate the buffer
        lda #0
        sta wordwrap_buffer, X
        ; save Y
        tya
        pha
        ; render buffer
        lda #<wordwrap_buffer
        clc
        adc Console::window1 + Console::Window::CURSOR_X
        ldy #>wordwrap_buffer
        bcc :+
            iny
        :
        jsr putstr_render_off
        ; restore Y in A
        pla
        ; advance string pointer
        clc
        adc string
        sta string
        bcc :+
            lda #0
            adc string + 1
            sta string + 1
        :
        jmp @start

    @input_null:
        ; we finished the input string. render buffer and be done
        lda #<wordwrap_buffer
        clc
        adc Console::window1 + Console::Window::CURSOR_X
        ldy #>wordwrap_buffer
        bcc :+
            iny
        :
        jsr putstr_render_off
    rts
.endproc

Console::scroll_up = scroll_up
.proc scroll_up
    ; A = # pixels to scroll up
    ; A <- PPU::scroll_y - A (reverse subtraction)
    eor #$ff
    sec
    adc PPU::scroll_y

    sta PPU::scroll_y
    bcs @done
        ; new scroll_y is < 0. add 240 back to it and toggle the vertical
        ; nametable
        clc
        adc #240
        sta PPU::scroll_y
        ; test current vertical nametable
        lda #%00000010
        bit PPU::ctrl
        bne :+
            ; currently cleared; set it
            lda PPU::ctrl
            ora #%00000010
            sta PPU::ctrl
            rts
        :
        ; currently set; clear it
        lda PPU::ctrl
        and #%11111101
        sta PPU::ctrl
    @done:
    rts
.endproc

Console::scroll_down = scroll_down
.proc scroll_down
    ; A = # pixels to scroll down
    clc
    adc PPU::scroll_y
    sta PPU::scroll_y
    cmp #240
    bcc @done ; scroll_y < 240?
        ; we've scrolled >= 240 px; toggle vertical nametable
        sec
        sbc #240
        ; new scroll_y is modulus 240. assume we didn't scroll more than a
        ; whole screenful at once
        sta PPU::scroll_y 
        ; test current vertical nametable
        lda #%00000010
        bit PPU::ctrl
        bne :+
            ; currently cleared; set it
            lda PPU::ctrl
            ora #%00000010
            sta PPU::ctrl
            rts
        :
        ; currently set; clear it
        lda PPU::ctrl
        and #%11111101
        sta PPU::ctrl
    @done:
    rts
.endproc
