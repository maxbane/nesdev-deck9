; Assembler options

.linecont +

; imports

.include "locals.inc"
.include "ines.inc"
.include "ppu.inc"
.include "joy.inc"
.include "constants.inc"
;.include "actor.inc"
;.include "physics.inc"
;.include "actor_routines.inc"
;.include "ai.inc"
.include "coroutine.inc"
.include "effects.inc"
.include "console.inc"
.include "ascii.inc"

.include "math_macros.inc"
;.include "sprites_manifest.inc"

;
; iNES header
;

.segment "HEADER"

INES_PRG_BANK_COUNT = 2 ; 16k PRG bank count
INES_CHR_BANK_COUNT = 1 ; 8k CHR bank count
INES_MAPPER         = 0 ; 0 = NROM (iNES standard mapper number)
INES_MIRROR         = 0 ; 0 = horizontal mirroring, 1 = vertical mirroring
INES_SRAM           = 0 ; 1 = battery backed SRAM at $6000-7FFF

; INES_HEADER macro constructs the header bytes given arguments
INES_HEADER INES_PRG_BANK_COUNT, INES_CHR_BANK_COUNT, INES_MAPPER, INES_MIRROR, INES_SRAM

;
; CHR ROM
;

.segment "TILES"
.incbin "chr/pc-bios.chr"
.incbin "chr/pc-bios.chr"
;.incbin "chr/westminster-ascii.chr"
;.incbin "chr/westminster-ascii.chr"
;.incbin "chr/sprites.chr"
;.incbin "chr/sprites.chr"

;
; interrupt vectors 
;

.segment "VECTORS"
.word PPU::nmi_buffered
.word reset
.word irq

.segment "RODATA"
main_palettes:
.byte $1D,$10,$00,$21 ; sp0 player!
.byte $1D,$09,$19,$29 ; bg1 green
.byte $1D,$01,$11,$21 ; bg2 blue
.byte $1D,$00,$10,$30 ; bg3 greyscale

.byte $1D,$10,$00,$21 ; sp0 player!
.byte $1D,$14,$24,$34 ; sp1 purple
.byte $1D,$1B,$2B,$3B ; sp2 teal
.byte $1D,$12,$22,$32 ; sp3 marine


; .segment "ZEROPAGE"
; Actor_RESERVE_ACTORS Constants::N_ACTORS
; .define the_player actor_00

;
; do-nothing irq
;

.segment "CODE"
irq:
    rti

;
; reset routine
;

.segment "CODE"
.proc reset
    sei       ; mask interrupts
    cld       ; disable decimal mode

    lda #0
    sta $4015 ; disable APU sound
    sta $4010 ; disable DMC IRQ
    lda #$40
    sta $4017 ; disable APU IRQ

    ldx #$FF
    txs       ; initialize stack

    ; clear all RAM to 0 (except $100 stack area)
    lda #0
    ldx #0
    :
        sta $0000, X
        sta $0200, X
        sta $0300, X
        sta $0400, X
        sta $0500, X
        sta $0600, X
        sta $0700, X
        inx
        bne :-

    jsr PPU::reset
    jmp main
    ; no rts
.endproc

;
; main
;

.segment "RODATA"
str_long:   .byte   "We hold these truths to be self-evident: ", \
                    "that all men are created equal; that they are endowed ", \
                    "by their creator with certain unalienable rights; ", \
                    "that among these are life, liberty, and the pursuit ", \
                    "of happiness; that to secure these rights, governments ", \
                    "are instituted among men, deriving their just powers ", \
                    "from the consent of the governed.", Ascii::LF, 0
str_hello:  .byte   "Hello, world.", Ascii::LF, Ascii::LF, \
                    "The world's cats say: ", 0
str_meow:   .asciiz "Meow!" 

str_longword: .byte "0123456789abcdef0123456789ABCDEF0123456789abcdef", 0

.segment "CODE"
.proc hello
    ; draw row of characters starting at addressed tile
    lda #<str_long
    ldy #>str_long
    jsr Console::putstr_wordwrapped_render_off
    rts
.endproc

.proc meow
    ; draw row of characters starting at addressed tile
    lda #<str_meow
    ldy #>str_meow
    jsr Console::putstr_render_off
    rts
.endproc

.segment "RAM"
mynum:  .res 1
mynum_strbuf: .res 5

.segment "CODE"
.proc print_num
    jsr PPU::render_off
    ldx mynum
    inc mynum
    lda #<mynum_strbuf
    ldy #>mynum_strbuf
    jsr Ascii::si8toa
    lda #<mynum_strbuf
    ldy #>mynum_strbuf
    jsr Console::putstr_render_off
    rts
.endproc

.proc test_render_on
    lda str_meow ; first char
    jsr Console::putch_render_on
    ;lda str_meow + 1
    ;jsr PPU::update_next_byte
    rts
.endproc

.proc test_render_off
    jsr PPU::render_off
    lda str_meow
    jsr Console::putch_render_off
    rts
.endproc

.proc main
    ; setup 
    ldx #0
    :
        lda main_palettes, X
        sta PPU::palette_buffer, X
        inx
        cpx #32
        bcc :-

    jsr Console::init_console
    lda #2*Console::SCREEN_ROWS
    sta Console::window1 + Console::Window::TEXT_ROWS
    jsr PPU::clear_background
    ;jsr init_actors

    lda #%00011110
    sta PPU::mask
    lda #%10001000
    sta PPU::ctrl
    ;jsr Effects::init
    jsr hello
    jmp loop_gameplay
    ; no rts
.endproc

.segment "CODE"

; Call with A = Joy::new_buttons_?
.proc handle_input_gameplay
    jsr Joy::store_new_buttons
    sta local_0
    and #Joy::BUTTON_START
    beq :+
        jsr loop_paused
        rts
    :
    lda local_0
    and #Joy::BUTTON_A
    beq :+
        ;jsr PPU::render_off
        ;jsr meow
        ;jsr test_render_on
        ;jsr test_render_off
        jsr print_num
        rts
    :
    lda local_0
    and #Joy::BUTTON_B
    beq :+
        lda #0
        sta mynum
        jsr PPU::render_off
        jsr PPU::clear_background
        rts
    :
    lda local_0
    and #Joy::BUTTON_LEFT
    beq :+
        ldx Console::window1 + Console::Window::CURSOR_X
        dex
        stx Console::window1 + Console::Window::CURSOR_X
        jsr Console::draw_cursor
        rts
    :
    lda local_0
    and #Joy::BUTTON_RIGHT
    beq :+
        ldx Console::window1 + Console::Window::CURSOR_X
        inx
        stx Console::window1 + Console::Window::CURSOR_X
        jsr Console::draw_cursor
        rts
    :
    lda local_0
    and #Joy::BUTTON_UP
    beq :+
        ldy Console::window1 + Console::Window::CURSOR_Y
        dey
        sty Console::window1 + Console::Window::CURSOR_Y
        jsr Console::draw_cursor
        rts
    :
    lda local_0
    and #Joy::BUTTON_DOWN
    beq :+
        ldy Console::window1 + Console::Window::CURSOR_Y
        iny
        sty Console::window1 + Console::Window::CURSOR_Y
        jsr Console::draw_cursor
        rts
    :
    lda local_0
    and #Joy::BUTTON_SELECT
    beq :+
        jsr Console::hide_cursor
    :
    rts
.endproc

; main loop for core gameplay
.proc loop_gameplay
    jsr handle_input_gameplay

    ; Update effects
    ; Coroutine_select Effects::effects
    ; jsr Coroutine::step_all

    jsr Console::update_frame
    ; Wait for NMI
    jsr PPU::update

    ; Loop
    jmp loop_gameplay
.endproc

.proc loop_paused
    ; whoo do nothing
    jsr Console::update_frame
    jsr PPU::update
    jsr Joy::store_new_buttons
    sta local_0
    and #Joy::BUTTON_START
    beq :+
        rts
    :
    lda local_0
    and #Joy::BUTTON_DOWN
    beq :+
        lda #4
        jsr Console::scroll_down
        jsr Console::draw_cursor
        jmp loop_paused
    :
    lda local_0
    and #Joy::BUTTON_UP
    beq :+
        lda #4
        jsr Console::scroll_up
        jsr Console::draw_cursor
        jmp loop_paused
    :
    jmp loop_paused
.endproc
