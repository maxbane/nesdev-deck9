.ifndef Console_included
Console_included = 1

.scope Console

    .struct Window
        CURSOR_X    .byte
        CURSOR_Y    .byte
        TEXT_ROWS   .byte
        ; OFFSET_Y  .byte
    .endstruct

    ; main window state
    .global window1
    .global text_palette

    ; Constants
    SCREEN_COLS         = 32
    SCREEN_ROWS         = 30
    PALETTE_NUM         = 0
    CURSOR_CHAR         = 219 ; Extended ASCII
    CURSOR_SPRITE_NUM   = 1

    .global init_console

    ; call once per logical game frame to update visuals
    .global update_frame

    ; @jsr
    ; @param A Character.
    ; @clobbers A, X, Y
    .global putch_render_on

    ; @jsr
    ; @param A Character.
    ; @clobbers A, X, Y
    .global putch_render_off

    ; @jsr
    ; @param AAYY Pointer to null-terminated string.
    ; @clobbers A, X, Y, addr_0
    .global putstr_render_off

    ; @jsr
    ; @clobbers A, X, Y
    .global draw_cursor

    ; Move cursor sprite to invisible screen position y=$ff.
    ; @jsr
    ; @clobbers A
    .global hide_cursor

    ; @jsr
    ; @param AAYY Pointer to null-terminated string.
    ; @clobbers A, X, Y, addr_0, addr_1, local_1
    .global putstr_wordwrapped_render_off

    ; @jsr
    ; @param A Pixels.
    ; @clobbers A
    .global scroll_up

    ; @jsr
    ; @param A Pixels.
    ; @clobbers A
    .global scroll_down

.endscope

.endif ; Console_included
