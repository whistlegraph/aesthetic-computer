; Background Tile Example for the Nintendo Game Boy
; by Dave VanEe 2022
; Tested with RGBDS 1.0.0
; License: CC0 (https://creativecommons.org/publicdomain/zero/1.0/)

include "hardware.inc"  ; Include hardware definitions so we can use nice names for things

; Define a section that starts at the point the bootrom execution ends
SECTION "Start", ROM0[$0100]
    jp EntryPoint       ; Jump past the header space to our actual code

    ds $150-@, 0        ; Allocate space for RGBFIX to insert our ROM header by allocating
                        ;  the number of bytes from our current location (@) to the end of the
                        ;  header ($150)

EntryPoint:
    di                  ; Disable interrupts as we won't be using them
    ld sp, $e000        ; Set the stack pointer to the end of WRAM

    ; Turn off the LCD when it's safe to do so (during VBlank)
.waitVBlank
    ldh a, [rLY]        ; Read the LY register to check the current scanline
    cp SCREEN_HEIGHT_PX ; Compare the current scanline to the first scanline of VBlank
    jr c, .waitVBlank   ; Loop as long as the carry flag is set
    ld a, 0             ; Once we exit the loop we're safely in VBlank
    ldh [rLCDC], a      ; Disable the LCD (must be done during VBlank to protect the LCD)

    ; Copy our tiles to VRAM
    ld hl, TileData     ; Load the source address of our tiles into HL
    ld de, STARTOF(VRAM); Load the destination address in VRAM into DE
    ld b, 2 * TILE_SIZE ; Load the number of bytes to copy into B (2 * 16)
.copyLoop
    ld a, [hl]          ; Load a byte from the address HL points to into the register A
    ld [de], a          ; Load the byte in the A register to the address DE points to
    inc hl              ; Increment the source pointer in HL
    inc de              ; Increment the destination pointer in DE
    dec b               ; Decrement the loop counter in B
    jr nz, .copyLoop    ; If B isn't zero, continue looping

    ; This code writes 1 for our tile, and then fills the rest of the tilemap with zero
    ld hl, TILEMAP0     ; Point HL to the first byte of the tilemap ($9800)
    ld [hl], 1          ; Load one into the first byte of the tilemap, as pointed to by HL
    inc hl              ; Increment the destination pointer in HL

    ld bc, TILEMAP1-TILEMAP0-1 ; Load the size of the remaining tilemap into BC (32x32=1024, or $400, minus 1)
    ld d, 0             ; Load the value to fill the rest of the tilemap with into D
.clearLoop
    ld [hl], d          ; Load the value in D into the location pointed to by HL
    inc hl              ; Increment the destination pointer in HL
    dec bc              ; Decrement the loop counter in BC
    ld a, b             ; Load the value in B into A
    or c                ; Logical OR the value in A (from B) with C
    jr nz, .clearLoop   ; If B and C are both zero, OR C will be zero, otherwise keep looping

    ; Setup palettes and scrolling
    ld a, %11100100     ; Define a 4-shade palette from darkest (11) to lightest (00)
    ldh [rBGP], a       ; Set the background palette

    ld a, 0             ; Load zero into the register A
    ldh [rSCX], a       ; Set the background scroll registers to show the top-left
    ldh [rSCY], a       ;  corner of the background in the top-left corner of the screen

    ; Combine flag constants defined in hardware.inc into a single value with logical ORs and load it into A
    ; Note that some of these constants (LCDC_OBJ_OFF, LCDC_WIN_OFF) are zero, but are included for clarity
    ld a, LCDC_ON | LCDC_BLOCK01 | LCDC_BG_ON | LCDC_OBJ_OFF | LCDC_WIN_OFF
    ldh [rLCDC], a      ; Enable and configure the LCD to show the background

LoopForever:
    jr LoopForever      ; Loop forever

; Our tile data in 2bpp planar format (https://gbdev.io/pandocs/Tile_Data.html)
TileData:
.empty ; The empty tile is just a zero byte repeated 16 times, so we use REPT to simplify this
    REPT TILE_SIZE
    db $00
    ENDR
.block ; For the block tile we'll use a more verbose syntax so you can see how the tile is built
    ;  low byte   high byte
    db %00000000, %11111111 ; single row of 8 pixels
    db %01000010, %10000001
    db %00000000, %11111111
    db %01000010, %10000001
    db %00000000, %11111111
    db %01000010, %10000001
    db %00000000, %11111111
    db %11111111, %11111111
