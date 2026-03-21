; Background Tilemap Example for the Nintendo Game Boy
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
    ld bc, TileData.end - TileData ; Load the number of bytes to copy into BC
.copyLoop
    ld a, [hl]          ; Load a byte from the address HL points to into the A register
    ld [de], a          ; Load the byte in the A register to the address DE points to
    inc hl              ; Increment the source pointer in HL
    inc de              ; Increment the destination pointer in DE
    dec bc              ; Decrement the loop counter in BC
    ld a, b             ; Load the value in B into A
    or c                ; Logical OR the value in A (from B) with C
    jr nz, .copyLoop    ; If B and C are both zero, OR B will be zero, otherwise keep looping

    ; Copy our 20x18 tilemap to VRAM
    ld de, TilemapData  ; Load the source address of our tilemap into DE
    ld hl, TILEMAP0     ; Point HL to the first byte of the tilemap ($9800)
    ld b, SCREEN_HEIGHT ; Load the height of the screen in tiles into B (18 tiles)
.tilemapLoop
    ld c, SCREEN_WIDTH  ; Load the width of the screen in tiles into C (20 tiles)
.rowLoop
    ld a, [de]          ; Load a byte from the address DE points to into the A register
    ld [hli], a         ; Load the byte in the A register to the address HL points to and increment HL
    inc de              ; Increment the source pointer in DE
    dec c               ; Decrement the loop counter in C (tiles per row)
    jr nz, .rowLoop     ; If C isn't zero, continue copying bytes for this row
    push de             ; Push the contents of the register pair DE to the stack
    ld de, TILEMAP_WIDTH - SCREEN_WIDTH ; Load the number of tiles remaining in the row into DE
    add hl, de          ; Add the remaining row length to HL, advancing the destination pointer to the next row
    pop de              ; Recover the former contents of the the register pair DE
    dec b               ; Decrement the loop counter in B (total rows)
    jr nz, .tilemapLoop ; If B isn't zero, continue copying rows

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

; Tiles from public domain "Abandonauts" tileset by Adam Atomic (http://adamatomic.com/abandonauts/)
TileData:
    incbin "abandonauts-tilemap.2bpp"   ; Include binary tile data inline using incbin
.end                                    ; The .end label is used to let the assembler calculate the length of the data

TilemapData:
    incbin "abandonauts-tilemap.tilemap" ; Both the tiles and tilemap are generated using RGBGFX in the build script