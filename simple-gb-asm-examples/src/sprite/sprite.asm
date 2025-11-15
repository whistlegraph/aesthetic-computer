; Sprite Example for the Nintendo Game Boy
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

    ; Copy our tile to VRAM
    ld hl, TileData     ; Load the source address of our tiles into HL
    ld de, STARTOF(VRAM); Load the destination address in VRAM into DE
    ld b, 16            ; Load the number of bytes to copy into B (16 bytes per tile)
.copyLoop
    ld a, [hl]          ; Load a byte from the address HL points to into the register A
    ld [de], a          ; Load the byte in the A register to the address DE points to
    inc hl              ; Increment the source pointer in HL
    inc de              ; Increment the destination pointer in DE
    dec b               ; Decrement the loop counter in B
    jr nz, .copyLoop    ; If B isn't zero, continue looping

    ; Setup the two object palettes
    ld a, %11100100     ; Define a 4-shade palette from darkest (11) to lightest (00)
    ldh [rOBP0], a      ; Set the onject palette 0
    ld a, %00011011     ; Define a 4-shade palette from lightest (00) to darkest (11)
    ldh [rOBP1], a      ; Set the onject palette 1

    ; Set the attributes for a two sprites in OAMRAM
    ld hl, STARTOF(OAM) ; Load the destination address in OAM into HL

    ; The first sprite is in the top-left corner of the screen and uses the "normal" palette
    ld a, 16            ; Load the value 16 into the A register
    ld [hli], a         ; Set the Y coordinate (plus 16) for the sprite in OAMRAM, increment HL
    sub 8               ; Subtract 8 from the value stored in A and store the result in A
    ld [hli], a         ; Set the X coordinate (plus 8) for the sprite in OAMRAM, increment HL
    ld a, 0             ; Load the tile index 0 into the A register
    ld [hli], a         ; Set the tile index for the sprite in OAMRAM, increment HL
    ld [hli], a         ; Set the attributes (flips and palette) for the sprite in OAMRAM, increment HL

    ; The second sprite is shifted slightly and uses the "inverted" palette
    ld a, 19            ; Load the value 19 into the A register
    ld [hli], a         ; Set the Y coordinate (plus 16) for the sprite in OAMRAM, increment HL
    sub 6               ; Subtract 6 from the value stored in A and store the result in A
    ld [hli], a         ; Set the X coordinate (plus 8) for the sprite in OAMRAM, increment HL
    ld a, 0             ; Load the tile index 0 into the A register
    ld [hli], a         ; Set the tile index for the sprite in OAMRAM, increment HL
    ld a, OAM_YFLIP | OAM_PAL1 ; Load the flags to Y flip and use OBP1 for this sprite into A
    ld [hli], a         ; Set the attributes (flips and palette) for the sprite in OAMRAM, increment HL

    ; Zero the Y coordinates of the remaining entries to avoid garbage sprites
    xor a               ; XOR A with itself, which sets A to zero (and affects the flags)
                        ; Note: This is 1 cycle faster and 1 byte smaller than `ld a, 0`
    ld b, OAM_COUNT - 2 ; Load B with the number of loops, which is the total OAM count minus one
.oamClearLoop
    ld [hli], a         ; Set the Y coordinate of this entry to zero, increment HL
    inc l               ; Increment L to advance HL (three times)
    inc l               ; Note: Since OAMRAM is from $FE00->$FE9F we can safely increment
    inc l               ;  only the L register and HL will stay a valid pointer
    dec b               ; Decrement the loop counter in B
    jr nz, .oamClearLoop; If B isn't zero, continue looping

    ; Combine flag constants defined in hardware.inc into a single value with logical ORs and load it into A
    ; Note that some of these constants (LCDC_BG_OFF, LCDC_OBJ_8, LCDC_WIN_OFF) are zero, but are included for clarity
    ld a, LCDC_ON | LCDC_BG_OFF | LCDC_OBJ_8 | LCDC_OBJ_ON | LCDC_WIN_OFF
    ldh [rLCDC], a      ; Enable and configure the LCD to show the background

LoopForever:
    jr LoopForever      ; Loop forever

; Our tile data in 2bpp planar format (https://gbdev.io/pandocs/Tile_Data.html)
TileData:
.ball ; For the ball tile we'll use a more verbose syntax so you can see how the tile is built
    ;  low byte   high byte
    db %00111100, %00111100 ; single row of 8 pixels
    db %01011110, %01000010
    db %10001101, %10000011
    db %11011101, %10000011
    db %11111101, %10000011
    db %10111001, %11000111
    db %01000010, %01111110
    db %00111100, %00111100

