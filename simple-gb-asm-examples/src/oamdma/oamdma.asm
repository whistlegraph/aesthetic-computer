; OAMDMA Example for the Nintendo Game Boy
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

    ; Copy the OAMDMA routine to HRAM, since during DMA we're limited on which
    ;  memory the CPU can access (but HRAM is safe)
    ld hl, OAMDMA       ; Load the source address of our routine into HL
    ld b, OAMDMA.end - OAMDMA ; Load the length of the OAMDMA routine into B
    ld c, LOW(hOAMDMA)  ; Load the low byte of the destination into C
.oamdmaCopyLoop
    ld a, [hli]         ; Load a byte from the address HL points to into the register A, increment HL
    ldh [c], a          ; Load the byte in the A register to the address in HRAM with the low byte stored in C
    inc c               ; Increment the low byte of the HRAM pointer in C
    dec b               ; Decrement the loop counter in B
    jr nz, .oamdmaCopyLoop ; If B isn't zero, continue looping

    ; Copy our tile to VRAM
    ld hl, TileData     ; Load the source address of our tiles into HL
    ld de, STARTOF(VRAM); Load the destination address in VRAM into DE
    ld b, 16            ; Load the number of bytes to copy into B (16 bytes per tile)
.copyLoop
    ld a, [hli]         ; Load a byte from the address HL points to into the register A, increment HL
    ld [de], a          ; Load the byte in the A register to the address DE points to
    inc de              ; Increment the destination pointer in DE
    dec b               ; Decrement the loop counter in B
    jr nz, .copyLoop    ; If B isn't zero, continue looping

    ; Setup the an object palette
    ld a, %11100100     ; Define a 4-shade palette from darkest (11) to lightest (00)
    ldh [rOBP0], a      ; Set the onject palette 0

    ; Copy our static OAM data to Shadow OAM (in WRAM)
    ld hl, StaticOAMData
    ld de, wShadowOAM   ; Load the destination address in WRAM into DE
    ld b, StaticOAMData.end - StaticOAMData ; Load the length of the data into B
.oamDataLoop
    ld a, [hli]         ; Load a byte from the address HL points to into the register A, increment HL
    ld [de], a          ; Load the byte in the A register to the address DE points to
    inc e               ; Increment the destination pointer low byte, since wShadow
    dec b               ; Decrement the loop counter in B
    jr nz, .oamDataLoop ; If B isn't zero, continue looping

    ; Initiate the OAM DMA routine
    ld a, HIGH(wShadowOAM) ; Load the high byte of our Shadow OAM buffer into A
    call hOAMDMA         ; Call our OAM DMA routine (in HRAM), quickly copying from wShadowOAM to OAMRAM

    ; Combine flag constants defined in hardware.inc into a single value with logical ORs and load it into A
    ; Note that some of these constants (LCDC_BG_OFF, LCDC_OBJ_8, LCDC_WIN_OFF) are zero, but are included for clarity
    ld a, LCDC_ON | LCDC_BG_OFF | LCDC_OBJ_8 | LCDC_OBJ_ON | LCDC_WIN_OFF
    ldh [rLCDC], a      ; Enable and configure the LCD to show the background

LoopForever:
    jr LoopForever      ; Loop forever


SECTION "Shadow OAM", WRAM0, ALIGN[8]
; Reserve page-aligned space for a Shadow OAM buffer, to which we can safely write OAM data at any time, 
;  and then use our OAM DMA routine to copy it quickly to OAMRAM when desired. OAM DMA can only operate
;  on a block of data that starts at a page boundary, which is why we use ALIGN[8].
wShadowOAM:
    ds OAM_SIZE


SECTION "OAM DMA Routine", ROMX
; Initiate OAM DMA and then wait until the operation is complete, then return
; @param A High byte of the source data to DMA to OAM
OAMDMA:
    ldh [rDMA], a
    ld a, OAM_COUNT
.waitLoop
    dec a
    jr nz, .waitLoop
    ret
.end


SECTION "OAM DMA", HRAM
; Reserve space in HRAM for the OAMDMA routine, equal in length to the routine
hOAMDMA:
    ds OAMDMA.end - OAMDMA


SECTION "Tile Data", ROMX
; Our tile data in 2bpp planar format (https://gbdev.io/pandocs/Tile_Data.html)
TileData:
.ball ; Use the "Game Boy Graphics" compact representation of the tile data
    dw `00333300
    dw `03011130
    dw `30001123
    dw `31011123
    dw `31111123
    dw `32111223
    dw `03222230
    dw `00333300


SECTION "Static OAM Data", ROMX
; A table of Y, X, Index, Attr for a collection of 40 sprites, generated at 
;  assembly time using RGBDS trigonometry functions.
; See: https://rgbds.gbdev.io/docs/master/rgbasm.5#Fixed-point_expressions
StaticOAMData:
FOR ANGLE, 0.0, 1.0, 1.0 / 40 ; delta = 1 full turn / 40 entries
	db SIN(ANGLE) >> 10 + 84 ; Y coordinate
	db COS(ANGLE) >> 10 + 84 ; X coordinate
	db 0, 0 ; tile index, attr
ENDR
.end
