; VBlank (with OAM DMA) Example for the Nintendo Game Boy
; by Dave VanEe 2022
; Tested with RGBDS 1.0.0
; License: CC0 (https://creativecommons.org/publicdomain/zero/1.0/)

include "hardware.inc"  ; Include hardware definitions so we can use nice names for things

; The VBlank vector is where execution is passed when the VBlank interrupt fires
SECTION "VBlank Vector", ROM0[$40]
; We only have 8 bytes here, so push all the registers to the stack and jump to the rest of the handler
; Note: Since the VBlank handler used here only affects A and F, we don't have to push/pop BC, DE, and HL,
;  but it's done here for demonstration purposes.
VBlank:
    push af             ; Push AF, BC, DE, and HL to the stack
    push bc
    push de
    push hl
    jp VBlankHandler    ; Jump to the rest of the handler


; The rest of the handler is contained in ROM0 to ensure it's always accessible without banking
SECTION "VBlank Handler", ROM0
VBlankHandler:
    ; Initiate the OAM DMA routine
    ld a, HIGH(wShadowOAM) ; Load the high byte of our Shadow OAM buffer into A
    call hOAMDMA         ; Call our OAM DMA routine (in HRAM), quickly copying from wShadowOAM to OAMRAM

    pop hl              ; Pop HL, DE, BC, and AF off the stack (reverse order from the earlier pushes)
    pop de
    pop bc
    pop af
    reti                ; Return and enable interrupts (ret + ei)


; Define a section that starts at the point the bootrom execution ends
SECTION "Start", ROM0[$0100]
    jp EntryPoint       ; Jump past the header space to our actual code

    ds $150-@, 0        ; Allocate space for RGBFIX to insert our ROM header by allocating
                        ;  the number of bytes from our current location (@) to the end of the
                        ;  header ($150)

EntryPoint:
    di                  ; Disable interrupts during setup
    ld sp, $e000        ; Set the stack pointer to the end of WRAM

    ; Turn off the LCD when it's safe to do so (during VBlank)
.waitVBlank
    ldh a, [rLY]        ; Read the LY register to check the current scanline
    cp SCREEN_HEIGHT_PX ; Compare the current scanline to the first scanline of VBlank
    jr c, .waitVBlank   ; Loop as long as the carry flag is set
    xor a               ; Once we exit the loop we're safely in VBlank
    ldh [rLCDC], a      ; Disable the LCD (must be done during VBlank to protect the LCD)

    ldh [hFrameCounter], a ; Zero our frame counter just to be safe (A is already zero from earlier)

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

    ; Setup an object palette
    ld a, %11100100     ; Define a 4-shade palette from darkest (11) to lightest (00)
    ldh [rOBP0], a      ; Set the object palette 0

    ; Ensure the sprite locations (in wShadowOAM) are initalized for OAM DMA
    call PopulateShadowOAM

    ; Perform OAM DMA once to ensure OAM doesn't contain garbage
    ld a, HIGH(wShadowOAM) ; Load the high byte of our Shadow OAM buffer into A
    call hOAMDMA         ; Call our OAM DMA routine (in HRAM), quickly copying from wShadowOAM to OAMRAM

    ; Setup the VBlank interrupt
    ld a, IE_VBLANK    ; Load the flag to enable the VBlank interrupt into A
    ldh [rIE], a        ; Load the prepared flag into the interrupt enable register
    xor a               ; Set A to zero
    ldh [rIF], a        ; Clear any lingering flags from the interrupt flag register to avoid false interrupts
    ei                  ; enable interrupts!

    ; Combine flag constants defined in hardware.inc into a single value with logical ORs and load it into A
    ; Note that some of these constants (LCDC_BG_OFF, LCDC_OBJ_8, LCDC_WIN_OFF) are zero, but are included for clarity
    ld a, LCDC_ON | LCDC_BG_OFF | LCDC_OBJ_8 | LCDC_OBJ_ON | LCDC_WIN_OFF
    ldh [rLCDC], a      ; Enable and configure the LCD to show the background

LoopForever:
    halt                ; Halt the CPU, waiting until an interrupt fires (this will sync our loop with VBlank)

    ld hl, hFrameCounter ; Point HL to the frame counter in HRAM
    inc [hl]            ; Increment the contents of the memory address pointed to by HL (hFrameCounter)

    call PopulateShadowOAM ; Update the sprite locations for the next frame

    jr LoopForever      ; Loop forever

; Populate Shadow OAM with locations based on the current hFrameCounter
DEF ENTRY_GAP EQU 8     ; The gap between SineTable entries for each OAM entry (try adjusting this value to see what changes)
PopulateShadowOAM:
    ; Note: This code takes advantage of the fact that SineTable is page-aligned and 256 bytes long, which means
    ;  we can adjust the low byte of the pointer and it will automatically wrap around the table.

    ld d, HIGH(SineTable) ; Load the high byte of the SineTable into D
    ldh a, [hFrameCounter] ; Load the current hFrameCounter value into A
    ld e, a             ; Load the frame counter value into E, making DE a pointer to SineTable that advances each frame
    ld c, e             ; Initialize the offset between Y and X entries in SineTable for a given OAM entry
                        ;  Note: Varying this offset per frame is what makes the overall shape pivot. Try using a fixed C!
    ld hl, wShadowOAM   ; Load the destination address in WRAM into HL
    ld b, OAM_COUNT     ; Load the number of OAM entries to populate into B
.oamDataLoop
    ld a, [de]          ; Read Y coordinate from SineTable
    add $14             ; Add +20 pixel offset for Y coordinate to center everything on screen
    ld [hli], a         ; Write Y coordiante to wShadowOAM, increment pointer in HL
    ld a, e             ; Advance SineTable pointer C entries
    add c               ;  ...
    ld e, a             ;  ...
    ld a, [de]          ; Read X coordinate from SineTable
    add $14             ; Add 20 pixel offset for X coordinate to center everything on screen
    ld [hli], a         ; Write X coordiante to wShadowOAM, increment pointer in HL
    ld a, e             ; Retreat SineTable pointer C-ENTRY_GAP entries back for next OAM entry
    sub c               ;  ...
    add ENTRY_GAP       ;  ...
    ld e, a             ;  ...
    xor a               ; Set A to zero for tile index and attributes
    ld [hli], a         ; Write tile index to wShadowOAM, increment pointer in HL
    ld [hli], a         ; Write attribytes to wShadowOAM, increment pointer in HL
    dec b               ; Decrement the loop counter in B
    jr nz, .oamDataLoop ; If B isn't zero, continue looping

    ret


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


SECTION "Frame Counter", HRAM
; Reserve space in HRAM to track frame advancement
hFrameCounter:
    ds 1


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


SECTION "Sine Table", ROMX, ALIGN[8]
; Generate a 256 byte lookup of sine values ranging from 0-128, aligned to a page for automatic wrapping
; See: https://rgbds.gbdev.io/docs/master/rgbasm.5#Fixed-point_expressions
SineTable:
FOR ANGLE, 0.0, 1.0, 1.0 / 256 ; delta = 1 full turn / 256 entries
	db (MUL(64.0, SIN(ANGLE)) + 64.0) >> 16
ENDR
