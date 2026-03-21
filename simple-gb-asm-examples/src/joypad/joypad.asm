; Joypad Example for the Nintendo Game Boy
; by Dave VanEe 2022
; Tested with RGBDS 1.0.0
; License: CC0 (https://creativecommons.org/publicdomain/zero/1.0/)

include "hardware.inc"  ; Include hardware definitions so we can use nice names for things

; The VBlank vector is where execution is passed when the VBlank interrupt fires
SECTION "VBlank Vector", ROM0[$40]
VBlank:
    ; In this example we're only using VBlank as a convenient way to throttle the main loop
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
    ld a, 0             ; Once we exit the loop we're safely in VBlank
    ldh [rLCDC], a      ; Disable the LCD (must be done during VBlank to protect the LCD)

    ldh [hCurrentKeys], a ; Zero our current keys just to be safe (A is already zero from earlier)

    ; Copy our tiles to VRAM
    ld hl, TileData     ; Load the source address of our tiles into HL
    ld de, STARTOF(VRAM); Load the destination address in VRAM into DE
    ld bc, TileData.end - TileData ; Load the number of bytes to copy into BC
.copyLoop
    ld a, [hl]          ; Load a byte from the address HL points to into the register A
    ld [de], a          ; Load the byte in the A register to the address DE points to
    inc hl              ; Increment the source pointer in HL
    inc de              ; Increment the destination pointer in DE
    dec bc              ; Decrement the loop counter in BC
    ld a, b             ; Load the value in B into A
    or c                ; Logical OR the value in A (from B) with C
    jr nz, .copyLoop    ; If B and C are both zero, OR B will be zero, otherwise keep looping

    ; Fill the tilemap with tile zero
    ld hl, TILEMAP0     ; Point HL to the first byte of the tilemap ($9800)
    ld bc, TILEMAP1 - TILEMAP0 ; Load the size of the remaining tilemap into BC
    ld d, 0             ; Load the value to fill the tilemap with into D
.clearLoop
    ld [hl], d          ; Load the value in D into the location pointed to by HL
    inc hl              ; Increment the destination pointer in HL
    dec bc              ; Decrement the loop counter in BC
    ld a, b             ; Load the value in B into A
    or c                ; Logical OR the value in A (from B) with C
    jr nz, .clearLoop   ; If B and C are both zero, OR B will be zero, otherwise keep looping

    ; Setup palettes and scrolling
    ld a, %11100100     ; Define a 4-shade palette from darkest (11) to lightest (00)
    ldh [rBGP], a       ; Set the background palette

    ld a, -32           ; Load -32 into the A register
    ldh [rSCX], a       ; Set SCX to center the joypad display horizontally
    ld a, -60           ; Load -60 into the A register
    ldh [rSCY], a       ; Set SCY to position the joypad vertically as desired

    ; Setup the VBlank interrupt
    ld a, IE_VBLANK     ; Load the flag to enable the VBlank interrupt into A
    ldh [rIE], a        ; Load the prepared flag into the interrupt enable register
    xor a               ; Set A to zero
    ldh [rIF], a        ; Clear any lingering flags from the interrupt flag register to avoid false interrupts
    ei                  ; enable interrupts!

    ; Combine flag constants defined in hardware.inc into a single value with logical ORs and load it into A
    ; Note that some of these constants (LCDC_OBJ_OFF, LCDC_WIN_OFF) are zero, but are included for clarity
    ld a, LCDC_ON | LCDC_BLOCK01 | LCDC_BG_ON | LCDC_OBJ_OFF | LCDC_WIN_OFF
    ldh [rLCDC], a      ; Enable and configure the LCD to show the background


LoopForever:
    halt                ; Halt the CPU, waiting until an interrupt fires (this will sync our loop with VBlank)
    call UpdateJoypad   ; Call the routine which polls the joypad and stores the state
    call UpdateDisplay  ; Call the routine which updates the display based on the joypad state
    jr LoopForever      ; Loop forever


; Update the tilemap to reflect the joypad state as stored in hPressedKeys/hHeldKeys
UpdateDisplay:
    ld hl, TilemapLocations ; Point HL to our table of tilemap/tile entries for the buttons
    ldh a, [hCurrentKeys] ; Load the byte of current key states into A
    ld c, a             ;  ... and then move it to the C register
.nextButton
    ld a, l             ; Due to register pressure, instead of using a register as a loop counter, we check the
    cp LOW(TilemapLocations.end) ;  low byte of the TilemapLocations pointer to see when we've reached the end
    ret z               ; If we've reached the end of the table we're done, return

    srl c               ; Shift C right logically, pushing the state of the next button into the carry flag
    ld b, 0             ; Preload B with tile index offset for unpressed buttons
    jr nc, .notPressed  ; Skip the next instruction if the button we're checking isn't pressed
    ld b, $10           ; Change the tile index offset in B to $10 to use the 'pressed' tiles
.notPressed

.loop
    ld a, [hli]         ; Load the low byte of the next TilemapLocations entry
    or a                ; Check for the zero terminator value
    jr z, .nextButton   ; If the value is zero jump to process the next button
    ld e, a             ; Load the low byte of the pointer into E
    ld a, [hli]         ; Get the high byte of the pointer
    ld d, a             ;  ... and store it in D (DE now points to VRAM where we want to write a tile)

.waitVRAM
    ldh     a, [rSTAT]  ; Check the STAT register to figure out which mode the LCD is in
    and     STAT_BUSY   ; AND the value to see if VRAM access is safe
    jr      nz, .waitVRAM ; Loop until VRAM access is safe

    ld a, [hli]         ; Load the tile index we'd like to write
    add b               ; Add the B offset, which will be 0 for unpressed buttons, and $10 for pressed buttons
    ld [de], a          ; Write the tile index to the tilemap
    jr .loop            ; Jump to process the next entry in the TilemapLocations table for this button


SECTION "Joypad Variables", HRAM
; Reserve space in HRAM to track the joypad state
hCurrentKeys:   ds 1
hNewKeys:       ds 1


SECTION "Joypad Routine", ROM0

; Update the newly pressed keys (hNewKeys) and the held keys (hCurrentKeys) in memory
; Note: This routine is written to be easier to understand, not to be optimized for speed or size
UpdateJoypad:
    ; Poll half the controller
    ld a, JOYP_GET_BUTTONS ; Load a flag into A to select reading the buttons
    ldh [rP1], a        ; Write the flag to P1 to select which buttons to read
    ldh a, [rP1]        ; Perform a few dummy reads to allow the inputs to stabilize
    ldh a, [rP1]        ;  ...
    ldh a, [rP1]        ;  ...
    ldh a, [rP1]        ;  ...
    ldh a, [rP1]        ;  ...
    ldh a, [rP1]        ; The final read of the register contains the key state we'll use
    or $f0              ; Set the upper 4 bits, and leave the action button states in the lower 4 bits
    ld b, a             ; Store the state of the action buttons in B

    ld a, JOYP_GET_CTRL_PAD ; Load a flag into A to select reading the dpad
    ldh [rP1], a        ; Write the flag to P1 to select which buttons to read
    call .knownRet      ; Call a known `ret` instruction to give the inputs to stabilize
    ldh a, [rP1]        ; Perform a few dummy reads to allow the inputs to stabilize
    ldh a, [rP1]        ;  ...
    ldh a, [rP1]        ;  ...
    ldh a, [rP1]        ;  ...
    ldh a, [rP1]        ;  ...
    ldh a, [rP1]        ; The final read of the register contains the key state we'll use
    or $f0              ; Set the upper 4 bits, and leave the dpad state in the lower 4 bits

    swap a              ; Swap the high/low nibbles, putting the dpad state in the high nibble
    xor b               ; A now contains the pressed action buttons and dpad directions
    ld b, a             ; Move the key states to B

    ld a, JOYP_GET_NONE ; Load a flag into A to read nothing
    ldh [rP1], a        ; Write the flag to P1 to disable button reading

    ldh a, [hCurrentKeys] ; Load the previous button+dpad state from HRAM
    xor b               ; A now contains the keys that changed state
    and b               ; A now contains keys that were just pressed
    ldh [hNewKeys], a   ; Store the newly pressed keys in HRAM
    ld a, b             ; Move the current key state back to A
    ldh [hCurrentKeys], a ; Store the current key state in HRAM
.knownRet
    ret


SECTION "Tile Data", ROMX
TileData:
    incbin "joypad-tiles.2bpp"
.end

SECTION "Tilemap Locations", ROMX
; Define a macro (provided by Rangi, thanks!) to easily specify the coordinates and IDs of the
;  tiles that make up the joypad display.
MACRO bmap ; y, x, tile_id
    rept _NARG / 3
        dw TILEMAP0 + ((\1) * TILEMAP_WIDTH) + (\2)
        db \3
        shift 3
    endr
    db $00 ; end
ENDM

; Each button has a zero-terminated list of entries for the tiles used to draw that button, with
;  each entry made up of a tilemap address (little endian) and a tile index.
TilemapLocations:
.A      bmap 0,10,$06, 0,11,$07, 1,10,$08, 1,11,$09
.B      bmap 1, 7,$06, 1, 8,$07, 2, 7,$08, 2, 8,$09
.Select bmap 4, 3,$05
.Start  bmap 4, 5,$05
.Right  bmap 1, 2,$03
.Left   bmap 1, 0,$02
.Up     bmap 0, 1,$01
.Down   bmap 2, 1,$04
.end