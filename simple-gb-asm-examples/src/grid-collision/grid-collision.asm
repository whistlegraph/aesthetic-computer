; Grid Collision Example for the Nintendo Game Boy
; by Dave VanEe 2022
; Tested with RGBDS 1.0.0
; License: CC0 (https://creativecommons.org/publicdomain/zero/1.0/)

; This example builds on the following examples:
;  - background-tilemap (except using a manually constructed tilemap)
;  - vblank/oamdma/sprite
;  - joypad

; The main additions in this example are moving a player in response to input (ProcessInput), and checking the tilemap
;  entries to determine if attempted moves are into valid spaces based on tile ID (GetTileID). Note that instead of using an
;  automatically generated tilemap as in the background-filemap example, the tilemap is pre-constructed with tiles organized
;  such that walkable tiles start at 0 and are sequential to simplify the collision check.

include "hardware.inc"  ; Include hardware definitions so we can use nice names for things

;============================================================================================================================
; Game Constants
;============================================================================================================================

def MAX_WALKABLE_TILE_ID equ 8  ; All tiles from 0 to this tile ID will be considered walkable for the purposes of collision

def OBJ_Y_OFFSET equ -9         ; Since we're using two objects to draw the player larger than 8x8, but we're still moving
def OBJ_X_OFFSET equ -4         ;  on an 8x8 grid, we offset things slightly to center the player on the current tile

rsreset                         ; Reset the _RS counter to 0 for a new set of defines
def FACE_LEFT   rb 1            ; Define FACE_LEFT as 0
def FACE_RIGHT  rb 1            ; Define FACE_RIGHT as 1
def FACE_UP     rb 1            ; Define FACE_UP as 2
def FACE_DOWN   rb 1            ; Define FACE_DOWN as 3

;============================================================================================================================
; Game State Variables
;============================================================================================================================

SECTION "Game State Variables", WRAM0

wPlayer:
.y              ds 1    ; Player's Y coordinate (in grid space)
.x              ds 1    ; Player's X coordinate (in grid space)
.facing         ds 1    ; Player's facing direction (0=left, 1=right, 2=up, 3=down)

;============================================================================================================================
; Interrupts
;============================================================================================================================

; The VBlank vector is where execution is passed when the VBlank interrupt fires
SECTION "VBlank Vector", ROM0[$40]
; We only have 8 bytes here, so push all the registers to the stack and jump to the rest of the handler
; Note: Since the VBlank handler used here only affects A and F, we don't have to push/pop BC, DE, and HL,
;  but it's done here for demonstration purposes.
VBlank:
    push af             ; Push AF to the stack
    ld a, HIGH(wShadowOAM) ; Load the high byte of our Shadow OAM buffer into A
    jp VBlankHandler    ; Jump to the rest of the handler

; The rest of the handler is contained in ROM0 to ensure it's always accessible without banking
SECTION "VBlank Handler", ROM0
VBlankHandler:
    call hOAMDMA        ; Call our OAM DMA routine (in HRAM), quickly copying from wShadowOAM to OAMRAM
    pop af              ; Pop AF off the stack
    reti                ; Return and enable interrupts (ret + ei)

;============================================================================================================================
; Initialization
;============================================================================================================================

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
    xor a               ; Once we exit the loop we're safely in VBlank
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

    ; Copy our sprite and background tiles to VRAM
    ld hl, SpriteTileData ; Load the source address of our tiles into HL
    ld de, STARTOF(VRAM) ; Load the destination address in VRAM into DE
    ld bc, SpriteTileData.end - SpriteTileData ; Load the number of bytes to copy into BC
    call MemCopy        ; Call our general-purpose memory copy routine

    ld hl, BackgroundTileData ; Load the source address of our tiles into HL
    ld de, STARTOF(VRAM)+$1000 ; Load the destination address in VRAM into DE
    ld bc, BackgroundTileData.end - BackgroundTileData ; Load the number of bytes to copy into BC
    call MemCopy        ; Call our general-purpose memory copy routine

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
    ld a, %11010000     ; Define a 4-shade palette which omits the 10 value to increase player contrast
    ldh [rOBP0], a      ; Set an object palette

    xor a               ; Set A to zero
    ldh [rSCX], a       ; Set the background scroll registers to show the top-left
    ldh [rSCY], a       ;  corner of the background in the top-left corner of the screen

    ldh [hCurrentKeys], a ; Zero our current keys just to be safe (A is already zero from earlier)

    ; Initialize shadow OAM to zero
    ld hl, wShadowOAM   ; Point HL to the start of shadow OAM
    ld b, wShadowOAM.end - wShadowOAM ; Load the size of shadow OAM into B (it's less than 256 so we can use a single byte)
.clearOAM
    ld [hli], a         ; Zero this OAM byte
    dec b               ; Decrement the loop counter in B (bytes of OAM)
    jr nz, .clearOAM    ; If B isn't zero, continue zeroing bytes

    ; Perform OAM DMA once to ensure OAM doesn't contain garbage
    ld a, HIGH(wShadowOAM) ; Load the high byte of our Shadow OAM buffer into A
    call hOAMDMA         ; Call our OAM DMA routine (in HRAM), quickly copying from wShadowOAM to OAMRAM

    ; Setup the world state
    ld hl, wPlayer      ; Point HL to the start of the player's state in WRAM
    ld a, 4             ; Load the starting Y coordinate into A
    ld [hli], a         ; Set the starting wPlayer.y value in WRAM
    ld a, 2             ; Load the starting X coordinate into A
    ld [hli], a         ; Set the starting wPlayer.x value in WRAM
    ld a, FACE_DOWN     ; Load the starting facing direction into A
    ld [hli], a         ; Set the starting wPlayer.facing value in WRAM

    ; Setup the VBlank interrupt
    ld a, IE_VBLANK     ; Load the flag to enable the VBlank interrupt into A
    ldh [rIE], a        ; Load the prepared flag into the interrupt enable register
    xor a               ; Set A to zero
    ldh [rIF], a        ; Clear any lingering flags from the interrupt flag register to avoid false interrupts
    ei                  ; enable interrupts!

    ; Combine flag constants defined in hardware.inc into a single value with logical ORs and load it into A
    ld a, LCDC_ON | LCDC_BLOCK21 | LCDC_BG_ON | LCDC_OBJ_16 | LCDC_OBJ_ON | LCDC_WIN_OFF
    ldh [rLCDC], a      ; Enable and configure the LCD to show the background and objects

;============================================================================================================================
; Main Loop
;============================================================================================================================

LoopForever:
    halt                ; Halt the CPU, waiting until an interrupt fires (this will sync our loop with VBlank)

    call UpdateJoypad   ; Poll the joypad and store the state in HRAM
    call ProcessInput   ; Update the game state in response to user input
    call PopulateShadowOAM ; Update the sprite locations for the next frame

    jr LoopForever      ; Loop forever

;============================================================================================================================
; Main Routines
;============================================================================================================================

SECTION "Main Routines", ROMX

; Process the user's inputs and update the game state accordingly
ProcessInput:
    ldh a, [hNewKeys]   ; Load the newly pressed keys byte into A
    bit B_PAD_LEFT, a   ; Check the state of the LEFT bit in A
    ld bc, $00ff        ; Preload B/C with dy/dx for left movement (0, -1)
    ld d, FACE_LEFT     ; Preload D with the facing value for LEFT
    jr nz, .attemptMove ; If the bit was set, jump to attempt movement in that direction
    bit B_PAD_RIGHT, a  ; Check the state of the RIGHT bit in A
    ld bc, $0001        ; Preload B/C with dy/dx for left movement (0, +1)
    ld d, FACE_RIGHT    ; Preload D with the facing value for RIGHT
    jr nz, .attemptMove ; If the bit was set, jump to attempt movement in that direction
    bit B_PAD_UP, a     ; Check the state of the UP bit in A
    ld bc, $ff00        ; Preload B/C with dy/dx for left movement (-1, 0)
    ld d, FACE_UP       ; Preload D with the facing value for UP
    jr nz, .attemptMove ; If the bit was set, jump to attempt movement in that direction
    bit B_PAD_DOWN, a   ; Check the state of the DOWN bit in A
    ld bc, $0100        ; Preload B/C with dy/dx for left movement (+1, 0)
    ld d, FACE_DOWN     ; Preload D with the facing value for DOWN
    jr nz, .attemptMove ; If the bit was set, jump to attempt movement in that direction
    ret                 ; No inputs to handle, return to main loop

; Attempt a move in a direction defined by the contents of BC and D
; @param: B Delta Y to apply to current player position
; @param: C Delta X to apply to current player position
; @param: D New facing direction value to apply
.attemptMove
    ld a, d             ; Move new facing direction from D to A
    ld [wPlayer.facing], a ; Store new facing direction regardless of move success

    ; Calculate the destination coordinates by applying the deltas
    ld a, [wPlayer.y]   ; Load the current player Y coordinate into A
    add b               ; Add the dY value from B to get the new Y coordinate
    ld b, a             ; Store the new Y coordinate back in B
    ld a, [wPlayer.x]   ; Load the current player X coordinate into A
    add c               ; Add the dX value from C to get the new X coordinate
    ld c, a             ; Store the new Y coordinate back in C

    ; Check if the attempted move is valid
    call GetTileID      ; Call a routine to get the tile ID at the B=y, C=x coordinates
    cp MAX_WALKABLE_TILE_ID ; Compare the tile ID from TilemapData to the maximum walkable tile ID
    ret nc              ; If the tile ID is greater than the maximum walkable tile ID, return

    ; Store the new coordinates
    ld a, b             ; Load the new Y coordinate into A
    ld [wPlayer.y], a   ; Store the new Y coordinate in memory
    ld a, c             ; Load the new X coordinate into A
    ld [wPlayer.x], a   ; Store the new X coordinate in memory
    ret

; Return the tile ID in TilemapData at provided coordinates
; @param B: Y coordinate in tilemap
; @param C: X coordinate in tilemap
; @return A: Tile ID at coordinates given
GetTileID:
    push bc             ; Store the input coordinates on the stack
    ld hl, TilemapData  ; Load the start address of the TilemapData into HL
    ld a, b             ; Load the Y coordinate into A
    or a                ; Check if the Y coordinate is zero
    jr z, .yZero        ; If zero, skip the Y seeking code
    ld de, SCREEN_WIDTH ; Load the number of tiles per row of TilemapData into DE
.yLoop
    add hl, de          ; Add the number of tiles per row to the pointer in HL
    dec b               ; Decrease the loop counter in B
    jr nz, .yLoop       ; Loop until we've offset to the correct row
.yZero

    ld a, c             ; Load the X coordinate into A

    ; Add the X coordinate offset to HL (this is a common way to add A to a 16-bit register)
    add l               ; Add the X coordinate to the low byte of the pointer in HL
    ld l, a             ; Store the new low byte of the pointer in L
    adc h               ; Add H plus the carry flag to the contents of A
    sub l               ; Subtract the contents of L from A
    ld h, a             ; Store the new high byte of the pointer in H

    ld a, [hl]          ; Read the value of TilemapData at the coordinates of interest into A
    pop bc              ; Recover the original input coordinates from the stack
    ret

; Populate ShadowOAM with sprites based on the game state
PopulateShadowOAM:
    ld hl, wShadowOAM   ; Point HL at the beginning of wShadowOAM

    ; First sprite
    ld a, [wPlayer.y]   ; Load the player's Y coordinate into A
    add a               ; To convert the Y grid coordinate into screen coordinates we have to multiply
    add a               ;  by 8, which can be done quickly by adding A to itself 3 times
    add a               ;  ...
    add $10+OBJ_Y_OFFSET ; Add the sprite offset ($10), plus the centering offset
    ld [hli], a         ; Store the sprite's Y coordinate in shadow OAM
    ld b, a             ; Cache the Y coordinate in B for use by the second sprite
    ld a, [wPlayer.x]   ; Load the player's X coordinate into A
    add a               ; Multiply the X coordinate by 8 the same as we did for Y above
    add a               ;  ...
    add a               ;  ...
    add $08+OBJ_X_OFFSET ; Add the sprite offset ($08), plus the centering offset
    ld [hli], a         ; Store the sprite's X coordinate in shadow OAM
    add $08             ; Add 8 to the X coordinate for the second sprite
    ld c, a             ; Cache the X coordinate in C for use by the second sprite
    ld a, [wPlayer.facing] ; Load the player's facing direction into A
    add a               ; The player tiles have been stored in VRAM such that the facing direction multiplied
    add a               ;  by 4 will yield the tile ID for the first sprite, so multiply by 4 using adds
    ld [hli], a         ; Store the sprite's tile ID in shadow OAM
    add 2               ; Add 2 to the tile ID for the second sprite
    ld d, a             ; Cache the tile ID in D for use by the second sprite
    xor a               ; Set A to zero
    ld [hli], a         ; Store the sprite's attributes in shadow OAM

    ; Second sprite
    ld a, b             ; Load the prepared Y coordinate from B to A
    ld [hli], a         ; Store the sprite's Y coordinate in shadow OAM
    ld a, c             ; Load the prepared X coordinate from C to A
    ld [hli], a         ; Store the sprite's X coordinate in shadow OAM
    ld a, d             ; Load the prepared tile ID from D to A
    ld [hli], a         ; Store the sprite's tile ID in shadow OAM
    xor a               ; Set A to zero
    ld [hli], a         ; Store the sprite's attributes in shadow OAM

    ; Zero the remaning shadow OAM entries
    ; Note: Since we're only using 2/40 sprites, we could just loop 38 times, but the following approach will scale better if
    ;  additional sprites are added. This will also clear previously used entires in cases where the number of sprites used
    ;  each frame varies (which isn't the case here).
    ld b, a             ; Load zero (from the prior use) into B, since A will be used to check loop completion
.clearOAM
    ld [hl], b          ; Set the Y coordinate of this OAM entry to zero to hide it
    ld a, l             ; Load the low byte of the shadow OAM pointer into A
    add OBJ_SIZE        ; Advance 4 bytes to the next OAM entry
    ld l, a             ; Store the new low byte of the pointer in L
    cp LOW(wShadowOAM.end) ; Compare the low byte to the end of wShadowoAM
    jr nz, .clearOAM    ; Loop until we've hidden every unused sprite
    
    ret

;============================================================================================================================
; Utility Routines
;============================================================================================================================

SECTION "MemCopy Routine", ROM0
; Since we're copying data few times, we'll define a reusable memory copy routine
; Copy BC bytes of data from HL to DE
; @param HL: Source address to copy from
; @param DE: Destination address to copy to
; @param BC: Number of bytes to copy
MemCopy:
    ld a, [hli]         ; Load a byte from the address HL points to into the register A, increment HL
    ld [de], a          ; Load the byte in the A register to the address DE points to
    inc de              ; Increment the destination pointer in DE
    dec bc              ; Decrement the loop counter in BC
    ld a, b             ; Load the value in B into A
    or c                ; Logical OR the value in A (from B) with C
    jr nz, MemCopy      ; If B and C are both zero, OR B will be zero, otherwise keep looping
    ret                 ; Return back to where the routine was called from

;============================================================================================================================
; Joypad Handling
;============================================================================================================================

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

;============================================================================================================================
; OAM Handling
;============================================================================================================================

SECTION "Shadow OAM", WRAM0, ALIGN[8]
; Reserve page-aligned space for a Shadow OAM buffer, to which we can safely write OAM data at any time, 
;  and then use our OAM DMA routine to copy it quickly to OAMRAM when desired. OAM DMA can only operate
;  on a block of data that starts at a page boundary, which is why we use ALIGN[8].
wShadowOAM:
    ds OAM_SIZE
.end

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

;============================================================================================================================
; Tile/Tilemap Data
;============================================================================================================================

SECTION "Tile/Tilemap Data", ROMX

; Obj tiles based on "Micro Character Bases" by Kacper Woźniak (https://thkaspar.itch.io/micro-character-bases)
; Licensed under CC BY 4.0 (https://creativecommons.org/licenses/by/4.0/)
; Skeleton tiles adjusted to 3-shade, and additional facing directions created based on the original art
SpriteTileData:
    incbin "grid-collision-obj-ztiles.2bpp"
.end

; BG tiles based on "Dungeon Package" tileset by nyk-nck (https://nyknck.itch.io/dungeonpack)
; License for original assets not clearly specified, but not CC0. Attribution/link included here for completness.
BackgroundTileData:
    incbin "grid-collision-bg-tiles.2bpp"  ; Include binary tile data inline using incbin
.end                                    ; The .end label is used to let the assembler calculate the length of the data

TilemapData:
    incbin "grid-collision.tilemap"     ; Include tilemap built using Tilemap Studio and the grid-collision-bg-tiles tileset