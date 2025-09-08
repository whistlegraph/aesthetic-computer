; GameBoy ROM Header Template
; This provides the basic ROM structure needed for any GameBoy program

SECTION "ROM Header", ROM0[$100]
    nop
    jp main

; Nintendo logo data (required for GameBoy boot)
SECTION "Nintendo Logo", ROM0[$104]
    DB $CE,$ED,$66,$66,$CC,$0D,$00,$0B,$03,$73,$00,$83,$00,$0C,$00,$0D
    DB $00,$08,$11,$1F,$88,$89,$00,$0E,$DC,$CC,$6E,$E6,$DD,$DD,$D9,$99
    DB $BB,$BB,$67,$63,$6E,$0E,$EC,$CC,$DD,$DC,$99,$9F,$BB,$B9,$33,$3E

; ROM header information
SECTION "ROM Info", ROM0[$134]
    DB "KIDLISP-GAME    " ; 16-byte title
    DB $80 ; GBC flag (GB compatible)
    DB $00, $00 ; New licensee code
    DB $00 ; SGB flag
    DB $00 ; Cartridge type (ROM only)
    DB $00 ; ROM size (32KB)
    DB $00 ; RAM size (none)
    DB $01 ; Destination code (non-Japanese)
    DB $33 ; Old licensee code
    DB $00 ; ROM version
    DB $00 ; Header checksum (will be calculated)
    DB $00, $00 ; Global checksum (will be calculated)

SECTION "Main Program", ROM0[$150]
main:
    ; Disable interrupts during initialization
    di
    
    ; Initialize stack pointer
    ld sp, $FFFE
    
    ; Turn off LCD before configuring it
    call wait_vblank
    ld a, 0
    ld [$FF40], a ; LCDC register
    
    ; Clear VRAM
    call clear_vram
    
    ; Set up a simple black-on-white palette
    ld a, %11100100 ; 11=black, 10=dark gray, 01=light gray, 00=white
    ld [$FF47], a ; Background palette
    
    ; Turn on LCD with background enabled
    ld a, %10000001 ; Bit 7=LCD on, Bit 0=background on
    ld [$FF40], a
    
    ; Call KidLisp generated code
    call kidlisp_main
    
    ; Main loop - just hang here
game_loop:
    jp game_loop

; Wait for vertical blank (safe time to access VRAM)
wait_vblank:
    ld a, [$FF44] ; LY register (current scanline)
    cp 144        ; Wait until scanline 144 (start of vblank)
    jr nz, wait_vblank
    ret

; Clear video RAM
clear_vram:
    ld hl, $8000  ; Start of VRAM
    ld bc, $2000  ; Size of VRAM (8KB)
clear_loop:
    ld [hl], 0
    inc hl
    dec bc
    ld a, b
    or c
    jr nz, clear_loop
    ret

; Set a pixel at coordinates (B, C) to current color (stored in A)
; B = X coordinate (0-159)
; C = Y coordinate (0-143)
; A = color (0-3)
set_pixel:
    push bc
    push de
    push hl
    
    ; Calculate tile map address
    ; Each tile is 8x8 pixels, so divide coordinates by 8
    ld a, c
    srl a ; divide Y by 2
    srl a ; divide Y by 4
    srl a ; divide Y by 8
    ld d, a ; D = tile row (Y / 8)
    
    ld a, b
    srl a ; divide X by 2
    srl a ; divide X by 4
    srl a ; divide X by 8
    ld e, a ; E = tile column (X / 8)
    
    ; Calculate tile map address: $9800 + (row * 32) + column
    ld h, $98
    ld l, $00 ; HL = $9800 (background tile map)
    
    ; Add row * 32
    ld a, d
    add a, a ; multiply by 2
    add a, a ; multiply by 4
    add a, a ; multiply by 8
    add a, a ; multiply by 16
    add a, a ; multiply by 32
    add l
    ld l, a
    adc h
    sub l
    ld h, a
    
    ; Add column
    ld a, e
    add l
    ld l, a
    adc h
    sub l
    ld h, a
    
    ; For now, just set the tile to a solid color tile (tile index 1)
    ld [hl], 1
    
    pop hl
    pop de
    pop bc
    ret

; KidLisp generated code will be inserted here
kidlisp_main:
    ; This is where the compiled KidLisp code will go
    ; For our proof of concept, plot a pixel at (80, 72)
    ld b, 80  ; X coordinate
    ld c, 72  ; Y coordinate
    ld a, 3   ; Color (black)
    call set_pixel
    ret

SECTION "Tile Data", ROM0[$8000]
    ; Tile 0: Empty (white)
    DB $00,$00,$00,$00,$00,$00,$00,$00
    DB $00,$00,$00,$00,$00,$00,$00,$00
    
    ; Tile 1: Solid (black)
    DB $FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF
    DB $FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF
