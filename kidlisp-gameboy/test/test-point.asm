
SECTION "Header", ROM0[$100]
  jp Main

SECTION "Nintendo Logo", ROM0[$104]
  db $CE,$ED,$66,$66,$CC,$0D,$00,$0B,$03,$73,$00,$83,$00,$0C,$00,$0D
  db $00,$08,$11,$1F,$88,$89,$00,$0E,$DC,$CC,$6E,$E6,$DD,$DD,$D9,$99
  db $BB,$BB,$67,$63,$6E,$0E,$EC,$CC,$DD,$DC,$99,$9F,$BB,$B9,$33,$3E

SECTION "Cartridge Header", ROM0[$134]
  db "KIDLISP-ROM"  ; Title start
  db 0,0,0,0,0       ; Title padding (15 bytes total)
  db $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00 ; Manufacturer & cartridge
  db $00    ; CGB flag
  dw $0000  ; New licensee code
  db $00    ; SGB flag
  db $00    ; Cartridge type
  db $00    ; ROM size
  db $00    ; RAM size
  db $01    ; Destination code
  db $33    ; Old licensee code
  db $00    ; ROM version
  db $00    ; Header checksum (calculated by assembler)
  dw $0000  ; Global checksum (calculated by assembler)

SECTION "Main", ROM0[$150]
Main:
  ; Initialize LCD
  ld a, $91
  ldh [$FF40], a
  
  ; Call KidLisp compiled code
  call kidlisp_main
  
  ; Main loop
.loop:
    halt
    jr .loop

; Simple pixel plotting function
set_pixel:
  ; Input: B = X, C = Y, A = color (0-3)
  ; This is a simplified version for proof of concept
  push af
  push bc
  
  ; For now, just return (actual GameBoy graphics are more complex)
  ; In a full implementation, this would write to VRAM
  
  pop bc
  pop af
  ret

; KidLisp compiled code goes here
kidlisp_main:

    ; Plot point at (80, 72)
    ld b, 80    ; X coordinate
    ld c, 72    ; Y coordinate  
    ld a, 3       ; Color (black)
    call set_pixel

  ret
