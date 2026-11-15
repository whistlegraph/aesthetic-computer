; A diamond you move with the D-pad. A beeps.
; NROM (mapper 0): 16 KB of program, 8 KB of graphics. asm6 syntax.

  .db "NES", $1a      ; iNES header
  .db 1               ; 1 x 16 KB PRG
  .db 1               ; 1 x 8 KB CHR
  .db 0, 0            ; mapper 0, horizontal mirroring
  .dsb 8, 0

px   = $00            ; diamond position
py   = $01
pad  = $02            ; buttons this frame: A B Select Start Up Down Left Right
last = $03            ; buttons last frame

  .base $c000

reset:
  sei
  cld
  ldx #$40
  stx $4017           ; no APU frame interrupts
  ldx #$ff
  txs
  inx                 ; X = 0
  stx $2000           ; NMI off
  stx $2001           ; rendering off
  stx $4010           ; no sample interrupts
  bit $2002
@warm1:
  bit $2002           ; the PPU needs two frames to wake
  bpl @warm1
@clear:
  lda #0
  sta $00,x
  sta $0100,x
  sta $0300,x
  sta $0400,x
  sta $0500,x
  sta $0600,x
  sta $0700,x
  lda #$fe
  sta $0200,x         ; park every sprite off screen
  inx
  bne @clear
@warm2:
  bit $2002
  bpl @warm2

  lda #$3f            ; palettes live at $3F00
  sta $2006
  lda #$00
  sta $2006
  ldx #0
@palette:
  lda colors,x
  sta $2007
  inx
  cpx #32
  bne @palette

  lda #124
  sta px
  lda #116
  sta py
  lda #%00000001
  sta $4015           ; square channel 1 on
  lda #%10000000
  sta $2000           ; NMI every frame
  lda #%00010100
  sta $2001           ; show sprites
forever:
  jmp forever         ; the game runs in nmi

nmi:
  pha
  txa
  pha

  lda py              ; sprite 0: Y, tile, attributes, X
  sta $0200
  lda #1
  sta $0201
  lda #0
  sta $0202
  lda px
  sta $0203
  lda #$00
  sta $2003
  lda #$02
  sta $4014           ; copy $0200-$02FF to the PPU

  lda pad
  sta last
  lda #1              ; latch the controller, then shift out 8 buttons
  sta $4016
  lda #0
  sta $4016
  ldx #8
@read:
  lda $4016
  lsr a
  rol pad
  dex
  bne @read

  lda pad
  and #%00001000
  beq @down
  dec py
@down:
  lda pad
  and #%00000100
  beq @left
  inc py
@left:
  lda pad
  and #%00000010
  beq @right
  dec px
@right:
  lda pad
  and #%00000001
  beq @beep
  inc px
@beep:
  lda pad             ; A newly pressed: a short square note
  and #%10000000
  beq @done
  lda last
  and #%10000000
  bne @done
  lda #%10011111
  sta $4000
  lda #$c9
  sta $4002
  lda #%00101000
  sta $4003
@done:
  pla
  tax
  pla
irq:
  rti

colors:
  .db $0f,$00,$10,$30, $0f,$00,$10,$30, $0f,$00,$10,$30, $0f,$00,$10,$30  ; background
  .db $0f,$15,$25,$30, $0f,$00,$10,$30, $0f,$00,$10,$30, $0f,$00,$10,$30  ; sprites

  .org $fffa
  .dw nmi, reset, irq

  .base $0000         ; CHR: 16 bytes a tile, two bit planes
  .dsb 16, 0          ; tile 0: blank
  .db %00011000, %00111100, %01111110, %11111111
  .db %11111111, %01111110, %00111100, %00011000
  .db %00000000, %00011000, %00111100, %01100110
  .db %01100110, %00111100, %00011000, %00000000
  .pad $2000
