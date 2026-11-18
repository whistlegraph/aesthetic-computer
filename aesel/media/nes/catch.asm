; Catch: gems fall, a basket catches them. Left and Right move. Three misses
; end the game; Start begins another. Every five catches they fall faster,
; and from ten points on there are two at a time.
; NROM (mapper 0): 16 KB of program, 8 KB of graphics. asm6 syntax.

  .db "NES", $1a      ; iNES header
  .db 1               ; 1 x 16 KB PRG
  .db 1               ; 1 x 8 KB CHR
  .db 0, 0            ; mapper 0, horizontal mirroring
  .dsb 8, 0

px      = $00         ; basket's left edge
pad     = $01         ; buttons this frame: A B Select Start Up Down Left Right
last    = $02         ; buttons last frame
rnd     = $03         ; random number, never zero
lives   = $04
over    = $05         ; 1 once the last life is gone
catches = $06         ; counts to five, then the gems speed up
spdlo   = $07         ; fall speed, in 256ths of a pixel a frame
spdhi   = $08
t       = $09
frame   = $0a
sc0     = $0b         ; score: hundreds, tens, ones
sc1     = $0c
sc2     = $0d
beep    = $0e         ; frames of sound left
gx      = $10         ; two gems: left edge, height (fraction, whole), falling or not
gyl     = $12
gyh     = $14
gon     = $16

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

  lda #$a7
  sta rnd
  jsr newgame
  lda #%00000001
  sta $4015           ; square channel 1 on
  lda #$08
  sta $4001           ; no pitch sweep
  lda #%10000000
  sta $2000           ; NMI every frame
  lda #%00010100
  sta $2001           ; show sprites
forever:
  jmp forever         ; the game runs in nmi

newgame:
  lda #112
  sta px
  lda #3
  sta lives
  lda #0
  sta over
  sta catches
  sta sc0
  sta sc1
  sta sc2
  sta gon
  sta gon+1
  lda #$40            ; one and a quarter pixels a frame to begin
  sta spdlo
  lda #1
  sta spdhi
  rts

rand:                 ; an 8-bit shift register; A and rnd get the next number
  lda rnd
  asl a
  bcc @keep
  eor #$1d
@keep:
  sta rnd
  rts

spawn:                ; gem X starts again at the top, somewhere across
  jsr rand
  and #$7f
  sta t
  jsr rand
  and #$3f
  clc
  adc t
  adc #16             ; 16 to 206
  sta gx,x
  lda #16
  sta gyh,x
  lda #0
  sta gyl,x
  lda #1
  sta gon,x
  rts

caught:               ; gem X landed in the basket
  lda #0
  sta gon,x
  inc sc2             ; one point, carried through the three digits
  lda sc2
  cmp #10
  bne @speed
  lda #0
  sta sc2
  inc sc1
  lda sc1
  cmp #10
  bne @speed
  lda #0
  sta sc1
  inc sc0
@speed:
  inc catches
  lda catches
  cmp #5
  bne @sound
  lda #0
  sta catches
  lda spdhi
  cmp #4              ; fast enough
  bcs @sound
  lda spdlo
  clc
  adc #$30
  sta spdlo
  bcc @sound
  inc spdhi
@sound:
  lda #%10111111      ; a short high note
  sta $4000
  lda #$6a
  sta $4002
  lda #%00001000
  sta $4003
  lda #5
  sta beep
  rts

missed:               ; gem X reached the floor
  lda #0
  sta gon,x
  lda #%10111111      ; a longer low note
  sta $4000
  lda #$ff
  sta $4002
  lda #%00001011
  sta $4003
  lda #14
  sta beep
  dec lives
  bne @alive
  lda #1
  sta over
@alive:
  rts

nmi:
  pha
  txa
  pha
  tya
  pha

  lda #$00            ; show what the last frame drew
  sta $2003
  lda #$02
  sta $4014           ; copy $0200-$02FF to the PPU

  inc frame
  lda beep            ; a note ends when its frames run out
  beq @pad
  dec beep
  bne @pad
  lda #%10110000
  sta $4000
@pad:
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

  lda over
  beq @play
  lda pad             ; game over: Start, newly pressed, begins again
  and #%00010000
  beq @draw
  lda last
  and #%00010000
  bne @draw
  jsr newgame
  jmp @draw
@play:
  jsr move
  jsr fall
@draw:
  jsr draw

  pla
  tay
  pla
  tax
  pla
irq:
  rti

move:                 ; three pixels a frame, kept on screen
  lda pad
  and #%00000010
  beq @right
  lda px
  sec
  sbc #3
  cmp #8
  bcs @setl
  lda #8
@setl:
  sta px
@right:
  lda pad
  and #%00000001
  beq @done
  lda px
  clc
  adc #3
  cmp #217
  bcc @setr
  lda #216
@setr:
  sta px
@done:
  rts

fall:
  ldx #1
@gem:
  lda gon,x
  bne @drop
  cpx #0              ; the first gem always comes back
  beq @new
  lda sc0             ; the second waits for ten points
  ora sc1
  beq @next
@new:
  jsr spawn
  jmp @next
@drop:
  lda gyl,x
  clc
  adc spdlo
  sta gyl,x
  lda gyh,x
  adc spdhi
  sta gyh,x
  cmp #184            ; its 16 rows have not reached the basket yet
  bcc @next
  cmp #200
  bcs @floor
  lda gx,x            ; level with the basket: caught if they overlap across
  clc
  adc #16
  cmp px
  bcc @next
  beq @next
  lda px
  clc
  adc #32
  cmp gx,x
  bcc @next
  beq @next
  jsr caught
  jmp @next
@floor:
  cmp #220
  bcc @next
  jsr missed
@next:
  dex
  bpl @gem
  rts

draw:
  lda #200            ; the basket: four sprites in a row
  ldx over
  beq @basket
  lda frame           ; it blinks when the game is over
  and #%00010000
  beq @show
  lda #$fe
  bne @basket
@show:
  lda #200
@basket:
  sta $0200
  sta $0204
  sta $0208
  sta $020c
  lda #2
  sta $0201
  sta $020d
  lda #3
  sta $0205
  sta $0209
  lda #0
  sta $0202
  sta $0206
  sta $020a
  lda #%01000000      ; the right end is the left end, mirrored
  sta $020e
  lda px
  sta $0203
  clc
  adc #8
  sta $0207
  adc #8
  sta $020b
  adc #8
  sta $020f

  ldx #0              ; the gems: four quarters each, one tile flipped three ways
  ldy #0
@gem:
  lda gon,x
  beq @hide
  lda gyh,x
  sta $0210,y
  sta $0214,y
  clc
  adc #8
  sta $0218,y
  sta $021c,y
  jmp @tiles
@hide:
  lda #$fe
  sta $0210,y
  sta $0214,y
  sta $0218,y
  sta $021c,y
@tiles:
  lda #1
  sta $0211,y
  sta $0215,y
  sta $0219,y
  sta $021d,y
  txa                 ; palette 1 for the first gem, 2 for the second
  clc
  adc #1
  sta t
  sta $0212,y
  ora #%01000000
  sta $0216,y
  lda t
  ora #%10000000
  sta $021a,y
  lda t
  ora #%11000000
  sta $021e,y
  lda gx,x
  sta $0213,y
  sta $021b,y
  clc
  adc #8
  sta $0217,y
  sta $021f,y
  tya
  clc
  adc #16
  tay
  inx
  cpx #2
  bne @gem

  lda #16             ; the score, top left
  sta $0230
  sta $0234
  sta $0238
  lda sc0
  clc
  adc #16
  sta $0231
  lda sc1
  clc
  adc #16
  sta $0235
  lda sc2
  clc
  adc #16
  sta $0239
  lda #3
  sta $0232
  sta $0236
  sta $023a
  lda #16
  sta $0233
  lda #24
  sta $0237
  lda #32
  sta $023b

  ldx #0              ; a heart for each life left, top right
  ldy #0
@heart:
  cpx lives
  bcs @gone
  lda #16
  bne @put
@gone:
  lda #$fe
@put:
  sta $023c,y
  lda #4
  sta $023d,y
  lda #0
  sta $023e,y
  txa
  asl a
  asl a
  asl a
  clc
  adc #208
  sta $023f,y
  iny
  iny
  iny
  iny
  inx
  cpx #3
  bne @heart
  rts

colors:
  .db $01,$00,$10,$30, $01,$00,$10,$30, $01,$00,$10,$30, $01,$00,$10,$30  ; background
  .db $01,$27,$16,$30, $01,$2a,$1a,$30, $01,$24,$14,$30, $01,$30,$10,$00  ; basket and hearts, green gem, pink gem, digits

  .org $fffa
  .dw nmi, reset, irq

  .base $0000         ; CHR: 16 bytes a tile, two bit planes
  .dsb 16, 0          ; tile 0: blank
  ; tile 1: the top-left quarter of a gem; the other three are this one flipped
  .db %00000000, %00000001, %00000011, %00000111, %00001111, %00011111, %00111111, %01111111
  .db %00000001, %00000010, %00000101, %00001010, %00010100, %00101000, %01010000, %10100000
  ; tile 2: the basket's left end (the right end is this one flipped)
  .db %11000000, %11000000, %11000000, %11000000, %11000000, %11100000, %01100000, %00111111
  .db %01000000, %01000000, %01000000, %01000000, %01000000, %00100000, %00011111, %00000000
  ; tile 3: the basket's floor
  .db %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %11111111
  .db %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %11111111, %00000000
  ; tile 4: a heart
  .db %00000000, %01100000, %01000000, %00000000, %00000000, %00000000, %00000000, %00000000
  .db %01100110, %11111110, %11111110, %11111110, %01111100, %00111000, %00010000, %00000000
  .dsb 176, 0         ; tiles 5-15: blank
  ; tiles 16-25: the digits 0-9
  .db %00111000, %01000100, %01001100, %01010100, %01100100, %01000100, %00111000, %00000000
  .db %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000
  .db %00010000, %00110000, %00010000, %00010000, %00010000, %00010000, %00111000, %00000000
  .db %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000
  .db %00111000, %01000100, %00000100, %00001000, %00010000, %00100000, %01111100, %00000000
  .db %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000
  .db %01111000, %00000100, %00000100, %00111000, %00000100, %00000100, %01111000, %00000000
  .db %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000
  .db %00001000, %00011000, %00101000, %01001000, %01111100, %00001000, %00001000, %00000000
  .db %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000
  .db %01111100, %01000000, %01111000, %00000100, %00000100, %01000100, %00111000, %00000000
  .db %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000
  .db %00011000, %00100000, %01000000, %01111000, %01000100, %01000100, %00111000, %00000000
  .db %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000
  .db %01111100, %00000100, %00001000, %00010000, %00100000, %00100000, %00100000, %00000000
  .db %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000
  .db %00111000, %01000100, %01000100, %00111000, %01000100, %01000100, %00111000, %00000000
  .db %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000
  .db %00111000, %01000100, %01000100, %00111100, %00000100, %00001000, %00110000, %00000000
  .db %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000
  .pad $2000
