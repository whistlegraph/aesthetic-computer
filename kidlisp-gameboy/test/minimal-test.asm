SECTION "Header", ROM0[$100]
  jp Main

SECTION "Main", ROM0[$150]
Main:
  ld a, $91
  halt
