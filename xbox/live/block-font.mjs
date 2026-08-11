// The 5x7 block font the oskiewar HUD writes with, lifted out of the shell so
// more than one host can draw with the same letters. Each glyph is seven rows of
// five bits, most significant bit leftmost, which is why a row reads as a small
// number rather than a picture.
//
// mac-test.html still carries its own inline copy: the social-preview manifest
// hashes that file, so moving its table would force a preview re-burn on a
// change that alters no pixels. Worth doing next time that file is touched
// anyway -- until then this is the copy new hosts should use.
//
// Two glyphs are added here that the game never needed. oskiewar counts rounds
// and health, which never reach a thousand and are never a percentage, so the
// HUD alphabet has no comma and no percent sign -- and an unknown character
// falls back to "?", which is how "1,361 views" first rendered as "1?361" and
// "4.5% of rounds" as "4.5?". A dashboard is mostly large numbers and rates, so
// it needs both.

    export const blockGlyphs = {
      A:[14,17,17,31,17,17,17], B:[30,17,17,30,17,17,30], C:[14,17,16,16,16,17,14],
      D:[30,17,17,17,17,17,30], E:[31,16,16,30,16,16,31], F:[31,16,16,30,16,16,16],
      G:[14,17,16,23,17,17,15], H:[17,17,17,31,17,17,17], I:[31,4,4,4,4,4,31],
      J:[7,2,2,2,18,18,12], K:[17,18,20,24,20,18,17], L:[16,16,16,16,16,16,31],
      M:[17,27,21,21,17,17,17], N:[17,25,21,19,17,17,17], O:[14,17,17,17,17,17,14],
      P:[30,17,17,30,16,16,16], Q:[14,17,17,17,21,18,13], R:[30,17,17,30,20,18,17],
      S:[15,16,16,14,1,1,30], T:[31,4,4,4,4,4,4], U:[17,17,17,17,17,17,14],
      V:[17,17,17,17,17,10,4], W:[17,17,17,21,21,21,10], X:[17,17,10,4,10,17,17],
      Y:[17,17,10,4,4,4,4], Z:[31,1,2,4,8,16,31],
      0:[14,17,19,21,25,17,14], 1:[4,12,4,4,4,4,14], 2:[14,17,1,2,4,8,31],
      3:[30,1,1,14,1,1,30], 4:[2,6,10,18,31,2,2], 5:[31,16,16,30,1,1,30],
      6:[14,16,16,30,17,17,14], 7:[31,1,2,4,8,8,8], 8:[14,17,17,14,17,17,14],
      9:[14,17,17,15,1,1,14], ".":[0,0,0,0,0,12,12], ":":[0,4,4,0,4,4,0],
      "-":[0,0,0,31,0,0,0], "/":[1,2,2,4,8,8,16], "!":[4,4,4,4,4,0,4],
      " ":[0,0,0,0,0,0,0], "?":[14,17,1,2,4,0,4],
  // Rows are five bits, most significant bit leftmost.
  //   %  1 1 . . 1     ,  . . . . .
  //      1 1 . 1 .        . . . . .
  //      . . 1 . .        . . . . .
  //      . 1 . . .        . . . . .
  //      . . 1 . 1        . . 1 . .
  //      . 1 . 1 1        . . 1 . .
  //      1 . . 1 1        . 1 . . .
  "%": [25, 26, 4, 8, 5, 11, 19],
  ",": [0, 0, 0, 0, 4, 4, 8],
};
