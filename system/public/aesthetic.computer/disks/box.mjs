// box, 25.09.16.22.26 
// Draw rectangles with brush gestures, fade colors, modes, and thickness

// TODO: Support robotics / turtlization.
// TODO: Support tab sharing.
// TODO: Support GPU based compositiing.
// TODO: Support noise based and image based textures and kidlisp textures.
// TODO: Brushes should have a 'color coded' word top level and there
//       should be a Bauhaus like cateogorical method across AC pieces.

function overlay({ ink, color, mark }) {
  if (!mark) return;
  ink(color).box(mark);
}

function lift({ ink, color, mark }) {
  ink(color).box(mark);
}

export { overlay, lift };