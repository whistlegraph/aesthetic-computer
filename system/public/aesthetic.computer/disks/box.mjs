// box, 25.09.16.22.26 
// Draw rectangles with brush gestures, fade colors, modes, and thickness

// TODO: Support robotics / turtlization.
// TODO: Support tab sharing.
// TODO: Support GPU based compositiing.
// TODO: Support noise based and image based textures and kidlisp textures.
// TODO: Brushes should have a 'color coded' word top level and there
//       should be a Bauhaus like cateogorical method across AC pieces.

function overlay({ ink, color, mark, pen, system }) {
  if (!mark) {
    console.warn("📦 BOX: No mark provided - exiting");
    return;
  }
  
  try {
    const result = ink(color).box(mark);
    return result;
  } catch (error) {
    console.error("📦 BOX: Error during box drawing:", error);
    return undefined;
  }
}

function lift({ ink, color, mark }) {
  if (!mark) {
    console.warn("📦 Box lift: No mark provided!");
    return false;
  }
  
  ink(color).box(mark);
  return true; // Signal that drawing was performed and changes were made
}

export { overlay, lift };