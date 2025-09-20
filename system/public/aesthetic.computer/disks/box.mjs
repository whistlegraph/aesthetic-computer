// box, 25.09.16.22.26 
// Draw rectangles with brush gestures, fade colors, modes, and thickness

// TODO: Support robotics / turtlization.
// TODO: Support tab sharing.
// TODO: Support GPU based compositiing.
// TODO: Support noise based and image based textures and kidlisp textures.
// TODO: Brushes should have a 'color coded' word top level and there
//       should be a Bauhaus like cateogorical method across AC pieces.

function overlay({ ink, color, mark, pen, system }) {
  console.log("📦 BOX OVERLAY ENTRY - Received from caller:", {
    mark: mark,
    color: color,
    hasPen: !!pen,
    penCoords: pen ? {x: pen.x, y: pen.y} : null,
    hasInk: !!ink,
    hasSystem: !!system,
    nopaintInfo: system?.nopaint ? {
      state: system.nopaint.state,
      translation: system.nopaint.translation
    } : null,
    caller: "robo or direct"
  });
  
  if (!mark) {
    console.warn("📦 BOX: No mark provided - exiting");
    return;
  }
  
  console.log("📦 BOX: About to call ink(color).box(mark)");
  console.log("📦 BOX: ink function details:", {
    inkType: typeof ink,
    inkString: ink.toString().substring(0, 100),
    colorValue: color,
    markValue: mark
  });
  
  try {
    const inkWithColor = ink(color);
    console.log("📦 BOX: ink(color) returned:", {
      type: typeof inkWithColor,
      hasBox: !!inkWithColor?.box,
      boxType: typeof inkWithColor?.box
    });
    
    const result = inkWithColor.box(mark);
    console.log("📦 BOX: inkWithColor.box(mark) returned:", {
      result: result,
      type: typeof result
    });
    
    console.log("📦 BOX: About to return result:", result);
    console.log("📦 BOX: Return value type:", typeof result);
    console.log("📦 BOX: Return value is undefined?", result === undefined);
    return result;
  } catch (error) {
    console.error("📦 BOX: Error during box drawing:", error);
    return undefined;
  }
}

function lift({ ink, color, mark }) {
  console.log("📦 Box lift called with mark:", mark, "color:", color);
  console.log("📦 Box lift mark details:", {
    x: mark?.x,
    y: mark?.y, 
    w: mark?.w,
    h: mark?.h,
    type: typeof mark
  });
  
  if (!mark) {
    console.warn("📦 Box lift: No mark provided!");
    return false;
  }
  
  console.log(`📦 Drawing box at (${mark.x}, ${mark.y}) size ${mark.w}x${mark.h} with color:`, color);
  ink(color).box(mark);
  console.log("📦 Box drawing completed - returning true to signal changes made");
  return true; // Signal that drawing was performed and changes were made
}

export { overlay, lift };