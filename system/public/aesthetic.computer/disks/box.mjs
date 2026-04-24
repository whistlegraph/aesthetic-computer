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
    mark,
    color,
    hasPen: !!pen,
    penCoords: pen ? {x: pen.x, y: pen.y} : null,
    hasInk: !!ink,
    hasSystem: !!system,
    markDetails: mark ? {x: mark.x, y: mark.y, w: mark.w, h: mark.h} : null,
    isFromRobot: system?.robotActive || false,
    nopaintState: system?.nopaint ? {
      needsPresent: system.nopaint.needsPresent,
      needsBake: system.nopaint.needsBake
    } : null
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
      hasBox: !!inkWithColor.box,
      boxType: typeof inkWithColor.box
    });
    
    const result = inkWithColor.box(mark);
    console.log("📦 BOX: inkWithColor.box(mark) returned:", {result, type: typeof result});
    
    console.log("📦 BOX: About to return result:", result);
    console.log("📦 BOX: Return value type:", typeof result);
    console.log("📦 BOX: Return value is undefined?", result === undefined);
    return result;
  } catch (error) {
    console.error("📦 BOX: Error during box drawing:", error);
    return undefined;
  }
}

function lift({ ink, color, mark, system }) {
  console.log("📦🤖 BOX LIFT CALLED - Robot vs Manual Detection:", {
    mark,
    color,
    hasSystem: !!system,
    nopaintState: system?.nopaint ? {
      needsPresent: system.nopaint.needsPresent,
      needsBake: system.nopaint.needsBake,
      hasBuffer: !!system.nopaint.buffer
    } : null
  });
  
  if (!mark) {
    console.warn("📦 Box lift: No mark provided!");
    return false;
  }
  
  console.log(`📦🤖 LIFT: Drawing box at (${mark.x}, ${mark.y}) size ${mark.w}x${mark.h} with color:`, color);
  
  try {
    const result = ink(color).box(mark);
    console.log("📦🤖 LIFT: Box drawing completed, ink result:", result);
    console.log("📦🤖 LIFT: Returning true to signal changes made");
    return true; // Signal that drawing was performed and changes were made
  } catch (error) {
    console.error("📦🤖 LIFT: Error during box drawing:", error);
    return false;
  }
}

function meta() {
  return {
    title: "Box",
    desc: "Draw rectangles with brush gestures.",
    controls: "drag from one corner to the opposite corner to draw a rectangle; release to bake",
    params: "color (e.g. red, blue, fade:red-blue, or r,g,b[,a])",
    example: "box red",
  };
}

export { overlay, lift, meta };