// 🖌️ NeoBrush, 25.09.16
// Enhanced rectangular brush with fade colors, modes, and thickness

// TODO: Support robotics.
// TODO: Brushes should have a 'color coded' word top level and there
//       should be a Bauhaus like cateogorical method across AC pieces.

/* region docs 📚
  Draw colored rectangles with brush gestures.
  // neobrush color
  Use `neobrush:type:center color`
            ^ use `o`, `i` or `f` for `outline`, `inline` or `fill`
              `outline` and `inline` both take an integer for thickness.

  Ex. `neobrush:o:c 255 0 0` for an outlined 1px rectangle.
  Ex. `neobrush:i-2` for a randomly colored 2px inline rectangle.
              ^ add a number here for thickness! 
  Ex. `neobrush fade:red-blue` for a red to blue fade rectangle.
  Ex. `neobrush fade:red-blue-yellow:vertical` for a multi-color vertical fade.
#endregion */

let mode = "fill", centered = false;

// 🥾 Boot (Runs once before first paint and sim)
function boot({ colon, system }) {
  // Parse mode and centering from colon parameters
  if (colon.length > 0) {
    const param = colon[0];
    if (param.includes("c")) centered = true;
    if (param.includes("o")) mode = "outline";
    else if (param.includes("i")) mode = "inline";
    else if (param.includes("f")) mode = "fill";
  }
}

function paint({ pen, ink, system }) {
  // No preview rendering in paint - overlay handles this
}

function overlay({ ink, system }) {
  if (!system.nopaint.brush?.dragBox) return;
  const dragBox = system.nopaint.brush.dragBox;
  const rect = centered ? { ...dragBox, w: dragBox.w * 2, h: dragBox.h * 2 } : dragBox;
  
  const useColor = system.nopaint.color;
  
  // ink() now automatically handles both regular colors and fade colors
  ink(useColor).box(rect, mode);
}

function brush({ ink, lift, system }) {
  if (!lift) return;
  const dragBox = system.nopaint.finalDragBox;
  if (!dragBox) return;
  const rect = centered ? { ...dragBox, w: dragBox.w * 2, h: dragBox.h * 2 } : dragBox;
  
  const useColor = system.nopaint.color;
  
  // ink() now automatically handles both regular colors and fade colors
  ink(useColor).box(rect, mode);
}

export { boot, paint, overlay, brush };