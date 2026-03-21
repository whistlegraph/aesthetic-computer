// Signature, 24.01.26.19.42
// Formerly `sign`, 23.01.30.02.19
// Stamp a timestamped signature onto the margin of a painting's contents.

/* #region 📚 TODO 
  + Next
  - [] Color options / font options + download code?
  - [] What does `minting` or uploading look like?
  + Done
  - [x] Prototype stamp behavior for bottom left corner.
#endregion */

function paint({ ink, wipe, params, num, screen: { height } }) {
  let text; // Timestamped signature text with byline from parameters. 
  if (params.length > 0) text = `${params.join(" ")} ${num.timestamp()}`;
  else text = num.timestamp();

  const blockHeight = 9;
  const shadow = 1;
  const margin = 6;
  const x = margin;
  const y = height - blockHeight - margin;

  ink().box(x - 1, y, text.length * 6 + 2, blockHeight + 1); // Box w/ outline.
  ink().box(x - 1, y, text.length * 6 + 2, blockHeight + 1, "outline");

  // Text with shadow.
  ink(0).write(text, { x: x + shadow, y: y + shadow }); // Text w/ shadow.
  ink(255).write(text, { x, y });
}

export { paint };

export const system = "nopaint";

// 📚 Library (Useful functions used throughout the piece)
// ...