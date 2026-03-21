// Resource: aesthetic-computer://piece-template
// Starter template for a new aesthetic.computer piece

export const pieceTemplateResource = {
  uri: "aesthetic-computer://piece-template",
  name: "Aesthetic Computer Piece Template",
  description: "A starter template for creating a new aesthetic.computer piece with all lifecycle functions",
  mimeType: "text/javascript",
};

export function getPieceTemplate() {
  return `// my-piece, ${new Date().getFullYear()}.${String(new Date().getMonth() + 1).padStart(2, '0')}.${String(new Date().getDate()).padStart(2, '0')}
// A short description of this piece.

// \`boot\` runs once at startup.
export function boot($) {
  // Initialize state here.
  // $.screen.width, $.screen.height for dimensions.
}

// \`sim\` runs every logic tick (before paint).
export function sim($) {
  // Update simulation / physics / state.
}

// \`paint\` runs every frame for rendering.
export function paint($) {
  const { wipe, ink, line, box, circle, pixel, screen } = $;
  wipe("black");        // Clear screen.
  ink("red");           // Set draw color.
  box(10, 10, 50, 50);  // Draw a filled rect.
}

// \`act\` runs on user input events.
export function act($) {
  const { event: e } = $;
  // e.is("touch"), e.is("draw"), e.is("lift")
  // e.x, e.y for coordinates
}
`;
}
