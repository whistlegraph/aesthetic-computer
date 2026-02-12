// Prompt: create-piece
// Help the AI write a good aesthetic.computer piece

export const createPiecePrompt = {
  name: "create-piece",
  description: "Guide for creating an aesthetic.computer piece with best practices",
  arguments: [
    {
      name: "name",
      description: "Name of the piece",
      required: true,
    },
    {
      name: "description",
      description: "What the piece should do",
      required: true,
    },
  ],
};

export function getCreatePiecePrompt(args: { name: string; description: string }) {
  return `Create an aesthetic.computer piece called "${args.name}".

Description: ${args.description}

Use the piece template from the \`aesthetic-computer://piece-template\` resource as a starting point. The piece should:

- Export the appropriate lifecycle functions (boot, paint, sim, act)
- Use the $ API for drawing (wipe, ink, box, line, circle, pixel, etc.)
- Be creative and visually interesting
- Work at any screen resolution (use $.screen.width and $.screen.height)
- Follow aesthetic.computer conventions and style

Lifecycle functions:
- \`boot($)\` - Runs once at startup for initialization
- \`sim($)\` - Runs every logic tick (before paint) for state updates
- \`paint($)\` - Runs every frame for rendering
- \`act($)\` - Runs on user input events (touch, draw, lift, keyboard)

Common $ API functions:
- \`$.wipe(color)\` - Clear screen
- \`$.ink(color)\` - Set draw color
- \`$.box(x, y, width, height)\` - Draw filled rectangle
- \`$.line(x1, y1, x2, y2)\` - Draw line
- \`$.circle(x, y, radius)\` - Draw filled circle
- \`$.pixel(x, y)\` - Set single pixel
- \`$.screen.width\`, \`$.screen.height\` - Screen dimensions
- \`$.event.is(type)\` - Check event type
- \`$.event.x\`, \`$.event.y\` - Event coordinates

After writing the piece, publish it using the \`publish_piece\` tool.`;
}
