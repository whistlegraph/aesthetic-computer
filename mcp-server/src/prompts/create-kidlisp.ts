// Prompt: create-kidlisp
// Guide the AI to write a good KidLisp piece for aesthetic.computer

export const createKidLispPrompt = {
  name: "create-kidlisp",
  description:
    "Guide for creating a KidLisp piece — a creative coding Lisp dialect for generative art on aesthetic.computer",
  arguments: [
    {
      name: "description",
      description: "What the piece should do or look like",
      required: true,
    },
  ],
};

export function getCreateKidLispPrompt(args: { description: string }) {
  return `Create a KidLisp piece for aesthetic.computer.

Description: ${args.description}

Use the \`aesthetic-computer://kidlisp-reference\` resource for the full language reference, and \`aesthetic-computer://piece-examples\` for real top-hit examples with 500+ views.

## KidLisp Quick Primer

KidLisp uses S-expressions: \`(function arg1 arg2)\`
Shorthand comma syntax also works: \`purple, ink, line, blur 5\`

Key functions:
- \`(wipe color)\` or bare word \`black\` — set background
- \`(ink color [alpha])\` — set draw color (CSS names, RGB, rainbow, erase)
- \`(box x y w h)\`, \`(circle x y r)\`, \`(tri x1 y1 x2 y2 x3 y3)\` — shapes
- \`(line x1 y1 x2 y2)\` or \`(line)\` for random — lines
- \`(plot x y)\` — single pixel
- \`(flood x y)\` — flood fill
- \`(def name value)\` — define variable
- \`(later name params body)\` — define function
- \`(repeat count iter expr...)\` — loop
- \`(? a b c)\` — random pick, \`(... a b c)\` — cycle over time
- \`(wiggle amount)\` — random ±amount/2
- \`w\` / \`h\` / \`f\` — width / height / frame
- \`(scroll dx dy)\`, \`(zoom factor)\`, \`(spin angle)\`, \`(blur amount)\` — transforms
- \`(contrast amount)\`, \`(suck strength)\`, \`(sort)\` — more transforms
- \`(bake)\` — commit drawing to background layer
- \`(coat color alpha)\` — semi-transparent overlay
- \`fade:red-blue-black\` — gradient colors
- Timing: \`0.1s\`, \`1s...\`, \`2s!\` — timed/repeating execution
- \`erase\` — transparent ink for creating holes
- \`($codeId x y w h alpha)\` — embed another piece

## Guidelines

1. Study the top-hit examples — minimal code with maximum visual impact
2. Start with a background color (bare word or wipe)
3. Use feedback loops: scroll + zoom + blur for evolving trails
4. Use \`?\` for weighted randomness: \`(? 0 0 0 1)\` makes 0 most likely
5. Use \`...\` for cycling values over time
6. Multiple timers at different speeds create layered rhythm
7. Keep it short — many top hits are under 5 lines
8. Variable names use underscores, not dashes

After writing the piece, publish it using the \`publish_kidlisp\` tool. The user will get a live URL.`;
}
