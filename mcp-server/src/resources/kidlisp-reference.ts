// Resource: aesthetic-computer://kidlisp-reference
// Quick reference for KidLisp syntax

export const kidlispReferenceResource = {
  uri: "aesthetic-computer://kidlisp-reference",
  name: "KidLisp Quick Reference",
  description: "A quick reference guide for KidLisp syntax and common functions",
  mimeType: "text/markdown",
};

export function getKidLispReference() {
  return `# KidLisp Quick Reference

KidLisp is a creative coding Lisp dialect for aesthetic.computer.

## Basic Syntax

\`\`\`lisp
; Comments start with semicolon
(function-name arg1 arg2 ...)
\`\`\`

## Drawing Functions

- \`(wipe color)\` - Clear screen with color
- \`(ink color)\` - Set draw color
- \`(line x1 y1 x2 y2)\` - Draw a line
- \`(box x y width height)\` - Draw filled rectangle
- \`(circle x y radius)\` - Draw filled circle
- \`(pixel x y)\` - Set single pixel

## Variables & Math

- \`w\` - Screen width
- \`h\` - Screen height
- \`(/ a b)\` - Division
- \`(* a b)\` - Multiplication
- \`(+ a b)\` - Addition
- \`(- a b)\` - Subtraction

## Colors

Common colors: \`red\`, \`green\`, \`blue\`, \`yellow\`, \`white\`, \`black\`, \`gray\`, \`purple\`, \`orange\`, \`pink\`, \`brown\`, \`cyan\`, \`magenta\`

## Example

\`\`\`lisp
(wipe blue)
(ink yellow)
(circle (/ w 2) (/ h 2) 100)
\`\`\`

For more info, visit https://kidlisp.com
`;
}
