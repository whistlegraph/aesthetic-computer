// A deliberately small local vocabulary. Output is real editable AC source.
export function instantPiece(text) {
 if(typeof text!=='string')return null;
 const match=text.trim().toLowerCase().match(/^(?:(?:please )?(?:make|draw|create)(?: me)? |i want )?(?:a |an )?(pink|red|blue|green|yellow|orange|purple|white)?\s*(circle|square)(?:\s*[.!?]?|\s+(?:with|that|and)\s+.+)$/);
 if(!match)return null;
 const color=match[1]||'pink',shape=match[2];
 return `// Whistlegraph starter: ${color} ${shape}\nexport function paint({wipe, ink, screen}) {\n  wipe(24, 18, 30);\n  const r = Math.min(screen.width, screen.height) / 5;\n  ink("${color}").${shape==='circle'?'circle(screen.width / 2, screen.height / 2, r, true)':'box(screen.width / 2 - r, screen.height / 2 - r, r * 2, r * 2)'};\n}\n`;
}
