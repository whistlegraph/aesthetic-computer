// Bounded commands accepted by the specialized shaders. Validate the entire
// frame before encoding so a rejected command cannot advance feedback state.
export function validateRozNodes(nodes) {
  if (!Array.isArray(nodes) || nodes.length > 6) throw new TypeError("Invalid graph node budget");
  const byte = n => Number.isInteger(n) && n >= 0 && n <= 255;
  for (const n of nodes) {
    let valid = !!n;
    switch (n?.op) {
      case "line": valid &&= Array.isArray(n.color) && n.color.length === 4 && n.color.every(byte); break;
      case "circle": valid &&= [2,4,8].includes(n.radius) && Array.isArray(n.color) && n.color.length === 4 && n.color[3] === 8 && n.color.slice(0,3).every(v => v === 0 || v === 255); break;
      case "spin": valid &&= [-2,-1,1,2].includes(n.value); break;
      case "zoom": valid &&= n.value === 1.1; break;
      case "contrast": valid &&= n.value === 1.05; break;
      case "scroll": valid &&= [n.x,n.y].every(v => Number.isInteger(v) && Math.abs(v) <= 1); break;
      default: valid = false;
    }
    if (!valid) throw new TypeError("Unsupported GPU graph node or parameter");
  }
  return nodes;
}
