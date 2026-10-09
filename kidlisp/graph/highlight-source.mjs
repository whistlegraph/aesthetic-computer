import {KidLisp} from "../../system/public/aesthetic.computer/lib/kidlisp.mjs";

// Preserve every source character, including comments and whitespace. Text is
// inserted as text nodes; source cannot inject markup into the preview.
export function highlightSource(element, source) {
  const lisp = new KidLisp(); lisp.isEditMode = true;
  const parts = source.match(/;[^\n]*|[(),]|"(?:[^"\\]|\\.)*"|'(?:[^'\\]|\\.)*'|\s+|[^\s()";',]+/g) || [];
  const tokens = parts.filter(p => !/^\s|^;/.test(p));
  const fragment = document.createDocumentFragment();let index = 0;
  for (const part of parts) {
    if (/^\s/.test(part)) { fragment.append(document.createTextNode(part)); continue; }
    const span = document.createElement("span");span.textContent = part;
    const color = part.startsWith(";") ? "#9aadc1" : lisp.getTokenColor(part,tokens,index++);
    if (color === "RAINBOW") span.className = "syntax-rainbow";
    else if (color?.startsWith("COMPOUND:")) span.style.color = color.split(":").at(-1);
    else {
      const css = color?.includes(",") ? `rgb(${color})` : color;
      span.style.color = CSS.supports("color",css) ? css : "#c9a7ff";
    }
    fragment.append(span);
  }
  element.replaceChildren(fragment);
}
