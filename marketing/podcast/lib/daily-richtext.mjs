// Explicit destinations: names stay prose; only recognized subjects become links.
import { KidLisp } from "../../../system/public/aesthetic.computer/lib/kidlisp.mjs";
import { richBlocks, richNode, richLink } from "../../../system/public/aesthetic.computer/lib/rich-text.mjs";
export const DAILY_LINKS = {
  Aesel: "https://aesel.app",
  Slab: "https://github.com/whistlegraph/aesthetic-computer/tree/main/slab",
  Loopboy: "https://github.com/whistlegraph/aesthetic-computer/tree/main/slab",
  Whistlegraph: "https://aesthetic.computer/whistlegraph",
  Give: "https://give.aesthetic.computer",
  KidLisp: "https://kidlisp.com",
  "Aesthetic Computer": "https://aesthetic.computer",
  GitHub: "https://github.com/whistlegraph/aesthetic-computer",
  Tangled: "https://tangled.org/aesthetic.computer/core",
};
// KidLisp resolves escaped quotes but otherwise keeps backslashes literal.
const quote = value => '"' + value.replace(/"/g, '\\"').replace(/\r?\n/g, " ") + '"';

export function linkedParagraph(text, destinations = DAILY_LINKS) {
  const names = Object.keys(destinations).sort((a, b) => b.length - a.length);
  if (!names.length) return `(paragraph ${quote(text)})`;
  const pattern = new RegExp(`\\b(${names.map(name => name.replace(/[.*+?^${}()|[\]\\]/g, "\\$&")).join("|")})\\b`, "g");
  const parts = [];
  let cursor = 0;
  for (const match of text.matchAll(pattern)) {
    if (match.index > cursor) parts.push(quote(text.slice(cursor, match.index)));
    parts.push(`(link ${quote(match[0])} ${quote(destinations[match[0]])})`);
    cursor = match.index + match[0].length;
  }
  if (cursor < text.length) parts.push(quote(text.slice(cursor)));
  return `(paragraph ${parts.join(" ")})`;
}

export function richDailyPiece({ title, body, destinations }) {
  return `(wipe black)\n(flow\n  (heading ${quote(title)})\n  ${body.split(/\n\s*\n/).map(paragraph => linkedParagraph(paragraph, destinations)).join("\n  ")}\n)`;
}

// Inspect literal data, without executing a piece or following its links.
export function readRichDailySource(source) {
  const lisp = new KidLisp();
  const ast = lisp.parse(source);
  const flows = ast.filter(form => form[0] === "flow");
  if (lisp.lastValidationErrors || flows.length !== 1) throw new Error("Expected one literal rich-text flow");
  const literal = value => {
    if (Array.isArray(value)) {
      if (value[0] !== "link" || value.length !== 3) throw new Error("Expected a literal link");
      return richLink(literal(value[1]), literal(value[2]));
    }
    if (typeof value !== "string" || !value.startsWith('"')) throw new Error("Expected literal prose");
    // KidLisp's tokenizer has already resolved escaped quotes.
    return value.slice(1, -1);
  };
  const blocks = richBlocks(flows[0].slice(1).map(form => {
    if (!Array.isArray(form) || !["heading", "paragraph"].includes(form[0])) throw new Error("Expected a heading or paragraph");
    return richNode(form[0], form.slice(1).map(literal));
  }));
  const headings = blocks.filter(block => block.kind === "heading");
  if (headings.length !== 1) throw new Error("Expected one daily title");
  return { title: headings[0].text, body: blocks.filter(block => block.kind === "paragraph").map(block => block.text).join("\n\n") };
}
