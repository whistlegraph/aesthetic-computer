// KidLisp AST extractor — Node-only, dependency-free.
//
// Produces a flat list of nodes with parent refs, suitable for sending
// to the sidecar's POST /kidlisp/:code/ast endpoint. The shape is
// structural, not semantic — we don't resolve references or evaluate
// anything. Good enough for corpus-wide structural queries like
// "find all pieces using `wipe`" or "pieces with `repeat` at depth ≥ 2".
//
// This is NOT a strict decree-compliant parser. It's a lightweight
// structural tokenizer aligned with the conventions in
// system/public/aesthetic.computer/lib/kidlisp.mjs:
//   - semicolon comments
//   - parenthesized s-expressions with optional comma arg separators
//   - bare identifiers at line start → wrapped as (ident)
//   - $code references
//   - timing tokens like 1s, 2.5s., 1.5s...
//   - fade:red-blue gradient tokens
//   - string literals in "..." or '...'
//   - numbers (int/float, optional leading -)
//
// Node shape:
//   { id, kind, op?, literal?, parent?, position, depth }
// Where `id` is batch-local integer, `parent` is another id or null.

const TOKEN_RE =
  /\s*(;.*|[(),]|"(?:[^"\\]|\\.)*"|'(?:[^'\\]|\\.)*'|[^\s(),;"']+)/g;

function tokenize(src) {
  const out = [];
  let m;
  TOKEN_RE.lastIndex = 0;
  while ((m = TOKEN_RE.exec(src)) !== null) {
    out.push(m[1]);
  }
  return out;
}

// Detect which pre-parse transform to apply (mirrors kidlisp.mjs).
// Returns a source string where bare-word commands / comma lists have
// been wrapped in parens so the recursive descent below can stay simple.
function normalize(src) {
  // Strip line comments up-front, keeping blank lines so line-splitting stays aligned.
  const lines = src.split("\n").map((l) => {
    const i = l.indexOf(";");
    return i === -1 ? l : l.substring(0, i);
  });

  // Wrap comma-separated one-liners into parenthesized exprs.
  // e.g. "blue, ink rainbow" -> "(blue) (ink rainbow)"
  const transformed = lines.map((line) => {
    const trimmed = line.trim();
    if (!trimmed) return "";
    if (trimmed.includes(",")) {
      return trimmed
        .split(",")
        .map((e) => e.trim())
        .filter(Boolean)
        .map((e) =>
          e.startsWith("(")
            ? e
            : /^[a-zA-Z_$]/.test(e)
              ? `(${e})`
              : e,
        )
        .join(" ");
    }
    // Bare-word command at line start: wrap
    if (!trimmed.startsWith("(") && /^[a-zA-Z_$]/.test(trimmed)) {
      return `(${trimmed})`;
    }
    return trimmed;
  });

  return transformed.join("\n");
}

// Classify a non-call atom token.
function classifyAtom(tok) {
  if (/^\$[a-zA-Z0-9]+$/.test(tok)) return { kind: "ref", literal: tok };
  if (/^\d*\.?\d+s(\.{1,3})?!?$/.test(tok))
    return { kind: "timing", literal: tok };
  if (tok.startsWith("fade:")) return { kind: "fade", literal: tok };
  if (/^-?\d+(\.\d+)?$/.test(tok)) return { kind: "number", literal: tok };
  if (/^["'].*["']$/.test(tok)) return { kind: "string", literal: tok };
  return { kind: "atom", literal: tok };
}

/**
 * Parse a kidlisp source string into a flat AST node list with parent
 * refs. Top-level forms are siblings under an implicit root (which is
 * not emitted — top-level nodes just have `parent: null`).
 *
 * @param {string} source
 * @returns {Array<{id:number, kind:string, op?:string, literal?:string,
 *                  parent:number|null, position:number, depth:number}>}
 */
export function extractAst(source) {
  if (!source || typeof source !== "string") return [];

  const normalized = normalize(source);
  const tokens = tokenize(normalized);

  const nodes = [];
  let nextId = 0;
  const push = (node) => {
    const id = nextId++;
    nodes.push({ id, ...node });
    return id;
  };

  // Recursive descent over tokens[ptr]. Returns new ptr.
  let ptr = 0;
  const siblingCounts = new Map(); // parentId(or -1) -> count

  function nextSiblingPos(parent) {
    const key = parent === null ? -1 : parent;
    const n = siblingCounts.get(key) ?? 0;
    siblingCounts.set(key, n + 1);
    return n;
  }

  function readExpr(parent, depth) {
    if (ptr >= tokens.length) return;
    const tok = tokens[ptr];

    if (tok === "(") {
      ptr++;
      // First token inside parens is the op symbol (best effort — if
      // missing or itself a paren, record as unknown).
      let op = null;
      if (ptr < tokens.length && tokens[ptr] !== "(" && tokens[ptr] !== ")") {
        op = tokens[ptr];
        ptr++;
      }
      const callId = push({
        kind: "call",
        op: op ?? undefined,
        parent,
        position: nextSiblingPos(parent),
        depth,
      });
      // Remaining tokens up to matching `)` are children
      while (ptr < tokens.length && tokens[ptr] !== ")") {
        readExpr(callId, depth + 1);
      }
      if (tokens[ptr] === ")") ptr++;
      return;
    }

    if (tok === ")") {
      // Stray close paren — skip
      ptr++;
      return;
    }

    // Atom
    ptr++;
    const cls = classifyAtom(tok);
    push({
      kind: cls.kind,
      literal: cls.literal,
      parent,
      position: nextSiblingPos(parent),
      depth,
    });
  }

  while (ptr < tokens.length) readExpr(null, 0);

  return nodes;
}

// Helper for debugging / tests: reconstruct a text tree.
export function astToTree(nodes) {
  const byId = new Map(nodes.map((n) => [n.id, n]));
  const kids = new Map();
  for (const n of nodes) {
    const p = n.parent ?? -1;
    if (!kids.has(p)) kids.set(p, []);
    kids.get(p).push(n);
  }
  for (const arr of kids.values()) arr.sort((a, b) => a.position - b.position);

  const lines = [];
  function walk(id, indent) {
    const n = byId.get(id);
    const label = n.kind === "call" ? `(${n.op ?? "?"})` : `${n.kind}:${n.literal}`;
    lines.push("  ".repeat(indent) + label);
    for (const c of kids.get(id) ?? []) walk(c.id, indent + 1);
  }
  for (const top of kids.get(-1) ?? []) walk(top.id, 0);
  return lines.join("\n");
}
