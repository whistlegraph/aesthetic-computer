// `validatePieceSource` without a subprocess.
//
// The real one in `easel/src/revisions.mjs` spawns `node --input-type=module
// --check` and lets V8 be the judge. A webview has no subprocess and no
// non-executing module parser — `import()` and `new Function` both compile *and*
// run, and the rule that file states plainly ("user code must never execute in
// Aesel") is the whole reason it shells out rather than importing.
//
// So this is a scanner, not a parser, and it is weaker on purpose. Be clear
// about the trade: it will not catch `let let = 1` or a bad destructuring
// pattern. What it does catch is the failure this check exists for — a stream
// that stopped mid-file, leaving an unclosed brace, string or template. That is
// what a truncated `write_piece` looks like, and shipping it would blank the
// preview. Everything subtler is caught a second later by the runtime, which
// reports into the transcript the same way it already does on desktop.
//
// If this ever needs to be strict, the honest fix is a real parser (acorn is
// ~120 KB) or a `--check` endpoint on the server — not a cleverer regex.

const OPENERS = { "(": ")", "[": "]", "{": "}" };
const CLOSERS = { ")": "(", "]": "[", "}": "{" };

// A `/` starts a regex rather than a division when the last meaningful thing
// before it cannot end an expression. Approximate, and that is fine: guessing
// wrong costs a false report on code that divides by a bracketed expression,
// which is rare in a piece and still only refuses one revision.
function regexCanFollow(previous) {
  if (!previous) return true;
  if (/[)\]]/.test(previous)) return false;
  if (/[A-Za-z0-9_$]/.test(previous)) return false;
  return true;
}

function scan(source) {
  const stack = [];
  // Template literals nest: `${ `inner` }` is legal, so depth has to be a stack
  // too, tracking whether each `{` we meet belongs to a template substitution.
  const templates = [];
  let previous = "";
  let i = 0;

  while (i < source.length) {
    const char = source[i];
    const next = source[i + 1];

    if (char === "/" && next === "/") {
      const end = source.indexOf("\n", i);
      i = end === -1 ? source.length : end;
      continue;
    }

    if (char === "/" && next === "*") {
      const end = source.indexOf("*/", i + 2);
      if (end === -1) return "a block comment is never closed";
      i = end + 2;
      continue;
    }

    if (char === '"' || char === "'") {
      const failure = skipQuoted(source, i, char);
      if (typeof failure === "string") return failure;
      i = failure;
      previous = char;
      continue;
    }

    if (char === "`") {
      templates.push(stack.length);
      const moved = skipTemplate(source, i, stack, templates);
      if (typeof moved === "string") return moved;
      i = moved;
      previous = "`";
      continue;
    }

    if (char === "/" && regexCanFollow(previous)) {
      const moved = skipRegex(source, i);
      if (moved !== null) {
        i = moved;
        previous = "/";
        continue;
      }
    }

    if (OPENERS[char]) {
      stack.push(char);
    } else if (CLOSERS[char]) {
      const open = stack.pop();
      if (open !== CLOSERS[char]) {
        return open === undefined
          ? `a stray \`${char}\` closes something that was never opened`
          : `a \`${open}\` is closed by \`${char}\``;
      }
    }

    if (!/\s/.test(char)) previous = char;
    i += 1;
  }

  if (stack.length) {
    const open = stack.at(-1);
    return `a \`${open}\` is never closed — the file looks cut off`;
  }
  return null;
}

function skipQuoted(source, start, quote) {
  let i = start + 1;
  while (i < source.length) {
    const char = source[i];
    if (char === "\\") {
      i += 2;
      continue;
    }
    if (char === "\n") return `a ${quote === '"' ? "double" : "single"}-quoted string runs past the end of its line`;
    if (char === quote) return i + 1;
    i += 1;
  }
  return "a string is never closed — the file looks cut off";
}

// Walks a template literal, recursing through `${ … }` so brackets inside a
// substitution are balanced against the same stack the rest of the file uses.
function skipTemplate(source, start, stack, templates) {
  let i = start + 1;
  while (i < source.length) {
    const char = source[i];
    if (char === "\\") {
      i += 2;
      continue;
    }
    if (char === "`") {
      templates.pop();
      return i + 1;
    }
    if (char === "$" && source[i + 1] === "{") {
      let depth = 1;
      i += 2;
      while (i < source.length && depth > 0) {
        const inner = source[i];
        if (inner === "\\") {
          i += 2;
          continue;
        }
        if (inner === "`") {
          const moved = skipTemplate(source, i, stack, templates);
          if (typeof moved === "string") return moved;
          i = moved;
          continue;
        }
        if (inner === '"' || inner === "'") {
          const moved = skipQuoted(source, i, inner);
          if (typeof moved === "string") return moved;
          i = moved;
          continue;
        }
        if (inner === "{") depth += 1;
        else if (inner === "}") depth -= 1;
        i += 1;
      }
      if (depth > 0) return "a `${` substitution is never closed — the file looks cut off";
      continue;
    }
    i += 1;
  }
  return "a template literal is never closed — the file looks cut off";
}

// Returns the index after the regex, or null when this `/` was division after
// all (an unterminated one on a single line is the tell).
function skipRegex(source, start) {
  let i = start + 1;
  let inClass = false;
  while (i < source.length) {
    const char = source[i];
    if (char === "\\") {
      i += 2;
      continue;
    }
    if (char === "\n") return null;
    if (char === "[") inClass = true;
    else if (char === "]") inClass = false;
    else if (char === "/" && !inClass) {
      i += 1;
      while (i < source.length && /[a-z]/.test(source[i])) i += 1;
      return i;
    }
    i += 1;
  }
  return null;
}

export async function validatePieceSource(source, file) {
  if (typeof source !== "string" || !source.trim()) throw new Error("The piece is empty.");
  // Matches the original: other runtimes keep their own loader validation.
  if (!String(file).endsWith(".mjs")) return;
  const failure = scan(source);
  if (failure) {
    throw new Error(`Incomplete or invalid JavaScript; previous preview kept. ${failure}.`);
  }
}

// The desktop module also exports `PieceRevisions`, a local snapshot store under
// ~/.local/share/easel/history. The phone keeps its history somewhere else, so
// this is a stub that fails loudly rather than a half-implementation that looks
// like it is saving and is not.
export class PieceRevisions {
  constructor() {
    throw new Error("PieceRevisions is not available in the phone client.");
  }
}
