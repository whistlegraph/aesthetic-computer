function paramNameFromTypedParam(raw) {
  const withoutDefault = String(raw || "")
    .replace(/\s*=.*/, "")
    .replace(/\[\]/g, "")
    .trim();
  if (!withoutDefault) return "";
  const bits = withoutDefault.split(/\s+/);
  return bits[bits.length - 1] || "";
}

function convertParams(paramList) {
  return String(paramList || "")
    .split(",")
    .map((part) => paramNameFromTypedParam(part))
    .filter(Boolean)
    .join(", ");
}

function normalizeExpression(expr) {
  return String(expr || "")
    .replace(/!=/g, "~=")
    .replace(/&&/g, " and ")
    .replace(/\|\|/g, " or ")
    .replace(/!\s*(?=[A-Za-z_(])/g, "not ");
}

function rewriteMathCalls(line) {
  return line
    .replace(/\bsin\s*\(/g, "math.sin(")
    .replace(/\bcos\s*\(/g, "math.cos(")
    .replace(/\btan\s*\(/g, "math.tan(")
    .replace(/\babs\s*\(/g, "math.abs(")
    .replace(/\bsqrt\s*\(/g, "math.sqrt(")
    .replace(/\bfloor\s*\(/g, "math.floor(")
    .replace(/\bceil\s*\(/g, "math.ceil(")
    .replace(/\bpow\s*\(/g, "math.pow(")
    .replace(/\bmin\s*\(/g, "math.min(")
    .replace(/\bmax\s*\(/g, "math.max(");
}

function rewriteForLoop(line) {
  const match = line.match(
    /^(\s*)for\s*\(\s*(?:(?:int|float|double|long)\s+)?([A-Za-z_][A-Za-z0-9_]*)\s*=\s*([^;]+?)\s*;\s*\2\s*(<=|<|>=|>)\s*([^;]+?)\s*;\s*\2\s*(\+\+|--|\+=\s*[^;]+|-=\s*[^;]+)\s*\)\s*\{?\s*$/,
  );

  if (!match) return null;

  const indent = match[1];
  const name = match[2];
  const start = normalizeExpression(match[3].trim());
  const op = match[4];
  const bound = normalizeExpression(match[5].trim());
  const stepExpr = match[6].replace(/\s+/g, "");

  let step = "1";
  if (stepExpr === "--") step = "-1";
  else if (stepExpr.startsWith("+=")) step = normalizeExpression(stepExpr.slice(2));
  else if (stepExpr.startsWith("-=")) step = `-${normalizeExpression(stepExpr.slice(2))}`;

  let finish = bound;
  if (op === "<") finish = `(${bound}) - 1`;
  if (op === ">") finish = `(${bound}) + 1`;

  return `${indent}for ${name} = ${start}, ${finish}, ${step} do {`;
}

function rewriteAugmentedAssignments(line) {
  return line
    .replace(/\b([A-Za-z_][A-Za-z0-9_]*)\s*\+\=\s*([^;]+);?$/g, "$1 = $1 + ($2)")
    .replace(/\b([A-Za-z_][A-Za-z0-9_]*)\s*\-\=\s*([^;]+);?$/g, "$1 = $1 - ($2)")
    .replace(/\b([A-Za-z_][A-Za-z0-9_]*)\s*\*\=\s*([^;]+);?$/g, "$1 = $1 * ($2)")
    .replace(/\b([A-Za-z_][A-Za-z0-9_]*)\s*\/\=\s*([^;]+);?$/g, "$1 = $1 / ($2)")
    .replace(/\b([A-Za-z_][A-Za-z0-9_]*)\+\+;?$/g, "$1 = $1 + 1")
    .replace(/\b([A-Za-z_][A-Za-z0-9_]*)--;?$/g, "$1 = $1 - 1");
}

export function transpileProcessingToLua(source) {
  let text = String(source || "").replace(/\r\n?/g, "\n");

  text = text.replace(/\/\*([\s\S]*?)\*\//g, (_all, inner) => `--[[${inner.trim()}]]`);
  text = text.replace(/}\s*else if\s*\(([^)]*)\)\s*{/g, "\nelseif ($1) {\n");
  text = text.replace(/}\s*else\s*{/g, "\nelse {\n");

  const rewritten = text
    .split("\n")
    .map((raw) => {
      let line = raw.replace(/\t/g, "  ");
      line = line.replace(/\/\/(.*)$/g, "--$1");

      const numericFor = rewriteForLoop(line);
      if (numericFor) {
        return numericFor;
      }

      line = line.replace(
        /^(\s*)(?:public\s+|private\s+|protected\s+|static\s+)*(?:void|float|double|int|long|boolean|String|char)\s+([A-Za-z_][A-Za-z0-9_]*)\s*\(([^)]*)\)\s*\{?\s*$/,
        (_all, indent, name, params) => `${indent}function ${name}(${convertParams(params)}) {`,
      );

      line = line.replace(
        /^(\s*)(?:final\s+)?(?:float|double|int|long|boolean|String|char)\s+([A-Za-z_][A-Za-z0-9_]*)\s*(=\s*.+)?;\s*$/,
        (_all, indent, name, assign) => `${indent}local ${name}${assign ? ` ${assign.trim()}` : ""}`,
      );

      line = line.replace(
        /^(\s*)if\s*\((.*)\)\s*\{?\s*$/,
        (_all, indent, cond) => `${indent}if ${normalizeExpression(cond)} then {`,
      );

      line = line.replace(
        /^(\s*)elseif\s*\((.*)\)\s*\{?\s*$/,
        (_all, indent, cond) => `${indent}elseif ${normalizeExpression(cond)} then {`,
      );

      line = line.replace(
        /^(\s*)while\s*\((.*)\)\s*\{?\s*$/,
        (_all, indent, cond) => `${indent}while ${normalizeExpression(cond)} do {`,
      );

      line = line.replace(/^(\s*)else\s*\{?\s*$/, "$1else {");
      line = line.replace(/\bprintln\s*\(/g, "print(");
      line = rewriteAugmentedAssignments(line);
      line = rewriteMathCalls(line);
      line = line.replace(/;\s*$/g, "");
      line = line.replace(/\btrue\b/g, "true").replace(/\bfalse\b/g, "false");
      line = line.replace(/!=/g, "~=");
      line = line.replace(/&&/g, " and ").replace(/\|\|/g, " or ");
      line = line.replace(/!\s*(?=[A-Za-z_(])/g, "not ");
      return line;
    })
    .join("\n");

  return rewritten
    .replace(/\{/g, "")
    .replace(/\}/g, "\nend")
    .replace(/\n{3,}/g, "\n\n")
    .split("\n")
    .map((line) => line.replace(/[ \t]+$/g, ""))
    .join("\n")
    .trim();
}

export function compileProcessingSourceForRuntime(source) {
  const text = String(source || "");
  if (/^\s*function\s+[A-Za-z_][A-Za-z0-9_]*\s*\(/m.test(text)) {
    return text;
  }
  return transpileProcessingToLua(text);
}
