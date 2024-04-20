// Lisp, 24.4.17.12.03
// A lisp interpreter / compiler for writing Aesthetic Computer pieces.

// Working expressions:
// (+ 4 5 5)
// (* (+ 2 3) (- 4 1))

/* #region 🏁 TODO 
 - [] Get a named command running through, like "line" and "ink". 
#endregion */

function processArgStringTypes(args) {
  return args.map((arg) => {
    if (typeof arg === "string" && arg.startsWith('"') && arg.endsWith('"')) {
      // Remove the first and last character which are the quotes
      return arg.substring(1, arg.length - 1);
    }
    return arg;
  });
}

const globalEnv = {
  // Mathematical Operators
  "+": (args) => args.reduce((a, b) => a + b, 0),
  "-": (args) => args.reduce((a, b) => a - b),
  "*": (args) => args.reduce((a, b) => a * b, 1),
  "/": (args) => args.reduce((a, b) => a / b),
  // Paint API
  ink: (args, api) => {
    console.log("Ink arg:", args);
    // Map each argument, removing quotes if the argument is a string
    return api.ink?.(processArgStringTypes(args));
  },
  line: (args = [], api) => {
    return api.line?.(...args);
  },
};

// Parse and evaluate a lisp source module
// into a running aesthetic computer piece.
function module(source) {
  const parsed = parse(source);
  console.log("🐍 Parsed:", parsed);
  const evaluated = evaluate(parsed);
  console.log("🐍 Evaluated:", evaluated);
  return {
    paint: ({ wipe, api }) => {
      wipe("blue")
        .ink("white")
        .write(evaluated || "nada", { center: "xy" });
      evaluate(parsed, api);
    },
  };
}

function tokenize(input) {
  return input.replace(/\(/g, " ( ").replace(/\)/g, " ) ").trim().split(/\s+/);
}

function parse(program) {
  return readFromTokens(tokenize(program));
}

function evaluate(parsed, api = {}) {
  console.log("Evaluating:", parsed);
  let result;
  for (const item of parsed) {
    if (Array.isArray(item)) {
      // The first element indicates the function to call
      const [fn, ...args] = item;
      // Check if the function requires recursive evaluation
      if (globalEnv[fn]) {
        // Prepare arguments, evaluate if they are also functions
        const evaluatedArgs = args.map((arg) =>
          Array.isArray(arg) ? evaluate([arg], api) : arg,
        );
        // Call the function from globalEnv with the evaluated arguments
        result = globalEnv[fn](evaluatedArgs, api);
        console.log(`Result of ${fn}: ${result}`);
      }
    }
  }
  return result;
}

function readFromTokens(tokens) {
  const result = [];
  console.log("Tokens:", tokens);
  while (tokens.length > 0) {
    if (tokens[0] === ")") {
      throw new SyntaxError("Unexpected ')'");
    }
    result.push(readExpression(tokens));
  }
  return result;
}

function readExpression(tokens) {
  if (tokens.length === 0) {
    throw new SyntaxError("Unexpected EOF");
  }
  let token = tokens.shift();
  if (token === "(") {
    let subExpr = [];
    while (tokens.length > 0 && tokens[0] !== ")") {
      subExpr.push(readExpression(tokens));
    }
    if (tokens[0] === ")") {
      tokens.shift(); // Remove the closing ')'
    } else {
      throw new SyntaxError("Expected ')'");
    }
    return subExpr;
  } else if (token === ")") {
    throw new SyntaxError("Unexpected ')'");
  } else {
    return atom(token);
  }
}

function atom(token) {
  if (token[0] === '"' && token[token.length - 2] === '"') {
    // Remove the surrounding quotes and return the string
    return token.slice(1, -1);
  } else {
    // Attempt to convert the token to a number
    let num = Number(token);
    return isNaN(num) ? token : num;
  }
}

export { module, parse, evaluate };
