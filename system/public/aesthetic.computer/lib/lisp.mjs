// Lisp, 24.4.17.12.03
// A lisp interpreter / compiler for writing Aesthetic Computer pieces.

// Working expressions:
// (+ 4 5 5)
// (* (+ 2 3) (- 4 1))

/* #region 🏁 TODO 
  - [🟡] Get publishing working for lisp code / pasted code.
  - [-] Make a basic graphical editor / viewer...
  + Done
  - [x] Add global variables / more complex programming.
  - [x] Add support for comments.
  - [x] Get a named command running through, like "line" and "ink". 
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
  "+": (api, args) => args.reduce((a, b) => a + b, 0),
  "-": (api, args) => args.reduce((a, b) => a - b),
  "*": (api, args) => args.reduce((a, b) => a * b, 1),
  "/": (api, args) => args.reduce((a, b) => a / b),
  // Paint API
  resolution: (api, args) => {
    return api.resolution?.(...args);
  },
  wipe: (api, args) => {
    return api.wipe?.(processArgStringTypes(args));
  },
  ink: (api, args) => {
    return api.ink?.(processArgStringTypes(args));
  },
  line: (api, args = []) => {
    return api.line(...args);
  },
  wiggle: (api, args = []) => {
    return api.wiggle(...args);
  },
  box: (api, args = []) => {
    return api.box(...args);
  },
  // Getters / globals.
  width: (api) => {
    return api.screen.width;
  },
  height: (api) => {
    return api.screen.height;
  },
};

// Parse and evaluate a lisp source module
// into a running aesthetic computer piece.
function module(source) {
  const parsed = parse(source);
  console.log("🐍 Parsed:", parsed);

  return {
    paint: ({ wipe, ink, api }) => {
      console.log("🖌️ Painting");
      const evaluated = evaluate(parsed, api);
      ink("white").write(evaluated || "nada", { center: "xy" });
      // return false;
    },
  };
}

function tokenize(input) {
  // Remove comments from input (anything from ';' to the end of the line)
  input = input.replace(/;.*$/gm, "");
  // Replace parentheses with spaces around them and split into an array by whitespace
  return input.replace(/\(/g, " ( ").replace(/\)/g, " ) ").trim().split(/\s+/);
}

function parse(program) {
  return readFromTokens(tokenize(program));
}

function evaluate(parsed, api = {}) {
  // console.log("Evaluating:", parsed);
  let result;
  for (const item of parsed) {
    // console.log("Item:", item);
    if (Array.isArray(item)) {
      // The first element indicates the function to call
      const [fn, ...args] = item;
      // Check if the function requires recursive evaluation
      if (globalEnv[fn]) {
        // Prepare arguments, evaluate if they are also functions
        const evaledArgs = args.map((arg) =>
          Array.isArray(arg) || (typeof arg === "string" && !/^".*"$/.test(arg))
            ? evaluate([arg], api)
            : arg,
        );
        result = globalEnv[fn](api, evaledArgs);
      }
    } else {
      result = globalEnv[item]?.(api);
    }
  }
  return result;
}

function readFromTokens(tokens) {
  const result = [];
  // console.log("Tokens:", tokens);
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
