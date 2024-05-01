// Lisp, 24.4.17.12.03
// A lisp interpreter / compiler for writing Aesthetic Computer pieces.

/* #region 📚 Examples / Notebook 
 Working programs:

  (later cross x y (line (- 10 x) (- 10 y) (+ 10 x) (+ 10 y)))
  (cross (/ width 2) (/ height 2))

  // TODO: Could I get this to evaluate?
  (cross width/2*(10+x) height/2)
  //     ^ Where this just leans on a Javascript expression evaluator
  //       that can access the local context.


 Conceptual program:

 (later cross x y (line x-10 y-10 x+10 y+10))
 (cross width/2 height/2)

 Even more conceptual program:

 to cross x y -> line x-10 y-10 x+10 y+10
 cross width/2 height/2


  (ink "red")
  (cross 16 16)
  (ink "yellow")
  (cross 32 32)
  (ink "blue")
  (cross (+ 1 2 3) (+ 4 5 6))

  (+ 4 5 5)
  (* (+ 2 3) (- 4 1))

  (def x 10)
  (def y (+ x 10))
  (+ x y)

  ; Paste me into the AC prompt!
  ; (resolution 128)
  (wipe "blue")
  (ink "lime")
  (line 90 (+ 10 20 30) 30 80)
  (ink 255 0 0)
  (line 30 20 100 190)
  (ink "orange" (wiggle 64))
  (box 8 32 (- width 20) (wiggle 32))
  (+ 1 2 3)
#endregion */

/* #region 🏁 TODO 
  - [🟠] Set up this module with some actual JavaScript testing framework
       with expectations so I can make sure it passes.
    - [] This mode can include a mock api.
  + Done
  - [x] Add named function definitions.
  + [x] Add variable setting and retrieval.
  - [x] Get publishing working for lisp code / pasted code.
  - [x] Add global variables / more complex programming.
  - [x] Add support for comments.
  - [x] Get a named command running through, like "line" and "ink". 
#endregion */

// Parse and evaluate a lisp source module
// into a running aesthetic computer piece.
function module(source) {
  const parsed = parse(source);
  console.log("🐍 Parsed:", parsed);

  return {
    paint: ({ wipe, ink, api }) => {
      // console.log("🖌️ Painting");
      const evaluated = evaluate(parsed, api);
      ink("white").write(evaluated || "nada", { center: "xy" });
      return false;
    },
  };
}

const globalDef = {};

const globalEnv = {
  // Program Architecture
  now: (api, args) => {
    // Three length argument assumes a value setting.
    if (args.length === 2) {
      const name = unquoteString(args[0]);
      console.log("Now:", name, args.slice(1));
      globalDef[name] = args[1];
      console.log(globalDef);
      return args[1];
    }
    console.error("Invalid now. Wrong number of arguments.");
  },
  later: (api, args) => {
    if (args.length >= 2) {
      const name = args[0]; // The first argument as the name.
      let params = [];
      let body = null;

      // Iterate over the arguments starting from the second item.
      args.slice(1).forEach((arg, index) => {
        if (Array.isArray(arg) && body === null) {
          body = [arg];
        } else if (body === null) {
          params.push(arg);
        }
      });

      if (body === null) {
        console.error("No body found in arguments for 'later' function.");
        return;
      }

      globalDef[name] = { body, params }; // Assign the array to the global definitions under the name.

      console.log(
        `Latered '${name}' as:`,
        globalDef[name],
        "with parameters:",
        params,
      );

      return body;
    }
  },
  // Mathematical Operators
  "+": (api, args) => args.reduce((a, b) => a + b, 0),
  "-": (api, args) => args.reduce((a, b) => a - b),
  "*": (api, args) => args.reduce((a, b) => a * b, 1),
  "/": (api, args) => args.reduce((a, b) => a / b),
  // Paint API
  resolution: (api, args) => {
    api.resolution?.(...args);
  },
  wipe: (api, args) => {
    api.wipe?.(processArgStringTypes(args));
  },
  ink: (api, args) => {
    api.ink?.(processArgStringTypes(args));
  },
  line: (api, args = []) => {
    api.line(...args);
  },
  wiggle: (api, args = []) => {
    api.wiggle(...args);
  },
  box: (api, args = []) => {
    api.box(...args);
  },
  // Getters / globals.
  width: (api) => {
    api.screen.width;
  },
  height: (api) => {
    api.screen.height;
  },
};

function tokenize(input) {
  // Remove comments from input (anything from ';' to the end of the line)
  input = input.replace(/;.*$/gm, "");
  // Replace parentheses with spaces around them and split into an array by whitespace
  return input.replace(/\(/g, " ( ").replace(/\)/g, " ) ").trim().split(/\s+/);
}

function parse(program) {
  return readFromTokens(tokenize(program));
}

const localEnv = {};

function evaluate(parsed, api = {}, env) {
  // console.log("➗ Evaluating:", parsed);
  let body;

  console.log("Parsed:", parsed);

  // Create a local environment for a function from the params.
  if (parsed.body) {
    body = parsed.body;

    parsed.params.forEach((param, i) => {
      if (Array.isArray(env[i])) {
        localEnv[param] = [env[i]];
      } else {
        localEnv[param] = env[i];
      }
    });

    console.log("Running:", body, "with environment:", localEnv);
  } else {
    // Or just evaluate with the global environment.
    body = parsed;
  }

  let result;
  for (const item of body) {
    if (Array.isArray(item)) {
      // The first element indicates the function to call
      const [fn, ...args] = item;
      // Check if the function requires recursive evaluation
      if (fn === "now") args[0] = `"${args[0]}"`; // Pre-wrap the first arg as a string.

      if (localEnv[fn]) {
        console.log("Local definition found!", fn, localEnv[fn]);
        result = localEnv[fn];
      } else if (globalEnv[fn]) {
        let processedArgs;
        if (fn === "later") {
          processedArgs = args; // No need to process these until
          //                       the function is run.
        } else {
          processedArgs = args.map((arg) =>
            Array.isArray(arg) ||
            (typeof arg === "string" && !/^".*"$/.test(arg))
              ? evaluate([arg], api)
              : arg,
          );
        }

        // Prepare arguments, evaluate if they are also functions or strings.
        result = globalEnv[fn](api, processedArgs);
      } else if (globalDef[fn]) {
        // Check if the value needs recursive evaluation.
        console.log("📖 Definition call:", fn, args, globalDef[fn]);
        result =
          Array.isArray(globalDef[fn]) || globalDef[fn].body
            ? evaluate(globalDef[fn], api, args)
            : globalDef[fn];
      }
    } else {
      if (localEnv[item]) {
        console.log("Solo local definition found!", item, localEnv[item]);
        // TODO: This needs to be evaluated?
        result = resolve(localEnv[item], api);
      } else if (globalEnv[item]) {
        // Assume a function on the globalEnv.
        result = globalEnv[item](api);
      } else if (globalDef[item]) {
        // Check if the value needs recursive evaluation.
        console.log("Solo definition:", item, globalDef[item]);
        // ⚠️ No parameters would ever be passed here, but maybe
        //    default would have to come into play?
        result = resolve(globalDef[item], api);
      }
    }
  }
  return result;
}

function resolve(expression, api) {
  return Array.isArray(expression) ? evaluate(expression, api) : expression;
}

/*
function evaluate(parsed, api = {}) {
  if (!Array.isArray(parsed)) parsed = [parsed];

  let result;
  for (const item of parsed) {
    console.log(item);
    if (Array.isArray(item)) {
      // The first element indicates the function to call
      const [fn, ...args] = item;

      if (fn === "now") {
        args[0] = `"${args[0]}"`; // Pre-wrap the first arg as a string.
      }

      if (globalEnv[fn]) {
        let processedArgs;
        if (fn === "later") {
          processedArgs = args; // No need to process these until the function is run.
        } else {
          processedArgs = args.map((arg) =>
            Array.isArray(arg) ||
            (typeof arg === "string" && !/^".*"$/.test(arg))
              ? evaluate([arg], api)
              : arg,
          );
        }

        result = globalEnv[fn](api, processedArgs);
      } else if (globalDefinition[fn]) {
        result = Array.isArray(globalDefinition[item])
          ? evaluate(globalDefinition[item], api)
          : globalDefinition[item];
      }
    }
  }
  return result;
}
*/

function readFromTokens(tokens) {
  const result = [];
  while (tokens.length > 0) {
    if (tokens[0] === ")") {
      throw new SyntaxError("Unexpected ')'");
    }
    result.push(readExpression(tokens));
  }
  console.log("Result:", result);
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
function processArgStringTypes(args) {
  return args.map((arg) => {
    // Remove the first and last character which are the quotes
    if (typeof arg === "string") return unquoteString(arg);
    return arg;
  });
}

function unquoteString(str) {
  if (str.startsWith('"') && str.endsWith('"')) {
    return str.substring(1, str.length - 1);
  } else return str;
}

export { module, parse, evaluate };
