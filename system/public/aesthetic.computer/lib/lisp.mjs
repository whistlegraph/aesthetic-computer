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
  + Done
  - [x] Have tests run automatically in some window.
  - [x] Set up this module with some actual JavaScript testing framework
       with expectations so I can make sure it passes.
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
  // console.log("🐍 Parsed:", parsed);

  return {
    boot: ({ params }) => {
      globalDef.paramA = params[0];
      globalDef.paramB = params[1];
      globalDef.paramC = params[2];
    },
    paint: ($) => {
      // console.log("🖌️ Painting");
      const evaluated = evaluate(parsed, $);
      // Print the program output value in the center of the screen.
      // ink("white").write(evaluated || "nada", { center: "xy" });
      // return false;
    },
    act: ({ event: e, api }) => {
      if (e.is("touch")) tap(api);
      if (e.is("draw")) {
        console.log(e);
        draw(api);
      } 
    },
  };
}

// 🎆 Top-level events.

// 🫵 Tap
let tapper;
function tap(api) {
  // console.log("Tapping!");
  if (tapper) evaluate(tapper, api);
}

// ✏️ Draw
let drawer;
function draw(api) {
  if (drawer) evaluate(drawer, api);
}

// 💻 System

const globalDef = {};

const globalEnv = {
  // Program Architecture
  now: (api, args) => {
    // Three length argument assumes a value setting.
    // console.log("Nowing:", args);
    if (args.length === 2) {
      const name = unquoteString(args[0]);
      // Validate the identifier.
      if (!validIdentifierRegex.test(name)) {
        console.error("️❗ Invalid identifier name:", name);
        return;
      }

      globalDef[name] = args[1];
      return args[1];
    }
    console.error("❗ Invalid `now`. Wrong number of arguments.");
  },
  die: (api, args) => {
    const name = unquoteString(args[0]);
    const def = globalDef[name];
    if (def) {
      delete globalDef[name];
      def.kill?.(); // Kill a sound or the object has a destructor.
    }
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
  tap: (api, args) => {
    tapper = args;
  },
  draw: (api, args) => {
    drawer = args;
  },
  if: (api, args) => {
    // console.log("If:", args);
    const cond = args[0];
    // TODO: Still need to check for nada / nullish / falsey values.
    if (globalDef[cond]) {
      evaluate([args[1]], api);
    } else {
      evaluate([args[2]], api);
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
  write: (api, args = []) => {
    api.write(processArgStringTypes(args[0]), { x: args[1], y: args[2] });
  },
  // (Getters / globals).
  width: (api) => {
    return api.screen.width;
  },
  height: (api) => {
    return api.screen.height;
  },
  // 🔈 Sound
  overtone: (api, args = []) => {
    // console.log("Synth at:", args);
    let tone;
    if (args[0] === undefined) {
      tone = 440;
    } else {
      tone = (args[0] * args[1]) / args[2];
    }
    return api.sound.synth({
      type: "square",
      tone,
      // beats: 0.1,
      duration: Infinity,
      attack: 0.01,
      decay: 0.5,
      volume: 0.15,
    });
  },
};

// 🎰 Parser & Evaluation

const identifierRegex = /[a-zA-Z_]\w*/g;
const validIdentifierRegex = /^[a-zA-Z_]\w*$/;
const localEnv = {};

function tokenize(input) {
  // Remove comments from input (anything from ';' to the end of the line)
  input = input.replace(/;.*$/gm, "");
  // Replace parentheses with spaces around them and split into an array by whitespace
  return input.replace(/\(/g, " ( ").replace(/\)/g, " ) ").trim().split(/\s+/);
}

function parse(program) {
  return readFromTokens(tokenize(program));
}

function evaluate(parsed, api = {}, env) {
  // console.log("➗ Evaluating:", parsed);
  let body;

  // Create local environment for a function that temporarily keeps the params.
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
    // Or just evaluate with the global environment, ensuring it's an array.
    body = Array.isArray(parsed) ? parsed : [parsed];
    // console.log("Body:", body);
  }

  let result;
  for (const item of body) {
    // console.log("🥡 Item:", item);

    if (Array.isArray(item)) {
      // The first element indicates the function to call
      const [head, ...args] = item;
      // Check if the function requires recursive evaluation
      if (head === "now" || head === "die") args[0] = `"${args[0]}"`; // Pre-wrap the first arg as a string.

      if (existing(localEnv[head])) {
        console.log("Local definition found!", head, localEnv[head]);
        result = localEnv[head];
      } else if (existing(globalEnv[head])) {
        if (typeof globalEnv[head] === "function") {
          let processedArgs;
          if (
            head === "later" ||
            head === "tap" ||
            head === "draw" ||
            head === "if" ||
            head === "wipe" ||
            head === "ink" // ||
            // head === "write"
          ) {
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
          result = globalEnv[head](api, processedArgs);
        } else {
          result = globalEnv[head];
        }
      } else if (existing(globalDef[head])) {
        // Check if the value needs recursive evaluation.
        console.log("📖 Definition call:", head, args, globalDef[head]);
        result =
          Array.isArray(globalDef[head]) || globalDef[head].body
            ? evaluate(globalDef[head], api, args)
            : globalDef[head];
      } else {
        // console.log("⚠️ No match found for:", head);
        result = evalNotFound(head);
      }
    } else {
      const [root, tail] = item.split(".");

      if (existing(localEnv[item])) {
        console.log("Solo local definition found!", item, localEnv[item]);
        // TODO: This needs to be evaluated?
        result = resolve(localEnv[item], api);
      } else if (existing(globalEnv[item])) {
        // console.log("Assume a function on the globalEnv:", item);
        // console.log(typeof globalEnv[item]);
        // Assume a function on the globalEnv.
        if (typeof globalEnv[item] === "function") {
          result = globalEnv[item](api);
          // console.log("Result is:", result);
        } else {
          result = globalEnv[item];
        }
      } else if (existing(globalDef[root])) {
        // Check if the value needs recursive evaluation.
        // console.log("Solo definition:", item, globalDef[root]);
        // ⚠️ No parameters would ever be passed here, but maybe
        //    default would have to come into play?

        if (!tail) {
          result = resolve(globalDef[root], api);
        } else {
          // Assume a function with dot syntax.
          const end = globalDef[root][tail];
          result = typeof end === "function" ? end() : end;
        }
      } else {
        // console.log(item, "⛔ Not found");
        result = evalNotFound(item, api, env);
      }
    }
  }
  return result;
}

function evalNotFound(expression, api, env) {
  // console.log("🤖 Attempting JavaScript expression evaluation:", expression);

  // 📖 Identifiers can only start with a letter a-z or A-Z and can not
  // include mathermatical operators but can include underscores or
  // digits after the first character

  // Parse the expression to extract identifiers.
  const identifiers = expression.match(identifierRegex) || [];

  // Evaluate identifiers by running evaluate([id], api, env);
  identifiers.forEach((id) => {
    const value = evaluate(id, api, env);
    expression = expression.replace(new RegExp(`\\b${id}\\b`, "g"), value);
  });

  const compute = new Function(`return ${expression};`);
  const result = compute();
  // console.log("Evaluated result:", result);
  return result;
}

function resolve(expression, api) {
  return Array.isArray(expression) ? evaluate(expression, api) : expression;
}

function readFromTokens(tokens) {
  const result = [];
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

function processArgStringTypes(args) {
  if (!Array.isArray(args)) {
    return unquoteString(args);
  }
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

function existing(item) {
  return item !== undefined && item !== null;
}

export { module, parse, evaluate };
