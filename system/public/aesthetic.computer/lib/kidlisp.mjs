// Kidlisp, 24.4.17.12.03
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

  ((line 20 80 80 80))
  (later cross x y
    (line x-10 y-10 x+10 y+10)
    (line x-10 y+10 x+10 y-10)
  )
  (ink yellow)
  (cross 16 32)
#endregion */

/* #region 🏁 TODO 
 - [🟢] Write the graphics and interaction module for the step debugger.
 - [] Resaving the kidlisp file should not reload the whole frontend stack.
 - [] Evaluate `hoot` lang / give it a try? https://spritely.institute/hoot
  *** kidlisp debugger ***
    - [] light up each line as it executes
    - [] be able to see the values visually connect
    - [] the debugger would eventually become the editor...
    - [] and it needs to be built into the running page
    - [] this starts by making a drawing / graph of the ast
    - [] and by lighting up each list / atom as it gets evaluated / lighting
 - [] Closing and opening the VS Code extension should remember the last piece
      loaded.
   - [] This should work by broadcasting the piece slug to the extension
        on each switch then using that for the page open url, but not storing
        it across vscode opened and closed
        sessions.
 - [] `def` and `later` should be the same keyword.
 - [] Should code reloads reset the state or could I dump
      the state on each reload?
  - [] Add a "loading..." display until the network request completes.
  - [] Add a button to each handle, similar to the `list` command.
  - [] How should assignment actually work?
  - [] And shorthand for incrementing a value.
  - [] Implement: ; (once (wipe black))
  - [] Is there a way to get live updates
      to be even faster while using the
      lisp?
    - [] Like being able to soft-reset the environment
        parse the code but nothing else.
  - [] Runtime Modes
    - [] Everything should be "alive" in a continuous loop
        and for optimization or one-time effects, elements
        can be striken from the AST like global definitions.
  - [] Could all `defs` be removed from the parsed AST once they run once?
  - [] Add a repeat like goto statement?
  - [] Or something to enable looping.
  - [] Add the ability to make sound.
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

const VERBOSE = false;
const { floor, max } = Math;
let ast; // Abstract syntax tree.


// Parse and evaluate a lisp source module
// into a running aesthetic computer piece.
function module(source) {
  const parsed = parse(source);
  ast = JSON.parse(JSON.stringify(parsed)); // Deep copy of original source. 🙃
  /*if (VERBOSE)*/ console.log("🐍 Snake:", parsed);

  // 🧩 Piece API
  return {
    boot: ({ params }) => {
      globalDef.paramA = params[0];
      globalDef.paramB = params[1];
      globalDef.paramC = params[2];
    },
    paint: ($) => {
      // console.log("🖌️ Painting");
      try {
        // TODO: Eventually compose programs together by stringing their
        //       output in a unix pipe-like fashion?

        localEnvLevel = 0; // Reset state per program evaluation.
        localEnv = localEnvStore[localEnvLevel];
        /*const evaluated = */ evaluate(parsed, $);
      } catch (err) {
        console.error("⛔ Evaluation failure:", err);
      }
      // Print the program output value in the center of the screen.
      // ink("white").write(evaluated || "nada", { center: "xy" });
      return false;
    },
    act: ({ event: e, api }) => {
      if (e.is("touch")) {
        api.needsPaint();
        tap(api);
      }
      if (e.is("draw")) {
        api.needsPaint();
        draw(api, { dy: e.delta.y, dx: e.delta.x });
      }
    },
  };
}

// 💻 Interpreter
const networkCache = { sources: {} };
const globalDef = {};

// 📑 API
const globalEnv = {
  // once: (api, args) => {
  //   console.log("Oncing...", args);
  //   if (drawer) evaluate(drawer, api);
  // },
  now: (api, args) => {
    if (args.length === 2) {
      const name = unquoteString(args[0]);
      if (globalDef.hasOwnProperty(name)) {
        globalDef[name] = args[1];
        // console.warn("🧠 Now:", name);
      } else {
        console.warn("🚫🧠 Not defined:", name);
      }
      return args[1];
    }
    console.error("❗ Invalid `def`. Wrong number of arguments.");
  },
  // Program Architecture
  def: (api, args) => {
    if (args.length === 2) {
      const name = unquoteString(args[0]);
      // Validate the identifier.
      if (!validIdentifierRegex.test(name)) {
        console.error("️❗ Invalid identifier name:", name);
        return;
      }

      if (!globalDef.hasOwnProperty(name)) {
        globalDef[name] = args[1];
      } else {
        // console.warn("🧠 Already defined:", name);
        // console.warn("🧠 Re-declaring:", name, args[1]);
        // globalDef[name] = args[1];
      }
      return args[1];
    }
    console.error("❗ Invalid `def`. Wrong number of arguments.");
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
          // body = [arg];
          body = args.slice(index + 1);
        } else if (body === null) {
          params.push(arg);
        }
      });

      if (body === null) {
        console.error("No body found in arguments for 'later' function.");
        return;
      }

      globalDef[name] = { body, params }; // Assign the array to the global definitions under the name.

      if (VERBOSE) {
        console.log(
          `Latered '${name}' as:`,
          globalDef[name],
          "with parameters:",
          params,
        );
      }

      return body;
    }
  },
  net: {
    handles: (api) => {
      const iter = { iterable: true, data: [] };
      if (!networkCache.handles) {
        networkCache.handles = "loading";
        fetch("/api/handles")
          .then((response) => response.json())
          .then((data) => {
            // console.log("👱 Handles:", data);
            networkCache.handles = data.handles;
            iter.data = data.handles;
            api.needsPaint(); // 🖌️ Always require a paint after any network fetch.
          })
          .catch((error) => console.warn(error));
      } else if (Array.isArray(networkCache.handles)) {
        iter.data = networkCache.handles;
      }
      return iter;
    },
  },
  tap: (api, args) => {
    tapper = args;
  },
  draw: (api, args) => {
    drawer = args;
  },
  if: (api, args, env) => {
    const evaled = evaluate(args[0], api, env);
    const signal = evaled ? "🟢" : "🔴";
    // console.log(`${signal} If:`, args[0], "evaluated as:", evaled, "with env:", env);
    if (evaled) evaluate(args.slice(1), api, env);
    // evaluate([evaled ? args[1] : args[2]], api, env);
  },
  not: (api, args, env) => {
    const evaled = evaluate(args[0], api, env);
    const signal = evaled ? "🟢" : "🔴";
    // console.log(`${signal} Not:`, args[0], "evaluated as:", evaled, "with env:", env);
    // console.log(args[2]);
    if (!evaled) evaluate(args.slice(1), api, env);
    // evaluate([!evaled ? args[1] : args[2]], api, env);
  },
  range: (api, args) => {
    // console.log("Range detected:", args);
    if (args.length === 3) {
      const array = args[0].data; // Assume `{iterable: true, data: Array}`.
      const startIndex = max(0, floor(args[1]));
      const endIndex = max(0, floor(args[2]));
      // console.log("Range:", array.length, startIndex, endIndex);

      if (
        Array.isArray(array) &&
        typeof startIndex === "number" &&
        typeof endIndex === "number"
      ) {
        return { iterable: true, data: array.slice(startIndex, endIndex) };
      } else {
        console.error(
          "❗ Invalid arguments for `range`. Expected an array and two numbers.",
        );
      }
    } else {
      console.error("❗ Invalid `range`. Wrong number of arguments.");
    }
  },
  // 🧠 Logical Operators
  ">": (api, args, env) => {
    const left = evaluate(args[0], api, env),
      right = evaluate(args[1], api, env);
    if (left > right) {
      // console.log("✅", left, "is > than", right, args.slice(2));
      return evaluate(args.slice(1), api, env);
    } else {
      //if (args[3]) {
      //  return evaluate(args[3], api, env);
      //} else {
      return false;
      //}
    }
  },
  "<": (api, args, env) => {
    const left = evaluate(args[0], api, env),
      right = evaluate(args[1], api, env);
    if (left < right) {
      // console.log("✅", left, "is < than", right, args.slice(2));
      return evaluate(args.slice(2), api, env);
    } else {
      return false;
    }
  },
  "=": (api, args, env) => {
    const left = evaluate(args[0], api, env),
      right = evaluate(args[1], api, env);
    if (left === right) {
      // console.log("✅", left, "is equal to", right, args.slice(2));
      return evaluate(args.slice(2), api, env);
    } else {
      return false;
    }
  },
  // ➗ Mathematical Operators
  max: (api, args) => max(...args),
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
    // console.log(args);
    api.box(...args);
  },
  write: (api, args = []) => {
    const content = processArgStringTypes(args[0]);
    // console.log("✏️ Write:", content, args);
    api.write(content, { x: args[1], y: args[2] });
  },
  len: (api, args = []) => {
    return args[0]?.toString().length;
  },
  // (Getters / globals).
  source: (api, args = [], env, colon) => {
    // console.log("✍️ Source:", ast, args, env, colon);
    let sourcedAST = [];
    if (colon) {
      if (!networkCache.sources[colon]) {
        fetch(`/aesthetic.computer/disks/${colon}.lisp`)
          .then((response) => response.text())
          .then((data) => (networkCache.sources[colon] = parse(data)))
          .catch((err) => console.warn(`No source for ${colon}:`, err));
      } else {
        sourcedAST = networkCache.sources[colon];
      }
    } else {
      sourcedAST = ast;
    }
    return { iterable: true, data: sourcedAST };
  },
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

// 🎆 Events

// 🫵 Tap
let tapper;
function tap(api) {
  if (tapper) evaluate(tapper, api);
}

// ✏️ Draw
let drawer;
function draw(api, env) {
  if (drawer) {
    evaluate(drawer, api, env);
  }
}

// 🎰 Parser & Evaluation

const localEnvStore = [{}];
let localEnv = localEnvStore[0];
let localEnvLevel = 0;

const identifierRegex = /[a-zA-Z_]\w*/g;
const validIdentifierRegex = /^[a-zA-Z_]\w*$/;

function tokenize(input) {
  // Remove comments from input (anything from ';' to the end of the line)
  input = input.replace(/;.*$/gm, "");
  // Replace parentheses with spaces around them and split into an array by whitespace
  return input.replace(/\(/g, " ( ").replace(/\)/g, " ) ").trim().split(/\s+/);
}

function parse(program) {
  return readFromTokens(tokenize(program));
}

function evaluate(parsed, api = {}, env, inArgs) {
  if (VERBOSE) console.log("➗ Evaluating:", parsed);
  let body;

  // Create local environment for a function that temporarily keeps the params.
  if (parsed.body) {
    const newLocalEnv = {};

    body = parsed.body;

    parsed.params.forEach(
      // (param, i) => (localEnv[param] = evaluate(env[i], api, env)),
      (param, i) => {
        // console.log("😉 Param:", param, "Env:", env, "inArgs:", inArgs, "localEnv:", localEnv);
        newLocalEnv[param] = evaluate(env[inArgs[i]], api, env);
      },
    );

    // Make new localEnv level.
    if (localEnvLevel > 0) {
      localEnvStore[localEnvLevel] = newLocalEnv;
      localEnv = localEnvStore[localEnvLevel];
    }
    localEnvLevel += 1;
    // console.log("🟠 Local env level:", localEnvLevel, "Body:", parsed.body);

    if (VERBOSE) console.log("Running:", body, "with environment:", localEnv);
  } else {
    // Or just evaluate with the global environment, ensuring it's an array.
    body = Array.isArray(parsed) ? parsed : [parsed];
  }

  if (VERBOSE) console.log("🏃 Body:", body);

  let result;

  for (const item of body) {
    /*if (VERBOSE)*/ // console.log("🥡 Item:", item /*, "body:", body*/);

    if (Array.isArray(item)) {
      // The first element indicates the function to call
      let [head, ...args] = item;
      // const colon = head.split(":")[1]; // TODO: Take into account colon param / work it in.

      // Make sure head exists and re-evaluate or iterate if not a string.
      if (!existing(head)) return evalNotFound(head);

      if (Array.isArray(head)) {
        const evaledHead = evaluate([head], api, env);
        const newEval = [evaledHead, ...args];
        result = evaluate([newEval], api, env);
        continue;
      }

      if (typeof head !== "string" && head?.iterable) {
        result = iterate(head, api, args, env);
        continue;
      }

      // Parse the head string splitting on dots and colons.
      const splitHead = head.split(".");
      head = splitHead[0];

      const colonSplit = head.split(":");
      head = colonSplit[0];
      const colon = colonSplit[1]; // Will be incorporated in globalEnv api.

      // if (head === "box") {
      //  console.log("🧕 Head:", head);
      // }

      // Check if the function requires recursive evaluation
      if (head === "now" || head === "def" || head === "die")
        args[0] = `"${args[0]}"`; // Pre-wrap the first arg as a string.

      if (existing(localEnv[head])) {
        if (VERBOSE)
          console.log("😫 Local definition found!", head, localEnv[head]);

        if (localEnv[head].iterable)
          result = iterate(localEnv[head], api, args, env);

        result = localEnv[head];
      } else if (existing(env?.[head])) {
        // console.log("FOUND HEAD:", head, env);
        if (env[head].iterable) {
          result = iterate(env[head], api, args, env);
        }

        // 🟣 Handle calls to the Aesthetic Computer Piece API.
      } else if (existing(globalEnv[head])) {
        if (
          typeof globalEnv[head] === "function" ||
          typeof globalEnv[head] === "object"
        ) {
          let processedArgs;
          if (
            head === "later" ||
            head === "tap" ||
            head === "draw" ||
            head === "if" ||
            head === "not" ||
            head === "wipe" ||
            head === "ink" ||
            head === ">" ||
            head === "<" ||
            head === "=" ||
            head === "net" ||
            head === "source"
          ) {
            processedArgs = args;
          } else {
            processedArgs = args.map((arg) =>
              Array.isArray(arg) ||
              (typeof arg === "string" && !/^".*"$/.test(arg))
                ? evaluate([arg], api, env)
                : arg,
            );
          }
          // Prepare arguments, evaluate if they are also functions or strings.
          if (splitHead[1]) {
            result = getNestedValue(globalEnv, item[0])(api, processedArgs);
          } else {
            result = globalEnv[head](api, processedArgs, env, colon);
            if (result?.iterable) {
              result = iterate(result, api, args, env);
              continue;
            }
          }
        } else {
          result = globalEnv[head];
        }
      } else if (existing(globalDef[head])) {
        // Check if the value needs recursive evaluation.
        //if (VERBOSE)
        // console.log("🚀 LATER", head, "Args:", args, "Env:", env, "🐍 Snake:", globalDef[head]);
        result =
          Array.isArray(globalDef[head]) || globalDef[head].body
            ? evaluate(globalDef[head], api, env, args)
            : globalDef[head];
      } else {
        console.log("⛔ No match found for:", head, "localEnv:", localEnv);
        if (Array.isArray(head)) {
          if (VERBOSE) console.log("Environment:", localEnv);
          result = evaluate(head, api, localEnv);
        } else {
          result = evalNotFound(head, api, localEnv);
        }
      }
    } else {

      let root, tail;
      if (typeof item === "string") {
        // TODO: 🔵 First check to see if the string is a mathematical expression,
        //       and evaluate as javascript if it is.
        [root, tail] = item.split(".");
      }

      if (!Array.isArray(env) && existing(env?.[root])) {
        result = getNestedValue(env, item);
      } else if (existing(localEnv[item])) {
        result = evaluate(localEnv[item], api, env);
      } else if (existing(globalEnv[root])) {
        if (!tail) {
          result =
            typeof globalEnv[item] === "function"
              ? globalEnv[item](api)
              : (result = globalEnv[item]);
        } else {
          // Assume a function with dot syntax.
          const end = globalEnv[root][tail];
          // console.log(root, tail);
          result = typeof end === "function" ? end(api) : end;
        }
      } else if (existing(globalDef[root])) {
        if (!tail) {
          result = evaluate(globalDef[root], api, env);
        } else {
          // Assume a function with dot.syntax.
          const end = globalDef[root][tail];
          result = typeof end === "function" ? end() : end;
        }
      } else {
        /*if (VERBOSE)*/ // console.log(item, "⛔ Not found");
        if (Array.isArray(item)) {
          result = evaluate(item, api, env);
        } else {
          result = evalNotFound(item, api, env);
        }
      }
    }
  }
  return result;
}

function evalNotFound(expression, api, env) {
  if (typeof expression !== "string") {
    // console.log("🤖 Expression:", expression);
    return expression; // Return numbers.
  } else {
    // console.log("🤖 Attempting JavaScript expression evaluation:", expression);
  }

  // 📖 Identifiers can only start with a letter a-z or A-Z and cannot
  //    include mathematical operators but can include underscores or
  //    digits after the first character

  // Parse the expression to extract identifiers.
  const identifiers = expression.match(identifierRegex) || [];

  // Evaluate identifiers by running evaluate([id], api, env);
  identifiers.forEach((id) => {
    // console.log("🗼 Identifier:", id, env, localEnv, globalDef, globalEnv);
    let value;

    if (
      !existing(env?.[id]) && // What's the ultimate difference between all
      //                         These environments now? 24.05.27.02.27
      !existing(localEnv[id]) &&
      !existing(globalDef[id]) &&
      !existing(globalEnv[id])
    ) {
      value = 0; // Set nada keys to 0 because we are assuming
      //            an arithmetic expression.
    } else {
      // TODO: Technially maybe this should be th first thing to evaluate.
      // console.log("🚗 Evaluating:", expression, id, env);
      // TODO: ❤️‍🔥 It really shouldn't be going this far.
      value = existing(env?.[id]) ? env[id] : evaluate(id, api, env);
    }

    // Replace any identifiers and cancel out prefixed double negatives.
    expression = expression
      .replace(new RegExp(`\\b${id}\\b`, "g"), value)
      .replace(/(^|[^-])--/g, (match, p1) => p1 + "+")
      .replace(/^--/, "-");
  });

  const compute = new Function(`return ${expression};`);
  const result = compute();
  // console.log("Evaluated result:", result);
  return result;
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
    return typeof args === "string" ? unquoteString(args) : args;
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

function getNestedValue(obj, path) {
  // console.log("Nested value:", obj, path);
  return path?.split(".").reduce((acc, part) => acc && acc[part], obj);
}

// Loop over an iterable.
function iterate(iterable, api, args, outerEnv) {
  // console.log("Iterable:", iterable);
  iterable.data.forEach((item, index) => {
    const env = { ...outerEnv };
    // console.log("Iteration item:", JSON.stringify(item));
    if (Array.isArray(item)) item = { iterable: true, data: item.slice() };
    env[args[0]] = item;
    env[args[1]] = index;
    // TODO ^ Should I prevent overwrites if `env` already has
    //        these properties?
    // console.log("🟢 Evaluating:", item, args[0]);
    evaluate(args.slice(2), api, env);
  });
  return iterable;
}

export { module, parse, evaluate };
