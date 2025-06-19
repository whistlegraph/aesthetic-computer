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
  - [x] Refactored for multiple environment support - 2025.6.17
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

// 🎰 Parser & Evaluation utility functions (stateless)
const identifierRegex = /[a-zA-Z_]\w*/g;
const validIdentifierRegex = /^[a-zA-Z_]\w*$/;

function tokenize(input) {
  if (VERBOSE) console.log("🪙 Tokenizing:", input);
  
  const regex = /\s*(;.*|[()]|"(?:[^"\\]|\\.)*"|[^\s()";]+)/g;
  const tokens = [];
  let match;
  while ((match = regex.exec(input)) !== null) {
    const token = match[1];
    if (!token.startsWith(";")) {
      // Filter out comments
      tokens.push(token);
    }
  }
  
  // Auto-close incomplete expressions by counting parentheses balance
  let parenBalance = 0;
  for (const token of tokens) {
    if (token === "(") {
      parenBalance++;
    } else if (token === ")") {
      parenBalance--;
    }
  }
  
  // Add closing parentheses for any unclosed expressions
  while (parenBalance > 0) {
    tokens.push(")");
    parenBalance--;
  }
  
  if (VERBOSE) console.log("🪙 Tokens:", tokens);
  return tokens;
}

function readFromTokens(tokens) {
  const result = [];
  while (tokens.length > 0) {
    if (tokens[0] === ")") {
      throw new Error("Unexpected ')'");
    }
    result.push(readExpression(tokens));
  }
  return result;
}

function readExpression(tokens) {
  if (tokens.length === 0) {
    throw new Error("Unexpected end of input");
  }
  let token = tokens.shift();
  if (token === "(") {
    const list = [];
    while (tokens.length > 0 && tokens[0] !== ")") {
      list.push(readExpression(tokens));
    }
    if (tokens.length === 0) {
      // Auto-closing should have handled this, but just in case
      console.warn("🔧 Auto-closing: Missing ')' was handled by tokenizer");
      return list;
    }
    tokens.shift(); // Remove the ')'
    return list;
  } else {
    return atom(token);
  }
}

function atom(token) {
  if (token[0] === '"' && token[token.length - 1] === '"') {
    return token; // Return string with quotes intact for later processing
  } else {
    const num = parseFloat(token);
    return isNaN(num) ? token : num;
  }
}

function processArgStringTypes(args) {
  if (!Array.isArray(args)) {
    return args?.toString();
  }
  return args.map((arg) => {
    if (typeof arg === "string" && arg.startsWith('"') && arg.endsWith('"')) {
      return arg.slice(1, -1); // Remove quotes
    }
    return arg;
  });
}

function unquoteString(str) {
  if (str.startsWith('"') && str.endsWith('"')) {
    return str.slice(1, -1);
  } else {
    return str;
  }
}

function existing(item) {
  return item !== undefined && item !== null;
}

function getNestedValue(obj, path) {
  return path?.split(".").reduce((acc, part) => acc && acc[part], obj);
}

// KidLisp Environment Class
class KidLisp {
  constructor() {
    this.ast = null; // Abstract syntax tree.
    this.networkCache = { sources: {} };
    this.globalDef = {};
    this.localEnvStore = [{}];
    this.localEnv = this.localEnvStore[0];
    this.localEnvLevel = 0;
    this.tapper = null;
    this.drawer = null;
  }

  // Parse and evaluate a lisp source module
  // into a running aesthetic computer piece.
  module(source) {
    const parsed = this.parse(source);
    this.ast = JSON.parse(JSON.stringify(parsed)); // Deep copy of original source. 🙃
    /*if (VERBOSE)*/ // console.log("🐍 Snake:", parsed);

    // 🧩 Piece API
    return {
      boot: ({ params }) => {
        this.globalDef.paramA = params[0];
        this.globalDef.paramB = params[1];
        this.globalDef.paramC = params[2];
      },
      paint: ($) => {
        // console.log("🖌️ Painting");
        try {
          // TODO: Eventually compose programs together by stringing their
          //       output in a unix pipe-like fashion?

          this.localEnvLevel = 0; // Reset state per program evaluation.
          this.localEnv = this.localEnvStore[this.localEnvLevel];
          /*const evaluated = */ this.evaluate(this.ast, $);
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
          this.tap(api);
        }
        if (e.is("draw")) {
          api.needsPaint();
          this.draw(api, { dy: e.delta.y, dx: e.delta.x });
        }
      },
    };
  }

  // Main parsing method - handles both single expressions and multi-line input
  parse(input) {
    // Handle multi-line kidlisp input by auto-wrapping function calls
    if (input.includes('\n')) {
      const lines = input.split('\n')
        .map(line => line.trim())
        .filter(line => line.length > 0 && !line.startsWith(';'));
      
      const wrappedLines = lines.map(line => {
        // If line doesn't start with ( and looks like a function call, wrap it
        if (!line.startsWith('(') && /^[a-zA-Z_]\w*/.test(line)) {
          return `(${line})`;
        }
        return line;
      });
      
      // Join lines and parse as single input
      input = wrappedLines.join(' ');
    }

    const tokens = tokenize(input);
    const parsed = readFromTokens(tokens);
    return parsed;
  }

  // 🫵 Tap
  tap(api) {
    if (this.tapper) this.evaluate(this.tapper, api);
  }

  // ✏️ Draw
  draw(api, env) {
    if (this.drawer) {
      this.evaluate(this.drawer, api, env);
    }
  }

  // Create global environment
  getGlobalEnv() {
    return {
      // once: (api, args) => {
      //   console.log("Oncing...", args);
      //   if (this.drawer) this.evaluate(this.drawer, api);
      // },
      now: (api, args) => {
        if (args.length === 2) {
          const name = unquoteString(args[0]);
          if (this.globalDef.hasOwnProperty(name)) {
            this.globalDef[name] = args[1];
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

          if (!this.globalDef.hasOwnProperty(name)) {
            this.globalDef[name] = args[1];
          } else {
            // console.warn("🧠 Already defined:", name);
            // console.warn("🧠 Re-declaring:", name, args[1]);
            // this.globalDef[name] = args[1];
          }
          return args[1];
        }
        console.error("❗ Invalid `def`. Wrong number of arguments.");
      },
      die: (api, args) => {
        const name = unquoteString(args[0]);
        const def = this.globalDef[name];
        if (def) {
          delete this.globalDef[name];
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

          this.globalDef[name] = { body, params }; // Assign the array to the global definitions under the name.

          if (VERBOSE) {
            console.log(
              `Latered '${name}' as:`,
              this.globalDef[name],
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
          if (!this.networkCache.handles) {
            this.networkCache.handles = "loading";
            fetch("/api/handles")
              .then((response) => response.json())
              .then((data) => {
                // console.log("👱 Handles:", data);
                this.networkCache.handles = data.handles;
                iter.data = data.handles;
                api.needsPaint(); // 🖌️ Always require a paint after any network fetch.
              })
              .catch((error) => console.warn(error));
          } else if (Array.isArray(this.networkCache.handles)) {
            iter.data = this.networkCache.handles;
          }
          return iter;
        },
      },
      tap: (api, args) => {
        this.tapper = args;
      },
      draw: (api, args) => {
        this.drawer = args;
      },
      if: (api, args, env) => {
        const evaled = this.evaluate(args[0], api, env);
        const signal = evaled ? "🟢" : "🔴";
        // console.log(`${signal} If:`, args[0], "evaluated as:", evaled, "with env:", env);
        if (evaled) this.evaluate(args.slice(1), api, env);
        // this.evaluate([evaled ? args[1] : args[2]], api, env);
      },
      not: (api, args, env) => {
        const evaled = this.evaluate(args[0], api, env);
        const signal = evaled ? "🟢" : "🔴";
        // console.log(`${signal} Not:`, args[0], "evaluated as:", evaled, "with env:", env);
        // console.log(args[2]);
        if (!evaled) this.evaluate(args.slice(1), api, env);
        // this.evaluate([!evaled ? args[1] : args[2]], api, env);
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
        const left = this.evaluate(args[0], api, env),
          right = this.evaluate(args[1], api, env);
        if (left > right) {
          // console.log("✅", left, "is > than", right, args.slice(2));
          return this.evaluate(args.slice(1), api, env);
        } else {
          return false;
        }
      },
      "<": (api, args, env) => {
        const left = this.evaluate(args[0], api, env),
          right = this.evaluate(args[1], api, env);
        if (left < right) {
          // console.log("✅", left, "is < than", right, args.slice(2));
          return this.evaluate(args.slice(2), api, env);
        } else {
          return false;
        }
      },
      "=": (api, args, env) => {
        const left = this.evaluate(args[0], api, env),
          right = this.evaluate(args[1], api, env);
        if (left === right) {
          // console.log("✅", left, "is equal to", right, args.slice(2));
          return this.evaluate(args.slice(2), api, env);
        } else {
          return false;
        }
      },
      // ➗ Mathematical Operators
      max: (api, args) => {
        const nums = args.map(arg => parseFloat(unquoteString(arg))).filter(n => !isNaN(n));
        return nums.length > 0 ? Math.max(...nums) : 0;
      },
      "+": (api, args) => {
        const nums = args.map(arg => {
          if (arg === undefined || arg === null) return NaN;
          return typeof arg === 'number' ? arg : parseFloat(unquoteString(arg));
        }).filter(n => !isNaN(n));
        return nums.reduce((a, b) => a + b, 0);
      },
      "-": (api, args) => {
        const nums = args.map(arg => {
          if (arg === undefined || arg === null) return NaN;
          return typeof arg === 'number' ? arg : parseFloat(unquoteString(arg));
        }).filter(n => !isNaN(n));
        return nums.length > 0 ? nums.reduce((a, b) => a - b) : 0;
      },
      "*": (api, args) => {
        const nums = args.map(arg => {
          if (arg === undefined || arg === null) return NaN;
          return typeof arg === 'number' ? arg : parseFloat(unquoteString(arg));
        }).filter(n => !isNaN(n));
        return nums.reduce((a, b) => a * b, 1);
      },
      "/": (api, args) => {
        const nums = args.map(arg => {
          if (arg === undefined || arg === null) return NaN;
          return typeof arg === 'number' ? arg : parseFloat(unquoteString(arg));
        }).filter(n => !isNaN(n));
        return nums.length > 0 ? nums.reduce((a, b) => a / b) : 0;
      },
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
        // console.log("✍️ Source:", this.ast, args, env, colon);
        let sourcedAST = [];
        if (colon) {
          if (!this.networkCache.sources[colon]) {
            this.networkCache.sources[colon] = "loading";
            fetch(`/aesthetic.computer/disks/${colon}.lisp`)
              .then((response) => response.text())
              .then((code) => {
                this.networkCache.sources[colon] = this.parse(code);
                api.needsPaint(); // 🖌️ Always require a paint after any network fetch.
              })
              .catch((error) => console.warn(error));
          } else {
            sourcedAST = this.networkCache.sources[colon];
          }
        } else {
          sourcedAST = this.ast;
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
  }

  evaluate(parsed, api = {}, env, inArgs) {
    if (VERBOSE) console.log("➗ Evaluating:", parsed);
    let body;

    // Get global environment for this instance
    const globalEnv = this.getGlobalEnv();

    // Create local environment for a function that temporarily keeps the params.
    if (parsed.body) {
      const newLocalEnv = {};

      body = parsed.body;

      parsed.params.forEach(
        (param, i) => {
          console.log("😉 Binding param:", param, "to value:", inArgs?.[i], "at index:", i);
          // inArgs are already evaluated arguments, so just bind them directly
          if (i < inArgs.length) {
            newLocalEnv[param] = inArgs[i];
          } else {
            console.warn(`Parameter ${param} at index ${i} has no corresponding argument`);
            newLocalEnv[param] = undefined;
          }
        },
      );

      // Set up the new local environment
      this.localEnvLevel += 1;
      if (!this.localEnvStore[this.localEnvLevel]) {
        this.localEnvStore[this.localEnvLevel] = {};
      }
      // Copy existing environment and add new bindings
      this.localEnvStore[this.localEnvLevel] = { ...this.localEnv, ...newLocalEnv };
      this.localEnv = this.localEnvStore[this.localEnvLevel];
      
      console.log("🟠 Local env level:", this.localEnvLevel, "Environment:", this.localEnv);

      if (VERBOSE) console.log("Running:", body, "with environment:", this.localEnv);
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
        if (!existing(head)) return this.evalNotFound(head);

        if (Array.isArray(head)) {
          const evaledHead = this.evaluate([head], api, env);
          const newEval = [evaledHead, ...args];
          result = this.evaluate([newEval], api, env);
          continue;
        }

        if (typeof head !== "string" && head?.iterable) {
          result = this.iterate(head, api, args, env);
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

        if (existing(this.localEnv[head])) {
          if (VERBOSE)
            console.log("😫 Local definition found!", head, this.localEnv[head]);

          if (this.localEnv[head].iterable)
            result = this.iterate(this.localEnv[head], api, args, env);

          result = this.localEnv[head];
        } else if (existing(env?.[head])) {
          // console.log("FOUND HEAD:", head, env);
          if (env[head].iterable) {
            result = this.iterate(env[head], api, args, env);
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
                  ? this.evaluate([arg], api, env)
                  : arg,
              );
            }
            // Prepare arguments, evaluate if they are also functions or strings.
            if (splitHead[1]) {
              result = getNestedValue(globalEnv, item[0])(api, processedArgs);
            } else {
              result = globalEnv[head](api, processedArgs, env, colon);
              if (result?.iterable) {
                result = this.iterate(result, api, args, env);
                continue;
              }
            }
          } else {
            result = globalEnv[head];
          }
        } else if (existing(this.globalDef[head])) {
          // Check if the value needs recursive evaluation.
          
          // Evaluate arguments for user-defined functions
          const evaluatedArgs = args.map((arg) =>
            Array.isArray(arg) ||
            (typeof arg === "string" && !/^".*"$/.test(arg))
              ? this.evaluate([arg], api, env)
              : arg,
          );
          
          result =
            Array.isArray(this.globalDef[head]) || this.globalDef[head].body
              ? this.evaluate(this.globalDef[head], api, env, evaluatedArgs)
              : this.globalDef[head];
        } else {
          console.log("⛔ No match found for:", head, "localEnv:", this.localEnv);
          if (Array.isArray(head)) {
            if (VERBOSE) console.log("Environment:", this.localEnv);
            result = this.evaluate(head, api, this.localEnv);
          } else {
            result = this.evalNotFound(head, api, this.localEnv);
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
        } else if (existing(this.localEnv[item])) {
          result = this.evaluate(this.localEnv[item], api, env);
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
        } else if (existing(this.globalDef[root])) {
          if (!tail) {
            result = this.evaluate(this.globalDef[root], api, env);
          } else {
            // Assume a function with dot.syntax.
            const end = this.globalDef[root][tail];
            result = typeof end === "function" ? end() : end;
          }
        } else {
          /*if (VERBOSE)*/ // console.log(item, "⛔ Not found");
          if (Array.isArray(item)) {
            result = this.evaluate(item, api, env);
          } else {
            result = this.evalNotFound(item, api, env);
          }
        }
      }
    }
    
    // Restore previous local environment if we were in a function
    if (parsed.body) {
      this.localEnvLevel -= 1;
      this.localEnv = this.localEnvStore[this.localEnvLevel] || {};
      console.log("🔙 Restored env level:", this.localEnvLevel, "Environment:", this.localEnv);
    }
    
    return result;
  }

  evalNotFound(expression, api, env) {
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

    // Get global environment for this instance
    const globalEnv = this.getGlobalEnv();

    // Evaluate identifiers by running this.evaluate([id], api, env);
    identifiers.forEach((id) => {
      // console.log("🗼 Identifier:", id, env, this.localEnv, this.globalDef, globalEnv);
      let value;

      if (
        !existing(env?.[id]) &&      !existing(this.localEnv[id]) &&
        !existing(this.globalDef[id]) &&
        !existing(globalEnv[id])
      ) {
        console.warn("❗ Identifier not found:", id);
      } else {
        value =
          env?.[id] ||
          this.localEnv[id] ||
          this.globalDef[id] ||
          globalEnv[id] ||
          0;
      }

      // Replace any identifiers and cancel out prefixed double negatives.
      expression = expression.replace(
        new RegExp(`\\b${id}\\b`, "g"),
        value,
      );
    });

    const compute = new Function(`return ${expression};`);
    const result = compute();
    // console.log("Evaluated result:", result);
    return result;
  }

  // Loop over an iterable.
  iterate(iterable, api, args, outerEnv) {
    // console.log("Iterable:", iterable);
    iterable.data.forEach((item, index) => {
      const env = { ...outerEnv };
      // Make the list item available in the local environment.
      env.index = index;
      env.item = item;
      if (Array.isArray(item)) item = { iterable: true, data: item.slice() };
      env.data = item;
      // console.log("🔢 Index:", index, "Item:", item, "Env:", env);
      // Execute any provided function argument on each item.
      args.forEach((arg) => this.evaluate([arg], api, env));
    });
    return iterable;
  }
}

// Module function that creates and returns a new KidLisp instance
function module(source) {
  const lisp = new KidLisp();
  return lisp.module(source);
}

// Standalone parse function (for compatibility)
function parse(program) {
  const lisp = new KidLisp();
  return lisp.parse(program);
}

// Standalone evaluate function (for compatibility with tests, and an empty api)
function evaluate(parsed, api = {}) {
  const lisp = new KidLisp();
  return lisp.evaluate(parsed, api);
}

// URL encoding/decoding utilities for kidlisp pieces
function isKidlispSource(text) {
  if (!text) return false;
  
  // Traditional kidlisp indicators
  if (text.startsWith("(") || text.startsWith(";")) {
    return true;
  }
  
  // Check for encoded kidlisp (contains § or _ suggesting it was URL encoded)
  if (text.includes('§')) {
    const decoded = text.replace(/_/g, " ").replace(/§/g, "\n");
    // Check decoded version without recursion
    if (decoded.startsWith("(") || decoded.startsWith(";")) {
      return true;
    }
    if (decoded.includes('\n')) {
      const lines = decoded.split('\n');
      const hasKidlispLines = lines.some(line => {
        const trimmed = line.trim();
        return trimmed && (trimmed.startsWith('(') || /^[a-zA-Z_]\w*(\s|$)/.test(trimmed));
      });
      return hasKidlispLines;
    }
  }
  
  if (text.includes('_') && text.match(/[a-zA-Z_]\w*_[a-zA-Z]/)) {
    const decoded = text.replace(/_/g, " ").replace(/§/g, "\n");
    // Check decoded version without recursion
    if (decoded.startsWith("(") || decoded.startsWith(";")) {
      return true;
    }
    if (decoded.includes('\n')) {
      const lines = decoded.split('\n');
      const hasKidlispLines = lines.some(line => {
        const trimmed = line.trim();
        return trimmed && (trimmed.startsWith('(') || /^[a-zA-Z_]\w*(\s|$)/.test(trimmed));
      });
      return hasKidlispLines;
    }
    // Check if decoded looks like kidlisp function calls
    const trimmed = decoded.trim();
    if (/^[a-zA-Z_]\w*(\s|$)/.test(trimmed)) {
      return true;
    }
  }
  
  // Check if it contains newlines and looks like kidlisp (has function calls)
  if (text.includes('\n')) {
    const lines = text.split('\n');
    // If any line looks like a function call, treat as kidlisp
    const hasKidlispLines = lines.some(line => {
      const trimmed = line.trim();
      return trimmed && (trimmed.startsWith('(') || /^[a-zA-Z_]\w*(\s|$)/.test(trimmed));
    });
    return hasKidlispLines;
  }
  
  return false;
}

function encodeKidlispForUrl(source) {
  const isKidlisp = isKidlispSource(source);
  
  if (!isKidlisp) {
    return source;
  }
  
  const encoded = source.replace(/ /g, "_").replace(/\n/g, "§");
  return encoded;
}

function decodeKidlispFromUrl(encoded) {
  // Only decode if the result would be valid kidlisp
  const decoded = encoded.replace(/_/g, " ").replace(/§/g, "\n");
  return isKidlispSource(decoded) ? decoded : encoded;
}

// Check if the prompt is currently in kidlisp mode based on the input text
function isPromptInKidlispMode(promptText) {
  if (!promptText || typeof promptText !== 'string') return false;
  const trimmed = promptText.trim();
  // Check for traditional kidlisp indicators
  if (isKidlispSource(trimmed)) return true;
  // Check for newlines which also trigger kidlisp mode
  if (promptText.includes('\n')) return true;
  return false;
}

// Add result logging to see if detection is working
function logKidlispDetection(source) {
  const result = source.includes('\n') || source.includes('wipe') || source.includes('line');
  console.log("🔍 MANUAL DETECTION for:", JSON.stringify(source), "result:", result);
  return result;
}

export { module, parse, evaluate, KidLisp, isKidlispSource, encodeKidlispForUrl, decodeKidlispFromUrl, isPromptInKidlispMode };
