// Lisp, 24.4.17.12.03
// A lisp interpreter / compiler for writing Aesthetic Computer pieces.

// Working expressions:
// (+ 4 5 5)
// (* (+ 2 3) (- 4 1))

// Parse and evaluate a lisp source module
// into a running aesthetic computer piece.
function module(source) {
  const parsed = parse(source);
  console.log("🐍 Parsed:", parsed);
  const evaluated = evaluate(parsed);
  console.log("🐍 Evaluated:", evaluated);
  return {
    paint: ({ wipe }) => {
      wipe("blue").ink("white").write(evaluated, { center: "xy " });
    },
  };
}

function tokenize(input) {
  let tokens = [];
  let currentToken = "";
  let inNumber = false; // Flag to track if we're currently reading a number

  for (let char of input) {
    if (char === "(" || char === ")") {
      // If the character is a parenthesis, add the current token and the parenthesis to tokens
      if (currentToken) {
        tokens.push(currentToken);
        currentToken = "";
      }
      tokens.push(char);
      inNumber = false; // Reset number flag
    } else if (/\s/.test(char)) {
      // If the character is whitespace, add the current token to tokens
      if (currentToken) {
        tokens.push(currentToken);
        currentToken = "";
      }
      inNumber = false; // Reset number flag
    } else if (/\d/.test(char) || (char === "." && inNumber)) {
      // If the character is a digit or a decimal point in a number, add to currentToken
      currentToken += char;
      inNumber = true; // Set number flag
    } else {
      // Otherwise, handle it as part of a symbol or operator
      if (inNumber) {
        tokens.push(currentToken);
        currentToken = "";
      }
      currentToken += char;
      inNumber = false; // Reset number flag
    }
  }
  if (currentToken) {
    tokens.push(currentToken);
  }
  return tokens;
}

function readFromTokens(tokens) {
  if (tokens.length === 0) {
    throw new SyntaxError("Unexpected EOF while reading");
  }
  let token = tokens.shift();
  if (token === "(") {
    let L = [];
    while (tokens[0] !== ")") {
      L.push(readFromTokens(tokens));
    }
    tokens.shift(); // remove ')'
    return L;
  } else if (token === ")") {
    throw new SyntaxError("Unexpected )");
  } else {
    return atom(token);
  }
}

function atom(token) {
  if (isNumeric(token)) {
    return parseFloat(token);
  } else {
    return token; // Return the token as a string if it's not numeric
  }
}

function isNumeric(str) {
  return !isNaN(str) && !isNaN(parseFloat(str));
}

function parse(program) {
  return readFromTokens(tokenize(program));
}

const globalEnv = {
  "+": (args) => args.reduce((a, b) => a + b, 0),
  "-": (args) => args.reduce((a, b) => a - b),
  "*": (args) => args.reduce((a, b) => a * b, 1),
  "/": (args) => args.reduce((a, b) => a / b),
};

function evaluate(expr, env = globalEnv) {
  if (typeof expr === "number") {
    return expr;
  } else if (typeof expr === "string") {
    return env[expr];
  } else {
    const [operator, ...args] = expr;
    const func = env[operator];
    const evaledArgs = args.map((arg) => evaluate(arg, env));
    return func(evaledArgs);
  }
}

export { module, parse, evaluate };
