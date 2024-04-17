// Lisp, 24.4.17.12.03
// A lisp interpreter / compiler for writing Aesthetic Computer pieces.

// Working expressions:
// (+ 4 5 5)
// (* (+ 2 3) (- 4 1))

// Define the environment with some basic arithmetic functions
const global = {
  '+': (args) => args.reduce((a, b) => a + b, 0),
  '-': (args) => args.reduce((a, b) => a - b),
  '*': (args) => args.reduce((a, b) => a * b, 1),
  '/': (args) => args.reduce((a, b) => a / b),
};

// Parse S-expression into a nested array AST.
function parse(expr) {
  return JSON.parse(expr.replace(/\(/g, '[').replace(/\)/g, ']'));
}

// Evaluate the parsed expression
function evaluate(expr, environment = global) {
  if (typeof expr === 'number') {
    // If it's a number, return it directly
    return expr;
  } else if (typeof expr === 'string') {
    // If it's a variable (identifier), get its value from the environment
    return environment[expr];
  } else {
    // Otherwise, it's a list (function application)
    const [operator, ...args] = expr;
    const func = environment[operator];
    const evaledArgs = args.map(arg => evaluate(arg, environment));
    return apply(func, evaledArgs);
  }
}

// Apply the function to the evaluated arguments
function apply(func, args) { return func(args); }

export { parse, evaluate };