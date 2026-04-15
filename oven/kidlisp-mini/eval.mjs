// KidLisp Mini Evaluator — RBP-26 Profile Only
// Tree-walking interpreter targeting the minimal feature set needed for $roz
// ~400 lines, produces ~8-10 KB minified

// ─── Tokenizer ───────────────────────────────────────────────────────

function tokenize(source) {
  const tokens = [];
  let i = 0;

  while (i < source.length) {
    const ch = source[i];

    // Whitespace
    if (/\s/.test(ch)) {
      i++;
      continue;
    }

    // Comment
    if (ch === ';') {
      while (i < source.length && source[i] !== '\n') i++;
      continue;
    }

    // String
    if (ch === '"') {
      i++;
      let str = '';
      while (i < source.length && source[i] !== '"') {
        if (source[i] === '\\' && i + 1 < source.length) {
          i++;
          str += source[i];
        } else {
          str += source[i];
        }
        i++;
      }
      i++; // closing "
      tokens.push({ type: 'string', value: str });
      continue;
    }

    // Parens
    if (ch === '(' || ch === ')') {
      tokens.push({ type: ch, value: ch });
      i++;
      continue;
    }

    // Atom (may include /, :, -, .)
    const atomStart = i;
    while (i < source.length && !/[\s()"]/.test(source[i])) {
      i++;
    }
    const atom = source.slice(atomStart, i);

    // Check for special prefixes: fade:, Ns..., Ns
    if (atom.startsWith('fade:')) {
      tokens.push({ type: 'fade', value: atom.slice(5) }); // "red-blue-black"
    } else if (/^\d+s\.\.\.$/.test(atom)) {
      const ms = parseInt(atom) * 1000;
      tokens.push({ type: 'timing-repeating', value: ms }); // "2s..." → 2000ms
    } else if (/^\d+s$/.test(atom)) {
      const ms = parseInt(atom) * 1000;
      tokens.push({ type: 'timing-once', value: ms }); // "0.5s" → 500ms
    } else {
      tokens.push({ type: 'atom', value: atom });
    }
  }

  return tokens;
}

// ─── Parser ──────────────────────────────────────────────────────────

function parse(source) {
  const tokens = tokenize(source);
  let pos = 0;

  function peek() {
    return tokens[pos];
  }

  function advance() {
    return tokens[pos++];
  }

  function readExpr() {
    const t = peek();
    if (!t) return null;

    if (t.type === '(') {
      advance(); // consume (
      const list = [];
      while (peek() && peek().type !== ')') {
        const expr = readExpr();
        if (expr !== null) list.push(expr);
      }
      if (peek() && peek().type === ')') {
        advance(); // consume )
      }
      return list;
    } else if (t.type === 'string') {
      advance();
      return { type: 'string', value: t.value };
    } else if (t.type === 'fade') {
      advance();
      return { type: 'fade', colors: t.value.split('-') };
    } else if (t.type === 'timing-repeating') {
      advance();
      return { type: 'timing-repeating', ms: t.value };
    } else if (t.type === 'timing-once') {
      advance();
      return { type: 'timing-once', ms: t.value };
    } else {
      // Regular atom - try to parse as number
      const token = advance();
      const value = token.value;
      // Try to parse as float, fallback to string
      const num = parseFloat(value);
      return isNaN(num) ? value : num;
    }
  }

  const exprs = [];
  while (pos < tokens.length) {
    const expr = readExpr();
    if (expr !== null) exprs.push(expr);
  }

  return exprs;
}

// ─── Evaluator ──────────────────────────────────────────────────────

class KidLispMini {
  constructor() {
    this.globalEnv = this.createEnv();
    this.frameCount = 0;
    this.startTime = Date.now();
  }

  setApi(api) {
    this.api = api;
  }

  createEnv() {
    const self = this;
    const colorMap = {
      'red': [255, 0, 0, 255],
      'blue': [0, 0, 255, 255],
      'green': [0, 255, 0, 255],
      'black': [0, 0, 0, 255],
      'white': [255, 255, 255, 255],
      'cyan': [0, 255, 255, 255],
      'magenta': [255, 0, 255, 255],
      'yellow': [255, 255, 0, 255],
      'orange': [255, 165, 0, 255],
      'purple': [128, 0, 128, 255],
      'pink': [255, 192, 203, 255],
      'lime': [0, 255, 0, 255],
      'gray': [128, 128, 128, 255],
    };

    const resolveColor = (arg) => {
      // Handle color array
      if (Array.isArray(arg) && arg.length >= 3) {
        return arg;
      }
      // Handle string color names
      if (typeof arg === 'string') {
        if (arg === 'rainbow') {
          const hue = (self.frameCount / 60) % 1;
          const [r, g, b] = self.hslToRgb(hue, 1, 0.5);
          return [Math.round(r), Math.round(g), Math.round(b), 255];
        }
        if (colorMap[arg.toLowerCase()]) {
          return colorMap[arg.toLowerCase()];
        }
        return [255, 255, 255, 255];
      }
      // Handle numeric grayscale
      if (typeof arg === 'number') {
        const v = Math.max(0, Math.min(255, Math.round(arg)));
        return [v, v, v, 255];
      }
      return [255, 255, 255, 255];
    };

    const env = {
      // Drawing - directly call api methods
      wipe: (...args) => {
        if (self.api && self.api.wipe) {
          self.api.wipe(...args);
        }
      },
      ink: (...args) => {
        if (self.api && self.api.ink) {
          self.api.ink(...args);
        }
      },
      line: (...args) => {
        if (self.api && self.api.line) {
          self.api.line(...args);
        }
      },
      box: (...args) => {
        if (self.api && self.api.box) {
          self.api.box(...args);
        }
      },
      circle: (...args) => {
        if (self.api && self.api.circle) {
          self.api.circle(...args);
        }
      },
      plot: (...args) => {
        if (self.api && self.api.plot) {
          self.api.plot(...args);
        }
      },
      write: (...args) => {
        if (self.api && self.api.write) {
          self.api.write(...args);
        }
      },

      // Transforms - directly call api methods
      scroll: (...args) => {
        if (self.api && self.api.scroll) {
          self.api.scroll(...args);
        }
      },
      spin: (...args) => {
        if (self.api && self.api.spin) {
          self.api.spin(...args);
        }
      },
      zoom: (...args) => {
        if (self.api && self.api.zoom) {
          self.api.zoom(...args);
        }
      },
      contrast: (...args) => {
        if (self.api && self.api.contrast) {
          self.api.contrast(...args);
        }
      },

      // Math & Control
      '+': (...args) => args.reduce((a, b) => a + b, 0),
      '-': (...args) => args.length === 1 ? -args[0] : args.slice(1).reduce((a, b) => a - b, args[0]),
      '*': (...args) => args.reduce((a, b) => a * b, 1),
      '/': (...args) => args.length === 1 ? 1 / args[0] : args.slice(1).reduce((a, b) => a / b, args[0]),
      'sin': (x) => Math.sin(x),
      'cos': (x) => Math.cos(x),
      'min': (...args) => Math.min(...args),
      'max': (...args) => Math.max(...args),
      'abs': (x) => Math.abs(x),
      'sqrt': (x) => Math.sqrt(x),
      'floor': (x) => Math.floor(x),
      'ceil': (x) => Math.ceil(x),
      'round': (x) => Math.round(x),

      // Randomness
      'random': (min = 0, max = 1) => min + Math.random() * (max - min),
      '?': null, // handled specially

      // Conditionals
      'if': null, // special form
      'not': (x) => !x,
      '=': (a, b) => a === b,
      '<': (a, b) => a < b,
      '>': (a, b) => a > b,
      '<=': (a, b) => a <= b,
      '>=': (a, b) => a >= b,

      // Colors
      'rainbow': () => 'rainbow',
      'zebra': () => 'zebra',
      'def': null, // special
    };
    return env;
  }

  evaluate(ast, api, frame = 0) {
    const results = [];
    for (const expr of ast) {
      const result = this.evalExpr(expr, this.globalEnv, api, frame);
      if (result) results.push(result);
    }
    return results;
  }

  evalExpr(expr, env, api, frame = 0) {
    // Null/undefined
    if (expr === null || expr === undefined) return null;

    // String literal (atom)
    if (typeof expr === 'string') {
      // Dimension symbols
      if (expr === 'w') return api.screen.width;
      if (expr === 'h') return api.screen.height;
      if (expr === 'w/2') return api.screen.width / 2;
      if (expr === 'h/2') return api.screen.height / 2;
      if (expr === 'frame') return this.frameCount;

      // Color names
      const colorMap = {
        'red': [255, 0, 0, 255],
        'blue': [0, 0, 255, 255],
        'green': [0, 255, 0, 255],
        'black': [0, 0, 0, 255],
        'white': [255, 255, 255, 255],
        'cyan': [0, 255, 255, 255],
        'magenta': [255, 0, 255, 255],
        'yellow': [255, 255, 0, 255],
        'orange': [255, 165, 0, 255],
        'purple': [128, 0, 128, 255],
        'pink': [255, 192, 203, 255],
        'lime': [0, 255, 0, 255],
        'gray': [128, 128, 128, 255],
      };
      return colorMap[expr] || expr;
    }

    // Number
    if (typeof expr === 'number') return expr;

    // String object
    if (typeof expr === 'object' && expr.type === 'string') {
      return expr.value;
    }

    // Fade shorthand - directly apply fade background
    if (typeof expr === 'object' && expr.type === 'fade') {
      if (this.api && this.api.fadeBackground) {
        const colors = expr.colors.map(name => {
          const hslToRgb = this.hslToRgb.bind(this);
          if (name === 'red') return [255, 0, 0, 255];
          if (name === 'blue') return [0, 0, 255, 255];
          if (name === 'black') return [0, 0, 0, 255];
          if (name === 'white') return [255, 255, 255, 255];
          if (name === 'cyan') return [0, 255, 255, 255];
          if (name === 'magenta') return [255, 0, 255, 255];
          if (name === 'yellow') return [255, 255, 0, 255];
          if (name === 'orange') return [255, 165, 0, 255];
          return [0, 0, 0, 255];
        });
        this.api.fadeBackground(colors, frame);
      }
      return null;
    }

    // Rainbow special
    if (typeof expr === 'object' && expr.type === 'rainbow') {
      const hue = (expr.frame || 0) / 360 % 1;
      const [r, g, b] = this.hslToRgb(hue, 1, 0.5);
      return [Math.round(r), Math.round(g), Math.round(b), 255];
    }

    // Timing forms — interpolate to progress value (0-1)
    if (typeof expr === 'object' && expr.type === 'timing-repeating') {
      const progress = (frame * 16) % expr.ms / expr.ms;
      return progress;
    }
    if (typeof expr === 'object' && expr.type === 'timing-once') {
      const progress = Math.min(1, (frame * 16) / expr.ms);
      return progress;
    }

    // List / function call
    if (Array.isArray(expr)) {
      if (expr.length === 0) return null;

      const [fn, ...args] = expr;

      // Check if this is a timing list FIRST (even before function call)
      // Timing interpolation: (Ns... start end) or (Ns start end)
      if (expr.length >= 3 && typeof expr[0] === 'object' && (expr[0].type === 'timing-repeating' || expr[0].type === 'timing-once')) {
        if (args.length >= 2) {
          const startVal = this.evalExpr(args[0], env, api, frame);
          const endVal = this.evalExpr(args[1], env, api, frame);

          if (fn.type === 'timing-repeating') {
            const progress = (frame * 16) % fn.ms / fn.ms;
            if (typeof startVal === 'number' && typeof endVal === 'number') {
              return startVal + (endVal - startVal) * progress;
            }
            return progress < 0.5 ? startVal : endVal;
          } else {
            const progress = Math.min(1, (frame * 16) / fn.ms);
            if (typeof startVal === 'number' && typeof endVal === 'number') {
              return startVal + (endVal - startVal) * progress;
            }
            return progress < 1 ? startVal : endVal;
          }
        }
        return null;
      }

      // Special forms
      if (fn === 'if') {
        const [cond, thenBr, elseBr] = args;
        const condVal = this.evalExpr(cond, env, api, frame);
        return condVal ? this.evalExpr(thenBr, env, api, frame) : this.evalExpr(elseBr, env, api, frame);
      }

      if (fn === 'def') {
        const [name, val] = args;
        env[name] = this.evalExpr(val, env, api, frame);
        return null;
      }

      if (fn === '?') {
        // Random choice
        const choices = args.map(a => this.evalExpr(a, env, api, frame));
        return choices[Math.floor(Math.random() * choices.length)];
      }

      // Regular function call
      const fnVal = env[fn];
      if (!fnVal) return null;

      const evalArgs = args.map(arg => this.evalExpr(arg, env, api, frame));

      if (typeof fnVal === 'function') {
        return fnVal(...evalArgs);
      }

      return fnVal;
    }

    return expr;
  }

  hslToRgb(h, s, l) {
    const c = (1 - Math.abs(2 * l - 1)) * s;
    const x = c * (1 - Math.abs((h * 6) % 2 - 1));
    const m = l - c / 2;
    let r, g, b;

    if (h < 1 / 6) [r, g, b] = [c, x, 0];
    else if (h < 2 / 6) [r, g, b] = [x, c, 0];
    else if (h < 3 / 6) [r, g, b] = [0, c, x];
    else if (h < 4 / 6) [r, g, b] = [0, x, c];
    else if (h < 5 / 6) [r, g, b] = [x, 0, c];
    else [r, g, b] = [c, 0, x];

    return [
      (r + m) * 255,
      (g + m) * 255,
      (b + m) * 255
    ];
  }

  tick() {
    this.frameCount++;
  }
}

export { KidLispMini, parse };
