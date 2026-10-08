// Opt-in execution controls. One context owns the frame clock and work ledger;
// nested instances share the ledger while deriving independent random seeds.
export class KidLispExecutionError extends Error {
  constructor(code, message, details = {}) {
    super(message);
    this.name = "KidLispExecutionError";
    this.code = code;
    Object.assign(this, details);
  }
}

const positiveInteger = (value, name) => {
  if (!Number.isSafeInteger(value) || value < 1) throw new TypeError(`${name} must be a positive safe integer`);
  return value;
};

export class KidLispExecution {
  constructor({ seed = 0, epochMs = 0, stepMs = 1000 / 60, maxSteps = 100000, maxDepth = 128 } = {}) {
    if (!Number.isInteger(seed) || seed < 0 || seed > 0xffffffff) throw new TypeError("seed must be uint32");
    if (!Number.isFinite(epochMs) || Math.abs(epochMs) > 8.64e15) throw new TypeError("epochMs must be a valid Date time");
    if (!Number.isFinite(stepMs) || stepMs <= 0) throw new TypeError("stepMs must be positive and finite");
    this.seed = seed;
    this.epochMs = epochMs;
    this.stepMs = stepMs;
    this.maxSteps = positiveInteger(maxSteps, "maxSteps");
    this.maxDepth = positiveInteger(maxDepth, "maxDepth");
    if (this.maxDepth > 256) throw new RangeError("maxDepth cannot exceed 256");
    this.state = { frame: 0, steps: 0, depth: 0, error: null };
    this.clock = Object.freeze({ time: () => new Date(this.timeMs), resync() {} });
    this.apiCache = new WeakMap();
  }

  get timeMs() { return this.epochMs + this.state.frame * this.stepMs; }

  beginFrame(frame) {
    if (!Number.isSafeInteger(frame) || frame < 0) throw new TypeError("frame must be a nonnegative safe integer");
    if (this.state.depth) throw new Error("Cannot begin a frame during evaluation");
    if (!Number.isFinite(this.epochMs + frame * this.stepMs) || Math.abs(this.epochMs + frame * this.stepMs) > 8.64e15) throw new RangeError("Frame time exceeds Date range");
    Object.assign(this.state, { frame, steps: 0, depth: 0, error: null });
  }

  consume(expression, count = 1) {
    if (this.state.error) throw this.state.error;
    if (!Number.isSafeInteger(count) || count < 0) throw new TypeError("Work cost must be a nonnegative integer");
    this.state.steps += count;
    if (this.state.steps > this.maxSteps) this.fail("STEP_BUDGET", "KidLisp frame work budget exceeded", expression);
  }

  enter(expression) {
    this.consume(expression);
    if (this.state.depth >= this.maxDepth) this.fail("DEPTH_BUDGET", "KidLisp evaluation depth exceeded", expression);
    this.state.depth++;
  }

  exit() { this.state.depth--; }

  fail(code, message, expression) {
    const operation = Array.isArray(expression) ? expression[0] : expression;
    const error = new KidLispExecutionError(code, message, {
      frame: this.state.frame, steps: this.state.steps,
      operation: typeof operation === "string" || typeof operation === "number" ? operation : null,
    });
    this.state.error = error;
    throw error;
  }

  bindApi(api) {
    if (this.apiCache.has(api)) return this.apiCache.get(api);
    // No global clock patching: independent runtimes may execute concurrently.
    const bound = new Proxy(api, {
      get: (target, key) => key === "clock" ? this.clock : key === "paintCount" ? this.state.frame : Reflect.get(target, key),
    });
    this.apiCache.set(api, bound);
    this.apiCache.set(bound, bound);
    return bound;
  }

  fork(label) {
    if (typeof label !== "string") throw new TypeError("Child execution needs a stable label");
    let seed = this.seed;
    for (let i = 0; i < label.length; i++) seed = Math.imul(seed ^ label.charCodeAt(i), 16777619) >>> 0;
    const child = new KidLispExecution({ seed, epochMs: this.epochMs, stepMs: this.stepMs, maxSteps: this.maxSteps, maxDepth: this.maxDepth });
    child.state = this.state;
    return child;
  }
}
