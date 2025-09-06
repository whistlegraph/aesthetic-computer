# KidLisp Optimization Implementation Guide

**Generated:** August 17, 2025  
**Analyst:** GitHub Copilot  
**Target:** Optimizing KidLisp based on TinyLisp architecture insights  
**Implementation:** JavaScript/ES6 with backward compatibility  
**Performance Goal:** 3-10x improvement while maintaining feature set  

---

## Immediate Wins (Phase 1)

### 1. Bytecode Compilation System

Replace the current AST evaluation with a bytecode compilation approach:

```javascript
// New bytecode operations
const OPCODES = {
  LOAD_NUM: 0,
  LOAD_VAR: 1,  
  LOAD_FUNC: 2,
  CALL: 3,
  RETURN: 4,
  JUMP_IF_FALSE: 5,
  JUMP: 6,
  POP: 7,
  DUP: 8
};

class KidLispBytecode {
  constructor() {
    this.instructions = [];
    this.constants = [];
    this.stack = [];
    this.pc = 0; // program counter
  }

  compile(ast) {
    this.instructions = [];
    this.constants = [];
    this.compileExpression(ast);
    return this;
  }

  compileExpression(expr) {
    if (typeof expr === 'number') {
      const constIndex = this.addConstant(expr);
      this.emit(OPCODES.LOAD_NUM, constIndex);
    } else if (typeof expr === 'string') {
      this.emit(OPCODES.LOAD_VAR, this.addConstant(expr));
    } else if (Array.isArray(expr)) {
      const [head, ...args] = expr;
      
      // Compile arguments first (reverse order for stack)
      for (let i = args.length - 1; i >= 0; i--) {
        this.compileExpression(args[i]);
      }
      
      // Load function
      this.emit(OPCODES.LOAD_FUNC, this.addConstant(head));
      this.emit(OPCODES.CALL, args.length);
    }
  }

  addConstant(value) {
    const index = this.constants.indexOf(value);
    if (index !== -1) return index;
    this.constants.push(value);
    return this.constants.length - 1;
  }

  emit(opcode, operand = 0) {
    this.instructions.push({ opcode, operand });
  }

  execute(api, globalEnv) {
    this.stack = [];
    this.pc = 0;

    while (this.pc < this.instructions.length) {
      const { opcode, operand } = this.instructions[this.pc];

      switch (opcode) {
        case OPCODES.LOAD_NUM:
          this.stack.push(this.constants[operand]);
          break;

        case OPCODES.LOAD_VAR:
          const varName = this.constants[operand];
          const value = globalEnv[varName] || this.localEnv[varName];
          this.stack.push(value);
          break;

        case OPCODES.LOAD_FUNC:
          const funcName = this.constants[operand];
          this.stack.push(globalEnv[funcName]);
          break;

        case OPCODES.CALL:
          const argCount = operand;
          const func = this.stack.pop();
          const args = [];
          for (let i = 0; i < argCount; i++) {
            args.unshift(this.stack.pop());
          }
          const result = func(api, args);
          this.stack.push(result);
          break;
      }
      this.pc++;
    }

    return this.stack.pop();
  }
}

// Integration with existing KidLisp class
class OptimizedKidLisp extends KidLisp {
  constructor() {
    super();
    this.compiledBytecode = null;
    this.bytecodeCache = new Map();
  }

  parse(input) {
    const ast = super.parse(input);
    
    // Check cache first
    const cacheKey = JSON.stringify(ast);
    if (this.bytecodeCache.has(cacheKey)) {
      this.compiledBytecode = this.bytecodeCache.get(cacheKey);
    } else {
      // Compile to bytecode
      this.compiledBytecode = new KidLispBytecode().compile(ast);
      this.bytecodeCache.set(cacheKey, this.compiledBytecode);
    }
    
    return ast;
  }

  evaluate(expr, api, env) {
    if (this.compiledBytecode) {
      return this.compiledBytecode.execute(api, this.getGlobalEnv());
    }
    return super.evaluate(expr, api, env);
  }
}
```

### 2. Function Memoization for Pure Functions

```javascript
class MemoizedFunction {
  constructor(func, isPure = true, maxCacheSize = 1000) {
    this.func = func;
    this.isPure = isPure;
    this.cache = new Map();
    this.maxCacheSize = maxCacheSize;
    this.hitCount = 0;
    this.missCount = 0;
  }

  call(api, args) {
    if (!this.isPure) {
      // Non-pure functions always execute
      return this.func(api, args);
    }

    const key = this.hashArgs(args);
    
    if (this.cache.has(key)) {
      this.hitCount++;
      return this.cache.get(key);
    }

    this.missCount++;
    const result = this.func(api, args);

    // LRU eviction
    if (this.cache.size >= this.maxCacheSize) {
      const firstKey = this.cache.keys().next().value;
      this.cache.delete(firstKey);
    }

    this.cache.set(key, result);
    return result;
  }

  hashArgs(args) {
    // Simple hash for memoization key
    return JSON.stringify(args);
  }

  getStats() {
    const total = this.hitCount + this.missCount;
    return {
      hitCount: this.hitCount,
      missCount: this.missCount,
      hitRate: total > 0 ? (this.hitCount / total * 100).toFixed(1) + '%' : '0%'
    };
  }
}

// Mark functions as pure for memoization
const PURE_FUNCTIONS = new Set([
  '+', '-', '*', '/', '%', 'mod',
  '=', '>', '<', '>=', '<=', 
  'abs', 'sqrt', 'sin', 'cos', 'tan',
  'floor', 'ceil', 'round',
  'min', 'max'
]);

// Enhanced global environment with memoization
class OptimizedKidLisp extends KidLisp {
  getGlobalEnv() {
    if (this.globalEnvCache) {
      return this.globalEnvCache;
    }

    const baseEnv = super.getGlobalEnv();
    const optimizedEnv = {};

    // Wrap pure functions with memoization
    for (const [name, func] of Object.entries(baseEnv)) {
      if (typeof func === 'function' && PURE_FUNCTIONS.has(name)) {
        optimizedEnv[name] = new MemoizedFunction(func, true).call.bind(
          new MemoizedFunction(func, true)
        );
      } else {
        optimizedEnv[name] = func;
      }
    }

    this.globalEnvCache = optimizedEnv;
    return optimizedEnv;
  }
}
```

### 3. Fast Environment Lookup

```javascript
class FastEnvironment {
  constructor() {
    this.scopes = [new Map()]; // Stack of scope maps
    this.localVarStack = []; // For faster local variable access
  }

  pushScope() {
    this.scopes.push(new Map());
    this.localVarStack.push({});
  }

  popScope() {
    this.scopes.pop();
    this.localVarStack.pop();
  }

  define(name, value) {
    const currentScope = this.scopes[this.scopes.length - 1];
    currentScope.set(name, value);
    
    // Also add to local var stack for O(1) access
    const currentLocals = this.localVarStack[this.localVarStack.length - 1];
    currentLocals[name] = value;
  }

  lookup(name) {
    // Check local var stack first (fastest)
    for (let i = this.localVarStack.length - 1; i >= 0; i--) {
      if (this.localVarStack[i].hasOwnProperty(name)) {
        return this.localVarStack[i][name];
      }
    }

    // Fallback to scope chain
    for (let i = this.scopes.length - 1; i >= 0; i--) {
      if (this.scopes[i].has(name)) {
        return this.scopes[i].get(name);
      }
    }

    return undefined;
  }

  update(name, value) {
    // Update in the scope where it was first defined
    for (let i = this.scopes.length - 1; i >= 0; i--) {
      if (this.scopes[i].has(name)) {
        this.scopes[i].set(name, value);
        // Also update local var stack
        for (let j = this.localVarStack.length - 1; j >= 0; j--) {
          if (this.localVarStack[j].hasOwnProperty(name)) {
            this.localVarStack[j][name] = value;
            break;
          }
        }
        return true;
      }
    }
    return false;
  }
}

// Integration with KidLisp
class OptimizedKidLisp extends KidLisp {
  constructor() {
    super();
    this.fastEnv = new FastEnvironment();
  }

  evaluate(expr, api, env) {
    // Use fast environment for variable lookups
    if (typeof expr === 'string') {
      const value = this.fastEnv.lookup(expr);
      if (value !== undefined) {
        return value;
      }
      // Fallback to global environment
      const globalEnv = this.getGlobalEnv();
      return globalEnv[expr];
    }

    return super.evaluate(expr, api, env);
  }
}
```

## Medium-Term Optimizations (Phase 2)

### 4. Optimized Timing Expression Engine

```javascript
class TimingEngine {
  constructor() {
    this.timers = new Map();
    this.sequenceStates = new Map();
    this.compiledTimingExpressions = new Map();
  }

  compileTimingExpression(timingToken, expressions) {
    const cacheKey = timingToken + JSON.stringify(expressions);
    
    if (this.compiledTimingExpressions.has(cacheKey)) {
      return this.compiledTimingExpressions.get(cacheKey);
    }

    let interval, isRepeating, isInstant;
    
    if (/^\d*\.?\d+s\.\.\.?$/.test(timingToken)) {
      // Repeating timer like "1s..."
      interval = parseFloat(timingToken.slice(0, -4)) * 1000; // Convert to ms
      isRepeating = true;
      isInstant = false;
    } else if (/^\d*\.?\d+s!?$/.test(timingToken)) {
      // Delay timer like "1s" or instant "1s!"
      interval = parseFloat(timingToken.replace(/[s!]/g, '')) * 1000;
      isRepeating = false;
      isInstant = timingToken.endsWith('!');
    }

    const compiled = {
      interval,
      isRepeating,
      isInstant,
      expressions: expressions.slice(), // Copy array
      lastExecutionTime: 0,
      currentIndex: 0,
      hasExecuted: false
    };

    this.compiledTimingExpressions.set(cacheKey, compiled);
    return compiled;
  }

  executeTiming(timingToken, expressions, api, currentTime) {
    const compiled = this.compileTimingExpression(timingToken, expressions);
    const timeSinceLastExecution = currentTime - compiled.lastExecutionTime;

    // Handle instant triggers
    if (compiled.isInstant && !compiled.hasExecuted) {
      compiled.hasExecuted = true;
      compiled.lastExecutionTime = currentTime;
      return this.executeExpressions(compiled.expressions, api);
    }

    // Handle interval-based execution
    if (timeSinceLastExecution >= compiled.interval) {
      compiled.lastExecutionTime = currentTime;

      if (compiled.isRepeating) {
        // Cycle through expressions
        const result = this.executeExpressions([compiled.expressions[compiled.currentIndex]], api);
        compiled.currentIndex = (compiled.currentIndex + 1) % compiled.expressions.length;
        return result;
      } else {
        // Execute all expressions once
        return this.executeExpressions(compiled.expressions, api);
      }
    }

    // Return current state for repeating expressions
    if (compiled.isRepeating && compiled.expressions.length > 0) {
      return compiled.expressions[compiled.currentIndex];
    }

    return null;
  }

  executeExpressions(expressions, api) {
    let result = null;
    for (const expr of expressions) {
      result = this.evaluateExpression(expr, api);
    }
    return result;
  }

  evaluateExpression(expr, api) {
    // This would delegate back to the main evaluator
    // but with optimized context
    return expr; // Simplified for example
  }
}

// Integration with main evaluator
class OptimizedKidLisp extends KidLisp {
  constructor() {
    super();
    this.timingEngine = new TimingEngine();
  }

  evaluate(expr, api, env) {
    if (Array.isArray(expr)) {
      const [head, ...args] = expr;

      // Fast path for timing expressions
      if (typeof head === 'string' && /^\d*\.?\d+s/.test(head)) {
        const currentTime = Date.now();
        return this.timingEngine.executeTiming(head, args, api, currentTime);
      }
    }

    return super.evaluate(expr, api, env);
  }
}
```

### 5. Symbol Interning for Reduced String Allocation

```javascript
class SymbolTable {
  constructor() {
    this.symbols = new Map(); // string -> symbol id
    this.symbolNames = []; // symbol id -> string
    this.nextId = 0;
  }

  intern(name) {
    if (this.symbols.has(name)) {
      return this.symbols.get(name);
    }

    const id = this.nextId++;
    this.symbols.set(name, id);
    this.symbolNames[id] = name;
    return id;
  }

  getName(id) {
    return this.symbolNames[id];
  }

  getSymbol(name) {
    return this.symbols.get(name);
  }

  size() {
    return this.nextId;
  }
}

// Use interned symbols for function names and variables
class OptimizedKidLisp extends KidLisp {
  constructor() {
    super();
    this.symbolTable = new SymbolTable();
    this.internedFunctions = new Map();
  }

  parse(input) {
    const ast = super.parse(input);
    return this.internSymbols(ast);
  }

  internSymbols(expr) {
    if (typeof expr === 'string' && /^[a-zA-Z_]/.test(expr)) {
      // Intern function names and variables
      return this.symbolTable.intern(expr);
    } else if (Array.isArray(expr)) {
      return expr.map(item => this.internSymbols(item));
    }
    return expr;
  }

  evaluate(expr, api, env) {
    // Handle interned symbols
    if (typeof expr === 'number' && expr < this.symbolTable.size()) {
      const symbolName = this.symbolTable.getName(expr);
      if (symbolName) {
        const globalEnv = this.getGlobalEnv();
        return globalEnv[symbolName];
      }
    }

    return super.evaluate(expr, api, env);
  }
}
```

## Advanced Optimizations (Phase 3)

### 6. Tagged Union Value System (JavaScript NaN Boxing Alternative)

```javascript
// Value type constants
const VALUE_TYPES = {
  NUMBER: 0,
  STRING: 1,
  FUNCTION: 2,
  LIST: 3,
  BOOLEAN: 4,
  NIL: 5
};

class KidLispValue {
  constructor(type, data) {
    this.type = type;
    this.data = data;
  }

  static number(n) {
    return new KidLispValue(VALUE_TYPES.NUMBER, n);
  }

  static string(s) {
    return new KidLispValue(VALUE_TYPES.STRING, s);
  }

  static func(f) {
    return new KidLispValue(VALUE_TYPES.FUNCTION, f);
  }

  static list(items) {
    return new KidLispValue(VALUE_TYPES.LIST, items);
  }

  static bool(b) {
    return new KidLispValue(VALUE_TYPES.BOOLEAN, b);
  }

  static nil() {
    return new KidLispValue(VALUE_TYPES.NIL, null);
  }

  isNumber() { return this.type === VALUE_TYPES.NUMBER; }
  isString() { return this.type === VALUE_TYPES.STRING; }
  isFunction() { return this.type === VALUE_TYPES.FUNCTION; }
  isList() { return this.type === VALUE_TYPES.LIST; }
  isBoolean() { return this.type === VALUE_TYPES.BOOLEAN; }
  isNil() { return this.type === VALUE_TYPES.NIL; }

  getValue() { return this.data; }

  toString() {
    switch (this.type) {
      case VALUE_TYPES.NUMBER: return this.data.toString();
      case VALUE_TYPES.STRING: return this.data;
      case VALUE_TYPES.FUNCTION: return '[Function]';
      case VALUE_TYPES.LIST: return `(${this.data.map(v => v.toString()).join(' ')})`;
      case VALUE_TYPES.BOOLEAN: return this.data ? '#t' : '#f';
      case VALUE_TYPES.NIL: return '()';
      default: return '[Unknown]';
    }
  }
}

// Memory pool for value objects to reduce GC pressure
class ValuePool {
  constructor(initialSize = 1000) {
    this.pool = [];
    this.index = 0;
    
    // Pre-allocate value objects
    for (let i = 0; i < initialSize; i++) {
      this.pool.push(new KidLispValue(VALUE_TYPES.NIL, null));
    }
  }

  getValue(type, data) {
    if (this.index >= this.pool.length) {
      // Pool exhausted, create new value
      return new KidLispValue(type, data);
    }

    const value = this.pool[this.index++];
    value.type = type;
    value.data = data;
    return value;
  }

  reset() {
    this.index = 0;
  }
}
```

### 7. JIT Compilation for Hot Paths

```javascript
class JITCompiler {
  constructor() {
    this.hotPaths = new Map();
    this.executionCounts = new Map();
    this.compiledFunctions = new Map();
    this.JIT_THRESHOLD = 100; // Compile after 100 executions
  }

  trackExecution(expression) {
    const key = JSON.stringify(expression);
    const count = this.executionCounts.get(key) || 0;
    this.executionCounts.set(key, count + 1);

    if (count + 1 >= this.JIT_THRESHOLD && !this.compiledFunctions.has(key)) {
      this.compileHotPath(key, expression);
    }
  }

  compileHotPath(key, expression) {
    try {
      // Generate optimized JavaScript code for hot mathematical expressions
      if (this.isMathExpression(expression)) {
        const compiledFunction = this.compileMathExpression(expression);
        this.compiledFunctions.set(key, compiledFunction);
        console.log(`📊 JIT compiled hot path: ${key}`);
      }
    } catch (error) {
      console.warn(`⚠️ JIT compilation failed for ${key}:`, error);
    }
  }

  isMathExpression(expr) {
    if (!Array.isArray(expr)) return false;
    const [head] = expr;
    return ['+', '-', '*', '/', '%', 'sin', 'cos', 'tan', 'sqrt'].includes(head);
  }

  compileMathExpression(expr) {
    const jsCode = this.generateJavaScript(expr);
    return new Function('args', `return ${jsCode}`);
  }

  generateJavaScript(expr) {
    if (typeof expr === 'number') {
      return expr.toString();
    }
    
    if (typeof expr === 'string') {
      // Variable reference
      return `args['${expr}']`;
    }

    if (Array.isArray(expr)) {
      const [head, ...args] = expr;
      
      switch (head) {
        case '+':
          return args.map(arg => this.generateJavaScript(arg)).join(' + ');
        case '-':
          if (args.length === 1) {
            return `-${this.generateJavaScript(args[0])}`;
          }
          return args.map(arg => this.generateJavaScript(arg)).join(' - ');
        case '*':
          return args.map(arg => this.generateJavaScript(arg)).join(' * ');
        case '/':
          return args.map(arg => this.generateJavaScript(arg)).join(' / ');
        case '%':
          return args.map(arg => this.generateJavaScript(arg)).join(' % ');
        case 'sin':
          return `Math.sin(${this.generateJavaScript(args[0])})`;
        case 'cos':
          return `Math.cos(${this.generateJavaScript(args[0])})`;
        case 'tan':
          return `Math.tan(${this.generateJavaScript(args[0])})`;
        case 'sqrt':
          return `Math.sqrt(${this.generateJavaScript(args[0])})`;
        default:
          throw new Error(`Unsupported operation: ${head}`);
      }
    }

    throw new Error(`Cannot compile expression: ${expr}`);
  }

  executeOrCompile(expression, context) {
    const key = JSON.stringify(expression);
    this.trackExecution(expression);

    if (this.compiledFunctions.has(key)) {
      // Use compiled version
      return this.compiledFunctions.get(key)(context);
    }

    // Fall back to interpreter
    return null; // Indicates should use interpreter
  }
}

// Integration with main evaluator
class OptimizedKidLisp extends KidLisp {
  constructor() {
    super();
    this.jitCompiler = new JITCompiler();
  }

  evaluate(expr, api, env) {
    // Try JIT compilation for hot mathematical expressions
    if (Array.isArray(expr)) {
      const jitResult = this.jitCompiler.executeOrCompile(expr, { ...this.globalDef, ...this.localEnv });
      if (jitResult !== null) {
        return jitResult;
      }
    }

    return super.evaluate(expr, api, env);
  }
}
```

## Benchmarking Implementation

```javascript
class KidLispBenchmark {
  constructor() {
    this.tests = {
      'arithmetic': ['(+ 1 2 3 4 5)', '(* (+ 2 3) (- 4 1))', '(/ (* 7 8) (+ 2 3))'],
      'function-calls': ['(line 0 0 100 100)', '(ink "red")', '(box 10 10 50 50)'],
      'timing': ['(1s (ink "red"))', '(0.5s... "red" "blue" "green")'],
      'variables': ['(def x 10) (+ x x x)', '(def y (+ 1 2 3)) (* y y)'],
      'complex': ['(repeat 10 (+ (* x 2) (/ y 3)))', '(if (> x 5) (+ x 10) (- x 5))']
    };
  }

  run(implementations, iterations = 1000) {
    const results = {};

    for (const [name, implementation] of Object.entries(implementations)) {
      console.log(`\n🏃 Benchmarking ${name}...`);
      results[name] = {};

      for (const [testName, testCases] of Object.entries(this.tests)) {
        const totalTime = this.benchmarkTestCase(implementation, testCases, iterations);
        results[name][testName] = {
          totalTime: totalTime.toFixed(2) + 'ms',
          avgTime: (totalTime / iterations).toFixed(4) + 'ms',
          opsPerSec: Math.round(iterations / (totalTime / 1000))
        };
      }
    }

    this.printResults(results);
    return results;
  }

  benchmarkTestCase(implementation, testCases, iterations) {
    const start = performance.now();
    
    for (let i = 0; i < iterations; i++) {
      for (const testCase of testCases) {
        try {
          const ast = implementation.parse(testCase);
          implementation.evaluate(ast, {}, {});
        } catch (e) {
          // Ignore errors for benchmarking
        }
      }
    }
    
    return performance.now() - start;
  }

  printResults(results) {
    console.log('\n📊 Benchmark Results:');
    console.log('='.repeat(80));
    
    const implementations = Object.keys(results);
    const testNames = Object.keys(results[implementations[0]]);
    
    // Print header
    console.log('Test'.padEnd(15) + implementations.map(impl => impl.padEnd(20)).join(''));
    console.log('-'.repeat(80));
    
    // Print results
    for (const testName of testNames) {
      let row = testName.padEnd(15);
      for (const impl of implementations) {
        const result = results[impl][testName];
        row += `${result.avgTime} (${result.opsPerSec}/s)`.padEnd(20);
      }
      console.log(row);
    }
  }
}

// Usage example
const benchmark = new KidLispBenchmark();
const implementations = {
  'Original': new KidLisp(),
  'Optimized': new OptimizedKidLisp()
};

// Run benchmarks
benchmark.run(implementations);
```

This implementation guide provides concrete code examples for each optimization phase, allowing for incremental improvement of KidLisp's performance while maintaining compatibility and extensibility.
