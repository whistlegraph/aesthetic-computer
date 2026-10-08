// Audited operation contracts, v1. Numeric implementations are deliberately
// independent of the reference evaluator so differential tests detect drift.
const define = (id, name, aliases, min, max, summary, apply) => Object.freeze({
  id, name, aliases: Object.freeze(aliases), effect: "pure", dependencies: Object.freeze([]),
  arguments: Object.freeze({ kind: "number", min, max }), result: "number", summary, apply,
});

export const NUMERIC_OPERATIONS = Object.freeze([
  define(1, "+", [], 0, null, "Sum numbers; empty sum is zero.", a => a.reduce((x, y) => x + y, 0)),
  define(2, "-", [], 0, null, "Subtract left to right; one argument negates; empty is zero.", a => !a.length ? 0 : a.length === 1 ? -a[0] : a.slice(1).reduce((x, y) => x - y, a[0])),
  define(3, "*", ["mul"], 0, null, "Multiply numbers; empty product is one.", a => a.reduce((x, y) => x * y, 1)),
  define(4, "/", [], 0, null, "Divide left to right, skipping zero divisors; empty is zero.", a => !a.length ? 0 : a.slice(1).reduce((x, y) => y === 0 ? x : x / y, a[0])),
  define(5, "%", ["mod"], 0, null, "Remainder of the first two arguments; missing or zero divisor returns zero.", a => a.length < 2 || a[1] === 0 ? 0 : a[0] % a[1]),
  define(6, "floor", [], 0, null, "Round the first argument down; missing returns zero.", a => Math.floor(a[0] ?? 0)),
  define(7, "ceil", [], 0, null, "Round the first argument up; missing returns zero.", a => Math.ceil(a[0] ?? 0)),
  define(8, "round", [], 0, null, "Round the first argument using reference JS rounding; missing returns zero.", a => Math.round(a[0] ?? 0)),
]);

const byName = new Map(NUMERIC_OPERATIONS.flatMap(op => [op.name, ...op.aliases].map(name => [name, op])));
const byId = new Map(NUMERIC_OPERATIONS.map(op => [op.id, op]));
export const numericOperation = name => byName.get(name);
export const numericOperationById = id => byId.get(id);
export const numericOperationNames = () => [...byName.keys()];
export const operationManifest = () => ({ version: 1, profile: "numeric-v1", operations: NUMERIC_OPERATIONS.map(({ apply, ...contract }) => contract) });
