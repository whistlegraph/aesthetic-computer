import test from "node:test";
import assert from "node:assert/strict";
import { revealSignatures, sign, signatures, unsign } from "../lib/prox-signature.mjs";

test("a prox signature is invisible, survives copy and decodes", () => {
  const signed = sign("hello fia", "blueberry:niluda");
  assert.equal(unsign(signed), "hello fia");
  assert.deepEqual(signatures(`pasted: ${signed} more`), ["blueberry:niluda"]);
  assert.equal(revealSignatures(signed), "hello fia ⟨prox blueberry:niluda⟩");
  assert.doesNotMatch(signed.replace(/[\u{E0000}-\u{E007F}]/gu, ""), /prox|niluda/);
});

test("re-signing replaces, and nothing signs without a handle", () => {
  assert.deepEqual(signatures(sign(sign("x", "neo:a"), "neo:b")), ["neo:b"]);
  assert.equal(sign("plain", null), "plain");
  assert.equal(sign("", "neo:a"), "");
});
