import assert from "node:assert/strict";
import test from "node:test";
import { compilePieceLisp, createPieceVmState, defaultPieceSenses, PIECE_VM, runPieceVm, verifyPieceProgram } from "../src/piece-vm.mjs";

const branchingText = `(piece
  (constant x 2)
  (constant y 3)
  (constant cursor 0)
  (constant one 1)
  (constant limit 6)
  (clear back 1 2 3)
  (label loop)
  (pixel back cursor y 255 0 0)
  (add cursor cursor one)
  (less again cursor limit)
  (jump-if again loop)
  (glyph back 65 x y 0 255 0)
  (swap)
  (halt))`;

test("PieceVM branches, renders a glyph, and atomically alternates framebuffers", () => {
  const program = compilePieceLisp(branchingText, { resolution: 32 });
  assert.equal(program.version, 4);
  assert.equal(program.labels.loop, 6);
  assert.equal(program.bytecode.length, program.instructionCount * PIECE_VM.instructionBytes * 2);
  const state = createPieceVmState(32);
  const first = runPieceVm(program, { state });
  assert.equal(first.fault, null);
  assert.equal(first.state.front, 1);
  assert.equal(first.state.frame, 1);
  const front = first.state.buffers[first.state.front];
  assert.deepEqual([...front.slice((3 * 32) * 3, (3 * 32) * 3 + 3)], [255, 0, 0]);
  assert.ok(Array.from({ length: front.length / 3 }, (_, pixel) => front[pixel * 3 + 1]).some((green) => green === 255));
  const second = runPieceVm(program, { state: first.state });
  assert.equal(second.fault, null);
  assert.equal(second.state.front, 0);
  assert.equal(second.state.frame, 2);
  assert.equal(second.frontHash, first.frontHash);
  assert.equal(verifyPieceProgram(program).valid, true);
});

test("PieceVM calls hierarchical transform code and projects a 3D point", () => {
  const source = `(piece
    (jump main)
    (label child)
    (push-transform)
    (translate dx dy dz)
    (rotate-y turn)
    (point3 back px py pz 20 80 255)
    (pop-transform)
    (return)
    (label main)
    (constant zero 0)
    (constant dx (ratio 1 2))
    (constant dy (ratio 1 4))
    (constant dz (ratio 1 8))
    (constant turn (ratio 1 8))
    (constant px 0)
    (constant py 0)
    (constant pz 0)
    (clear back 0 0 0)
    (identity)
    (call child)
    (swap)
    (halt))`;
  const program = compilePieceLisp(source, { resolution: 32 });
  const result = runPieceVm(program);
  assert.equal(result.fault, null);
  const front = result.state.buffers[result.state.front];
  const bluePixels = Array.from({ length: front.length / 3 }, (_, pixel) =>
    front[pixel * 3] === 20 && front[pixel * 3 + 1] === 80 && front[pixel * 3 + 2] === 255).filter(Boolean);
  assert.equal(bluePixels.length, 1);
  assert.equal(verifyPieceProgram(program).valid, true);
});

test("PieceVM composes vec3 lines, triangles, and stack-preserved sibling branches", () => {
  const source = `(piece
    (jump main)
    (label branch)
    (line3 back origin tip 255 0 128)
    (triangle3 back origin tip wing 0 180 255)
    (less again one depth)
    (jump-if again child)
    (return)
    (label child)
    (push depth)
    (sub depth depth one)
    (push-transform)
    (translate tip.x tip.y tip.z)
    (rotate-z turn)
    (call branch)
    (pop-transform)
    (pop depth)
    (return)
    (label main)
    (vec3 origin 0 0 0)
    (vec3 tip (ratio 1 2) 0 0)
    (vec3 wing (ratio 1 4) (ratio 1 8) 0)
    (constant one 1)
    (constant depth 4)
    (constant turn (ratio 1 12))
    (clear back 0 0 0)
    (identity)
    (call branch)
    (swap)
    (halt))`;
  const program = compilePieceLisp(source, { resolution: 64 });
  assert.equal(program.vectors.origin, 0);
  assert.equal(program.vectors.tip, 3);
  assert.equal(program.registers["tip.z"], 5);
  const result = runPieceVm(program);
  assert.equal(result.fault, null);
  const front = result.state.buffers[result.state.front];
  const colored = Array.from({ length: front.length / 3 }, (_, pixel) =>
    front[pixel * 3] || front[pixel * 3 + 1] || front[pixel * 3 + 2]).filter(Boolean).length;
  assert.ok(colored > 12);
  assert.equal(verifyPieceProgram(program).valid, true);
});

test("PieceVM fuel faults cannot publish a partial back buffer", () => {
  const program = compilePieceLisp("(piece (label spin) (jump spin) (swap) (halt))", { resolution: 32 });
  const state = createPieceVmState(32), originalFront = state.front;
  const result = runPieceVm(program, { state, fuel: 32 });
  assert.match(result.fault, /fuel exhausted/);
  assert.equal(result.state.front, originalFront);
  assert.equal(result.state.frame, 0);
  assert.equal(verifyPieceProgram(program, { fuel: 32 }).valid, false);
});

test("PieceVM data memory can drive deterministic change across double-buffered frames", () => {
  const source = `(piece
    (constant address 0)
    (constant one 1)
    (constant x 4)
    (load8 value address)
    (add value value one)
    (store8 address value)
    (clear back 0 0 0)
    (pixel back x value 255 255 255)
    (swap)
    (halt))`;
  const program = compilePieceLisp(source, { resolution: 32 });
  const first = runPieceVm(program);
  const second = runPieceVm(program, { state: first.state });
  assert.equal(first.fault, null);
  assert.equal(second.fault, null);
  assert.equal(second.state.data[0], 2);
  assert.notEqual(first.frontHash, second.frontHash);
  const proof = verifyPieceProgram(program);
  assert.equal(proof.valid, true);
  assert.notEqual(proof.frontHashes[0], proof.frontHashes[1]);
});

test("PieceVM function signatures pass scalar arguments and restore caller registers", () => {
  const source = `(piece
    (jump main)
    (function dot (px py)
      (pixel back px py 255 40 20)
      (constant px 20)
      (return))
    (label main)
    (constant x 5)
    (constant y 6)
    (constant one 1)
    (clear back 0 0 0)
    (call dot x y)
    (add x x one)
    (pixel back x y 20 255 80)
    (swap)
    (halt))`;
  const program = compilePieceLisp(source, { resolution: 32 });
  assert.deepEqual(program.functions.dot.parameters, ["px", "py"]);
  assert.equal(program.functions.dot.registers.length, 2);
  assert.match(program.source, /\(function dot \(px py\)/);
  const result = runPieceVm(program);
  assert.equal(result.fault, null);
  const front = result.state.buffers[result.state.front];
  assert.deepEqual([...front.slice((6 * 32 + 5) * 3, (6 * 32 + 5) * 3 + 3)], [255, 40, 20]);
  assert.deepEqual([...front.slice((6 * 32 + 6) * 3, (6 * 32 + 6) * 3 + 3)], [20, 255, 80]);
  assert.equal(result.state.registers[program.registers.x], 6 * PIECE_VM.fixedOne);
  assert.equal(verifyPieceProgram(program).valid, true);
});

test("PieceVM named data regions enforce their own runtime bounds", () => {
  const source = `(piece
    (data trail 4)
    (data counters 2)
    (constant index 0)
    (constant one 1)
    (constant x 4)
    (read8 value trail index)
    (add value value one)
    (write8 trail index value)
    (clear back 0 0 0)
    (pixel back x value 220 240 255)
    (swap)
    (halt))`;
  const program = compilePieceLisp(source, { resolution: 32 });
  assert.deepEqual(program.data, { trail: { offset: 0, length: 4 }, counters: { offset: 4, length: 2 } });
  const first = runPieceVm(program), second = runPieceVm(program, { state: first.state });
  assert.equal(first.fault, null);
  assert.equal(second.fault, null);
  assert.equal(second.state.data[0], 2);
  assert.notEqual(first.frontHash, second.frontHash);
  const escaped = compilePieceLisp(source.replace("(constant index 0)", "(constant index 4)"), { resolution: 32 });
  assert.match(runPieceVm(escaped).fault, /read8 index exceeds data region/);
});

test("PieceVM sensing is read-only, normalized, bounded, and frame-deterministic", () => {
  const source = `(piece
    (sense8 beat beat)
    (constant y 8)
    (constant scale 31)
    (mul x beat scale)
    (clear back 0 0 0)
    (pixel back x y 255 255 255)
    (swap)
    (halt))`;
  const program = compilePieceLisp(source, { resolution: 32 });
  const left = runPieceVm(program, { senses: Uint8Array.of(255, 0, 0, 0, 0, 0, 0, 0) });
  const right = runPieceVm(program, { senses: Uint8Array.of(0, 0, 0, 0, 0, 0, 0, 0) });
  assert.equal(left.fault, null);
  assert.notEqual(left.frontHash, right.frontHash);
  assert.equal(left.state.registers[program.registers.beat], PIECE_VM.fixedOne);
  assert.deepEqual([...defaultPieceSenses(0)], [0, 0, 255, 128, 192, 96, 160, 17]);
  assert.deepEqual([...defaultPieceSenses(1)], [7, 0, 255, 128, 192, 96, 160, 46]);
  assert.throws(() => runPieceVm(program, { senses: [0] }), /exactly eight/);
  assert.throws(() => compilePieceLisp("(piece (sense8 x internet) (swap) (halt))"), /invalid PieceVM instruction/);
  assert.equal(verifyPieceProgram(program).valid, true);
});

test("PieceVM compiler rejects unproven control and publication shapes", () => {
  assert.throws(() => compilePieceLisp("(piece (jump nowhere) (swap) (halt))"), /unknown PieceVM label/);
  assert.throws(() => compilePieceLisp("(piece (clear back 0 0 0) (halt))"), /exactly one swap/);
  assert.throws(() => compilePieceLisp("(piece (swap) (swap) (halt))"), /exactly one swap/);
  assert.throws(() => compilePieceLisp("(piece (line3 back missing missing 1 2 3) (swap) (halt))"), /unknown PieceVM vector/);
  assert.throws(() => compilePieceLisp("(piece (function f (x) (return)) (call f) (swap) (halt))"), /expects 1 arguments/);
  assert.throws(() => compilePieceLisp("(piece (data tiny 1) (constant i 1) (constant v 2) (write8 missing i v) (swap) (halt))"), /invalid PieceVM instruction/);
});
