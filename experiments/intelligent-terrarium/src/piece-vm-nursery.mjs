import { hash } from "./canonical.mjs";
import { createHash } from "node:crypto";
import { Prng } from "./prng.mjs";
import { PIECE_VM, compilePieceLisp, createPieceVmState, readPieceLisp, runPieceVm, verifyPieceProgram } from "./piece-vm.mjs";

const MAX_RESIDENTS = 32;
const MAX_LINEAGE = 128;
const EVALUATION_FRAMES = 8;
const MAX_CAPABILITY_LINEAGE = 16;
const SELECTION_POLICIES = new Set(["phenotype-lead", "champion-control", "branch-diversity", "probe-focus",
  "curriculum-chain"]);
export const PIECE_VM_MUTATION_CATALOG = Object.freeze({
  variation: Object.freeze(["color", "duplicate-draw", "delete-draw", "rotate-axis", "insert-rotation",
    "vector-component", "branch-depth", "clone-child", "prune-child"]),
  machinery: Object.freeze(["function-graft", "argument-function-graft", "data-layout",
    "memory-oscillator", "sense-graft"]),
  exchange: Object.freeze(["lineage-crossover", "environment-graft"]),
});
const MUTATION_FAMILIES = new Set(Object.keys(PIECE_VM_MUTATION_CATALOG));
const MACHINERY_MUTATIONS = new Set(PIECE_VM_MUTATION_CATALOG.machinery);
const EXCHANGE_MUTATIONS = new Set(PIECE_VM_MUTATION_CATALOG.exchange);
const KNOWN_MUTATIONS = new Set(Object.values(PIECE_VM_MUTATION_CATALOG).flat());
const DEVELOPMENT_CAPABILITIES = new Set(["named-memory", "temporal-memory", "abstraction", "sensing", "ecology"]);

export function pieceVmMutationFamily(mutation) {
  return MACHINERY_MUTATIONS.has(mutation) ? "machinery" :
    EXCHANGE_MUTATIONS.has(mutation) ? "exchange" : "variation";
}

function printForm(value) {
  return Array.isArray(value) ? `(${value.map(printForm).join(" ")})` : String(value);
}

function cloneForm(value) {
  return Array.isArray(value) ? value.map(cloneForm) : value;
}

function pick(rng, values) {
  return values[Math.floor(rng.float() * values.length)];
}

function clamp(value, low = 0, high = 1) {
  return Math.max(low, Math.min(high, value));
}

function capabilityRecord(donor, capability, probe = null) {
  if (!donor || !capability) return null;
  return Object.freeze({
    donor: String(donor).slice(0, 24), capability: String(capability).slice(0, 24),
    probeAt: Number(probe?.at) || 0,
    probeAddress: Number.isInteger(probe?.address) ? probe.address : -1,
    probeTrack: String(probe?.track || "").slice(0, 24),
    requestedBy: String(probe?.requestedBy || "").slice(0, 24),
  });
}

function mergeCapabilityLineage(...groups) {
  const byKey = new Map();
  for (const value of groups.flat()) {
    const normalized = capabilityRecord(value?.donor, value?.capability, {
      at: value?.probeAt, address: value?.probeAddress,
      track: value?.probeTrack, requestedBy: value?.requestedBy,
    });
    if (!normalized) continue;
    const key = `${normalized.donor}:${normalized.capability}:${normalized.probeAt}:${normalized.probeAddress}`;
    if (!byKey.has(key)) byKey.set(key, normalized);
  }
  return Object.freeze([...byKey.values()].slice(-MAX_CAPABILITY_LINEAGE));
}

function normalizeSelectionEvidence(value, parentId) {
  if (!value || !parentId) return null;
  const finite = (number, fallback = 0) => Number.isFinite(Number(number)) ? Number(number) : fallback;
  const clamp = (number, low, high) => Math.max(low, Math.min(high, finite(number)));
  const staticScore = clamp(value.staticScore, 0, 1);
  const nativeBias = clamp(value.nativeBias, -.06, .06);
  return Object.freeze({ schema: 1, parentId: String(parentId).slice(0, 24),
    policy: SELECTION_POLICIES.has(value.policy) ? value.policy : "branch-diversity",
    staticScore, phenotypeReports: Math.max(0, Math.min(60, Math.floor(finite(value.phenotypeReports)))),
    phenotypeReady: Boolean(value.phenotypeReady), phenotypeScore: clamp(value.phenotypeScore, 0, 1),
    nativeBias, combinedScore: staticScore + nativeBias,
    sonicVoices: Math.max(0, Math.min(5, Math.floor(finite(value.sonicVoices)))),
    capturedAt: Math.max(0, Math.floor(finite(value.capturedAt))),
  });
}

function normalizeCurriculumEvidence(value, parentId) {
  if (!value || !parentId) return null;
  const names = (items) => Object.freeze((items || []).filter((name, index, values) =>
    DEVELOPMENT_CAPABILITIES.has(name) && values.indexOf(name) === index));
  const gained = names(value.gained), lost = names(value.lost), retained = names(value.retained);
  const breadth = (number) => Math.max(0, Math.min(5, Math.floor(Number(number) || 0)));
  return Object.freeze({ schema: 1, parentId: String(parentId).slice(0, 24),
    before: /^[01]{5}$/.test(value.before) ? value.before : "00000",
    after: /^[01]{5}$/.test(value.after) ? value.after : "00000",
    beforeBreadth: breadth(value.beforeBreadth), afterBreadth: breadth(value.afterBreadth),
    gained, lost, retained, advancement: gained.length, regression: lost.length,
    compound: breadth(value.afterBreadth) >= 3, complete: breadth(value.afterBreadth) === 5 });
}

function rehydrateCapabilityLineage(records) {
  const seen = new Map();
  for (const record of records) {
    const direct = capabilityRecord(record.environmentDonor, record.environmentCapability, record.environmentProbe);
    record.capabilityLineage = mergeCapabilityLineage(
      seen.get(record.parent)?.capabilityLineage || [],
      seen.get(record.donor)?.capabilityLineage || [],
      record.capabilityLineage || [], direct ? [direct] : []);
    seen.set(record.id, record);
  }
  return records;
}

function balancedTransformBlocks(forms) {
  const blocks = [];
  const stack = [];
  for (let index = 0; index < forms.length; index += 1) {
    const name = forms[index]?.[0];
    if (name === "push-transform") stack.push(index);
    else if (name === "pop-transform" && stack.length) blocks.push([stack.pop(), index]);
  }
  return blocks.filter(([start, end]) => !forms.slice(start + 1, end).some((form) => form?.[0] === "push-transform"));
}

function portableTransformBlocks(forms) {
  const portable = new Set(["push-transform", "pop-transform", "translate", "rotate-x", "rotate-y", "rotate-z",
    "perspective", "point3", "line3", "triangle3", "glyph"]);
  return balancedTransformBlocks(forms).filter(([start, end]) => {
    const block = forms.slice(start, end + 1);
    return block.every((form) => portable.has(form?.[0])) &&
      block.some((form) => ["point3", "line3", "triangle3", "glyph"].includes(form?.[0]));
  });
}

function mutateInteger(rng, value, low, high, extent = 17) {
  const delta = 1 + Math.floor(rng.float() * extent);
  return Math.max(low, Math.min(high, value + (rng.float() < .5 ? -delta : delta)));
}

function mutateSource(source, rng, donorSource = null, donorId = null, ecology = [], preferredFamily = null,
    preferredMutations = null) {
  const root = cloneForm(readPieceLisp(source));
  const forms = root.slice(1);
  const compiled = compilePieceLisp(source, { resolution: 64 });
  const draws = forms.map((form, index) => [form, index]).filter(([form]) => ["line3", "triangle3", "point3", "glyph"].includes(form?.[0]));
  const spatialDraws = draws.filter(([form]) => form[0] !== "glyph");
  const rotations = forms.map((form, index) => [form, index]).filter(([form]) => ["rotate-x", "rotate-y", "rotate-z"].includes(form?.[0]));
  const colors = forms.map((form, index) => [form, index]).filter(([form]) => ["clear", "pixel", "glyph", "line3", "triangle3", "point3"].includes(form?.[0]));
  const vectors = forms.map((form, index) => [form, index]).filter(([form]) => form?.[0] === "vec3");
  const depths = forms.map((form, index) => [form, index]).filter(([form]) => form?.[0] === "constant" && form[1] === "depth" && Number.isInteger(form[2]));
  const labels = new Set(forms.filter((form) => form?.[0] === "label").map((form) => form[1]));
  for (const form of forms) if (form?.[0] === "function") labels.add(form[1]);
  const symbols = new Set(forms.flat(Infinity).filter((value) => typeof value === "string"));
  const blocks = balancedTransformBlocks(forms);
  const rawLoads = forms.map((form, index) => [form, index]).filter(([form]) => form?.[0] === "load8");
  const rawStores = forms.map((form, index) => [form, index]).filter(([form]) => form?.[0] === "store8");
  const memoryPairs = rawLoads.map(([load, loadIndex]) => ({ load, loadIndex,
    stores: rawStores.filter(([store]) => store[1] === load[2]) })).filter((pair) => pair.stores.length);
  let donorForms = null, donorBlocks = [];
  if (donorSource && donorSource !== source) {
    donorForms = cloneForm(readPieceLisp(donorSource)).slice(1);
    donorBlocks = portableTransformBlocks(donorForms);
  }
  const available = ["color"];
  if (draws.length) available.push("duplicate-draw");
  if (draws.length > 1) available.push("delete-draw");
  if (rotations.length) available.push("rotate-axis", "insert-rotation");
  if (vectors.length) available.push("vector-component");
  if (depths.length) available.push("branch-depth");
  if (blocks.length) available.push("clone-child");
  if (blocks.length > 1) available.push("prune-child");
  if (spatialDraws.length && rotations.length) available.push("function-graft");
  if (forms[0]?.[0] === "jump" && spatialDraws.length && rotations.length &&
      compiled.registerCount < PIECE_VM.registers) available.push("argument-function-graft");
  if (!forms.some((form) => form?.[0] === "data") && memoryPairs.length && colors.length) available.push("data-layout");
  if (compiled.registerCount <= PIECE_VM.registers - 3 &&
      !["memory-one", "wobble-speed", "wobble"].some((name) => symbols.has(name))) {
    const main = forms.findIndex((form) => form?.[0] === "label" && form[1] === "main");
    if (main >= 0 && forms.some((form, index) => index > main && form?.[0] === "call")) available.push("memory-oscillator");
  }
  if (compiled.registerCount < PIECE_VM.registers) {
    const main = forms.findIndex((form) => form?.[0] === "label" && form[1] === "main");
    if (main >= 0 && forms.some((form, index) => index > main && form?.[0] === "clear") &&
        forms.some((form, index) => index > main && form?.[0] === "call")) available.push("sense-graft");
  }
  if (donorBlocks.length) available.push("lineage-crossover");
  const ecologyModes = [];
  for (const neighbor of ecology.slice(0, 12)) {
    const capabilities = new Set(neighbor?.capabilities || []);
    if (available.includes("argument-function-graft") &&
        ["line", "triangle", "rotate", "shift"].some((name) => capabilities.has(name))) {
      ecologyModes.push({ mode: "argument-function-graft", donor: neighbor.id,
        capability: ["triangle", "line", "rotate", "shift"].find((name) => capabilities.has(name)),
        probe: neighbor.probe || null, priority: Boolean(neighbor.priority) });
    }
    if (available.includes("data-layout") &&
        ["copy", "paste", "cellular", "flood", "box", "edges"].some((name) => capabilities.has(name))) {
      ecologyModes.push({ mode: "data-layout", donor: neighbor.id,
        capability: ["copy", "paste", "cellular", "flood", "box", "edges"].find((name) => capabilities.has(name)),
        probe: neighbor.probe || null, priority: Boolean(neighbor.priority) });
    }
  }
  if (ecologyModes.length) available.push("environment-graft");
  const priorityEcology = ecologyModes.filter((value) => value.priority);
  const requestedFamily = MUTATION_FAMILIES.has(preferredFamily) ? preferredFamily : null;
  const mutationPreferences = (Array.isArray(preferredMutations) ? preferredMutations : [preferredMutations])
    .filter((value, index, values) => KNOWN_MUTATIONS.has(value) && values.indexOf(value) === index &&
      (!requestedFamily || pieceVmMutationFamily(value) === requestedFamily));
  const preferredMutation = mutationPreferences[0] || null;
  const requestedMutation = mutationPreferences.find((value) => available.includes(value)) || null;
  const preferred = requestedFamily ? available.filter((value) => pieceVmMutationFamily(value) === requestedFamily) : [];
  let mutation = priorityEcology.length ? "environment-graft" :
    requestedMutation ? requestedMutation :
      pick(rng, preferred.length ? preferred : available), ecological = null;
  if (mutation === "environment-graft") {
    ecological = pick(rng, priorityEcology.length ? priorityEcology : ecologyModes);
    mutation = ecological.mode;
  }

  if (mutation === "color") {
    const [form] = pick(rng, colors);
    const start = form[0] === "clear" ? 2 : form.length - 3;
    const channel = start + Math.floor(rng.float() * 3);
    form[channel] = mutateInteger(rng, form[channel], 0, 255, 63);
  } else if (mutation === "duplicate-draw") {
    const [, index] = pick(rng, draws);
    forms.splice(index + 1, 0, cloneForm(forms[index]));
  } else if (mutation === "delete-draw") {
    forms.splice(pick(rng, draws)[1], 1);
  } else if (mutation === "rotate-axis") {
    const [form] = pick(rng, rotations);
    form[0] = pick(rng, ["rotate-x", "rotate-y", "rotate-z"].filter((name) => name !== form[0]));
  } else if (mutation === "insert-rotation") {
    const [form] = pick(rng, rotations);
    const calls = forms.map((candidate, index) => [candidate, index]).filter(([candidate]) => candidate?.[0] === "call");
    if (!calls.length) return mutateSource(source, rng, donorSource, donorId, ecology, preferredFamily, mutationPreferences);
    forms.splice(pick(rng, calls)[1], 0, cloneForm(form));
  } else if (mutation === "vector-component") {
    const [form] = pick(rng, vectors);
    const component = 2 + Math.floor(rng.float() * 3);
    const value = form[component];
    if (Array.isArray(value) && value[0] === "ratio") value[1] = mutateInteger(rng, value[1], -16, 16, 3);
    else if (Number.isInteger(value)) form[component] = ["ratio", mutateInteger(rng, value * 8, -16, 16, 3), 8];
  } else if (mutation === "branch-depth") {
    const [form] = pick(rng, depths);
    form[2] = mutateInteger(rng, form[2], 2, 8, 2);
  } else if (mutation === "clone-child") {
    const [start, end] = pick(rng, blocks);
    forms.splice(end + 1, 0, ...forms.slice(start, end + 1).map(cloneForm));
  } else if (mutation === "prune-child") {
    const [start, end] = pick(rng, blocks);
    forms.splice(start, end - start + 1);
  } else if (mutation === "function-graft") {
    const [draw, drawIndex] = pick(rng, spatialDraws);
    const [rotation] = pick(rng, rotations);
    let ordinal = 1;
    while (labels.has(`graft${ordinal}`)) ordinal += 1;
    const label = `graft${ordinal}`;
    forms.splice(drawIndex, 1, ["call", label]);
    forms.splice(1, 0,
      ["label", label], ["push-transform"], cloneForm(rotation), cloneForm(draw), ["pop-transform"], ["return"]);
  } else if (mutation === "argument-function-graft") {
    const desiredDraw = ecological?.capability === "line" ? "line3" : ecological?.capability === "triangle" ? "triangle3" : null;
    const compatibleDraws = desiredDraw ? spatialDraws.filter(([form]) => form[0] === desiredDraw) : spatialDraws;
    const [draw, drawIndex] = pick(rng, compatibleDraws.length ? compatibleDraws : spatialDraws);
    const [rotation] = pick(rng, rotations);
    let ordinal = 1;
    while (labels.has(`arg${ordinal}`)) ordinal += 1;
    const label = `arg${ordinal}`;
    forms.splice(drawIndex, 1, ["call", label, rotation[1]]);
    forms.splice(1, 0, ["function", label, ["angle"],
      ["push-transform"], [rotation[0], "angle"], cloneForm(draw), ["pop-transform"], ["return"]]);
  } else if (mutation === "data-layout") {
    const pair = pick(rng, memoryPairs);
    let ordinal = 1;
    while (symbols.has(`region${ordinal}`)) ordinal += 1;
    const region = `region${ordinal}`;
    forms[pair.loadIndex] = ["read8", pair.load[1], region, pair.load[2]];
    for (const [, storeIndex] of pair.stores) {
      const store = forms[storeIndex];
      forms[storeIndex] = ["write8", region, store[1], store[2]];
    }
    forms.splice(1, 0, ["data", region, 256]);
    const [colorForm] = pick(rng, colors);
    const start = colorForm[0] === "clear" ? 2 : colorForm.length - 3;
    const channel = start + Math.floor(rng.float() * 3), previous = colorForm[channel];
    colorForm[channel] = mutateInteger(rng, previous, 0, 255, 15);
    if (colorForm[channel] === previous) colorForm[channel] = previous === 255 ? 254 : previous + 1;
  } else if (mutation === "memory-oscillator") {
    const main = forms.findIndex((form) => form?.[0] === "label" && form[1] === "main");
    const call = forms.map((form, index) => [form, index])
      .filter(([form, index]) => index > main && form?.[0] === "call").at(-1)[1];
    const setupAt = forms.findIndex((form, index) => index > main && form?.[0] === "clear");
    const denominator = pick(rng, [128, 192, 256, 384, 512]);
    forms.splice(setupAt, 0,
      ["constant", "memory-one", 1], ["constant", "wobble-speed", ["ratio", 1, denominator]],
      ["load8", "wobble", "memory-one"], ["add", "wobble", "wobble", "one"],
      ["store8", "memory-one", "wobble"], ["mul", "wobble", "wobble", "wobble-speed"]);
    const shiftedCall = call + 6;
    forms.splice(shiftedCall, 0, [pick(rng, ["rotate-x", "rotate-y", "rotate-z"]), "wobble"]);
  } else if (mutation === "sense-graft") {
    const main = forms.findIndex((form) => form?.[0] === "label" && form[1] === "main");
    const call = forms.map((form, index) => [form, index])
      .filter(([form, index]) => index > main && form?.[0] === "call").at(-1)[1];
    const setupAt = forms.findIndex((form, index) => index > main && form?.[0] === "clear");
    let ordinal = 1;
    while (symbols.has(`sense${ordinal}`)) ordinal += 1;
    const name = `sense${ordinal}`;
    forms.splice(setupAt, 0, ["sense8", name, pick(rng, ["beat", "bar", "fringe"])]);
    forms.splice(call + 1, 0, [pick(rng, ["rotate-x", "rotate-y", "rotate-z"]), name]);
  } else if (mutation === "lineage-crossover") {
    const [start, end] = pick(rng, donorBlocks);
    const main = forms.findIndex((form) => form?.[0] === "label" && form[1] === "main");
    const calls = forms.map((form, index) => [form, index])
      .filter(([form, index]) => index > main && form?.[0] === "call");
    if (!calls.length) return mutateSource(source, rng, donorSource, donorId, ecology, preferredFamily, mutationPreferences);
    const graft = donorForms.slice(start, end + 1).map(cloneForm);
    const graftDraws = graft.filter((form) => ["point3", "line3", "triangle3", "glyph"].includes(form?.[0]));
    const draw = pick(rng, graftDraws), channel = draw.length - 3 + Math.floor(rng.float() * 3);
    const previous = draw[channel];
    draw[channel] = mutateInteger(rng, previous, 0, 255, 31);
    if (draw[channel] === previous) draw[channel] = previous === 255 ? 254 : previous + 1;
    forms.splice(calls.at(-1)[1] + 1, 0, ...graft);
  }
  root.splice(1, root.length - 1, ...forms);
  const recordedMutation = ecological ? "environment-graft" : mutation;
  const causalRequest = requestedMutation || recordedMutation;
  return { source: printForm(root), mutation: recordedMutation,
    operatorFamily: pieceVmMutationFamily(recordedMutation), requestedOperatorFamily: requestedFamily,
    preferredMutation, requestedMutation: causalRequest,
    compatibilityFallback: Boolean(preferredMutation && preferredMutation !== causalRequest),
    donor: mutation === "lineage-crossover" ? donorId : null,
    environmentDonor: ecological?.donor || null, environmentCapability: ecological?.capability || null,
    environmentProbe: ecological?.probe || null };
}

function frameTraits(frames, side) {
  let temporal = 0, spatial = 0, occupied = 0;
  const palette = new Set();
  for (let frameIndex = 0; frameIndex < frames.length; frameIndex += 1) {
    const frame = frames[frameIndex], previous = frames[frameIndex - 1];
    const background = `${frame[0]},${frame[1]},${frame[2]}`;
    for (let pixel = 0; pixel < side * side; pixel += 1) {
      const at = pixel * 3, key = `${frame[at] >> 5},${frame[at + 1] >> 5},${frame[at + 2] >> 5}`;
      palette.add(key);
      if (`${frame[at]},${frame[at + 1]},${frame[at + 2]}` !== background) occupied += 1;
      if (previous) temporal += (Math.abs(frame[at] - previous[at]) + Math.abs(frame[at + 1] - previous[at + 1]) + Math.abs(frame[at + 2] - previous[at + 2])) / 765;
      if (pixel % side < side - 1) spatial += (Math.abs(frame[at] - frame[at + 3]) + Math.abs(frame[at + 1] - frame[at + 4]) + Math.abs(frame[at + 2] - frame[at + 5])) / 765;
    }
  }
  return {
    coverage: occupied / (frames.length * side * side),
    temporal: temporal / (Math.max(1, frames.length - 1) * side * side),
    spatial: spatial / (frames.length * side * (side - 1)),
    palette: Math.min(1, palette.size / 32),
  };
}

function structure(source) {
  const topLevel = readPieceLisp(source).slice(1);
  const forms = topLevel.flatMap((form) => form?.[0] === "function" ? form.slice(3) : [form]);
  const count = (names) => forms.filter((form) => names.includes(form?.[0])).length;
  const value = {
    calls: count(["call"]), branches: count(["jump", "jump-if"]),
    draws: count(["point3", "line3", "triangle3", "glyph"]),
    transforms: count(["push-transform", "pop-transform", "translate", "rotate-x", "rotate-y", "rotate-z", "perspective"]),
    memory: count(["load8", "store8", "read8", "write8"]), stack: count(["push", "pop"]),
    senses: count(["sense8"]),
    functions: topLevel.filter((form) => form?.[0] === "function").length,
    arguments: topLevel.filter((form) => form?.[0] === "function").reduce((sum, form) => sum + (Array.isArray(form[2]) ? form[2].length : 0), 0),
    layouts: topLevel.filter((form) => form?.[0] === "data").length,
    layoutBytes: topLevel.filter((form) => form?.[0] === "data").reduce((sum, form) => sum + (Number(form[2]) || 0), 0),
  };
  return { ...value, niche: `${Math.min(7, value.calls)}:${Math.min(7, value.draws)}:${Math.min(7, value.transforms)}:${Math.min(3, value.memory)}:${Math.min(3, value.stack)}:${Math.min(3, value.senses)}:${Math.min(3, value.functions + value.layouts)}` };
}

export function evaluatePieceVmSource(source, { resolution = 64, frames = EVALUATION_FRAMES } = {}) {
  const program = compilePieceLisp(source, { resolution });
  const proof = verifyPieceProgram(program);
  if (!proof.valid) throw new Error(proof.error || "PieceVM proof failed");
  let state = createPieceVmState(resolution), result;
  const rasters = [], fuel = [];
  for (let frame = 0; frame < frames; frame += 1) {
    result = runPieceVm(program, { state });
    if (result.fault) throw new Error(result.fault);
    state = result.state; fuel.push(result.fuelUsed);
    rasters.push(Buffer.from(state.buffers[state.front]));
  }
  const traits = frameTraits(rasters, resolution), shape = structure(source);
  const structural = clamp((shape.calls + shape.branches + shape.draws + shape.transforms + shape.memory + shape.stack +
    shape.senses + shape.functions + shape.arguments + shape.layouts) / 36);
  const meanFuel = fuel.reduce((sum, value) => sum + value, 0) / fuel.length;
  const score = clamp(traits.coverage * .25 + Math.min(1, traits.temporal * 24) * .25 + Math.min(1, traits.spatial * 8) * .15 + traits.palette * .10 + structural * .25 - meanFuel / 2_000_000);
  return Object.freeze({
    id: hash(program.bytecode).slice(0, 12), source: program.source, program,
    proof, traits: Object.freeze(traits), structure: Object.freeze(shape),
    score, meanFuel, frameHashes: Object.freeze(rasters.map((frame) => createHash("sha256").update(frame).digest("hex"))),
  });
}

export function rankPieceVmCandidates(candidates, residents) {
  const occupied = new Set((residents || []).map((value) => value?.structure?.niche).filter(Boolean));
  const residentMutations = new Set((residents || []).map((value) => value?.mutation).filter(Boolean));
  return [...(candidates || [])].sort((left, right) => {
    const capabilityMutations = new Set(["lineage-crossover", "argument-function-graft", "data-layout", "environment-graft"]);
    const leftCapability = capabilityMutations.has(left?.mutation) && !residentMutations.has(left.mutation) ? 1 : 0;
    const rightCapability = capabilityMutations.has(right?.mutation) && !residentMutations.has(right.mutation) ? 1 : 0;
    const leftNovel = occupied.has(left?.structure?.niche) ? 0 : 1;
    const rightNovel = occupied.has(right?.structure?.niche) ? 0 : 1;
    return rightCapability - leftCapability || rightNovel - leftNovel ||
      Number(right?.score || 0) - Number(left?.score || 0) ||
      Number(left?.iteration || 0) - Number(right?.iteration || 0);
  });
}

export class PieceVmNursery {
  constructor({ seed = "piece-vm-nursery-v1", foundingSource, stored = null } = {}) {
    if (!foundingSource) throw new TypeError("PieceVM nursery requires founding source");
    this.seed = seed; this.rng = stored?.rng ? Prng.fromJSON(stored.rng) : new Prng(seed);
    this.iteration = Number(stored?.iteration) || 0;
    this.accepted = Number(stored?.accepted) || 0; this.rejected = Number(stored?.rejected) || 0;
    this.lineage = rehydrateCapabilityLineage((stored?.lineage || stored?.residents || [])
      .filter((value) => value?.source)
      .map((value) => ({ ...value, structure: structure(value.source),
        selectionEvidence: normalizeSelectionEvidence(value.selectionEvidence, value.parent),
        curriculumEvidence: normalizeCurriculumEvidence(value.curriculumEvidence, value.parent) })).slice(-MAX_LINEAGE));
    const lineageById = new Map(this.lineage.map((value) => [value.id, value]));
    this.residents = (stored?.residents || []).filter((value) => value?.source)
      .map((value) => ({ ...value, structure: structure(value.source),
        selectionEvidence: normalizeSelectionEvidence(value.selectionEvidence, value.parent),
        curriculumEvidence: normalizeCurriculumEvidence(value.curriculumEvidence, value.parent),
        capabilityLineage: mergeCapabilityLineage(
          lineageById.get(value.id)?.capabilityLineage || [], value.capabilityLineage || []) }))
      .slice(-MAX_RESIDENTS);
    this.championId = stored?.championId || null;
    const founding = evaluatePieceVmSource(foundingSource);
    const foundingRecord = this.#stored(founding, { mutation: "foundation", parent: null, generation: 0, native: null });
    if (!this.lineage.some((value) => value.id === founding.id)) this.lineage.unshift(foundingRecord);
    if (this.lineage.length > MAX_LINEAGE) this.lineage.splice(0, this.lineage.length - MAX_LINEAGE);
    if (!this.residents.length) {
      this.residents.push(foundingRecord);
      this.championId = founding.id; this.accepted += 1;
    }
  }

  static fromJSON(value, options) {
    return new PieceVmNursery({ ...options, seed: value?.seed || options?.seed, stored: value });
  }

  #stored(candidate, lineage) {
    return {
      id: candidate.id, source: candidate.source, bytecode: candidate.program.bytecode,
      bytecodeHash: candidate.program.bytecodeHash, instructionCount: candidate.program.instructionCount,
      registerCount: candidate.program.registerCount, score: candidate.score, meanFuel: candidate.meanFuel,
      traits: candidate.traits, structure: candidate.structure, frameHashes: candidate.frameHashes,
      proof: candidate.proof, ...lineage,
      capabilityLineage: mergeCapabilityLineage(lineage.capabilityLineage || []),
      selectionEvidence: normalizeSelectionEvidence(lineage.selectionEvidence, lineage.parent),
      curriculumEvidence: normalizeCurriculumEvidence(lineage.curriculumEvidence, lineage.parent),
    };
  }

  get champion() {
    return this.residents.find((value) => value.id === this.championId) || this.residents.at(-1);
  }

  propose(parentId = this.championId, ecology = [], { operatorFamily = null, mutation = null, mutations = null } = {}) {
    const parent = this.residents.find((value) => value.id === parentId) || this.champion;
    const possibleDonors = this.lineage.filter((value) => value.id !== parent.id && value.source);
    const donor = possibleDonors.length ? pick(this.rng, possibleDonors) : null;
    this.iteration += 1;
    try {
      const edit = mutateSource(parent.source, this.rng, donor?.source, donor?.id, ecology, operatorFamily,
        mutations || mutation);
      const candidate = evaluatePieceVmSource(edit.source);
      const behaviorChanged = candidate.frameHashes.some((value, index) => value !== parent.frameHashes[index]);
      if (!behaviorChanged) this.rejected += 1;
      const directCapability = capabilityRecord(edit.environmentDonor, edit.environmentCapability, edit.environmentProbe);
      return { ...candidate, mutation: edit.mutation, parent: parent.id,
        operatorFamily: edit.operatorFamily, requestedOperatorFamily: edit.requestedOperatorFamily,
        preferredMutation: edit.preferredMutation, requestedMutation: edit.requestedMutation,
        compatibilityFallback: edit.compatibilityFallback,
        donor: edit.donor, environmentDonor: edit.environmentDonor,
        environmentCapability: edit.environmentCapability,
        environmentProbe: edit.environmentProbe,
        capabilityLineage: mergeCapabilityLineage(parent.capabilityLineage || [],
          edit.donor === donor?.id ? donor.capabilityLineage || [] : [], directCapability ? [directCapability] : []),
        generation: (parent.generation || 0) + 1,
        iteration: this.iteration, behaviorChanged };
    } catch (error) {
      this.rejected += 1;
      return { error: String(error?.message || error), parent: parent.id, iteration: this.iteration };
    }
  }

  admit(candidate, native) {
    if (!candidate?.program || !candidate.behaviorChanged || !native?.valid || native.bytecodeHash !== candidate.program.bytecodeHash || native.frameHashes.join(":") !== candidate.frameHashes.slice(0, native.frameHashes.length).join(":")) {
      this.rejected += 1; return null;
    }
    const incumbent = this.residents.find((value) => value.structure.niche === candidate.structure.niche);
    if (incumbent && incumbent.score > candidate.score * 1.08) { this.rejected += 1; return null; }
    const stored = this.#stored(candidate, { mutation: candidate.mutation, parent: candidate.parent,
      donor: candidate.donor || null, environmentDonor: candidate.environmentDonor || null,
      environmentCapability: candidate.environmentCapability || null, generation: candidate.generation,
      environmentProbe: candidate.environmentProbe || null,
      operatorFamily: candidate.operatorFamily || pieceVmMutationFamily(candidate.mutation),
      requestedOperatorFamily: candidate.requestedOperatorFamily || null,
      preferredMutation: candidate.preferredMutation || null,
      requestedMutation: candidate.requestedMutation || null,
      compatibilityFallback: Boolean(candidate.compatibilityFallback),
      capabilityLineage: candidate.capabilityLineage || [],
      selectionEvidence: candidate.selectionEvidence || null,
      curriculumEvidence: candidate.curriculumEvidence || null,
      iteration: candidate.iteration, behaviorChanged: true, native });
    if (incumbent) this.residents.splice(this.residents.indexOf(incumbent), 1);
    this.residents.push(stored);
    if (this.residents.length > MAX_RESIDENTS) this.residents.splice(0, this.residents.length - MAX_RESIDENTS);
    if (!this.lineage.some((value) => value.id === stored.id)) this.lineage.push(stored);
    if (this.lineage.length > MAX_LINEAGE) this.lineage.splice(0, this.lineage.length - MAX_LINEAGE);
    this.championId = stored.id; this.accepted += 1;
    return stored;
  }

  snapshot() {
    return { schema: 1, seed: this.seed, iteration: this.iteration, accepted: this.accepted, rejected: this.rejected,
      championId: this.championId, champion: this.champion,
      residents: this.residents.map((value) => ({ ...value })),
      lineage: this.lineage.map((value) => ({ ...value })), rng: this.rng.toJSON() };
  }

  toJSON() {
    const value = this.snapshot();
    delete value.champion;
    return value;
  }
}
