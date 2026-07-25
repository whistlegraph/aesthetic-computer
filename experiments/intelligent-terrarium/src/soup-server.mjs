#!/usr/bin/env node
import { createServer } from "node:http";
import { createHash } from "node:crypto";
import { execFile } from "node:child_process";
import { appendFile, mkdir, mkdtemp, readFile, rm } from "node:fs/promises";
import { tmpdir } from "node:os";
import { dirname, extname, join, resolve } from "node:path";
import { fileURLToPath } from "node:url";
import { promisify } from "node:util";
import { NoveltyArchive } from "./sort-soup.mjs";
import { SoupHistory } from "./soup-history.mjs";
import { SpecimenLedger } from "./specimen-ledger.mjs";
import { VisualCurator } from "./visual-curator.mjs";
import { EvaluationPool } from "./evaluation-pool.mjs";
import { GROOVE_LAYOUT, hardwareProfile, inspectPixelGroove, printPixelGroove, readGrooveInstruction } from "./pixel-groove.mjs";
import { compilePieceLisp } from "./piece-vm.mjs";
import { evaluatePieceVmSource, PieceVmNursery, rankPieceVmCandidates } from "./piece-vm-nursery.mjs";
import { PieceVmPhenotypeOracle } from "./piece-vm-phenotype.mjs";
import { PieceVmPolicyBandit } from "./piece-vm-policy-bandit.mjs";
import { PieceVmOperatorBandit } from "./piece-vm-operator-bandit.mjs";
import { PieceVmOutcomeModel } from "./piece-vm-outcome-model.mjs";
import { PieceVmCurriculum, pieceVmCurriculumEvidence, pieceVmDevelopment,
  pieceVmCurriculumParent, pieceVmCurriculumTarget, prioritizePieceVmCurriculum } from "./piece-vm-curriculum.mjs";

const here = dirname(fileURLToPath(import.meta.url));
const defaultWebRoot = resolve(here, "../web");
const runFile = promisify(execFile);
const pieceVmCanarySource = await readFile(resolve(here, "../examples/piece-vm-canary.lisp"), "utf8");
const pieceVmCanary = compilePieceLisp(pieceVmCanarySource, { resolution: 64 });
const LOOPBACK = new Set(["127.0.0.1", "::1"]);
const MARGIN_RIM_PIXELS = 556;
const MARGIN_FRINGE_RIM_END = GROOVE_LAYOUT.tracks.fringe.base + MARGIN_RIM_PIXELS;
const RASTER_OPCODE_NAMES = Object.freeze([
  null, "add", "xor", "shift", "mix", "solarize", "blur", "edges", "rotate", "mirror",
  "channels", "and", "or", "line", "triangle", "flood", "box", "copy", "paste", "cellular",
]);
const STATIC = new Map([
  ["/", "soup.html"], ["/soup", "soup.html"], ["/board", "board.html"],
  ["/soup.mjs", "soup.mjs"], ["/board.mjs", "board.mjs"], ["/soup.css", "soup.css"],
]);

function json(response, status, value) {
  const data = Buffer.from(JSON.stringify(value));
  response.writeHead(status, {
    "content-type": "application/json; charset=utf-8",
    "content-length": data.length,
    "cache-control": "no-store",
    "x-content-type-options": "nosniff",
  });
  response.end(data);
}

async function body(request, limit = 4096) {
  const chunks = [];
  let size = 0;
  for await (const chunk of request) {
    size += chunk.length;
    if (size > limit) throw new Error("request body too large");
    chunks.push(chunk);
  }
  return JSON.parse(Buffer.concat(chunks).toString("utf8") || "{}");
}

function bearer(request) {
  const match = /^Bearer\s+(.+)$/i.exec(request.headers.authorization || "");
  return match?.[1] || null;
}

function contentType(filename) {
  if (extname(filename) === ".html") return "text/html; charset=utf-8";
  if (extname(filename) === ".css") return "text/css; charset=utf-8";
  return "text/javascript; charset=utf-8";
}

function nativeField(value) {
  return String(value ?? "").replace(/[\t\r\n]/g, " ");
}

function resampleRgbHex(hex, sourceWidth, sourceHeight, targetWidth = 128, targetHeight = 128) {
  const source = Buffer.from(String(hex || ""), "hex");
  if (source.length !== sourceWidth * sourceHeight * 3) return "";
  if (sourceWidth === targetWidth && sourceHeight === targetHeight) return source.toString("hex");
  const target = Buffer.alloc(targetWidth * targetHeight * 3);
  for (let y = 0; y < targetHeight; y += 1) for (let x = 0; x < targetWidth; x += 1) {
    const sx = Math.min(sourceWidth - 1, Math.floor(x * sourceWidth / targetWidth));
    const sy = Math.min(sourceHeight - 1, Math.floor(y * sourceHeight / targetHeight));
    const from = (sy * sourceWidth + sx) * 3, to = (y * targetWidth + x) * 3;
    target[to] = source[from]; target[to + 1] = source[from + 1]; target[to + 2] = source[from + 2];
  }
  return target.toString("hex");
}

function median(values) {
  const sorted = [...values].sort((a, b) => a - b);
  if (!sorted.length) return 0;
  const middle = Math.floor(sorted.length / 2);
  return sorted.length % 2 ? sorted[middle] : (sorted[middle - 1] + sorted[middle]) / 2;
}

function addressDistance(a, b) {
  if (!/^[A-D][1-3]$/.test(a || "") || !/^[A-D][1-3]$/.test(b || "")) return 8;
  return Math.abs(a.charCodeAt(0) - b.charCodeAt(0)) + Math.abs(Number(a[1]) - Number(b[1]));
}

// Sixty-second population-relative lifecycle windows. Red is descriptive only:
// a resident is prodded when it is a sustained statistical outlier, and is
// retired only after several distinct memory interventions fail.
export class PerformanceOracle {
  constructor({ now = Date.now, windowMs = 60_000, terminalWindowMs = 5_000, random = Math.random, maxInterventions = 3 } = {}) {
    this.now = now;
    this.windowMs = windowMs;
    this.terminalWindowMs = terminalWindowMs;
    this.random = random;
    this.maxInterventions = maxInterventions;
    this.tracks = new Map();
  }

  #resetWindow(track, at) {
    track.since = at; track.samples = 0; track.lowSamples = 0; track.redSamples = 0;
    track.yellowSamples = 0; track.healthySamples = 0; track.hpTotal = 0;
    track.hpMean = 0; track.hpM2 = 0; track.hpMin = Infinity; track.hpMax = -Infinity;
  }

  #donor(target, rows) {
    const donors = rows.filter((row) => row.id !== target.id && row.hp > target.hp + 5);
    if (!donors.length) return null;
    const weighted = donors.map((row) => {
      const distance = addressDistance(target.address, row.address);
      const neighborBias = distance === 1 ? 6 : 1 / Math.max(1, distance);
      return { row, weight: neighborBias * (1 + row.hp / 25) };
    });
    // A small distant-graft chance keeps the topology from becoming parochial.
    if (this.random() < .12) return donors[Math.floor(this.random() * donors.length)];
    let roll = this.random() * weighted.reduce((sum, value) => sum + value.weight, 0);
    for (const value of weighted) {
      roll -= value.weight;
      if (roll <= 0) return value.row;
    }
    return weighted.at(-1).row;
  }

  ingest(rows, interventionCounts = new Map(), displayFps = 0) {
    const at = this.now();
    const eligible = rows.filter((row) => Number.isFinite(row.hp));
    if (eligible.length < 4) return { action: null, metrics: [] };
    const center = median(eligible.map((row) => row.hp));
    const mad = median(eligible.map((row) => Math.abs(row.hp - center)));
    const cutoff = center - Math.max(5, mad * 1.5);
    const metrics = [];
    let action = null;
    for (const row of eligible) {
      let track = this.tracks.get(row.id);
      if (!track) {
        track = { since: at, samples: 0, lowSamples: 0, redSamples: 0, yellowSamples: 0, hpTotal: 0,
          healthySamples: 0, hpMean: 0, hpM2: 0, hpMin: Infinity, hpMax: -Infinity,
          terminalSince: null, terminalSamples: 0,
          interventions: interventionCounts.get(row.id) || 0, lastIntervention: false };
        this.tracks.set(row.id, track);
      }
      const low = row.hp < cutoff;
      track.samples += 1;
      track.lowSamples += low ? 1 : 0;
      track.redSamples += row.hp < 38 ? 1 : 0;
      track.yellowSamples += row.hp >= 38 && row.hp < 68 ? 1 : 0;
      track.healthySamples += row.hp >= 82 ? 1 : 0;
      track.hpTotal += row.hp;
      const hpDelta = row.hp - track.hpMean;
      track.hpMean += hpDelta / track.samples;
      track.hpM2 += hpDelta * (row.hp - track.hpMean);
      track.hpMin = Math.min(track.hpMin, row.hp);
      track.hpMax = Math.max(track.hpMax, row.hp);
      const terminal = row.hp < 10 && row.failedReprobes >= 3;
      if (terminal) {
        if (track.terminalSince === null) track.terminalSince = at;
        track.terminalSamples += 1;
      } else {
        track.terminalSince = null; track.terminalSamples = 0;
      }
      const strikes = Math.round(track.lowSamples / track.samples * 100);
      const elapsed = at - track.since;
      metrics.push({ id: row.id, hp: row.hp, life: row.life, traits: row.traits,
        strikes, cutoff, median: center, mad, rubricVersion: 2, samples: track.samples,
        elapsedMs: elapsed, healthMean: track.hpMean,
        healthStdDev: Math.sqrt(track.hpM2 / Math.max(1, track.samples - 1)),
        healthRange: track.hpMax - track.hpMin,
        healthyRatio: track.healthySamples / Math.max(1, track.samples) });
      const redRatio = track.redSamples / Math.max(1, track.samples);
      if (!action && terminal && track.terminalSamples >= 2 && at - track.terminalSince >= this.terminalWindowMs) {
        action = { type: "cull", id: row.id, hp: row.hp, averageHp: track.hpTotal / track.samples,
          cutoff, reason: "terminal-after-failed-reprobes" };
        continue;
      }
      if (!action && row.ageMs >= this.windowMs / 2 && elapsed >= this.windowMs / 2 &&
          redRatio >= .7 && track.interventions < 1) {
        const donor = this.#donor(row, eligible);
        const roll = this.random();
        const strategy = donor && roll < .55 ? "copy" : roll < .82 ? "organized" : "self";
        track.interventions += 1; track.lastIntervention = true;
        action = { type: "prod", id: row.id, hp: row.hp, averageHp: track.hpTotal / track.samples,
          cutoff, strategy, donorId: strategy === "copy" ? donor.id : null };
        this.#resetWindow(track, at);
        continue;
      }
      if (action || row.ageMs < this.windowMs || elapsed < this.windowMs) continue;
      const lowRatio = track.lowSamples / Math.max(1, track.samples);
      const yellowRatio = track.yellowSamples / Math.max(1, track.samples);
      const averageHp = track.hpTotal / Math.max(1, track.samples);
      if (redRatio >= .6 && averageHp < 38) {
        action = { type: "cull", id: row.id, hp: row.hp, averageHp, cutoff,
          reason: "sustained-red-health" };
      } else if (yellowRatio >= .6 && displayFps >= 120) {
        action = { type: "fork", id: row.id, hp: row.hp, averageHp, cutoff, displayFps };
      } else if (lowRatio >= .6 && averageHp < cutoff) {
        const donor = this.#donor(row, eligible);
        const roll = this.random();
        const strategy = donor && roll < .55 ? "copy" : roll < .82 ? "organized" : "self";
        track.interventions += 1; track.lastIntervention = true;
        action = { type: "prod", id: row.id, hp: row.hp, averageHp, cutoff,
          strategy, donorId: strategy === "copy" ? donor.id : null };
      } else if (track.lastIntervention) {
        action = { type: "recovered", id: row.id, hp: row.hp, averageHp, cutoff };
        track.lastIntervention = false;
      }
      this.#resetWindow(track, at);
    }
    return { action, metrics };
  }
}

// Compact, bounded text membrane for the native SDL renderer. The browser
// JSON snapshot intentionally carries full verification traces; the display
// only needs truthful identities, scores, and one real sample vector.
const PIECE_VM_ECOLOGY_CAPABILITIES = new Set([
  "line", "triangle", "rotate", "shift", "copy", "paste", "cellular", "flood", "box", "edges",
]);

export function pieceVmEcology(programs = []) {
  return (programs || []).slice(0, 12).map((program) => {
    const source = String(program?.source || "").slice(0, 4096);
    const capabilities = [...source.matchAll(/\(([a-z][a-z0-9-]*)\b/g)]
      .map((match) => match[1]).filter((name) => PIECE_VM_ECOLOGY_CAPABILITIES.has(name));
    return capabilities.length ? { id: String(program.id || "").slice(0, 24),
      address: String(program.address || "").slice(0, 3), capabilities: [...new Set(capabilities)] } : null;
  }).filter((value) => value?.id);
}

function visibleMarginAddress(address) {
  return Number.isInteger(address) && ((address >= 0 && address < MARGIN_RIM_PIXELS) ||
    (address >= GROOVE_LAYOUT.tracks.fringe.base && address < MARGIN_FRINGE_RIM_END));
}

function rimCoordinate(address) {
  const layer = address >= GROOVE_LAYOUT.tracks.fringe.base ? "fringe" : "core";
  const index = layer === "fringe" ? address - GROOVE_LAYOUT.tracks.fringe.base : address;
  if (index < 160) return { layer, index, side: "top", offset: index };
  if (index < 279) return { layer, index, side: "right", offset: index - 159 };
  if (index < 438) return { layer, index, side: "bottom", offset: index - 279 };
  return { layer, index, side: "left", offset: index - 437 };
}

export function decodeMarginProbe(program, address, { at = Date.now(), requestedBy = "loopback" } = {}) {
  if (!program?.id || !visibleMarginAddress(address)) throw new RangeError("probe address is not visible on the fixed rim");
  const encoded = String(program.sample?.groove || "");
  if (encoded.length !== GROOVE_LAYOUT.bytes * 2 || !/^[a-f0-9]+$/i.test(encoded)) throw new Error("resident has no valid Groove");
  const groove = new Uint8Array(Buffer.from(encoded, "hex"));
  const inspected = inspectPixelGroove(groove);
  if (!inspected.valid || inspected.id !== program.id) throw new Error("resident Groove failed protected validation");
  const [trackName, track] = Object.entries(GROOVE_LAYOUT.tracks)
    .find(([, value]) => address >= value.base && address < value.base + value.pixels) || [];
  if (!track) throw new RangeError("probe address is outside Groove memory");
  let opcode = null;
  if (trackName === "sequence") {
    const instruction = readGrooveInstruction(groove, Math.floor((address - track.base) / 8));
    opcode = instruction?.[0] || null;
  } else if (trackName === "functions") {
    opcode = Math.floor((address - track.base) / 3) + 1;
  } else if (trackName === "bodies") {
    opcode = Math.floor((address - track.base) / 16) + 1;
  }
  const decoded = RASTER_OPCODE_NAMES[opcode] || null;
  const capability = PIECE_VM_ECOLOGY_CAPABILITIES.has(decoded) ? decoded : null;
  const byte = address * 3;
  return Object.freeze({
    id: String(program.id).slice(0, 24), displayAddress: String(program.address || "").slice(0, 3),
    at: Number(at), address, ...rimCoordinate(address), track: trackName,
    protected: Boolean(track.protected), rgb: Object.freeze([...groove.slice(byte, byte + 3)]),
    opcode, operation: decoded, capability,
    requestedBy: String(requestedBy || "loopback").replace(/[^a-z0-9:_-]/gi, "").slice(0, 24) || "loopback",
    protectedHash: inspected.protectedHash,
  });
}

export function pieceVmEcologyWithProbe(programs = [], probe = null) {
  const ecology = pieceVmEcology(programs);
  if ((probe?.status && probe.status !== "pending") || !probe?.capability ||
      !PIECE_VM_ECOLOGY_CAPABILITIES.has(probe.capability)) return ecology;
  const resident = ecology.find((value) => value.id === probe.id);
  if (!resident || !resident.capabilities.includes(probe.capability)) return ecology;
  const focused = { ...resident, capabilities: [probe.capability], probe: {
    address: probe.address, track: probe.track, requestedBy: probe.requestedBy, at: probe.at,
  }, priority: true };
  return [focused, ...ecology.filter((value) => value.id !== resident.id)];
}

export function pieceVmCarriesProbe(record, probe) {
  return Boolean(record?.capabilityLineage?.some((capability) =>
    capability.donor === probe?.id && capability.capability === probe?.capability &&
    capability.probeAt === probe?.at && capability.probeAddress === probe?.address));
}

export function nativeSnapshot(state, pieceVmPrograms = pieceVmCanary) {
  const checkpoint = state.checkpoint || {};
  const raster = state.domains?.raster || {};
  const memory = state.memory || {};
  const curation = state.runtime?.visualCuration?.persisted || {};
  const recommendations = curation.recommendations || {};
  const history = state.history || {};
  const pieceVm = state.runtime?.pieceVm || {};
  const pieceVmChampion = pieceVm.champion || {};
  const pieceVmSelection = pieceVm.selection || {};
  const pieceVmPhenotype = (pieceVm.phenotypes || []).find((value) => value.id === pieceVmSelection.parentId) || {};
  const pieceVmPolicy = pieceVm.policyBandit || {};
  const policy = (name) => (pieceVmPolicy.policies || []).find((value) => value.policy === name) || {};
  const phenotypePolicy = policy("phenotype-lead"), championPolicy = policy("champion-control");
  const diversityPolicy = policy("branch-diversity");
  const pieceVmOperator = pieceVm.operatorBandit || {};
  const operator = (name) => (pieceVmOperator.policies || []).find((value) => value.policy === name) || {};
  const variationOperator = operator("variation"), machineryOperator = operator("machinery");
  const exchangeOperator = operator("exchange");
  const pieceVmOutcome = pieceVm.outcomeModel || {};
  const pieceVmCurriculum = pieceVm.curriculum || {};
  const pieceVmDevelopmentState = pieceVmCurriculum.champion || pieceVmChampion.development || {};
  const pieceVmEcology = pieceVmChampion.environmentCapability ? pieceVmChampion : (pieceVm.lastEnvironmentGraft || {});
  const marginProbe = state.runtime?.marginProbe || {};
  const assignments = Array.isArray(pieceVmPrograms) ? pieceVmPrograms : [
    { address: "A1", resolution: 64, role: "canary", program: pieceVmPrograms },
    ...(pieceVmPrograms?.native?.profiles?.standard?.valid
      ? [{ address: "B1", resolution: 128, role: "canary", program: pieceVmPrograms }] : []),
    ...(pieceVmPrograms?.native?.profiles?.double?.valid
      ? [{ address: "C1", resolution: 256, role: "canary", program: pieceVmPrograms }] : []),
  ];
  const assignmentByAddress = new Map(assignments.map((value) => [value.address, value]));
  const lines = [
    ["S", state.iteration, state.accepted, state.rejected, state.coverage,
      state.capacity, state.selected || "", checkpoint.nextIteration || 0,
      checkpoint.iterationsRemaining || 0, checkpoint.estimatedMs || 0,
      raster.coverage || 0, raster.capacity || 0, memory.residentBytes || 0,
      memory.activeReads || 0, memory.activeWrites || 0,
      Number(state.runtime?.evaluationsPerSecond || 0).toFixed(2),
      Math.round(state.runtime?.utcMs || Date.now()), Number(state.runtime?.musicalBpm || 60),
      state.runtime?.clockSynced ? 1 : 0, curation.reviews || 0,
      recommendations.retain || 0, recommendations.watch || 0,
      recommendations.reject || 0, history.editions || 0,
      history.shortHead || "", history.lastEdition?.iteration || 0,
      pieceVmChampion.generation || 0, nativeField(pieceVmChampion.mutation || "foundation"),
      pieceVm.accepted || 0, pieceVm.rejected || 0, pieceVm.lineage || 0,
      nativeField(pieceVmChampion.id || ""),
      pieceVmChampion.registerCount || 0, pieceVmChampion.structure?.calls || 0,
      pieceVmChampion.structure?.memory || 0, pieceVmChampion.structure?.senses || 0,
      Number(pieceVmChampion.score || 0).toFixed(4), pieceVmChampion.native?.valid ? 1 : 0,
      pieceVmChampion.native?.profiles?.standard?.valid ? 1 : 0,
      pieceVmChampion.native?.profiles?.double?.valid ? 1 : 0,
      pieceVm.crossovers || 0,
      pieceVmChampion.structure?.functions || 0, pieceVmChampion.structure?.arguments || 0,
      pieceVmChampion.structure?.layouts || 0, pieceVmChampion.structure?.layoutBytes || 0,
      nativeField(pieceVmEcology.environmentCapability || ""), nativeField(pieceVmEcology.environmentDonor || ""),
      nativeField(marginProbe.id || ""), Number.isInteger(marginProbe.address) ? marginProbe.address : -1,
      nativeField(marginProbe.track || ""), nativeField(marginProbe.capability || ""),
      nativeField(marginProbe.requestedBy || ""),
      nativeField(marginProbe.status || ""), nativeField(marginProbe.descendantId || ""),
      marginProbe.attempts || 0, nativeField(marginProbe.descendant?.state || ""),
      marginProbe.descendant?.descendants || 0, marginProbe.descendant?.generation || 0,
      marginProbe.descendant?.children || 0,
      marginProbe.descendant?.propagation?.descendants || 0,
      marginProbe.descendant?.propagation?.residents || 0,
      marginProbe.descendant?.propagation?.maxGeneration || 0,
      nativeField(marginProbe.descendant?.propagation?.frontierId || ""),
      marginProbe.descendant?.propagation?.champion ? 1 : 0,
      nativeField(pieceVmSelection.parentId || ""), pieceVmPhenotype.reports || 0,
      pieceVmPhenotype.ready ? 1 : 0, Number(pieceVmPhenotype.score || 0).toFixed(4),
      Number(pieceVmPhenotype.selectionBias || 0).toFixed(4), pieceVmPhenotype.sonicVoices || 0,
      nativeField(pieceVmPhenotype.role || ""),
      nativeField(pieceVmPolicy.nextBonus || ""),
      phenotypePolicy.trials || 0, Number(phenotypePolicy.meanReward || 0).toFixed(4),
      Number(phenotypePolicy.admissionRate || 0).toFixed(4), Number(phenotypePolicy.capabilityRate || 0).toFixed(4),
      championPolicy.trials || 0, Number(championPolicy.meanReward || 0).toFixed(4),
      Number(championPolicy.admissionRate || 0).toFixed(4), Number(championPolicy.capabilityRate || 0).toFixed(4),
      diversityPolicy.trials || 0, Number(diversityPolicy.meanReward || 0).toFixed(4),
      Number(diversityPolicy.admissionRate || 0).toFixed(4), Number(diversityPolicy.capabilityRate || 0).toFixed(4),
      nativeField(pieceVmOperator.nextBonus || ""),
      variationOperator.trials || 0, Number(variationOperator.meanReward || 0).toFixed(4),
      Number(variationOperator.capabilityRate || 0).toFixed(4),
      machineryOperator.trials || 0, Number(machineryOperator.meanReward || 0).toFixed(4),
      Number(machineryOperator.capabilityRate || 0).toFixed(4),
      exchangeOperator.trials || 0, Number(exchangeOperator.meanReward || 0).toFixed(4),
      Number(exchangeOperator.capabilityRate || 0).toFixed(4),
      nativeField(pieceVmOutcome.next?.[pieceVmOperator.nextBonus] || ""),
      pieceVmCurriculum.nextLead ? 1 : 0, pieceVmCurriculum.trials || 0,
      pieceVmCurriculum.advancements || 0, pieceVmCurriculum.compoundAdmissions || 0,
      pieceVmCurriculum.maxBreadth || 0, pieceVmDevelopmentState.breadth || 0,
      nativeField(pieceVmDevelopmentState.signature || "00000"),
    ].join("\t"),
  ];
  const displayPrograms = state.displayPrograms || state.programs.filter((program) => program.domain === "raster").slice(-12);
  for (const [programIndex, program] of displayPrograms.entries()) {
    const nativeRgb = resampleRgbHex(program.sample?.rgb, Number(program.sample?.width), Number(program.sample?.height));
    const profile = program.address === "A1" || (!program.address && programIndex === 0) ? { resolution: 64, key: "half" }
      : program.address === "B1" ? { resolution: 128, key: "standard" }
      : program.address === "C1" ? { resolution: 256, key: "double" } : null;
    const assignment = assignmentByAddress.get(program.address || (programIndex === 0 ? "A1" : ""));
    const pieceVmProgram = assignment?.program || null;
    const profileEvidence = profile?.resolution === 64 ? pieceVmProgram?.native
      : pieceVmProgram?.native?.profiles?.[profile?.key];
    const hasPieceVm = Boolean(pieceVmProgram && profile && (profile.resolution === 64 || profileEvidence?.valid));
    lines.push([
      "P", nativeField(program.id), nativeField(program.origin),
      nativeField(program.status), program.generation || 0,
      Number(program.novelty || 0).toFixed(6),
      Number(program.quality || 0).toFixed(6),
      Number(program.metrics?.operations || 0), program.retained ? 1 : 0,
      nativeField(program.parent), nativeField(program.source || program.error),
      (program.sample?.input || []).slice(0, 32).map(Number).join(","),
      nativeField(program.domain || "sort"), nativeField(nativeRgb),
      nativeRgb ? 128 : 0, nativeRgb ? 128 : 0,
      nativeField(program.sample?.bytecode || ""), nativeField(program.address || ""),
      nativeField(program.sample?.groove || ""),
      hasPieceVm ? pieceVmProgram.bytecode : "",
      hasPieceVm ? profile.resolution : 0,
      hasPieceVm && (assignment?.probeCarrier ?? pieceVmCarriesProbe(pieceVmProgram, marginProbe)) ? 1 : 0,
      hasPieceVm ? nativeField(pieceVmProgram.id || "") : "",
      hasPieceVm ? nativeField(assignment?.role || "resident") : "",
    ].join("\t"));
  }
  return `${lines.join("\n")}\n`;
}

// Proposals are derived from one authoritative archive state, evaluated by a
// bounded worker queue, then admitted in their original order. A window deeper
// than the worker count keeps the queue work-conserving across heterogeneous
// 32/64/128/256 fields without allowing wall-clock completion order to rewrite
// the Git-grown lineage.
const EVALUATION_WEIGHTS = Object.freeze({
  cellular: 2.4, blur: 1.8, flood: 1.55, edges: 1.3, mix: 1.15, box: 1.15,
  triangle: 1.1, line: 1.05, copy: 1.05, paste: 1.05,
});

export function proposalEvaluationCost({ source, options = {} } = {}) {
  let resolution = 128;
  try { resolution = hardwareProfile(options.profile || "standard").resolution; }
  catch { /* Invalid profiles are still evaluated and rejected by the worker. */ }
  const operations = String(source || "").match(/\((?:add|xor|and|or|shift|mix|solarize|blur|edges|rotate|mirror|channels|line|triangle|flood|box|copy|paste|cellular)\b/g) || [];
  const weightedStages = operations.reduce((sum, token) => {
    const name = token.slice(1);
    return sum + (EVALUATION_WEIGHTS[name] || 1);
  }, 0);
  return resolution * resolution * Math.max(1, weightedStages);
}

export function scheduleProposalEvaluations(proposals) {
  if (!Array.isArray(proposals)) throw new TypeError("proposal schedule needs an array");
  return proposals.map((proposal, index) => ({ proposal, index, cost: proposalEvaluationCost(proposal) }))
    .sort((a, b) => b.cost - a.cost || a.index - b.index);
}

export function prioritizePieceVmAdaptation(candidates, adaptivePolicy, adaptiveOperator = null) {
  if (!Array.isArray(candidates)) throw new TypeError("PieceVM policy priority needs candidates");
  if (!adaptivePolicy && !adaptiveOperator) return candidates;
  const priority = (candidate) => Number(candidate?.selectionEvidence?.policy === adaptivePolicy) +
    Number(candidate?.operatorFamily === adaptiveOperator);
  return candidates.sort((left, right) => priority(right) - priority(left));
}

export function prioritizePieceVmPolicy(candidates, adaptivePolicy) {
  return prioritizePieceVmAdaptation(candidates, adaptivePolicy);
}

export async function evaluateProposalWindow({ archive, evaluationPool, count, authorityUtcMs = 0 }) {
  if (!archive || !evaluationPool) throw new TypeError("evaluation window needs an archive and worker pool");
  if (!Number.isInteger(count) || count < 1 || count > 256) throw new RangeError("evaluation window count must be 1..256");
  const proposals = Array.from({ length: count }, () => archive.proposeMutation(null, { authorityUtcMs }));
  const dispatch = scheduleProposalEvaluations(proposals);
  const completed = await Promise.all(dispatch.map(({ proposal }) => evaluationPool.evaluate(proposal)));
  const evaluations = Array(count);
  for (let scheduled = 0; scheduled < dispatch.length; scheduled++)
    evaluations[dispatch[scheduled].index] = completed[scheduled];
  const costs = dispatch.map(({ cost }) => cost);
  return {
    proposals,
    evaluations,
    candidates: evaluations.map((evaluation) => archive.submitEvaluation(evaluation)),
    scheduling: {
      policy: "predicted-cost-descending/original-admission",
      predictedCost: costs.reduce((sum, cost) => sum + cost, 0),
      predictedMin: costs.length ? Math.min(...costs) : 0,
      predictedMax: costs.length ? Math.max(...costs) : 0,
    },
  };
}

export async function createSortSoupServer({
  host = "127.0.0.1",
  port = 0,
  seed = "piecefarm-sort-soup-v1",
  cycleMs = 700,
  webRoot = defaultWebRoot,
  proposalToken = null,
  proposalLog = null,
  historyRoot = null,
  ledgerPath = null,
  editionEvery = 2048,
  now = Date.now,
  clockAuthorityUrl = null,
  musicalBpm = 60,
  openaiApiKey = null,
  visualModel = "gpt-5.6-sol",
  visualReviewCooldownMs = 10 * 60_000,
  performanceWindowMs = 60_000,
  workerCount = 1,
  evaluationWindow = workerCount * 8,
  pieceVmCycleMs = cycleMs > 0 ? 30_000 : 0,
  pieceVmBatch = 4,
  pieceVmNativeRunner = resolve(here, "../native/piece-vm-runner"),
  pieceVmNativeVerifier = null,
} = {}) {
  if (!LOOPBACK.has(host)) throw new Error("sort-soup server refuses non-loopback binding");
  if (!Number.isInteger(workerCount) || workerCount < 1 || workerCount > 8) throw new RangeError("evaluation worker count must be 1..8");
  if (!Number.isInteger(evaluationWindow) || evaluationWindow < workerCount || evaluationWindow > 256) {
    throw new RangeError("evaluation window must be worker count..256");
  }
  if (!Number.isInteger(pieceVmCycleMs) || pieceVmCycleMs < 0 || pieceVmCycleMs > 86_400_000) throw new RangeError("PieceVM cycle must be 0..86400000ms");
  if (!Number.isInteger(pieceVmBatch) || pieceVmBatch < 1 || pieceVmBatch > 32) throw new RangeError("PieceVM batch must be 1..32");
  if (pieceVmNativeVerifier !== null && typeof pieceVmNativeVerifier !== "function") throw new TypeError("PieceVM native verifier must be a function");
  let archive;
  let history = null;
  let ledger = null;
  let restored = false;
  if (historyRoot) {
    ({ history, archive, restored } = await SoupHistory.open(historyRoot, { seed }));
  } else {
    archive = new NoveltyArchive({ seed });
  }
  if (!restored) archive.seedClassics();
  archive.seedFoundations();
  const historicalPieceVm = history ? await history.pieceVmLineage() : [];
  const mergedPieceVmLineage = new Map();
  for (const value of [...historicalPieceVm, ...(archive.pieceVm?.lineage || [])]) {
    if (value?.id && !mergedPieceVmLineage.has(value.id)) mergedPieceVmLineage.set(value.id, value);
  }
  const storedPieceVm = archive.pieceVm ? { ...archive.pieceVm, lineage: [...mergedPieceVmLineage.values()] } : null;
  const pieceVmNursery = PieceVmNursery.fromJSON(storedPieceVm, { foundingSource: pieceVmCanarySource });
  const pieceVmPhenotypes = PieceVmPhenotypeOracle.fromJSON(storedPieceVm?.phenotypes);
  const pieceVmPolicyBandit = PieceVmPolicyBandit.fromJSON(storedPieceVm?.policyBandit);
  const pieceVmOperatorBandit = PieceVmOperatorBandit.fromJSON(storedPieceVm?.operatorBandit);
  const pieceVmOutcomeModel = PieceVmOutcomeModel.fromJSON(storedPieceVm?.outcomeModel);
  const pieceVmCurriculum = PieceVmCurriculum.fromJSON(storedPieceVm?.curriculum);
  const pieceVmArchiveState = () => ({ ...pieceVmNursery.toJSON(),
    phenotypes: pieceVmPhenotypes.toJSON(), policyBandit: pieceVmPolicyBandit.toJSON(),
    operatorBandit: pieceVmOperatorBandit.toJSON(), outcomeModel: pieceVmOutcomeModel.toJSON(),
    curriculum: pieceVmCurriculum.toJSON() });
  archive.pieceVm = pieceVmArchiveState();
  let marginProbe = null;
  if (ledgerPath) {
    ledger = await SpecimenLedger.open(ledgerPath);
    const known = new Map([...archive.cells.values(), ...archive.recent].map((candidate) => [candidate.id, candidate]));
    for (const candidate of known.values()) {
      ledger.observe(candidate);
      const storedReview = ledger.get(candidate.id)?.visual_review_json;
      if (storedReview) {
        try { archive.recordVisualReview(candidate.id, JSON.parse(storedReview)); }
        catch { /* Malformed historical advice remains inert. */ }
      }
    }
    ledger.reconcileResidents([...archive.cells.values()].filter((candidate) => candidate.domain === "raster").map((candidate) => candidate.id));
    marginProbe = ledger.latestMarginProbe();
  }
  const streams = new Set();
  let selected = null;
  let persistTail = Promise.resolve();
  let lastSavedIteration = restored ? archive.iteration : 0;
  let nextEdition = (Math.floor(archive.iteration / editionEvery) + 1) * editionEvery;
  let clockOffsetMs = 0;
  let clockSynced = false;
  let lastLifecycleAction = null;
  let liveTelemetry = { receivedAt: null, displayFps: 0, vmHz: 240, deadlineMisses: 0, residents: [], analysis: [] };
  let evaluationPool = null;
  let batchPromise = null;
  let pieceVmPromise = null;
  let lastPieceVmEvolution = null;
  const pieceVmLeaseTtlMs = 90_000;
  const pieceVmNativeLeases = new Map();
  let evaluationWindowsCompleted = 0;
  let lastEvaluationWindowMs = 0;
  let lastEvaluationScheduling = {
    policy: "predicted-cost-descending/original-admission",
    predictedCost: 0, predictedMin: 0, predictedMax: 0,
  };
  let persistedCuration = ledger?.curationStats() || {
    specimens: 0, observations: 0, reviews: 0,
    recommendations: { retain: 0, watch: 0, reject: 0 },
  };
  const evaluationStartedAt = performance.now();
  const evaluationCompletions = [];

  function measuredEvaluationRate(at = performance.now()) {
    const cutoff = at - 10_000;
    while (evaluationCompletions.length && evaluationCompletions[0] < cutoff) evaluationCompletions.shift();
    const windowMs = Math.max(1_000, Math.min(10_000, at - evaluationStartedAt));
    return evaluationCompletions.length * 1_000 / windowMs;
  }
  const performanceOracle = new PerformanceOracle({
    now, windowMs: performanceWindowMs, random: () => archive.rng.float(),
  });
  const visualCurator = new VisualCurator({
    apiKey: openaiApiKey,
    model: visualModel,
    now,
    cooldownMs: visualReviewCooldownMs,
    onReview(candidate, review) {
      archive.recordVisualReview(candidate.id, review);
      ledger?.recordVisualReview(candidate.id, review);
      if (ledger) persistedCuration = ledger.curationStats();
      broadcast("visual-review", { candidateId: candidate.id, review });
    },
  });

  async function syncClock() {
    if (!clockAuthorityUrl) return;
    const sent = now();
    try {
      const response = await fetch(clockAuthorityUrl, { signal: AbortSignal.timeout(3000) });
      const authorityMs = Date.parse(await response.text());
      const received = now();
      if (!response.ok || !Number.isFinite(authorityMs)) throw new Error("invalid UTC response");
      clockOffsetMs = authorityMs - (sent + received) / 2;
      clockSynced = true;
    } catch (error) {
      console.error("clock authority sync failed:", error.message);
    }
  }
  void syncClock();
  const clockInterval = clockAuthorityUrl ? setInterval(() => void syncClock(), 60_000) : null;
  clockInterval?.unref();

  if (proposalLog) await mkdir(dirname(proposalLog), { recursive: true });
  if (history && !restored) await history.save(archive, { commit: true, reason: "genesis" });

  function persist({ commit = false, reason = "checkpoint" } = {}) {
    if (!history) return Promise.resolve(null);
    archive.pieceVm = pieceVmArchiveState();
    persistTail = persistTail.then(() => history.save(archive, { commit, reason }));
    return persistTail;
  }

  function pieceVmSelectionScore(record) {
    return Number(record?.score || 0) + pieceVmPhenotypes.selectionBias(record?.id);
  }

  function pieceVmSelectionParent() {
    return [...pieceVmNursery.residents].sort((left, right) =>
      pieceVmSelectionScore(right) - pieceVmSelectionScore(left) ||
      right.generation - left.generation || String(left.id).localeCompare(String(right.id)))[0] ||
      pieceVmNursery.champion;
  }

  function pieceVmSelectionEvidence(parentId, policy) {
    const parent = pieceVmNursery.residents.find((value) => value.id === parentId) ||
      pieceVmNursery.champion;
    if (!parent) return null;
    const phenotype = pieceVmPhenotypes.summary(parent.id);
    const nativeBias = phenotype?.selectionBias || 0;
    return Object.freeze({ schema: 1, parentId: parent.id, policy,
      staticScore: Number(parent.score || 0), phenotypeReports: phenotype?.reports || 0,
      phenotypeReady: Boolean(phenotype?.ready), phenotypeScore: Number(phenotype?.score || 0),
      nativeBias, combinedScore: Number(parent.score || 0) + nativeBias,
      sonicVoices: phenotype?.sonicVoices || 0, capturedAt: now() });
  }

  function pieceVmCapabilityDelta(candidate) {
    const parent = pieceVmNursery.lineage.find((value) => value.id === candidate?.parent) ||
      pieceVmNursery.residents.find((value) => value.id === candidate?.parent);
    if (!parent || !candidate?.structure) return {};
    const fields = ["functions", "arguments", "layouts", "layoutBytes", "memory", "senses", "transforms"];
    return Object.fromEntries([...fields.map((field) => [field, Math.max(0,
      Number(candidate.structure[field] || 0) - Number(parent.structure?.[field] || 0))]),
    ["capabilityLineage", Math.max(0, (candidate.capabilityLineage?.length || 0) -
      (parent.capabilityLineage?.length || 0))]]);
  }

  function pieceVmCapabilityGain(candidate) {
    return Object.values(pieceVmCapabilityDelta(candidate)).some((value) => value > 0);
  }

  function recordPieceVmPolicyTrial(candidate, nativeValid, admitted) {
    const parent = pieceVmNursery.lineage.find((value) => value.id === candidate?.parent) ||
      pieceVmNursery.residents.find((value) => value.id === candidate?.parent);
    const capabilityDelta = pieceVmCapabilityDelta(candidate);
    const outcome = { at: now(),
      parentId: candidate?.parent, candidateId: candidate?.id, mutation: candidate?.mutation,
      nativeValid, admitted: Boolean(admitted), capabilityGain: Boolean(admitted && pieceVmCapabilityGain(candidate)),
      capabilityDelta: admitted ? capabilityDelta : {}, requestedMutation: candidate?.requestedMutation,
      preferredMutation: candidate?.preferredMutation,
      compatibilityFallback: Boolean(candidate?.compatibilityFallback),
      staticDelta: Number(candidate?.score || 0) - Number(parent?.score || 0) };
    const lane = candidate?.selectionEvidence?.policy;
    const learnedLane = lane !== "probe-focus" && lane !== "curriculum-chain";
    const policy = learnedLane ? pieceVmPolicyBandit.record({ ...outcome, policy: lane }) : null;
    const operator = learnedLane ?
      pieceVmOperatorBandit.record({ ...outcome, policy: candidate?.operatorFamily }) : null;
    const specific = lane === "probe-focus" ? null :
      pieceVmOutcomeModel.record(outcome);
    const curriculum = lane === "probe-focus" ? null :
      pieceVmCurriculum.record({ ...outcome, lead: candidate?.curriculumLead,
        evidence: candidate?.curriculumEvidence });
    return { policy, operator, specific, curriculum };
  }

  function pieceVmEmbodiment() {
    const eligible = pieceVmNursery.residents.filter((value) => value?.bytecode && value.native?.valid &&
      value.native?.profiles?.standard?.valid && value.native?.profiles?.double?.valid);
    if (!eligible.length) return [{ address: "A1", resolution: 64, role: "canary",
      probeCarrier: false, program: pieceVmCanary }];
    const byScore = [...eligible].sort((left, right) => pieceVmSelectionScore(right) - pieceVmSelectionScore(left) ||
      right.generation - left.generation || String(left.id).localeCompare(String(right.id)));
    const lead = eligible.find((value) => value.id === pieceVmSelectionParent()?.id) || byScore[0];
    const curriculumResident = pieceVmCurriculumParent(eligible);
    const carriers = eligible.filter((value) => pieceVmCarriesProbe(value, marginProbe))
      .sort((left, right) => right.generation - left.generation ||
        right.iteration - left.iteration || String(left.id).localeCompare(String(right.id)));
    const carrier = carriers[0] || null;
    const reserved = new Set([lead.id, carrier?.id, curriculumResident?.id].filter(Boolean));
    const explorationPool = byScore.filter((value) => !reserved.has(value.id));
    const minimumReports = explorationPool.length ? Math.min(...explorationPool.map((value) =>
      pieceVmPhenotypes.summary(value.id)?.reports || 0)) : 0;
    const leastObserved = explorationPool.filter((value) =>
      (pieceVmPhenotypes.summary(value.id)?.reports || 0) === minimumReports);
    const explorationEpoch = Math.floor(pieceVmNursery.iteration / 16);
    const explorer = leastObserved.length ? leastObserved[explorationEpoch % leastObserved.length] : null;
    const assignments = [{ address: "A1", resolution: 64, role: "phenotype-lead",
      probeCarrier: pieceVmCarriesProbe(lead, marginProbe), program: lead }];
    if (explorer) assignments.push({ address: "B1", resolution: 128, role: "explorer",
      probeCarrier: pieceVmCarriesProbe(explorer, marginProbe), program: explorer });
    if (carrier) assignments.push({ address: "C1", resolution: 256, role: "probe-carrier",
      probeCarrier: true, program: carrier });
    else {
      const third = curriculumResident && !assignments.some((entry) => entry.program.id === curriculumResident.id)
        ? curriculumResident : byScore.find((value) => !assignments.some((entry) => entry.program.id === value.id));
      if (third) assignments.push({ address: "C1", resolution: 256,
        role: third.id === curriculumResident?.id ? "curriculum" : "ecology",
        probeCarrier: false, program: third });
    }
    return assignments;
  }

  function prunePieceVmNativeLeases(at = now()) {
    for (const [key, lease] of pieceVmNativeLeases) {
      if (at - lease.servedAt > pieceVmLeaseTtlMs) pieceVmNativeLeases.delete(key);
    }
    while (pieceVmNativeLeases.size > 96) pieceVmNativeLeases.delete(pieceVmNativeLeases.keys().next().value);
  }

  function recordPieceVmNativeLeases(state, assignments, servedAt = now()) {
    prunePieceVmNativeLeases(servedAt);
    const displayByAddress = new Map((state.displayPrograms || [])
      .filter((program) => program?.id && program?.address)
      .map((program) => [program.address, program]));
    for (const assignment of assignments) {
      const raster = displayByAddress.get(assignment.address);
      const program = assignment.program;
      if (!raster || !program?.id || !program.native?.valid) continue;
      const profileValid = assignment.resolution === 64 ||
        assignment.resolution === 128 && program.native?.profiles?.standard?.valid ||
        assignment.resolution === 256 && program.native?.profiles?.double?.valid;
      if (!profileValid) continue;
      const lease = Object.freeze({ rasterId: raster.id, address: assignment.address,
        pieceVmId: program.id, role: assignment.role, resolution: assignment.resolution,
        probeCarrier: Boolean(assignment.probeCarrier), servedAt });
      const key = `${lease.rasterId}:${lease.address}:${lease.pieceVmId}:${lease.role}:${lease.resolution}:${Number(lease.probeCarrier)}`;
      pieceVmNativeLeases.delete(key);
      pieceVmNativeLeases.set(key, lease);
    }
    prunePieceVmNativeLeases(servedAt);
  }

  function hasPieceVmNativeLease({ rasterId, pieceVmId, role, resolution, probeCarrier }, at = now()) {
    prunePieceVmNativeLeases(at);
    for (const lease of pieceVmNativeLeases.values()) {
      if (lease.rasterId === rasterId && lease.pieceVmId === pieceVmId && lease.role === role &&
          lease.resolution === resolution && lease.probeCarrier === probeCarrier) return true;
    }
    return false;
  }

  function marginProbeState() {
    if (!marginProbe) return null;
    const descendantId = marginProbe.descendantId;
    if (!descendantId) return { ...marginProbe };
    const lineage = pieceVmNursery.lineage;
    const record = lineage.find((value) => value.id === descendantId) || null;
    const family = new Set([descendantId]);
    let changed = true;
    while (changed) {
      changed = false;
      for (const value of lineage) if (value.parent && family.has(value.parent) && !family.has(value.id)) {
        family.add(value.id); changed = true;
      }
    }
    const resident = pieceVmNursery.residents.some((value) => value.id === descendantId);
    const children = lineage.filter((value) => value.parent === descendantId).length;
    const carriers = lineage.filter((value) => pieceVmCarriesProbe(value, marginProbe));
    const descendantCarriers = carriers.filter((value) => value.id !== descendantId);
    const residentIds = new Set(pieceVmNursery.residents.map((value) => value.id));
    const frontier = carriers.reduce((latest, value) => !latest || value.generation > latest.generation ||
      (value.generation === latest.generation && value.iteration > latest.iteration) ? value : latest, null);
    return { ...marginProbe, descendant: {
      id: descendantId, state: resident ? "resident" : record ? "lineage" : "historical",
      generation: record?.generation || 0, mutation: record?.mutation || "environment-graft",
      score: Number(record?.score || 0), children, descendants: Math.max(0, family.size - 1),
      nativeValid: Boolean(record?.native?.valid),
      profilesValid: Boolean(record?.native?.profiles?.half?.valid &&
        record?.native?.profiles?.standard?.valid && record?.native?.profiles?.double?.valid),
      propagation: {
        descendants: descendantCarriers.length,
        residents: carriers.filter((value) => residentIds.has(value.id)).length,
        maxGeneration: frontier?.generation || record?.generation || 0,
        frontierId: frontier?.id || descendantId,
        champion: pieceVmCarriesProbe(pieceVmNursery.champion, marginProbe),
      },
    } };
  }

  function snapshot({ includeGrooves = false } = {}) {
    const state = archive.snapshot({ selected, includeGrooves });
    const environmentLineage = pieceVmNursery.lineage.filter((value) => value.mutation === "environment-graft");
    const lastEnvironmentGraft = environmentLineage.at(-1) || null;
    const measuredRate = measuredEvaluationRate();
    const targetRate = cycleMs > 0 ? workerCount * 1000 / cycleMs : 0;
    const estimateRate = measuredRate || targetRate;
    if (ledger) {
      const visible = new Map(state.programs.map((program) => [program.id, program]));
      state.displayPrograms = ledger.addressed().map((record) => {
        const program = visible.get(record.id);
        return program ? { ...program, address: record.address, tags: record.tags, visualReview: record.visualReview } : null;
      }).filter(Boolean);
    }
    return {
      ...state,
      history: history?.snapshot() || { head: null, shortHead: null, editions: 0, lastEdition: null },
      checkpoint: {
        nextIteration: nextEdition,
        iterationsRemaining: Math.max(0, nextEdition - state.iteration),
        estimatedMs: estimateRate > 0 ? Math.max(0, nextEdition - state.iteration) * 1000 / estimateRate : 0,
      },
      runtime: {
        cycleMs, workerCount: cycleMs > 0 ? workerCount : 0,
        evaluationWindow: cycleMs > 0 ? evaluationWindow : 0,
        evaluationsPerSecond: measuredRate,
        targetEvaluationsPerSecond: targetRate,
        evaluationBatchInFlight: Boolean(batchPromise),
        evaluationWindowsCompleted,
        lastEvaluationWindowMs,
        evaluationScheduling: lastEvaluationScheduling,
        evaluationPool: evaluationPool?.snapshot() || null,
        utcMs: now() + clockOffsetMs, musicalBpm, clockSynced,
        clockAuthority: clockAuthorityUrl || "local-utc",
        visualCurator: visualCurator.status(),
        visualModel: visualCurator.status() === "armed" ? visualModel : null,
        visualCuration: { session: visualCurator.telemetry(), persisted: persistedCuration },
        vm: {
          sequenceHz: 30, readerHz: 240, slotsPerSequence: 8,
          continuousExecution: true,
          hpRubric: {
            version: 2, meaning: "viability, not aesthetic quality",
            state: "alive 1.00; dormant .42; collapsed .08; flicker .24",
            weights: { baseline: .65, boundedResponse: .20, differentiation: .15, continuity: ".75..1.00" },
            descriptiveOnly: ["noise", "coherence", "muddiness", "colorfulness"],
            intervention: "population-relative sustained windows; never one sample",
          },
          telemetry: liveTelemetry,
        },
        marginProbe: marginProbeState(),
        pieceVm: {
          schema: 1, cycleMs: pieceVmCycleMs, batch: pieceVmBatch,
          iteration: pieceVmNursery.iteration, accepted: pieceVmNursery.accepted, rejected: pieceVmNursery.rejected,
          residents: pieceVmNursery.residents.length, lineage: pieceVmNursery.lineage.length,
          crossovers: pieceVmNursery.lineage.filter((value) => value.mutation === "lineage-crossover").length,
          environmentGrafts: environmentLineage.length,
          embodiment: pieceVmEmbodiment().map(({ address, resolution, role, probeCarrier, program }) => ({
            address, resolution, role, probeCarrier, id: program.id,
            generation: program.generation || 0, score: Number(program.score || 0),
          })),
          phenotypes: pieceVmPhenotypes.snapshot(),
          policyBandit: pieceVmPolicyBandit.snapshot(),
          operatorBandit: pieceVmOperatorBandit.snapshot(),
          outcomeModel: pieceVmOutcomeModel.snapshot(),
          curriculum: pieceVmCurriculum.snapshot(pieceVmCurriculumParent(pieceVmNursery.residents),
            pieceVmNursery.iteration, pieceVmBatch),
          selection: (() => {
            const parent = pieceVmSelectionParent();
            return parent ? { parentId: parent.id, staticScore: Number(parent.score || 0),
              nativeBias: pieceVmPhenotypes.selectionBias(parent.id),
              combinedScore: pieceVmSelectionScore(parent) } : null;
          })(),
          nativeLease: { ttlMs: pieceVmLeaseTtlMs, active: pieceVmNativeLeases.size },
          lastEnvironmentGraft: lastEnvironmentGraft ? {
            id: lastEnvironmentGraft.id, environmentDonor: lastEnvironmentGraft.environmentDonor,
            environmentCapability: lastEnvironmentGraft.environmentCapability,
            environmentProbe: lastEnvironmentGraft.environmentProbe || null,
            generation: lastEnvironmentGraft.generation,
          } : null,
          champion: pieceVmNursery.champion ? {
            id: pieceVmNursery.champion.id, parent: pieceVmNursery.champion.parent,
            donor: pieceVmNursery.champion.donor || null,
            environmentDonor: pieceVmNursery.champion.environmentDonor || null,
            environmentCapability: pieceVmNursery.champion.environmentCapability || null,
            environmentProbe: pieceVmNursery.champion.environmentProbe || null,
            capabilityLineage: pieceVmNursery.champion.capabilityLineage || [],
            generation: pieceVmNursery.champion.generation, mutation: pieceVmNursery.champion.mutation,
            operatorFamily: pieceVmNursery.champion.operatorFamily || null,
            requestedOperatorFamily: pieceVmNursery.champion.requestedOperatorFamily || null,
            preferredMutation: pieceVmNursery.champion.preferredMutation || null,
            requestedMutation: pieceVmNursery.champion.requestedMutation || null,
            compatibilityFallback: Boolean(pieceVmNursery.champion.compatibilityFallback),
            development: pieceVmDevelopment(pieceVmNursery.champion),
            curriculumEvidence: pieceVmNursery.champion.curriculumEvidence || null,
            bytecodeHash: pieceVmNursery.champion.bytecodeHash,
            instructionCount: pieceVmNursery.champion.instructionCount,
            registerCount: pieceVmNursery.champion.registerCount,
            score: pieceVmNursery.champion.score, traits: pieceVmNursery.champion.traits,
            structure: pieceVmNursery.champion.structure, native: pieceVmNursery.champion.native,
            selectionEvidence: pieceVmNursery.champion.selectionEvidence || null,
          } : null,
          lastEvolution: lastPieceVmEvolution,
        },
        lifecycle: { windowMs: performanceWindowMs, terminalWindowMs: performanceOracle.terminalWindowMs,
          lastAction: lastLifecycleAction },
      },
    };
  }

  function broadcast(type = "snapshot", extra = {}) {
    if (!streams.size) return;
    const line = `${JSON.stringify({ type, state: snapshot(), ...extra })}\n`;
    for (const response of streams) {
      if (!response.write(line)) {
        response.end();
        streams.delete(response);
      }
    }
  }

  const server = createServer(async (request, response) => {
    try {
      const url = new URL(request.url, `http://${request.headers.host || "127.0.0.1"}`);
      if (request.method === "GET" && STATIC.has(url.pathname)) {
        const filename = STATIC.get(url.pathname);
        const data = await readFile(join(webRoot, filename));
        response.writeHead(200, {
          "content-type": contentType(filename),
          "content-length": data.length,
          "cache-control": "no-store",
          "content-security-policy": "default-src 'self'; script-src 'self'; style-src 'self'; connect-src 'self'",
          "x-content-type-options": "nosniff",
          "x-frame-options": "DENY",
        });
        response.end(data);
        return;
      }
      if (request.method === "GET" && url.pathname === "/api/piecevm") {
        const state = snapshot();
        json(response, 200, {
          iteration: state.iteration, history: state.history,
          marginProbe: state.runtime.marginProbe,
          pieceVm: state.runtime.pieceVm,
          nativePhenotypes: state.runtime.vm.telemetry.pieceVmPhenotypes || [],
        });
        return;
      }
      if (request.method === "GET" && url.pathname === "/api/state") {
        json(response, 200, snapshot());
        return;
      }
      if (request.method === "GET" && url.pathname === "/api/native") {
        const state = snapshot({ includeGrooves: true });
        const assignments = pieceVmEmbodiment();
        recordPieceVmNativeLeases(state, assignments);
        const data = Buffer.from(nativeSnapshot(state, assignments));
        response.writeHead(200, {
          "content-type": "text/plain; charset=utf-8",
          "content-length": data.length,
          "cache-control": "no-store",
          "x-content-type-options": "nosniff",
        });
        response.end(data);
        return;
      }
      if (request.method === "GET" && url.pathname === "/api/specimens") {
        json(response, 200, { specimens: ledger?.addressed() || [] });
        return;
      }
      if (request.method === "GET" && url.pathname.startsWith("/api/groove/")) {
        const id = decodeURIComponent(url.pathname.slice("/api/groove/".length));
        if (!/^[a-f0-9]{12}$/.test(id)) throw new Error("invalid groove id");
        const candidate = [...archive.cells.values(), ...archive.recent]
          .find((value) => value.id === id && value.domain === "raster" && value.sample?.groove);
        if (!candidate) {
          json(response, 404, { error: "unknown groove" });
          return;
        }
        const bytes = new Uint8Array(Buffer.from(candidate.sample.groove, "hex"));
        const record = inspectPixelGroove(bytes);
        if (url.searchParams.get("format") === "ppm") {
          const data = printPixelGroove(bytes, { field: Buffer.from(candidate.sample.rgb, "hex") });
          response.writeHead(200, { "content-type": "image/x-portable-pixmap",
            "content-length": data.length, "cache-control": "no-store", "x-content-type-options": "nosniff" });
          response.end(data);
        } else {
          json(response, 200, { id, record, groove: candidate.sample.groove });
        }
        return;
      }
      if (request.method === "GET" && url.pathname === "/api/stream") {
        response.writeHead(200, {
          "content-type": "application/x-ndjson; charset=utf-8",
          "cache-control": "no-store",
          connection: "keep-alive",
          "x-content-type-options": "nosniff",
        });
        response.write(`${JSON.stringify({ type: "welcome", state: snapshot() })}\n`);
        streams.add(response);
        request.on("close", () => streams.delete(response));
        return;
      }
      if (request.method === "POST" && url.pathname === "/api/select") {
        const input = await body(request);
        const id = String(input.id || "");
        if (!snapshot().programs.some((program) => program.id === id)) throw new Error("unknown program id");
        selected = id;
        broadcast("selection");
        json(response, 200, { selected });
        return;
      }
      if (request.method === "POST" && url.pathname === "/api/margin-probe") {
        const input = await body(request);
        const id = String(input.id || ""), address = Number(input.address);
        const state = snapshot({ includeGrooves: true });
        const visible = (state.displayPrograms || []).find((program) => program.id === id);
        if (!visible) throw new Error("probe specimen is not on the visible wall");
        marginProbe = { ...decodeMarginProbe(visible, address, { at: now(), requestedBy: input.requestedBy }),
          status: "pending", attempts: 0, descendantId: null };
        selected = id;
        ledger?.recordMarginProbe(id, marginProbe);
        broadcast("margin-probe", { probe: marginProbe });
        json(response, 200, { probe: marginProbe, nurseryEligible: Boolean(marginProbe.capability) });
        return;
      }
      if (request.method === "POST" && url.pathname === "/api/groove-state") {
        const input = await body(request, 64 * 1024);
        const id = String(input.id || ""), groove = String(input.groove || "");
        if (!/^[a-f0-9]{12}$/.test(id) || groove.length !== 55_296 || !/^[a-f0-9]+$/.test(groove)) {
          throw new Error("invalid live groove envelope");
        }
        const record = inspectPixelGroove(groove);
        if (!record.valid || record.id !== id) throw new Error("invalid live groove record");
        const candidates = [...archive.cells.values(), ...archive.recent]
          .filter((candidate) => candidate.id === id && candidate.domain === "raster" && candidate.sample);
        if (!candidates.length || candidates.some((candidate) => candidate.source !== record.source)) {
          throw new Error("unknown live groove record");
        }
        for (const candidate of candidates) {
          candidate.sample.groove = groove;
          candidate.sample.grooveVersion = record.version;
          candidate.sample.grooveBytes = GROOVE_LAYOUT.bytes;
          candidate.sample.grooveHash = record.protectedHash;
          candidate.sample.groovePc = record.pc;
        }
        json(response, 200, { id, pc: record.pc, needlePixel: record.needlePixel, protectedHash: record.protectedHash });
        return;
      }
      if (request.method === "POST" && url.pathname === "/api/health") {
        const input = await body(request, 16_384);
        if (!Array.isArray(input.residents) || input.residents.length > 12) throw new Error("invalid health membrane");
        const candidatesById = new Map();
        for (const candidate of [...archive.recent, ...archive.cells.values()])
          if (candidate?.id && candidate.domain === "raster") candidatesById.set(candidate.id, candidate);
        const addressed = ledger?.addressed() || [...candidatesById.values()].slice(-12)
          .map((candidate, index) => ({ id: candidate.id, address: candidate.address ||
            `${String.fromCharCode(65 + index % 4)}${1 + Math.floor(index / 4)}` }));
        const visible = new Map(addressed.map((record) => {
          const candidate = candidatesById.get(record.id);
          return [record.id, candidate ? { ...candidate, address: record.address } : { id: record.id, address: record.address }];
        }));
        const embodimentByAddress = new Map(pieceVmEmbodiment().map((value) => [value.address, value]));
        const rows = input.residents.flatMap((value) => {
          const id = String(value.id || "");
          const program = visible.get(id);
          /* SDL keeps a fixed tile stable through brief ledger churn. An old
             raster id may therefore survive one health envelope after its
             address has been reassigned. Ignore that stale row; every row we
             do ingest still has to match the current ledger and PieceVM
             authority exactly. */
          if (!program) return [];
          const hp = Number(value.hp), ageMs = Number(value.ageMs);
          if (!Number.isFinite(hp) || hp < 0 || hp > 100 || !Number.isFinite(ageMs) || ageMs < 0) throw new Error("invalid health value");
          const life = Number(value.life), failedReprobes = Number(value.failedReprobes);
          if (!Number.isInteger(life) || life < 0 || life > 3 || !Number.isInteger(failedReprobes) || failedReprobes < 0 || failedReprobes > 1_000_000) {
            throw new Error("invalid native lifecycle value");
          }
          const trait = (name) => Math.max(0, Math.min(1, Number(value[name]) || 0));
          const resolution = [32, 64, 128, 256].includes(Number(value.resolution)) ? Number(value.resolution) : 128;
          const pieceVmId = String(value.pieceVmId || "");
          let pieceVm = null;
          if (pieceVmId) {
            const expected = embodimentByAddress.get(program.address || "");
            const role = String(value.pieceVmRole || "");
            const probeCarrier = Boolean(value.pieceVmProbeCarrier);
            const sonicVoices = Number(value.sonicVoices);
            const currentIdentity = expected?.program?.id === pieceVmId && expected.role === role &&
              expected.probeCarrier === probeCarrier && expected.resolution === resolution;
            const leasedIdentity = hasPieceVmNativeLease({ rasterId: id, pieceVmId, role,
              resolution, probeCarrier });
            if (!/^[a-f0-9]{12}$/.test(pieceVmId) || (!currentIdentity && !leasedIdentity) ||
                !Number.isInteger(sonicVoices) || sonicVoices < 0 || sonicVoices > 5) {
              throw new Error("invalid PieceVM phenotype identity");
            }
            pieceVm = { id: pieceVmId, role, probeCarrier, sonicVoices };
          }
          return [{ id, hp, ageMs: Math.min(ageMs, 365 * 86400_000),
            life, failedReprobes, pc: Number(value.pc) || 0, needlePixel: Number(value.needlePixel) || 0,
            sequencePasses: Number(value.sequencePasses) || 0, lifecycleVector: Number(value.lifecycleVector) || 0,
            resolution,
            traits: { actual: trait("actual"), variance: trait("variance"), spatial: trait("spatial"),
              noise: trait("noise"), coherence: trait("coherence"), muddiness: trait("muddiness") },
            vmHz: Number(value.vmHz) || 240, deadlineMisses: Number(value.deadlineMisses) || 0,
            address: program.address || "", pieceVm }];
        });
        const counts = new Map(rows.map((row) => [row.id, Number(ledger?.get(row.id)?.interventions || 0)]));
        const displayFps = Math.max(0, Math.min(1000, Number(input.displayFps) || 0));
        const result = performanceOracle.ingest(rows, counts, displayFps);
        const receivedAt = now();
        const phenotypeSummaries = pieceVmPhenotypes.ingest(rows.filter((row) => row.pieceVm).map((row) => ({
          id: row.pieceVm.id, role: row.pieceVm.role, sonicVoices: row.pieceVm.sonicVoices,
          hp: row.hp, life: row.life, resolution: row.resolution, traits: row.traits,
        })), receivedAt);
        pieceVmPolicyBandit.observePhenotypes(phenotypeSummaries);
        pieceVmOperatorBandit.observePhenotypes(phenotypeSummaries);
        pieceVmOutcomeModel.observePhenotypes(phenotypeSummaries);
        pieceVmCurriculum.observePhenotypes(phenotypeSummaries);
        archive.pieceVm = pieceVmArchiveState();
        liveTelemetry = {
          receivedAt, displayFps, vmHz: 240,
          deadlineMisses: Math.max(0, ...rows.map((row) => row.deadlineMisses)),
          residents: rows, analysis: result.metrics, pieceVmPhenotypes: phenotypeSummaries,
        };
        for (const metric of result.metrics) ledger?.recordHealth(metric.id, {
          hp: metric.hp, at: now(), strikes: metric.strikes,
        });
        let lifecycleReviewRequested = false;
        for (const metric of result.metrics) {
          const candidate = visible.get(metric.id);
          if (!candidate) continue;
          const triggers = [];
          if (metric.healthRange >= 24 && metric.healthStdDev >= 7) triggers.push("health-variability");
          if (metric.healthMean >= 82 && metric.healthyRatio >= .75) triggers.push("high-health");
          for (const trigger of triggers) {
            lifecycleReviewRequested = true;
            void visualCurator.consider(candidate, { trigger, lifecycle: metric })
              .catch((error) => console.error("visual curator failed:", error.message));
          }
        }
        /* Health gets first refusal on the one bounded review lane. Visual
           novelty is only a fallback after every current resident has had an
           eight-sample lifecycle window, so proposal churn cannot starve it. */
        if (!lifecycleReviewRequested && result.metrics.length && result.metrics.every((metric) => metric.samples >= 8)) {
          const novelty = [...visible.values()].sort((a, b) => (b.quality || 0) - (a.quality || 0))
            .find((candidate) => visualCurator.eligible(candidate));
          if (novelty) void visualCurator.consider(novelty)
            .catch((error) => console.error("visual curator failed:", error.message));
        }
        const commands = [];
        if (result.action?.type === "prod") {
          const action = { ...result.action, at: now() };
          ledger?.recordIntervention(action.id, { at: action.at, strategy: action.strategy,
            donorId: action.donorId, beforeHp: action.hp });
          lastLifecycleAction = action;
          commands.push(["PROD", nativeField(action.id), nativeField(action.strategy),
            nativeField(action.donorId || "-")].join("\t"));
          broadcast("lifecycle", { action });
        } else if (result.action?.type === "recovered") {
          ledger?.recordOutcome(result.action.id, "recovered");
          lastLifecycleAction = { ...result.action, at: now() };
          broadcast("lifecycle", { action: lastLifecycleAction });
        } else if (result.action?.type === "fork") {
          const child = archive.fork(result.action.id);
          if (child) {
            ledger?.observe(child);
            ledger?.reconcileResidents([...archive.cells.values()].filter((value) => value.domain === "raster").map((value) => value.id));
          }
          lastLifecycleAction = { ...result.action, childId: child?.id || null, at: now() };
          broadcast("lifecycle", { action: lastLifecycleAction });
        } else if (result.action?.type === "cull") {
          const retired = archive.retire(result.action.id, result.action.reason);
          if (retired) {
            ledger?.recordOutcome(result.action.id, "culled");
            ledger?.cull(result.action.id, result.action.reason);
            ledger?.fillVacancies([...archive.cells.values()]);
          }
          lastLifecycleAction = { ...result.action, at: now() };
          broadcast("lifecycle", { action: lastLifecycleAction });
        }
        broadcast("telemetry", { telemetry: liveTelemetry });
        const data = Buffer.from(commands.length ? `${commands.join("\n")}\n` : "OK\n");
        response.writeHead(200, { "content-type": "text/plain; charset=utf-8",
          "content-length": data.length, "cache-control": "no-store" });
        response.end(data);
        return;
      }
      if (request.method === "POST" && url.pathname === "/api/propose") {
        if (!proposalToken || bearer(request) !== proposalToken) {
          json(response, 401, { error: "proposal capability required" });
          return;
        }
        const input = await body(request);
        const source = String(input.source || "");
        const origin = String(input.origin || "prox").slice(0, 64);
        const parent = input.parent ? String(input.parent).slice(0, 64) : null;
        const candidate = archive.submit(source, { origin, parent, generation: Number(input.generation) || 0 });
        ledger?.observe(candidate);
        if (candidate.retained) ledger?.reconcileResidents([...archive.cells.values()].filter((value) => value.domain === "raster").map((value) => value.id));
        void visualCurator.consider(candidate).catch((error) => console.error("visual curator failed:", error.message));
        if (proposalLog) {
          const record = { at: new Date(now()).toISOString(), origin, parent, source: source.slice(0, 512), result: candidate.status, id: candidate.id };
          await appendFile(proposalLog, `${JSON.stringify(record)}\n`, { mode: 0o600 });
        }
        broadcast("proposal", { candidateId: candidate.id });
        json(response, candidate.status === "rejected" ? 422 : 202, { candidate });
        return;
      }
      json(response, 404, { error: "not found" });
    } catch (error) {
      json(response, /too large/.test(error.message) ? 413 : 400, { error: error.message });
    }
  });

  await new Promise((resolveListen, reject) => {
    server.once("error", reject);
    server.listen(port, host, resolveListen);
  });

  async function verifyPieceVmNative(candidate, resolution = 64) {
    const profile = resolution === candidate.program.resolution ? candidate
      : evaluatePieceVmSource(candidate.source, { resolution, frames: 2 });
    if (pieceVmNativeVerifier) return { ...await pieceVmNativeVerifier(profile, resolution), resolution };
    const directory = await mkdtemp(join(tmpdir(), "piecevm-diff-"));
    const frameHashes = [];
    try {
      for (let frame = 1; frame <= 2; frame += 1) {
        const output = join(directory, `frame-${frame}.rgb`);
        await runFile(pieceVmNativeRunner, [String(resolution), String(frame), profile.program.bytecode, output], {
          timeout: 5_000, maxBuffer: 1_048_576,
        });
        frameHashes.push(createHash("sha256").update(await readFile(output)).digest("hex"));
      }
      const valid = frameHashes.every((value, index) => value === profile.frameHashes[index]);
      return { valid, engine: "native-c11", resolution, bytecodeHash: profile.program.bytecodeHash,
        frameHashes, checkedFrames: 2, at: new Date(now()).toISOString(),
        error: valid ? null : "native framebuffer mismatch" };
    } catch (error) {
      return { valid: false, engine: "native-c11", resolution, bytecodeHash: profile.program.bytecodeHash,
        frameHashes, checkedFrames: frameHashes.length, at: new Date(now()).toISOString(),
        error: String(error?.message || error).slice(0, 240) };
    } finally {
      await rm(directory, { recursive: true, force: true });
    }
  }

  async function evolvePieceVm() {
    const probeFocus = Boolean(marginProbe?.capability &&
      (!marginProbe.status || marginProbe.status === "pending") && (marginProbe.attempts || 0) < 8);
    const environment = pieceVmEcologyWithProbe(snapshot().displayPrograms || [], probeFocus ? marginProbe : null);
    const branchParent = pieceVmNursery.residents.length
      ? pieceVmNursery.residents[Math.floor(pieceVmNursery.iteration / Math.max(1, pieceVmBatch)) % pieceVmNursery.residents.length]
      : pieceVmNursery.champion;
    const phenotypeParent = pieceVmSelectionParent();
    const iterationBase = pieceVmNursery.iteration;
    const curriculumParent = pieceVmCurriculumParent(pieceVmNursery.residents);
    const curriculumTarget = pieceVmCurriculumTarget(curriculumParent);
    const curriculumLead = !probeFocus && pieceVmBatch > 3 && Boolean(curriculumTarget) &&
      pieceVmCurriculum.shouldLead(iterationBase, pieceVmBatch);
    const policySchedule = probeFocus ? Array(pieceVmBatch).fill("probe-focus") :
      pieceVmPolicyBandit.schedule(pieceVmBatch);
    const operatorSchedule = probeFocus ? Array(pieceVmBatch).fill("exchange") :
      pieceVmOperatorBandit.schedule(pieceVmBatch);
    const mutationPreferenceSchedule = probeFocus ? Array.from({ length: pieceVmBatch }, () => []) :
      pieceVmOutcomeModel.preferenceSchedule(operatorSchedule);
    if (curriculumLead) {
      const lane = pieceVmBatch - 1;
      policySchedule[lane] = "curriculum-chain";
      operatorSchedule[lane] = curriculumTarget.family;
      mutationPreferenceSchedule[lane] = [...new Set([...curriculumTarget.mutations,
        ...pieceVmOutcomeModel.rank(curriculumTarget.family)])];
    }
    const mutationSchedule = mutationPreferenceSchedule.map((values) => values[0] || null);
    const proposed = Array.from({ length: pieceVmBatch }, (_, index) => {
      const focusedParent = probeFocus && pieceVmNursery.residents.length
        ? pieceVmNursery.residents[(iterationBase + index) % pieceVmNursery.residents.length]
        : null;
      const policy = policySchedule[index];
      const parentId = focusedParent?.id || (policy === "curriculum-chain" ? curriculumParent?.id :
        policy === "branch-diversity" ? branchParent?.id :
        policy === "champion-control" ? pieceVmNursery.championId : phenotypeParent?.id);
      const candidate = pieceVmNursery.propose(parentId, environment,
        { operatorFamily: operatorSchedule[index], mutations: mutationPreferenceSchedule[index] });
      if (candidate?.program) {
        candidate.selectionEvidence = pieceVmSelectionEvidence(candidate.parent, policy);
        const parent = pieceVmNursery.lineage.find((value) => value.id === candidate.parent) ||
          pieceVmNursery.residents.find((value) => value.id === candidate.parent);
        candidate.curriculumEvidence = pieceVmCurriculumEvidence(parent, candidate);
        candidate.curriculumLead = curriculumLead;
      }
      return candidate;
    }).filter((candidate) => candidate.program && candidate.behaviorChanged);
    const proposals = rankPieceVmCandidates(proposed, pieceVmNursery.residents);
    const adaptivePolicy = !probeFocus && pieceVmBatch > 3 ? pieceVmPolicyBandit.bonusPolicy() : null;
    const adaptiveOperator = !probeFocus && pieceVmBatch > 3 ? pieceVmOperatorBandit.bonusPolicy() : null;
    if (adaptivePolicy || adaptiveOperator || curriculumLead)
      prioritizePieceVmCurriculum(proposals, adaptivePolicy, adaptiveOperator, curriculumLead);
    if (probeFocus) proposals.sort((left, right) =>
      Number(Boolean(right.environmentProbe)) - Number(Boolean(left.environmentProbe)));
    let admitted = null, native = null, verificationAttempts = 0;
    for (const candidate of proposals) {
      verificationAttempts += 1;
      native = await verifyPieceVmNative(candidate);
      if (!native.valid) {
        recordPieceVmPolicyTrial(candidate, false, false);
        continue;
      }
      const standard = await verifyPieceVmNative(candidate, 128);
      const double = standard.valid ? await verifyPieceVmNative(candidate, 256) : null;
      if (!standard.valid || !double?.valid) {
        native = double || standard;
        recordPieceVmPolicyTrial(candidate, false, false);
        continue;
      }
      native = { ...native, profiles: { half: native, standard, double } };
      admitted = pieceVmNursery.admit(candidate, native);
      recordPieceVmPolicyTrial(candidate, true, admitted);
      if (admitted) break;
    }
    archive.pieceVm = pieceVmArchiveState();
    if (probeFocus) {
      const attempts = (marginProbe.attempts || 0) + 1;
      const probeAdmitted = admitted?.environmentProbe?.address === marginProbe.address &&
        admitted.environmentDonor === marginProbe.id && admitted.environmentCapability === marginProbe.capability;
      marginProbe = { ...marginProbe, attempts,
        status: probeAdmitted ? "admitted" : attempts >= 8 ? "no-admission" : "pending",
        descendantId: probeAdmitted ? admitted.id : null,
        lastAttemptAt: now(), candidates: proposed.filter((candidate) => candidate.environmentProbe).length };
      ledger?.recordMarginProbe(marginProbe.id, marginProbe);
    }
    lastPieceVmEvolution = {
      at: new Date(now()).toISOString(), proposed: pieceVmBatch, verified: verificationAttempts,
      admitted: admitted?.id || null, mutation: admitted?.mutation || null,
      parent: admitted?.parent || null, donor: admitted?.donor || null,
      environmentDonor: admitted?.environmentDonor || null,
      environmentCapability: admitted?.environmentCapability || null,
      environmentProbe: admitted?.environmentProbe || null,
      marginProbe: probeFocus ? marginProbe : null,
      selectionParentId: phenotypeParent?.id || null,
      selectionNativeBias: phenotypeParent ? pieceVmPhenotypes.selectionBias(phenotypeParent.id) : 0,
      selectionCombinedScore: phenotypeParent ? pieceVmSelectionScore(phenotypeParent) : 0,
      admittedSelectionEvidence: admitted?.selectionEvidence || null,
      policySchedule, adaptivePolicy, policyBandit: pieceVmPolicyBandit.snapshot(),
      operatorSchedule, adaptiveOperator, operatorBandit: pieceVmOperatorBandit.snapshot(),
      mutationSchedule, mutationPreferenceSchedule, outcomeModel: pieceVmOutcomeModel.snapshot(),
      curriculumLead, curriculum: pieceVmCurriculum.snapshot(admitted || pieceVmNursery.champion,
        pieceVmNursery.iteration, pieceVmBatch),
      curriculumParentId: curriculumParent?.id || null,
      curriculumTarget: curriculumTarget?.capability || null,
      generation: admitted?.generation ?? pieceVmNursery.champion?.generation ?? 0,
      nativeValid: Boolean(native?.valid), error: native?.error || null,
    };
    if (admitted) await persist({ commit: true, reason: "piecevm-admission" });
    broadcast("piecevm-evolution", { evolution: lastPieceVmEvolution });
    return admitted;
  }

  async function evaluateBatch() {
    const startedAt = performance.now();
    const authorityUtcMs = clockSynced ? Math.floor(now() + clockOffsetMs) : 0;
    const { candidates, scheduling } = await evaluateProposalWindow({
      archive, evaluationPool, count: evaluationWindow, authorityUtcMs,
    });
    const completedAt = performance.now();
    lastEvaluationWindowMs = completedAt - startedAt;
    lastEvaluationScheduling = scheduling;
    evaluationWindowsCompleted += 1;
    for (let index = 0; index < candidates.length; index += 1) evaluationCompletions.push(completedAt);
    for (const candidate of candidates) {
      ledger?.observe(candidate);
    }
    if (candidates.some((candidate) => candidate.retained)) {
      ledger?.reconcileResidents([...archive.cells.values()].filter((value) => value.domain === "raster").map((value) => value.id));
    }
    broadcast();
    const editionDue = archive.iteration >= nextEdition;
    if (archive.iteration - lastSavedIteration >= 16 || editionDue) {
      lastSavedIteration = archive.iteration;
      if (editionDue) nextEdition += editionEvery;
      void persist({ commit: editionDue, reason: editionDue ? "scheduled" : "checkpoint" })
        .catch((error) => console.error("sort-soup persistence failed:", error.message));
    }
  }

  if (cycleMs > 0) evaluationPool = new EvaluationPool({ size: workerCount });
  const interval = cycleMs > 0 ? setInterval(() => {
    if (batchPromise) return;
    batchPromise = evaluateBatch()
      .catch((error) => console.error("sort-soup evaluation batch failed:", error.message))
      .finally(() => { batchPromise = null; });
  }, cycleMs) : null;
  interval?.unref();
  const pieceVmInterval = pieceVmCycleMs > 0 ? setInterval(() => {
    if (pieceVmPromise) return;
    pieceVmPromise = evolvePieceVm()
      .catch((error) => {
        lastPieceVmEvolution = { at: new Date(now()).toISOString(), proposed: 0, verified: 0,
          admitted: null, nativeValid: false, error: String(error?.message || error).slice(0, 240) };
        console.error("PieceVM evolution failed:", error.message);
      })
      .finally(() => { pieceVmPromise = null; });
  }, pieceVmCycleMs) : null;
  pieceVmInterval?.unref();

  return {
    server,
    archive,
    history,
    ledger,
    pieceVmNursery,
    evolvePieceVm,
    restored,
    address: server.address(),
    snapshot,
    async stop() {
      if (interval) clearInterval(interval);
      if (pieceVmInterval) clearInterval(pieceVmInterval);
      if (clockInterval) clearInterval(clockInterval);
      if (batchPromise) await batchPromise;
      if (pieceVmPromise) await pieceVmPromise;
      if (evaluationPool) await evaluationPool.close();
      for (const response of streams) response.end();
      await new Promise((resolveClose) => server.close(resolveClose));
      await persist({ commit: true, reason: "shutdown" });
      ledger?.close();
    },
  };
}

function option(name, fallback) {
  const index = process.argv.indexOf(name);
  return index === -1 ? fallback : process.argv[index + 1];
}

if (process.argv[1] === fileURLToPath(import.meta.url)) {
  const host = option("--host", "127.0.0.1");
  const port = Number(option("--port", "8788"));
  const root = resolve(option("--root", "./piecefarm-state"));
  const app = await createSortSoupServer({
    host,
    port,
    seed: option("--seed", "piecefarm-sort-soup-v1"),
    cycleMs: Number(option("--cycle-ms", "700")),
    proposalToken: process.env.PIECEFARM_PROPOSAL_TOKEN || null,
    proposalLog: join(root, "inbox", "proposals.ndjson"),
    historyRoot: join(root, "history"),
    ledgerPath: join(root, "specimens.sqlite"),
    clockAuthorityUrl: process.env.PIECEFARM_CLOCK_URL || "https://aesthetic.computer/api/clock",
    musicalBpm: Number(process.env.PIECEFARM_MUSICAL_BPM || 60),
    openaiApiKey: process.env.OPENAI_API_KEY || null,
    visualModel: process.env.PIECEFARM_VISUAL_MODEL || "gpt-5.6-sol",
    visualReviewCooldownMs: Number(process.env.PIECEFARM_VISUAL_REVIEW_COOLDOWN_MS || 120_000),
    workerCount: Number(option("--workers", process.env.PIECEFARM_WORKERS || "1")),
    evaluationWindow: Number(option("--lookahead", process.env.PIECEFARM_LOOKAHEAD || "0")) ||
      Number(option("--workers", process.env.PIECEFARM_WORKERS || "1")) * 8,
  });
  console.log(JSON.stringify({
    listening: `http://${app.address.address}:${app.address.port}`,
    board: `http://${app.address.address}:${app.address.port}/board`,
    soup: `http://${app.address.address}:${app.address.port}/soup`,
    proposalMembrane: process.env.PIECEFARM_PROPOSAL_TOKEN ? "armed" : "closed",
    visualCurator: app.snapshot().runtime.visualCurator,
    restored: app.restored,
  }));
  const stop = async () => { await app.stop(); process.exit(0); };
  process.once("SIGINT", stop);
  process.once("SIGTERM", stop);
}
