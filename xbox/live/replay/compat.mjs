// Oskiewar replay compatibility lives outside the moving game implementation.
// A recording selects an immutable simulation contract; it is never silently
// reinterpreted using whatever physics happens to ship in the current build.

import { createHash } from "node:crypto";

export const REPLAY_FORMAT = "ac.oskiedemo";
export const CURRENT_REPLAY_VERSION = 1;
export const FIXTURE_MANIFEST_FORMAT = "ac.oskiewar.replay-fixtures";

// Append-only. Existing identifiers and their input layouts must not change.
// When physics changes, add another entry and retain the old playback adapter.
export const simulationContracts = Object.freeze({
  "oskiewar-physics-1": Object.freeze({
    id: "oskiewar-physics-1",
    tickRate: 60,
    tickUs: 16667,
    inputEncoding: "oskiewar-buttons-v1",
    buttons: Object.freeze([
      "left", "right", "up", "down", "kick", "punch", "shield", "item",
    ]),
    checkpointEncoding: "oskiewar-checkpoint-26-v1",
    checkpointWidth: 26,
    firstBuild: 1,
  }),
});

const integer = (value, min = 0, max = Number.MAX_SAFE_INTEGER) =>
  Number.isInteger(value) && value >= min && value <= max;

function clone(value) {
  return JSON.parse(JSON.stringify(value));
}

function migrateLegacyZero(source) {
  const demo = clone(source);
  demo.format = REPLAY_FORMAT;
  demo.version = 1;
  demo.game = demo.game || "oskiewar";
  demo.simulation = demo.simulation || "oskiewar-physics-1";
  demo.tickRate = demo.tickRate || 60;
  demo.commands = demo.commands || demo.inputs || [];
  delete demo.inputs;
  demo.events ||= [];
  demo.checkpoints ||= [];
  demo.rounds ||= [];
  demo.roundIds ||= demo.roundId ? [demo.roundId] : [];
  demo.roundIndex ??= 0;
  demo.durationTicks ??= demo.ticks ?? 0;
  delete demo.ticks;
  return demo;
}

export function migrateReplay(source) {
  if (!source || typeof source !== "object" || Array.isArray(source))
    throw new TypeError("Replay must be an object");
  if (source.format !== undefined && source.format !== REPLAY_FORMAT)
    throw new Error(`Unsupported replay format: ${source.format}`);
  if (source.version === undefined || source.version === 0)
    return migrateLegacyZero(source);
  if (source.version === 1) return clone(source);
  throw new Error(`No decoder for replay version ${source.version}`);
}

export function validateCompatibleReplay(source) {
  let demo;
  try { demo = migrateReplay(source); }
  catch (error) { return { ok: false, error: error.message }; }
  if (demo.format !== REPLAY_FORMAT || demo.version !== CURRENT_REPLAY_VERSION ||
      demo.game !== "oskiewar") return { ok: false, error: "Unsupported replay envelope" };
  const simulation = simulationContracts[demo.simulation];
  if (!simulation)
    return { ok: false, error: `Archived simulation unavailable: ${demo.simulation}` };
  if (demo.tickRate !== simulation.tickRate)
    return { ok: false, error: "Tick rate does not match simulation contract" };
  if (!integer(demo.durationTicks, 0, 216000))
    return { ok: false, error: "Invalid replay duration" };
  if (!Array.isArray(demo.commands) || demo.commands.length > 50000)
    return { ok: false, error: "Invalid command timeline" };
  let priorTick = -1;
  for (const row of demo.commands) {
    if (!Array.isArray(row) || row.length !== 3 ||
        !integer(row[0], 0, demo.durationTicks) || !integer(row[1], 0, 1) ||
        !integer(row[2], 0, 255))
      return { ok: false, error: "Invalid command row" };
    if (row[0] < priorTick)
      return { ok: false, error: "Command timeline is not monotonic" };
    priorTick = row[0];
  }
  if (!Array.isArray(demo.checkpoints) || demo.checkpoints.some((row) =>
      !Array.isArray(row) || row.length !== simulation.checkpointWidth ||
      row.some((value) => !Number.isFinite(value))))
    return { ok: false, error: "Invalid checkpoint stream" };
  return { ok: true, demo, simulation };
}

// Expands state changes into the exact two-pad mask visible at each sim tick.
// This is deliberately independent of rendering cadence and wall-clock time.
export function inputTimeline(source) {
  const result = validateCompatibleReplay(source);
  if (!result.ok) throw new Error(result.error);
  const { demo } = result;
  const states = [0, 0];
  const byTick = new Map();
  for (const [tick, pad, mask] of demo.commands) {
    const rows = byTick.get(tick) || [];
    rows.push([pad, mask]);
    byTick.set(tick, rows);
  }
  const timeline = [];
  for (let tick = 0; tick <= demo.durationTicks; tick++) {
    for (const [pad, mask] of byTick.get(tick) || []) states[pad] = mask;
    timeline.push([tick, states[0], states[1]]);
  }
  return timeline;
}

function stable(value) {
  if (Array.isArray(value)) return `[${value.map(stable).join(",")}]`;
  if (value && typeof value === "object") return `{${Object.keys(value).sort()
    .filter((key) => value[key] !== undefined)
    .map((key) => `${JSON.stringify(key)}:${stable(value[key])}`).join(",")}}`;
  return JSON.stringify(value);
}

export function replayDigest(source) {
  const result = validateCompatibleReplay(source);
  if (!result.ok) throw new Error(result.error);
  return createHash("sha256").update(stable(result.demo)).digest("hex");
}

export function timelineDigest(source) {
  return createHash("sha256").update(stable(inputTimeline(source))).digest("hex");
}

export function playbackPlan(source, playerBuild) {
  const result = validateCompatibleReplay(source);
  if (!result.ok) throw new Error(result.error);
  if (!integer(playerBuild, 1)) throw new Error("Player build must be an integer");
  return Object.freeze({
    demo: result.demo,
    simulation: result.simulation.id,
    decoder: `${REPLAY_FORMAT}-v${result.demo.version}`,
    clock: Object.freeze({ tickRate: result.simulation.tickRate,
      tickUs: result.simulation.tickUs }),
    recordingBuild: integer(result.demo.build, 1) ? result.demo.build : null,
    playerBuild,
    timeline: inputTimeline(result.demo),
  });
}

export function validateFixtureManifest(manifest) {
  if (!manifest || manifest.format !== FIXTURE_MANIFEST_FORMAT ||
      manifest.version !== 1 || !Array.isArray(manifest.fixtures))
    return { ok: false, error: "Unsupported fixture manifest" };
  const names = new Set();
  for (const item of manifest.fixtures) {
    if (!item || typeof item.name !== "string" || names.has(item.name) ||
        typeof item.file !== "string" || !/^[-a-z0-9]+\.json$/.test(item.file) ||
        !integer(item.recordingBuild, 1) || !integer(item.minimumPlayerBuild, 1) ||
        !/^[a-f0-9]{64}$/.test(item.replayDigest || "") ||
        !/^[a-f0-9]{64}$/.test(item.timelineDigest || ""))
      return { ok: false, error: `Invalid fixture: ${item?.name || "unnamed"}` };
    names.add(item.name);
  }
  return { ok: true, manifest: clone(manifest) };
}
