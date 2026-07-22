// Pure Loopboy family/addressing + host-heartbeat contracts.
// Runtime adapters may read files or send fixed nudges, but this module never
// performs I/O and deliberately exposes no remote-control primitive.

export const IRIS_ADDRESS = "agent:iris@panda";

export const IRIS_JOEYDON_RESPONSIBILITY = Object.freeze({
  id: "fuser-joeydon-recovery",
  priority: "blocking",
  executionHost: "chicken",
  preserveOriginal: true,
  original: "UGC Deal Tracker",
  verification: "deployed chunked-resource path",
  guardrail: "Unrelated Iris backlog must not outrank this recovery.",
});

export function agentAddress(name, machine) {
  const n = String(name || "").trim().toLowerCase();
  const m = String(machine || "").trim().toLowerCase();
  if (!n || !m) throw new Error("agent name and machine are required");
  return `agent:${n}@${m}`;
}

export function parseAgentAddress(address) {
  const match = /^agent:([a-z0-9][a-z0-9_-]*)@([a-z0-9][a-z0-9._-]*)$/i.exec(String(address || ""));
  if (!match) throw new Error(`invalid Loopboy agent address: ${address}`);
  return { kind: "agent", name: match[1].toLowerCase(), machine: match[2].toLowerCase() };
}

export function resolveFleetMachine(input, registry) {
  const requested = String(input || "").trim().toLowerCase();
  const machines = registry?.machines || {};
  for (const [id, machine] of Object.entries(machines)) {
    const aliases = [id, machine.hostname, machine.computerName,
      machine.tailscale?.name, machine.status?.key]
      .filter(Boolean).map((value) => String(value).split(".")[0].toLowerCase());
    if (aliases.includes(requested)) return { id, machine };
  }
  return null;
}

export function makeIrisContact() {
  return {
    kind: "agent",
    address: IRIS_ADDRESS,
    displayName: "Iris Bloom",
    machine: "panda",
    ledgerId: "iris",
    capabilities: ["bounded-accountability-nudge", "status"],
    responsibility: { ...IRIS_JOEYDON_RESPONSIBILITY },
  };
}

export function boundedNudge(contact, text) {
  if (contact?.kind !== "agent") throw new Error("accountability nudges require an agent contact");
  const message = String(text || "").trim().replace(/\s+/g, " ");
  if (!message) throw new Error("nudge text is required");
  if (message.length > 500) throw new Error("accountability nudges are limited to 500 characters");
  return { type: "accountability-nudge", to: contact.address, text: message };
}

const ACTIVE = new Set(["working", "rendering", "awaiting"]);
export function aggregateMachineHeartbeat({ known = true, online = null, now = Date.now(),
  machineUpdated = 0, rocks = [], mission = null } = {}) {
  if (!known) return { state: "unknown", level: 0, boundedProgress: null };
  if (online === false) return { state: "offline", level: 0, boundedProgress: null };
  const freshestRock = Math.max(0, ...rocks.map((rock) => Number(rock.updated) || 0));
  const freshest = Math.max(Number(machineUpdated) || 0, freshestRock, Number(mission?.updatedAt) || 0);
  const age = freshest ? Math.max(0, now - freshest) : Infinity;
  const active = rocks.some((rock) => ACTIVE.has(String(rock.status).toLowerCase()));
  const blocked = mission?.blocked === true;
  let state;
  if (blocked || (online !== false && age > 15 * 60_000 && active)) state = "stalled";
  else if (active && age <= 3 * 60_000) state = "active";
  else if (online === true && age <= 15 * 60_000) state = "healthy";
  else if (online === true || age <= 60 * 60_000) state = "quiet";
  else state = "offline";
  const decay = Number.isFinite(age) ? Math.max(0.08, 1 - age / (60 * 60_000)) : 0;
  const level = state === "active" ? 1 : state === "healthy" ? Math.max(.55, decay)
    : state === "quiet" ? Math.min(.45, decay) : state === "stalled" ? .22 : 0;
  const progress = mission?.bounded === true && Number.isFinite(mission.progress)
    ? Math.min(1, Math.max(0, mission.progress)) : null;
  return { state, level, boundedProgress: progress, freshest, activeRocks: rocks.filter((r) => ACTIVE.has(String(r.status).toLowerCase())).length };
}
