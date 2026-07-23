import { canonical, clone, hash } from "./canonical.mjs";
import { Prng } from "./prng.mjs";
import { decideReflection } from "./reflection-policy.mjs";

export const LIMITS = Object.freeze({
  entities: 12,
  visitors: 16,
  episodes: 256,
  signalLength: 160,
  advanceTicks: 600,
});

export const ORGAN_NAMES = Object.freeze([
  "sensory",
  "spatial",
  "drive",
  "memory",
  "action",
  "voice",
]);

const WORLD_RADIUS = 12;
const round = (value) => Math.round(value * 1e6) / 1e6;
const clamp = (value, low, high) => Math.max(low, Math.min(high, value));

export function normalizeHandle(value) {
  const handle = String(value || "").trim().toLowerCase();
  const normalized = handle.startsWith("@") ? handle : `@${handle}`;
  if (!/^@[a-z0-9][a-z0-9._-]{0,62}$/.test(normalized)) {
    throw new TypeError("invalid verified handle");
  }
  return normalized;
}

function position(value = {}) {
  return {
    x: round(clamp(Number(value.x) || 0, -WORLD_RADIUS, WORLD_RADIUS)),
    y: round(clamp(Number(value.y) || 0, 0, 8)),
    z: round(clamp(Number(value.z) || 0, -WORLD_RADIUS, WORLD_RADIUS)),
  };
}

function pushEpisode(state, episode) {
  state.mind.episodes.push(episode);
  if (state.mind.episodes.length > LIMITS.episodes) {
    state.mind.episodes.splice(0, state.mind.episodes.length - LIMITS.episodes);
  }
}

function genesis(seed) {
  const rng = new Prng(seed);
  const entities = [];
  for (let index = 0; index < LIMITS.entities; index += 1) {
    entities.push({
      id: `spore-${String(index + 1).padStart(2, "0")}`,
      species: ["moss", "bell", "mote"][index % 3],
      x: round(rng.signed() * 8),
      y: round(0.4 + rng.float() * 3),
      z: round(rng.signed() * 8),
      vx: round(rng.signed() * 0.04),
      vy: round(rng.signed() * 0.01),
      vz: round(rng.signed() * 0.04),
      energy: round(0.4 + rng.float() * 0.5),
    });
  }
  return {
    schema: 1,
    seed: String(seed),
    tick: 0,
    lastSeq: 0,
    sonicCount: 0,
    rng: rng.toJSON(),
    entities,
    visitors: {},
    mind: {
      drives: { curiosity: 0.5, rest: 0.2, social: 0.1 },
      weights: { approach: 0.5, sing: 0.5, wander: 0.5 },
      episodes: [],
    },
  };
}

export class Terrarium {
  constructor(seed, snapshot) {
    this.state = snapshot ? clone(snapshot) : genesis(seed);
  }

  static fromSnapshot(snapshot) {
    return new Terrarium(snapshot.seed, snapshot);
  }

  snapshot() {
    return clone(this.state);
  }

  stateHash() {
    return hash(this.state);
  }

  apply(record) {
    const expected = this.state.lastSeq + 1;
    if (record.seq !== expected) throw new Error(`event sequence ${record.seq}; expected ${expected}`);
    const payload = clone(record.payload || {});
    let outputs = [];

    switch (record.kind) {
      case "advance":
        outputs = this.#advance(payload.ticks);
        break;
      case "visitor-enter":
        this.#visitorEnter(payload);
        break;
      case "visitor-move":
        this.#visitorMove(payload);
        break;
      case "visitor-signal":
        outputs = this.#visitorSignal(payload);
        break;
      case "organ-prod":
        outputs = this.#organProd(payload);
        break;
      case "reflection-decision":
        this.#reflectionDecision(payload);
        break;
      case "visitor-leave":
        this.#visitorLeave(payload);
        break;
      default:
        throw new Error(`unknown event kind: ${record.kind}`);
    }

    this.state.lastSeq = record.seq;
    return outputs;
  }

  #visitorEnter(payload) {
    const handle = normalizeHandle(payload.handle);
    if (!this.state.visitors[handle] && Object.keys(this.state.visitors).length >= LIMITS.visitors) {
      throw new Error("visitor limit reached");
    }
    this.state.visitors[handle] = { handle, ...position(payload.position), enteredAtTick: this.state.tick };
    pushEpisode(this.state, { tick: this.state.tick, kind: "met", actor: handle });
    this.state.mind.drives.social = round(clamp(this.state.mind.drives.social + 0.08, 0, 1));
  }

  #visitorMove(payload) {
    const handle = normalizeHandle(payload.handle);
    const visitor = this.state.visitors[handle];
    if (!visitor) throw new Error("visitor is not present");
    Object.assign(visitor, position(payload.position));
  }

  #visitorSignal(payload) {
    const handle = normalizeHandle(payload.handle);
    const visitor = this.state.visitors[handle];
    if (!visitor) throw new Error("visitor is not present");
    const signal = String(payload.signal || "").trim().slice(0, LIMITS.signalLength);
    if (!signal) throw new Error("empty visitor signal");
    const salience = round(clamp(0.25 + signal.length / 240, 0.25, 0.9));
    pushEpisode(this.state, { tick: this.state.tick, kind: "signal", actor: handle, signal, salience });
    this.state.mind.weights.approach = round(clamp(this.state.mind.weights.approach + salience * 0.01, 0, 1));
    return [this.#sonic({
      entity: this.state.entities[0],
      cause: `visitor-signal:${handle}`,
      voice: "answer",
      pitch: 220 + signal.length,
      intensity: 0.45 + salience * 0.25,
    })];
  }

  #visitorLeave(payload) {
    const handle = normalizeHandle(payload.handle);
    if (this.state.visitors[handle]) {
      delete this.state.visitors[handle];
      pushEpisode(this.state, { tick: this.state.tick, kind: "parted", actor: handle });
    }
  }

  #organProd(payload) {
    const handle = normalizeHandle(payload.handle);
    const visitor = this.state.visitors[handle];
    if (!visitor) throw new Error("visitor is not present");
    const target = String(payload.target || "");
    if (!ORGAN_NAMES.includes(target)) throw new Error("unknown target organ");
    const modality = String(payload.modality || "text");
    if (!["text", "gesture", "proximity", "sound", "media"].includes(modality)) {
      throw new Error("unknown prod modality");
    }
    const prodId = String(payload.prodId || "").slice(0, 96);
    if (!prodId) throw new Error("prod requires a causal id");
    const stimulus = typeof payload.stimulus === "string"
      ? payload.stimulus.slice(0, LIMITS.signalLength)
      : clone(payload.stimulus || {});
    const summary = typeof stimulus === "string" ? stimulus : canonical(stimulus).slice(0, LIMITS.signalLength);
    pushEpisode(this.state, {
      tick: this.state.tick,
      kind: "prodded",
      actor: handle,
      organ: target,
      modality,
      prodId,
      stimulus: summary,
    });

    if (target === "spatial") Object.assign(visitor, position(payload.position || stimulus?.position));
    if (target === "drive" || target === "sensory") {
      this.state.mind.drives.curiosity = round(clamp(this.state.mind.drives.curiosity + 0.03, 0, 1));
    }
    if (target === "action") {
      this.state.mind.weights.approach = round(clamp(this.state.mind.weights.approach + 0.02, 0, 1));
    }
    if (target === "voice" || modality === "sound") {
      const entity = this.state.entities[this.state.tick % this.state.entities.length];
      return [this.#sonic({
        entity,
        cause: `prod:${prodId}:${target}`,
        voice: target === "voice" ? "mediorgan-answer" : "felt-sound",
        pitch: 180 + summary.length * 3,
        intensity: 0.48,
      })];
    }
    return [];
  }

  #reflectionDecision(payload) {
    if (payload.schema !== 1) throw new TypeError("reflection event schema must be 1");
    const expectedFields = ["decision", "engine", "outputDigest", "reason", "requestId", "schema", payload.proposal ? "proposal" : "failure"].sort();
    if (Object.keys(payload).sort().join(",") !== expectedFields.join(",")) {
      throw new TypeError("reflection event fields do not match schema");
    }
    if (!/^[a-f0-9]{24}$/.test(String(payload.requestId || ""))) throw new TypeError("invalid reflection request id");
    if (!/^[a-f0-9]{64}$/.test(String(payload.outputDigest || ""))) throw new TypeError("invalid reflection output digest");
    if (!/^[a-zA-Z0-9._:/-]{1,160}$/.test(String(payload.engine || ""))) throw new TypeError("invalid reflection engine");
    const evaluation = decideReflection({ proposal: payload.proposal, failure: payload.failure });
    if (payload.decision !== evaluation.decision || payload.reason !== evaluation.reason) {
      throw new Error("reflection decision does not match deterministic policy");
    }
    pushEpisode(this.state, {
      tick: this.state.tick,
      kind: "reflected",
      requestId: payload.requestId,
      outcome: evaluation.decision,
      reason: evaluation.reason,
      organ: evaluation.proposal?.target || null,
    });
    if (evaluation.decision !== "accepted") return;
    const { target, intensity } = evaluation.proposal;
    if (target === "sensory" || target === "drive") {
      this.state.mind.drives.curiosity = round(clamp(this.state.mind.drives.curiosity + intensity, 0, 1));
    } else if (target === "memory") {
      this.state.mind.drives.rest = round(clamp(this.state.mind.drives.rest - intensity, 0, 1));
    } else if (target === "voice") {
      this.state.mind.drives.social = round(clamp(this.state.mind.drives.social + intensity, 0, 1));
    } else {
      this.state.mind.weights.approach = round(clamp(this.state.mind.weights.approach + intensity, 0, 1));
    }
  }

  #advance(value) {
    const ticks = Number(value);
    if (!Number.isInteger(ticks) || ticks < 1 || ticks > LIMITS.advanceTicks) {
      throw new TypeError(`advance ticks must be 1..${LIMITS.advanceTicks}`);
    }
    const outputs = [];
    const rng = Prng.fromJSON(this.state.rng);
    for (let step = 0; step < ticks; step += 1) {
      this.state.tick += 1;
      for (let index = 0; index < this.state.entities.length; index += 1) {
        const entity = this.state.entities[index];
        if ((this.state.tick + index) % 17 === 0) {
          entity.vx = round(clamp(entity.vx + rng.signed() * 0.025, -0.09, 0.09));
          entity.vy = round(clamp(entity.vy + rng.signed() * 0.008, -0.025, 0.025));
          entity.vz = round(clamp(entity.vz + rng.signed() * 0.025, -0.09, 0.09));
        }
        entity.x = round(entity.x + entity.vx);
        entity.y = round(entity.y + entity.vy);
        entity.z = round(entity.z + entity.vz);
        if (Math.abs(entity.x) > WORLD_RADIUS) { entity.x = round(clamp(entity.x, -WORLD_RADIUS, WORLD_RADIUS)); entity.vx = -entity.vx; }
        if (entity.y < 0.2 || entity.y > 5) { entity.y = round(clamp(entity.y, 0.2, 5)); entity.vy = -entity.vy; }
        if (Math.abs(entity.z) > WORLD_RADIUS) { entity.z = round(clamp(entity.z, -WORLD_RADIUS, WORLD_RADIUS)); entity.vz = -entity.vz; }
        entity.energy = round(clamp(entity.energy + rng.signed() * 0.002, 0.05, 1));
        if ((this.state.tick + index * 7) % 53 === 0) {
          outputs.push(this.#sonic({
            entity,
            cause: "autonomous-pulse",
            voice: entity.species,
            pitch: 120 + index * 19 + Math.round(entity.energy * 90),
            intensity: 0.2 + entity.energy * 0.45,
          }));
        }
      }
      this.state.mind.drives.curiosity = round(clamp(this.state.mind.drives.curiosity + rng.signed() * 0.003, 0, 1));
      this.state.mind.drives.rest = round(clamp(this.state.mind.drives.rest + 0.0004, 0, 1));
      this.state.mind.drives.social = round(clamp(this.state.mind.drives.social - 0.0002, 0, 1));
    }
    this.state.rng = rng.toJSON();
    return outputs;
  }

  #sonic({ entity, cause, voice, pitch, intensity }) {
    this.state.sonicCount += 1;
    const event = {
      id: `sonic-${String(this.state.sonicCount).padStart(8, "0")}`,
      tick: this.state.tick,
      kind: "sonic",
      source: [entity.x, entity.y, entity.z],
      voice,
      pitch: round(clamp(pitch, 40, 4000)),
      intensity: round(clamp(intensity, 0, 1)),
      radius: round(4 + intensity * 14),
      duration: round(0.08 + intensity * 0.45),
      cause,
    };
    pushEpisode(this.state, { tick: this.state.tick, kind: "sang", actor: entity.id, cause, sonicId: event.id });
    return event;
  }
}

export function outputHash(outputs) {
  return hash(canonical(outputs));
}
