import { randomUUID, timingSafeEqual } from "node:crypto";
import { canonical } from "./canonical.mjs";
import { normalizeHandle, ORGAN_NAMES } from "./core.mjs";

const MODALITIES = new Set(["text", "gesture", "proximity", "sound", "media"]);

function sameSecret(a, b) {
  const left = Buffer.from(a);
  const right = Buffer.from(b);
  return left.length === right.length && timingSafeEqual(left, right);
}

export class Mediorgan {
  constructor(repository, { capabilities = {}, maxProdsPerSecond = 12 } = {}) {
    this.repository = repository;
    this.capabilities = new Map(Object.entries(capabilities).map(([token, handle]) => [token, normalizeHandle(handle)]));
    this.maxProdsPerSecond = maxProdsPerSecond;
    this.rate = new Map();
  }

  authenticate(header) {
    const match = /^Bearer ([^\s]+)$/.exec(String(header || ""));
    if (!match) return null;
    for (const [token, handle] of this.capabilities) {
      if (sameSecret(token, match[1])) return handle;
    }
    return null;
  }

  async ensurePresent(handle) {
    if (!this.repository.terrarium.state.visitors[handle]) {
      return this.repository.transact("visitor-enter", { handle, position: { x: 0, y: 1.7, z: 6 } });
    }
    return null;
  }

  async prod(handle, request) {
    this.#takeRate(handle);
    const target = String(request?.target || "");
    const modality = String(request?.modality || "text");
    if (!ORGAN_NAMES.includes(target)) throw new TypeError("unknown target organ");
    if (!MODALITIES.has(modality)) throw new TypeError("unknown prod modality");
    if (canonical(request?.stimulus ?? "").length > 4096) throw new TypeError("prod stimulus is too large");
    await this.ensurePresent(handle);
    const payload = {
      handle,
      prodId: randomUUID(),
      target,
      modality,
      stimulus: request?.stimulus ?? "",
    };
    if (request?.position !== undefined) payload.position = request.position;
    return this.repository.transact("organ-prod", payload);
  }

  #takeRate(handle) {
    const second = Math.floor(Date.now() / 1000);
    const previous = this.rate.get(handle);
    const current = previous?.second === second ? previous : { second, count: 0 };
    current.count += 1;
    this.rate.set(handle, current);
    if (current.count > this.maxProdsPerSecond) throw new Error("prod rate limit reached");
  }
}
