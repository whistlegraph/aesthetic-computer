import { deflateSync } from "node:zlib";

function crc32(buffer) {
  let crc = 0xffffffff;
  for (const byte of buffer) {
    crc ^= byte;
    for (let bit = 0; bit < 8; bit += 1) crc = (crc >>> 1) ^ (0xedb88320 & -(crc & 1));
  }
  return (crc ^ 0xffffffff) >>> 0;
}

function pngChunk(type, data) {
  const name = Buffer.from(type);
  const output = Buffer.alloc(12 + data.length);
  output.writeUInt32BE(data.length, 0); name.copy(output, 4); data.copy(output, 8);
  output.writeUInt32BE(crc32(Buffer.concat([name, data])), 8 + data.length);
  return output;
}

export function rgbPng(width, height, rgb) {
  if (rgb.length !== width * height * 3) throw new Error("invalid RGB field dimensions");
  const scanlines = Buffer.alloc((width * 3 + 1) * height);
  for (let y = 0; y < height; y += 1) {
    const output = y * (width * 3 + 1); scanlines[output] = 0;
    rgb.copy(scanlines, output + 1, y * width * 3, (y + 1) * width * 3);
  }
  const header = Buffer.alloc(13);
  header.writeUInt32BE(width, 0); header.writeUInt32BE(height, 4);
  header[8] = 8; header[9] = 2;
  return Buffer.concat([
    Buffer.from([137, 80, 78, 71, 13, 10, 26, 10]),
    pngChunk("IHDR", header), pngChunk("IDAT", deflateSync(scanlines, { level: 6 })), pngChunk("IEND", Buffer.alloc(0)),
  ]);
}

const REVIEW_SCHEMA = {
  type: "object",
  additionalProperties: false,
  properties: {
    quality: { type: "number", minimum: 0, maximum: 1 },
    coherence: { type: "number", minimum: 0, maximum: 1 },
    distinctiveness: { type: "number", minimum: 0, maximum: 1 },
    artifact: { type: "string", enum: ["none", "rainbow-noise", "muddy", "gray-wash", "raw-buffer", "flat", "banding", "flicker-risk", "other"] },
    tags: { type: "array", items: { type: "string" }, minItems: 1, maxItems: 5 },
    description: { type: "string", maxLength: 180 },
    criticism: { type: "string", maxLength: 180 },
    capability: { type: "string", enum: ["none", "feedback", "masking", "symmetry", "tiling", "displacement", "cellular", "sprite-memory", "projection", "color-dynamics"] },
    mutationHints: { type: "array", items: { type: "string", enum: ["add-feedback", "add-masking", "add-symmetry", "add-tiling", "add-displacement", "add-cellular", "use-sprites", "deepen-boxes", "diversify-color", "stabilize-temporal", "increase-contrast", "reduce-noise"] }, minItems: 1, maxItems: 3 },
    recommendation: { type: "string", enum: ["retain", "watch", "reject"] },
  },
  required: ["quality", "coherence", "distinctiveness", "artifact", "tags", "description", "criticism", "capability", "mutationHints", "recommendation"],
};

const CAPABILITIES = new Set(REVIEW_SCHEMA.properties.capability.enum);
const MUTATION_HINTS = new Set(REVIEW_SCHEMA.properties.mutationHints.items.enum);

function outputText(response) {
  for (const item of response.output || []) for (const content of item.content || []) {
    if (content.type === "refusal") throw new Error(`visual review refused: ${content.refusal}`);
    if (content.type === "output_text") return content.text;
  }
  throw new Error("visual review returned no structured output");
}

function validReview(value) {
  return value && ["retain", "watch", "reject"].includes(value.recommendation)
    && ["none", "rainbow-noise", "muddy", "gray-wash", "raw-buffer", "flat", "banding", "flicker-risk", "other"].includes(value.artifact)
    && [value.quality, value.coherence, value.distinctiveness].every((number) => Number.isFinite(number) && number >= 0 && number <= 1)
    && Array.isArray(value.tags) && value.tags.length >= 1 && value.tags.length <= 5
    && typeof value.criticism === "string" && value.criticism.length <= 180
    && CAPABILITIES.has(value.capability)
    && Array.isArray(value.mutationHints) && value.mutationHints.length >= 1 && value.mutationHints.length <= 3
    && value.mutationHints.every((hint) => MUTATION_HINTS.has(hint));
}

export class VisualCurator {
  constructor({ apiKey, model = "gpt-5.6-sol", now = Date.now, cooldownMs = 10 * 60_000,
    onReview = null, request = fetch, maxOutputTokens = 600 } = {}) {
    this.apiKey = apiKey || null;
    this.model = model;
    this.now = now;
    this.cooldownMs = cooldownMs;
    this.onReview = onReview;
    this.request = request;
    this.maxOutputTokens = maxOutputTokens;
    this.inflight = false;
    this.lastReviewAt = -Infinity;
    this.seen = new Set();
    this.disabledReason = apiKey ? null : "no-key";
    this.reviewed = 0;
    this.failures = 0;
    this.lastReview = null;
    this.lastError = null;
  }

  status() { return this.apiKey ? "armed" : this.disabledReason || "closed"; }

  telemetry() {
    return {
      status: this.status(), inflight: this.inflight, reviewed: this.reviewed,
      failures: this.failures, seen: this.seen.size,
      lastReviewAt: Number.isFinite(this.lastReviewAt) ? this.lastReviewAt : null,
      lastSpecimenId: this.lastReview?.specimenId || null,
      lastTrigger: this.lastReview?.trigger || null,
      lastRecommendation: this.lastReview?.recommendation || null,
      lastError: this.lastError,
    };
  }

  eligible(candidate, { trigger = "visual-novelty", lifecycle = null } = {}) {
    const energy = candidate?.sample?.energy?.at(-1) || {};
    const key = `${candidate?.id}:${trigger}`;
    const common = Boolean(this.apiKey && candidate?.domain === "raster" && candidate.retained
      && candidate.sample?.rgb && !this.seen.has(key));
    if (!common) return false;
    if (trigger === "high-health") return lifecycle?.samples >= 8 && lifecycle.healthMean >= 82 && lifecycle.healthyRatio >= .75;
    if (trigger === "health-variability") return lifecycle?.samples >= 8 && lifecycle.healthRange >= 24 && lifecycle.healthStdDev >= 7;
    return candidate.aliveness === "alive" && energy.actual >= .05 && energy.noise <= .20
      && (energy.muddiness || 0) <= .45 && energy.coherence >= .48 && candidate.quality >= .04;
  }

  async consider(candidate, context = {}) {
    const trigger = context.trigger || "visual-novelty";
    const seenKey = `${candidate?.id}:${trigger}`;
    if (!this.eligible(candidate, context) || this.inflight || this.now() - this.lastReviewAt < this.cooldownMs) return null;
    this.inflight = true; this.seen.add(seenKey);
    this.lastReviewAt = this.now();
    try {
      const sample = candidate.sample;
      const image = rgbPng(sample.width, sample.height, Buffer.from(sample.rgb, "hex"));
      const response = await this.request("https://api.openai.com/v1/responses", {
        method: "POST",
        headers: { authorization: `Bearer ${this.apiKey}`, "content-type": "application/json" },
        signal: AbortSignal.timeout(30_000),
        body: JSON.stringify({
          model: this.model,
          store: false,
          max_output_tokens: this.maxOutputTokens,
          input: [{ role: "user", content: [
            { type: "input_text", text: `This is one output from a bounded evolving Lisp RGB memory. Review trigger=${trigger}. Programmatic tests measured temporal actual=${sample.energy.at(-1)?.actual || 0}, noise=${sample.energy.at(-1)?.noise || 0}, coherence=${sample.energy.at(-1)?.coherence || 0}; lifecycle=${JSON.stringify(context.lifecycle || {})}. Curate visual structure conservatively. Penalize unstructured rainbow noise, flat fields, and generic static. Give one concise criticism, then nominate only bounded mutation hints from the schema that could add a real capability to verified descendants. The criticism is advisory: it cannot alter this specimen's health or bypass verification. Return JSON.` },
            { type: "input_image", image_url: `data:image/png;base64,${image.toString("base64")}`, detail: "low" },
          ] }],
          text: { format: { type: "json_schema", name: "piecefarm_visual_review", strict: true, schema: REVIEW_SCHEMA } },
        }),
      });
      if (!response.ok) {
        if (response.status === 401 || response.status === 403) {
          this.apiKey = null;
          this.disabledReason = "auth-failed";
        }
        throw new Error(`OpenAI visual review failed (${response.status})`);
      }
      const review = JSON.parse(outputText(await response.json()));
      if (!validReview(review)) throw new Error("OpenAI visual review failed local validation");
      const record = { ...review, model: this.model, at: new Date(this.now()).toISOString(), specimenId: candidate.id, trigger };
      await this.onReview?.(candidate, record);
      this.reviewed += 1;
      this.lastReview = record;
      this.lastError = null;
      return record;
    } catch (error) {
      this.failures += 1;
      this.lastError = String(error?.message || error).slice(0, 180);
      throw error;
    } finally {
      this.inflight = false;
    }
  }
}

export { REVIEW_SCHEMA };
