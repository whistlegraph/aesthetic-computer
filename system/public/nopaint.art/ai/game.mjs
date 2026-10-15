export const LOCAL_MODELS = [
  ["classic", "Classic"],
  ["classic-pixels", "Pixels"],
  ["classic-color", "Color"],
  ["classic-primitives", "Primitives"],
].map(([id, name]) => ({
  id,
  name,
  location: "Browser · CPU",
  braincells: 0,
  available: true,
}));
export function modelChoices(offers, account) {
  return [
    ...LOCAL_MODELS,
    ...offers
      .filter((m) => m.id.startsWith("ac-"))
      .map((m) => ({
        ...m,
        serviceAvailable: Boolean(m.available),
        available: Boolean(
          m.available &&
            account.connected &&
            !account.stale &&
            account.remaining + account.purchased >= m.braincells,
        ),
      })),
  ];
}
export function paintCapacity(price, account) {
  return price === 0
    ? "∞"
    : price > 0 && account.connected
      ? String(
          Math.floor(
            ((account.remaining || 0) + (account.purchased || 0)) / price,
          ),
        )
      : "—";
}
export class Game {
  constructor({
    store,
    images,
    account,
    onchange = () => {},
    uuid = () => crypto.randomUUID(),
    now = () => Date.now(),
    seed = () => crypto.getRandomValues(new Uint32Array(1))[0] % 2 ** 31,
  }) {
    Object.assign(this, { store, images, account, onchange, uuid, now, seed });
    this.selection = "classic";
    this.strength = 0.25;
    this.busy = false;
    this.mask = null;
    this.hint = "";
    this.error = null;
    this.job = null;
    this.quote = null;
    this.pendingCloud = 0;
    this.saving = Promise.resolve();
  }
  get accepted() {
    return this.track?.steps.at(-1)?.image;
  }
  get before() {
    return this.job?.before || this.track?.steps.at(-2)?.image || this.accepted;
  }
  get models() {
    return modelChoices(this.account.models, this.account.value);
  }
  get canChange() {
    return Boolean(
      this.track &&
        !this.readOnly &&
        !this.busy &&
        !this.publishing &&
        !this.track.publication?.pending,
    );
  }
  get canPaint() {
    return this.canChange && Boolean(this.quote) && !this.error;
  }
  async boot({ readOnly = false } = {}) {
    this.readOnly = readOnly;
    const id = await this.store.setting("current");
    this.track = id && (await this.store.get(id));
    const savedModel = await this.store.setting("model");
    // Old Random selections could become paid after sign-in. Cloud inference
    // now always requires choosing a specific model; Classic mixes CPU moves.
    this.selection =
      LOCAL_MODELS.some((m) => m.id === savedModel) ||
      savedModel?.startsWith("ac-")
        ? savedModel
        : "classic";
    this.strength = (await this.store.setting("strength")) || 0.25;
    if (!this.track) await this.fresh();
    else if (this.track.pending && !readOnly) {
      this.track.events.push({
        ...this.track.pending,
        type: "interrupted",
        at: this.now(),
        detail: "Result unknown; no paid request was replayed.",
      });
      this.track.pending = null;
      await this.persist();
      this.error =
        "The previous move was interrupted. Its billing may have settled; refresh your balance before another Paint.";
    }
    this.mask = this.track.mask?.slice() || null;
    this.prepare();
    this.changed();
  }
  changed() {
    this.onchange();
  }
  persist() {
    if (this.readOnly) return Promise.resolve();
    const snapshot = structuredClone(this.track);
    this.saving = this.saving
      .catch(() => {})
      .then(() => this.store.save(snapshot));
    return this.saving;
  }
  prepare() {
    if (this.busy) return;
    const model = this.models.find((m) => m.id === this.selection);
    this.engine = model?.id || this.selection;
    this.quote =
      model?.available && (!model.id.startsWith("ac-") || !this.pendingCloud)
        ? {
            id: this.uuid(),
            model,
            braincells: model.braincells,
            seed: this.seed(),
            strength: this.strength,
          }
        : null;
  }
  async fresh() {
    if (this.busy || this.publishing)
      throw Error("Wait for the current operation.");
    if (this.track) await this.persist();
    const image = this.images.noise(this.seed());
    this.track = {
      id: this.uuid(),
      createdAt: this.now(),
      steps: [{ id: this.uuid(), image, at: this.now(), kind: "start" }],
      events: [],
      publication: null,
    };
    this.mask = null;
    this.error = null;
    await this.persist();
    if (!this.readOnly) await this.store.setting("current", this.track.id);
    this.prepare();
    this.changed();
  }
  async select(id) {
    if (!this.canChange) return;
    if (!this.models.some((m) => m.id === id)) throw Error("Unknown model.");
    this.selection = id;
    this.error = null;
    await this.store.setting("model", id);
    this.prepare();
    this.changed();
  }
  async changeStrength(value) {
    if (!this.canChange) return;
    if (![0.25, 0.5, 0.75].includes(value)) throw Error("Invalid change size.");
    this.strength = value;
    await this.store.setting("strength", value);
    this.prepare();
    this.changed();
  }
  async no() {
    if (this.publishing || this.track.publication?.pending) return;
    if (this.job) {
      this.job.cancelled = true;
      this.track.events.push({ type: "no", move: this.job.id, at: this.now() });
      this.job = null;
      this.busy = false;
      this.preview = null;
    } else if (this.track.steps.length > 1) {
      const step = this.track.steps.pop();
      this.track.events.push({ type: "no", at: this.now(), step });
    }
    this.error = null;
    this.prepare();
    await this.persist();
    this.changed();
  }
  async paint(quoteID = this.quote?.id) {
    if (!this.canPaint || quoteID !== this.quote.id) return false;
    const quote = this.quote,
      model = quote.model,
      job = {
        id: this.uuid(),
        before: this.accepted,
        track: this.track,
        cancelled: false,
        started: this.now(),
        quote,
        hint: this.hint,
      };
    this.job = job;
    this.busy = true;
    this.quote = null;
    this.error = null;
    this.preview = null;
    job.track.pending = {
      id: job.id,
      model: model.id,
      at: job.started,
      braincells: quote.braincells,
    };
    this.changed();
    const cloud = model.id.startsWith("ac-");
    if (cloud) this.pendingCloud++;
    try {
      // Persist the request identity BEFORE anything paid can be submitted.
      await this.persist();
      if (job.cancelled) return false;
      const area = await this.images.region(job.before, this.mask?.slice());
      if (job.cancelled) return false;
      let result;
      if (cloud)
        result = await this.account.move({
          requestId: job.id,
          image: area.input.split(",")[1],
          seed: quote.seed,
          strength: quote.strength,
          model: model.id,
          quote: model.quote,
          hint: job.hint,
          paints: job.track.steps.length - 1,
        });
      else
        result = await this.images.classic(
          area.input,
          { engine: model.id, seed: quote.seed, strength: quote.strength },
          async (image) => {
            const preview = await area.finish(image);
            if (!job.cancelled) {
              this.preview = preview;
              this.changed();
            }
          },
        );
      const image = await area.finish(
        cloud ? "data:image/png;base64," + result.image : result.image,
      );
      const step = {
        id: job.id,
        image,
        at: this.now(),
        model: model.id,
        seed: quote.seed,
        strength: quote.strength,
        seconds: (this.now() - job.started) / 1000,
        receipt: { ...result, image: undefined },
      };
      job.track.events.push({
        type: job.cancelled ? "cancelled-result" : "paint",
        at: this.now(),
        step,
      });
      if (!job.cancelled && this.track === job.track)
        job.track.steps.push(step);
      if (job.track.pending?.id === job.id) job.track.pending = null;
      await this.persistTrack(job.track);
      return true;
    } catch (error) {
      if (job.track.pending?.id === job.id) job.track.pending = null;
      job.track.events.push({
        type: "error",
        move: job.id,
        at: this.now(),
        message: error.message,
        braincells: cloud && error.charged !== 0 ? "unknown" : 0,
      });
      await this.persistTrack(job.track).catch(() => {});
      if (!job.cancelled) {
        this.error = error.message;
        this.failure = cloud
          ? {
              model: model.name,
              modelId: model.id,
              code: error.code,
              message: error.message,
              charged: error.charged,
            }
          : null;
      }
      return false;
    } finally {
      if (job.track.pending?.id === job.id) {
        job.track.pending = null;
        await this.persistTrack(job.track).catch(() => {});
      }
      if (cloud) {
        this.pendingCloud--;
        await this.account.refresh().catch(() => {});
      }
      if (this.job === job) {
        this.job = null;
        this.busy = false;
        this.preview = null;
      }
      this.prepare();
      this.changed();
    }
  }
  async crop(box) {
    if (!this.canChange) return;
    const image = await this.images.crop(this.accepted, box);
    this.track.steps.push({
      id: this.uuid(),
      image,
      at: this.now(),
      kind: "crop",
      box,
    });
    this.track.mask = this.mask = null;
    this.prepare();
    await this.persist();
    this.changed();
  }
  retry() {
    this.error = null;
    this.prepare();
    this.changed();
  }
  persistTrack(track) {
    const snapshot = structuredClone(track);
    this.saving = this.saving
      .catch(() => {})
      .then(() => this.store.save(snapshot));
    return this.saving;
  }
}
