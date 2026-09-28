// Account-bound broadcasting of immutable artifact preview bytes.
// start/update(preview, {kind}); current/stop({artifactId, kind}).
// close/suspend cancel locally; only stop revokes publicly. Saved enabled is
// consent history, never restart authorization. No heartbeat: server TTL applies.
import {
  mkdir,
  readFile,
  writeFile,
  rename,
  lstat,
  realpath,
  rm,
} from "node:fs/promises";
import { resolve, join, relative, dirname } from "node:path";
import { createHash, randomBytes } from "node:crypto";
import { SITE, USER_AGENT } from "./ac-session.mjs";
import { decode } from "../media/picture/png.mjs";
import { verifyROM } from "../media/gameboy/rom.mjs";
const MAX = 8 * 1024 * 1024;
const MIMES = {
  picture: ["image/png", "image/jpeg", "image/webp"],
  sound: ["audio/wav", "audio/mpeg", "audio/ogg"],
  paper: ["application/pdf", "text/plain"],
  gameboy: ["application/x-gameboy-rom", "text/plain"],
};
const sha = (b) => createHash("sha256").update(b).digest("hex");
const sleep = (ms) => new Promise((resolve) => setTimeout(resolve, ms));
async function safe(base, path) {
  const target = resolve(base, path),
    rel = relative(base, target);
  if (rel === ".." || rel.startsWith("../"))
    throw new Error("Broadcast path leaves workspace.");
  let cursor = base;
  for (const part of ["", ...rel.split("/")]) {
    if (part) cursor = join(cursor, part);
    try {
      if ((await lstat(cursor)).isSymbolicLink())
        throw new Error("Broadcast paths cannot contain symlinks.");
    } catch (e) {
      if (e.code !== "ENOENT") throw e;
    }
  }
  return target;
}
async function bounded(file, max) {
  const stat = await lstat(file);
  if (!stat.isFile() || stat.size < 1 || stat.size > max)
    throw new Error(
      `Broadcast file must be a regular file under ${max} bytes.`,
    );
  const data = await readFile(file);
  if (!data.length || data.length > max) throw new Error("Broadcast file changed size while reading.");
  return data;
}
function validRecord(record, owner, artifactId, kind) {
  if (
    !record ||
    record.owner !== owner ||
    record.artifactId !== artifactId ||
    record.kind !== kind ||
    !/^[a-f0-9]{32}$/.test(record.id) ||
    !Number.isSafeInteger(record.sequence) ||
    record.sequence < 0
  )
    throw new Error("Invalid account-bound broadcast record.");
  return record;
}
export class DraftBroadcast {
  constructor({
    cwd,
    session,
    fetch = globalThis.fetch,
    site = SITE,
    onState = () => {},
    timeoutMs = 20000,
  } = {}) {
    this.cwd = resolve(cwd || process.cwd());
    this.session = session;
    this.fetch = fetch;
    this.site = site.replace(/\/$/, "");
    this.onState = onState;
    this.timeoutMs = timeoutMs;
    this.channels = new Map();
    this.queue = new Set();
    this.pumping = null;
    this.transport = Promise.resolve();
    this.closed = false;
    this.nextUpload = 0;
    this.timers = new Set();
  }
  wait(ms) {
    if (this.closed || ms <= 0) return Promise.resolve();
    return new Promise((resolve) => {
      const item = { resolve, timer: null };
      item.timer = setTimeout(() => {
        this.timers.delete(item);
        resolve();
      }, ms);
      this.timers.add(item);
    });
  }
  account() {
    const owner = this.session?.read?.()?.user?.sub,
      handle = this.session?.handle;
    if (typeof owner !== "string" || !owner || !handle)
      throw new Error("Sign in with an AC @handle before broadcasting.");
    return { owner, handle };
  }
  async channel({ artifactId, kind }) {
    if (!MIMES[kind])
      throw new Error(
        "Broadcast Picture, Sound, Paper or Game Boy output; Piece uses its existing live path.",
      );
    if (!/^[a-f0-9-]{36}$/.test(artifactId || ""))
      throw new Error("Invalid broadcast artifact identity.");
    const { owner, handle } = this.account(),
      key = sha(`${owner}\0${kind}\0${artifactId}`);
    if (this.channels.has(key)) return this.channels.get(key);
    const base = await realpath(this.cwd),
      file = await safe(base, `.easel/broadcasts/${key}.json`);
    let record = null;
    try {
      record = validRecord(
        JSON.parse(await bounded(file, 65536)),
        owner,
        artifactId,
        kind,
      );
    } catch (e) {
      if (e.code !== "ENOENT") throw e;
    }
    const channel = {
      key,
      file,
      owner,
      handle,
      artifactId,
      kind,
      record,
      authorized: false,
      active: false,
      pending: null,
      failed: null,
      waiters: [],
      epoch: 0,
      lastHash: null,
      controller: null,
    };
    this.channels.set(key, channel);
    return channel;
  }
  async mutate(channel, change) {
    const base = await realpath(this.cwd);
    await safe(base, relative(base, dirname(channel.file)));
    await mkdir(dirname(channel.file), { recursive: true, mode: 0o700 });
    const lock = await safe(base, relative(base, channel.file + ".lock"));
    let locked = false;
    for (let i = 0; i < 100; i++) {
      try {
        await mkdir(lock, { mode: 0o700 });
        locked = true;
        break;
      } catch (e) {
        if (e.code !== "EEXIST") throw e;
        await sleep(20);
      }
    }
    if (!locked)
      throw new Error("Broadcast record is locked by another operation.");
    try {
      let record;
      try {
        record = validRecord(
          JSON.parse(await bounded(channel.file, 65536)),
          channel.owner,
          channel.artifactId,
          channel.kind,
        );
      } catch (e) {
        if (e.code !== "ENOENT") throw e;
        record = {
          format: 1,
          id: randomBytes(16).toString("hex"),
          owner: channel.owner,
          handle: channel.handle,
          artifactId: channel.artifactId,
          kind: channel.kind,
          sequence: 0,
          enabled: false,
        };
      }
      change(record);
      const temp = await safe(base, relative(base, channel.file + ".pending"));
      await writeFile(temp, JSON.stringify(record) + "\n", { mode: 0o600 });
      await rename(temp, channel.file);
      channel.record = record;
      return record;
    } finally {
      await rm(lock, { recursive: true, force: true });
    }
  }
  snapshot(channel) {
    const m = channel.metadata || {};
    if (channel.active && m.expiresAt && Date.parse(m.expiresAt) <= Date.now())
      channel.active = false;
    return {
      id: channel.record?.id || null,
      enabled: !!channel.record?.enabled,
      paused: !!channel.record?.paused,
      active: !!channel.active,
      route: channel.reserved ? `${this.site}/watch/?id=${channel.record.id}` : channel.active ? m.route || null : null,
      scanUrl:
        channel.reserved ? `${this.site}/watch/?id=${channel.record.id}`.replace(/^https:\/\//, "") : channel.active && m.route ? m.route.replace(/^https:\/\//, "") : null,
      sequence: channel.record?.sequence || 0,
      version: m.version || null,
      expiresAt: m.expiresAt || null,
      artifactId: channel.artifactId,
      kind: channel.kind,
      handle: channel.handle,
      owner: channel.owner,
      error: channel.error || null,
    };
  }
  emit(channel) {
    try {
      this.onState(this.snapshot(channel));
    } catch {}
  }
  async current(identity) {
    if (!this.session?.read?.()?.user?.sub || !this.session?.handle)
      return { ...identity, id: null, enabled: false, active: false, route: null, scanUrl: null, sequence: 0, expiresAt: null };
    return this.snapshot(await this.channel(identity));
  }
  async reserve(identity) {
    const channel = await this.channel(identity);
    if (!channel.record) await this.mutate(channel, r => { r.enabled = true; });
    channel.reserved = true;
    return this.snapshot(channel);
  }
  async automatic(preview, {kind} = {}) {
    const identity = {artifactId: preview?.artifactId, kind};
    const channel = await this.channel(identity);
    if (channel.starting) await channel.starting;
    if (channel.authorized) return this.update(preview, identity);
    const starting = this.start(preview, identity);
    channel.starting = starting;
    try { return await starting; }
    finally { if (channel.starting === starting) channel.starting = null; }
  }
  fresh(channel) {
    return channel.active && Date.parse(channel.metadata?.expiresAt || '') > Date.now() + 1800000;
  }
  async frame(preview, kind) {
    if (!Number.isSafeInteger(preview?.version) || preview.version < 1)
      throw new Error("Broadcast preview needs a saved version.");
    if (!MIMES[kind]?.includes(preview.mime))
      throw new Error("Build/render a supported output before broadcasting.");
    const base = await realpath(this.cwd),
      versionRoot = await safe(
        base,
        `.easel-media/artifacts/${preview.artifactId}/v${preview.version}`,
      ),
      file = await safe(
        versionRoot,
        relative(versionRoot, resolve(base, relative(this.cwd, resolve(preview.path)))),
      );
    const revision = JSON.parse(
      await bounded(await safe(versionRoot, "revision.json"), 1024 * 1024),
    );
    const name = relative(versionRoot, file);
    if (
      revision.version !== preview.version ||
      revision.preview?.path !== name ||
      revision.preview?.mime !== preview.mime ||
      !revision.files?.includes(name)
    )
      throw new Error("Preview does not match its saved artifact revision.");
    if (kind === "gameboy" && revision.sourceAhead) {
      if (!revision.files?.includes("main.c") || !/^[a-f0-9]{64}$/.test(revision.hashes?.["main.c"] || ""))
        throw new Error("Game Boy draft needs a declared, hashed main.c source.");
      const data = await bounded(await safe(versionRoot, "main.c"), MAX);
      const hash = sha(data);
      if (hash !== revision.hashes["main.c"])
        throw new Error("Broadcast Game Boy source hash mismatch.");
      return { kind, mime: "text/plain", version: preview.version, hash, data };
    }
    if (revision.sourceAhead)
      throw new Error(
        "Build the changed source before broadcasting a new output.",
      );
    const data = await bounded(file, MAX),
      hash = sha(data);
    if (revision.hashes?.[name] !== hash)
      throw new Error("Broadcast preview hash mismatch.");
    if (preview.mime === "image/png") decode(data);
    if (preview.mime === "application/x-gameboy-rom") verifyROM(data);
    if (
      preview.mime === "application/pdf" &&
      data.subarray(0, 5).toString() !== "%PDF-"
    )
      throw new Error("Invalid PDF preview.");
    return { kind, mime: preview.mime, version: preview.version, hash, data };
  }
  async start(preview, { kind } = {}) {
    if (this.closed) throw new Error("Broadcaster is closed.");
    const channel = await this.channel({
      artifactId: preview?.artifactId,
      kind,
    });
    const startingEpoch = channel.epoch;
    const frame = await this.frame(preview, kind);
    if (this.closed || channel.epoch !== startingEpoch) throw new Error("Broadcast stopped.");
    await this.mutate(channel, (r) => {
      r.enabled = true;
      r.paused = false;
      r.handle = channel.handle;
    });
    if (this.closed || channel.epoch !== startingEpoch) {
      await this.mutate(channel, r => { r.enabled = false; });
      throw new Error("Broadcast stopped.");
    }
    channel.authorized = true;
    channel.active = false;
    channel.lastHash = null;
    channel.epoch++;
    channel.error = null;
    return this.enqueue(channel, frame);
  }
  async update(preview, { kind } = {}) {
    if (this.closed) throw new Error("Broadcaster is closed.");
    const channel = await this.channel({
        artifactId: preview?.artifactId,
        kind,
      }),
      epoch = channel.epoch;
    if (!channel.authorized)
      throw new Error(
        "This artifact has not connected yet.",
      );
    const frame = await this.frame(preview, kind);
    if (epoch !== channel.epoch || !channel.authorized)
      throw new Error("Broadcast was stopped.");
    return this.enqueue(channel, frame);
  }
  enqueue(channel, frame) {
    if (
      this.fresh(channel) &&
      channel.lastHash === frame.hash &&
      !channel.pending &&
      !channel.inflight
    )
      return Promise.resolve(this.snapshot(channel));
    if (channel.latestVersion && frame.version < channel.latestVersion)
      return Promise.resolve(this.snapshot(channel));
    channel.latestVersion = frame.version;
    channel.pending = frame;
    channel.failed = null;
    this.queue.add(channel.key);
    const pending = new Promise((resolve, reject) =>
      channel.waiters.push({ resolve, reject }),
    );
    this.pump();
    return pending;
  }
  serialized(work) {
    const next = this.transport.then(work, work);
    this.transport = next.catch(() => {});
    return next;
  }
  pump() {
    if (this.pumping || this.closed) return;
    this.pumping = (async () => {
      while (this.queue.size && !this.closed) {
        const key = this.queue.values().next().value;
        this.queue.delete(key);
        const channel = this.channels.get(key);
        if (!channel.authorized || !channel.pending) continue;
        const delay = this.nextUpload - Date.now();
        if (delay > 0) await this.wait(delay);
        if (this.closed || !channel.authorized) continue;
        const frame = channel.pending;
        channel.pending = null;
        if (this.fresh(channel) && channel.lastHash === frame.hash) {
          for (const waiter of channel.waiters.splice(0))
            waiter.resolve(this.snapshot(channel));
          continue;
        }
        channel.inflight = true;
        const epoch = channel.epoch;
        try {
          const state = await this.serialized(() =>
            this.send(channel, frame, epoch),
          );
          if (channel.authorized && epoch === channel.epoch) {
            channel.error = null;
            if (!channel.pending) {
              const waiters = channel.waiters.splice(0);
              for (const w of waiters) w.resolve(state);
            }
          }
        } catch (error) {
          channel.active = false;
          channel.error = error.message;
          channel.failed = channel.pending || frame;
          if (!channel.pending) {
            const waiters = channel.waiters.splice(0);
            for (const w of waiters) w.reject(error);
          }
          this.emit(channel);
        } finally {
          channel.inflight = false;
        }
        if (channel.pending && channel.authorized) this.queue.add(key);
      }
    })().finally(() => {
      this.pumping = null;
      if (this.queue.size && !this.closed) this.pump();
    });
  }
  async request(channel, method, payload) {
    if (method === "POST" && (this.closed || !channel.authorized))
      throw new Error("Broadcast stopped.");
    if (this.account().owner !== channel.owner)
      throw new Error("AC account changed; broadcast transport stopped.");
    const token = await this.session.token();
    if (this.account().owner !== channel.owner)
      throw new Error("AC account changed; broadcast transport stopped.");
    if (method === "POST" && (this.closed || !channel.authorized))
      throw new Error("Broadcast stopped.");
    const controller = new AbortController();
    channel.controller = controller;
    const timer = setTimeout(
      () => controller.abort(new Error("Broadcast transport timed out.")),
      this.timeoutMs,
    );
    try {
      const response = await this.fetch(`${this.site}/api/easel-live`, {
        method,
        headers: {
          Authorization: `Bearer ${token}`,
          "Content-Type": "application/json",
          "User-Agent": USER_AGENT,
        },
        body: JSON.stringify(payload),
        signal: controller.signal,
      });
      const body = await response.json().catch(() => ({}));
      return { response, body };
    } finally {
      clearTimeout(timer);
      if (channel.controller === controller) channel.controller = null;
    }
  }
  async send(channel, frame, epoch) {
    for (let attempt = 0; attempt < 3; attempt++) {
      if (this.closed || !channel.authorized || channel.epoch !== epoch)
        throw new Error("Broadcast stopped.");
      const record = await this.mutate(channel, (r) => {
        r.sequence = Math.max(r.sequence, channel.serverSequence || 0) + 1;
      });
      this.nextUpload =
        Date.now() + Math.max(500, (frame.data.length / 262144) * 1000);
      const { response, body } = await this.request(channel, "POST", {
        id: record.id,
        kind: frame.kind,
        version: frame.version,
        mime: frame.mime,
        data: frame.data.toString("base64"),
        status: "live",
        sequence: record.sequence,
      });
      if (response.status === 409) {
        const sequence = body.sequence ?? body.currentSequence;
        if (!Number.isSafeInteger(sequence) || sequence < 0)
          throw new Error(
            "Broadcast sequence was stale; server did not supply its current sequence.",
          );
        channel.serverSequence = sequence;
        await this.wait(Math.max(0, this.nextUpload - Date.now()));
        continue;
      }
      if (!response.ok)
        throw new Error(
          `Broadcast failed (HTTP ${response.status}). The latest frame is retained; reconnecting automatically.`,
        );
      if (
        body.id !== record.id ||
        body.sequence !== record.sequence ||
        body.status !== "live" ||
        body.kind !== frame.kind || body.version !== frame.version || body.mime !== frame.mime
      )
        throw new Error("Broadcast server returned mismatched metadata.");
      let route = null;
      if (body.route) {
        const url = new URL(body.route, this.site);
        if (
          url.origin !== new URL(this.site).origin ||
          url.pathname !== "/watch/" ||
          url.searchParams.get("id") !== record.id
        )
          throw new Error(
            "Broadcast server returned an unexpected viewer route.",
          );
        route = url.href;
      }
      if (this.account().owner !== channel.owner)
        throw new Error("AC account changed; broadcast response discarded.");
      if (channel.authorized && epoch === channel.epoch) {
        channel.active = true;
        channel.error = null;
        channel.lastHash = frame.hash;
        channel.metadata = { ...body, route };
        this.emit(channel);
      }
      return this.snapshot(channel);
    }
    throw new Error("Broadcast sequence remained stale after three attempts.");
  }
  async stop(identity) {
    const channel = await this.channel(identity);
    channel.authorized = false;
    channel.active = false;
    channel.epoch++;
    channel.pending = null;
    channel.failed = null;
    this.queue.delete(channel.key);
    channel.controller?.abort(new Error("Broadcast stopped."));
    for (const waiter of channel.waiters.splice(0))
      waiter.reject(new Error("Broadcast stopped."));
    this.emit(channel);
    if (!channel.record) {
      this.emit(channel);
      return this.snapshot(channel);
    }
    await this.mutate(channel, (r) => {
      r.enabled = false;
      r.paused = true;
    });
    return this.serialized(async () => {
      for (let attempt = 0; attempt < 3; attempt++) {
        const record = await this.mutate(channel, (r) => {
          r.enabled = false;
          r.sequence = Math.max(r.sequence, channel.serverSequence || 0) + 1;
        });
        const { response, body } = await this.request(channel, "DELETE", {
          id: record.id,
          sequence: record.sequence,
        });
        if (response.status === 409) {
          const sequence = body.sequence ?? body.currentSequence;
          if (!Number.isSafeInteger(sequence))
            throw new Error(
              "Cannot revoke stale broadcast without server sequence.",
            );
          channel.serverSequence = sequence;
          continue;
        }
        if (!response.ok)
          throw new Error(
            `Broadcast stop failed (HTTP ${response.status}); retry /live off. Saved consent is off.`,
          );
        if (
          body.id !== record.id ||
          body.sequence !== record.sequence ||
          body.status !== "stopped"
        )
          throw new Error("Broadcast stop returned mismatched metadata.");
        channel.error = null;
        channel.metadata = null;
        this.emit(channel);
        return this.snapshot(channel);
      }
      throw new Error("Broadcast stop sequence remained stale.");
    }).catch(error => { channel.error = error.message; this.emit(channel); throw error; });
  }
  suspend() {
    for (const channel of this.channels.values()) {
      channel.epoch++;
      channel.authorized = false;
      channel.active = false;
      channel.pending = null;
      channel.controller?.abort(new Error("Broadcast account suspended."));
      for (const waiter of channel.waiters.splice(0)) waiter.reject(new Error("Broadcast account suspended."));
      this.queue.delete(channel.key);
      this.emit(channel);
    }
  }
  close() {
    this.closed = true;
    for (const item of this.timers) {
      clearTimeout(item.timer);
      item.resolve();
    }
    this.timers.clear();
    this.queue.clear();
    for (const channel of this.channels.values()) {
      channel.authorized = false;
      channel.active = false;
      channel.controller?.abort(new Error("Broadcaster closed."));
      for (const waiter of channel.waiters.splice(0))
        waiter.reject(new Error("Broadcaster closed."));
      channel.pending = null;
      this.emit(channel);
    }
  }
}
