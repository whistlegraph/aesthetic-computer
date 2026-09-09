// live.mjs — the session's piece, and the channel that carries it to a phone.
//
// Every Aesthetic Code session opens on a new blank piece with a random name.
// The piece is a real file in the workspace, so the agent edits it like any
// other file, and every save is pushed to Aesthetic Computer's `/run` endpoint
// on a private code channel. Anything watching that channel — a phone that
// scanned the QR code — hot-reloads the new source immediately.
//
// This is the path the VS Code extension has always used: POST /run publishes
// { piece, source, codeChannel } to Redis, the session server relays it to the
// channel's subscribers, and the client swaps the running piece. A viewer joins
// the channel by opening `prompt~channel~<channel>~!autorun`, which runs the
// prompt's own `channel` command on arrival.
import { EventEmitter } from "node:events";
import { existsSync, mkdirSync, readFileSync, rmSync, watch, writeFileSync } from "node:fs";
import { basename, dirname, extname, join, resolve } from "node:path";
import { USER_AGENT } from "./ac-session.mjs";
import { randomChannel, randomSlug } from "./names.mjs";
import { DEFAULT_RUNTIME, runtimeFor, runtimeForExtension } from "./runtimes.mjs";

export const SITE = "https://aesthetic.computer";
// Scannable codes get smaller the shorter the text, and every phone camera
// adds the scheme back, so the QR carries a bare host.
export const SCAN_HOST = "aesthetic.computer";
const DISKS = join("system", "public", "aesthetic.computer", "disks");

// Pieces belong in the disks folder when the workspace is the Aesthetic
// Computer repository; anywhere else, the workspace root is the right home.
export function pieceDirectory(cwd) {
  const disks = join(cwd, DISKS);
  return existsSync(disks) ? disks : cwd;
}

export class LivePiece extends EventEmitter {
  constructor({
    cwd = process.cwd(),
    slug = randomSlug(),
    runtime = DEFAULT_RUNTIME,
    channel = randomChannel(),
    directory = pieceDirectory(cwd),
    fetch = globalThis.fetch,
    site = SITE,
    scanHost = SCAN_HOST,
  } = {}) {
    super();
    this.cwd = cwd;
    this.slug = slug;
    this.runtime = runtimeFor(runtime);
    this.channel = channel;
    this.directory = directory;
    this.fetch = fetch;
    this.site = site;
    this.scanHost = scanHost;
    // The exact blank written for the current file. A file still matching it
    // has never been touched, which is how an abandoned session knows what it
    // may delete. Deriving this from the content rather than tracking a flag
    // keeps the directory watcher — which fires on our own writes — out of it.
    this.blank = "";
    this.watcher = null;
    this.debounce = null;
    this.pushes = 0;
  }

  get file() {
    return join(this.directory, `${this.slug}${this.runtime.extension}`);
  }

  // The URL a phone scans: it joins the code channel and then sits waiting for
  // source, which arrives as soon as anything is pushed.
  get scanUrl() {
    return `${this.scanHost}/prompt~channel~${this.channel}~!autorun`;
  }

  // Where the piece answers once it has been published under a handle.
  publishedUrl(handle) {
    return handle ? `${this.site}/@${handle}/${this.slug}` : "";
  }

  // Write the blank piece if nothing is there yet. A blank that is never
  // edited is removed again on exit, so launching the interface and closing it
  // leaves no litter in the workspace.
  create() {
    mkdirSync(this.directory, { recursive: true });
    if (existsSync(this.file)) {
      this.blank = "";
      return this.file;
    }
    this.blank = this.runtime.blank(this.slug);
    writeFileSync(this.file, this.blank);
    return this.file;
  }

  // True while the piece is still exactly the blank this session wrote.
  get pristine() {
    if (!this.blank) return false;
    try {
      return readFileSync(this.file, "utf8") === this.blank;
    } catch {
      return false;
    }
  }

  // Drop the blank at `path` if nothing ever edited it.
  #discard(path, blank) {
    if (!blank) return false;
    try {
      if (readFileSync(path, "utf8") !== blank) return false;
      rmSync(path, { force: true });
      return true;
    } catch {
      return false;
    }
  }

  source() {
    try {
      return readFileSync(this.file, "utf8");
    } catch {
      return "";
    }
  }

  // Point at a different piece — the agent wrote or edited something else, or
  // the user renamed the session's piece.
  retarget(file) {
    const path = resolve(this.cwd, file);
    const runtime = runtimeForExtension(extname(path));
    if (!runtime) return false;
    const slug = basename(path, runtime.extension);
    if (path === this.file) return false;
    const previous = this.file;
    const previousBlank = this.blank;
    this.directory = dirname(path);
    this.slug = slug;
    this.runtime = runtime;
    this.blank = "";
    this.#discard(previous, previousBlank); // The blank was never used.
    this.emit("retarget", this.slug);
    return true;
  }

  // Rename the session's piece, carrying the blank file along with it.
  rename(name, runtimeId = this.runtime.id) {
    const slug = String(name || "").trim();
    if (!/^[a-zA-Z0-9_-]+$/.test(slug)) {
      throw new Error(`piece names may only use letters, digits, "-" and "_"`);
    }
    const previous = this.file;
    const previousBlank = this.blank;
    this.slug = slug;
    this.runtime = runtimeFor(runtimeId);
    this.blank = "";
    this.create();
    if (previous !== this.file) this.#discard(previous, previousBlank);
    return this.file;
  }

  // Push the current source onto the code channel.
  async push() {
    const source = this.source();
    if (!source.trim()) return false;
    const response = await this.fetch(`${this.site}/run`, {
      method: "POST",
      headers: { "Content-Type": "application/json", "User-Agent": USER_AGENT },
      body: JSON.stringify({ piece: this.slug, source, codeChannel: this.channel }),
    });
    if (!response.ok) throw new Error(`live push failed (HTTP ${response.status})`);
    this.pushes += 1;
    this.emit("push", this.pushes);
    return true;
  }

  // Watch the whole directory rather than the file: editors and patch tools
  // replace files instead of writing through them, which leaves a file watch
  // pointed at a discarded inode.
  watch(onError = () => {}) {
    this.unwatch();
    try {
      this.watcher = watch(this.directory, (_event, name) => {
        if (name && name !== basename(this.file)) return;
        clearTimeout(this.debounce);
        this.debounce = setTimeout(() => {
          this.push().catch(onError);
        }, 250);
        this.debounce.unref?.();
      });
      this.watcher.unref?.();
    } catch {}
    return this;
  }

  unwatch() {
    clearTimeout(this.debounce);
    try {
      this.watcher?.close();
    } catch {}
    this.watcher = null;
  }

  // Remove an untouched blank so an abandoned session leaves nothing behind.
  cleanup() {
    this.unwatch();
    return this.#discard(this.file, this.blank);
  }
}
