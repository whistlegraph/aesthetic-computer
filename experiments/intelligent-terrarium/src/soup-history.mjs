import { execFile } from "node:child_process";
import { mkdir, readFile } from "node:fs/promises";
import { promisify } from "node:util";
import { join, resolve } from "node:path";
import { atomicWrite, canonical } from "./canonical.mjs";
import { NoveltyArchive } from "./sort-soup.mjs";

const run = promisify(execFile);

async function git(root, args, options = {}) {
  return run("git", ["-C", root, ...args], { encoding: "utf8", ...options });
}

export class SoupHistory {
  constructor(root) {
    this.root = resolve(root);
    this.archivePath = join(this.root, "archive.json");
    this.editionPath = join(this.root, "edition.json");
    this.head = null;
    this.editions = 0;
    this.lastEdition = null;
  }

  static async open(root, { seed = "piecefarm-sort-soup-v1" } = {}) {
    const history = new SoupHistory(root);
    await mkdir(history.root, { recursive: true });
    try {
      await git(history.root, ["rev-parse", "--git-dir"]);
    } catch {
      await git(history.root, ["init", "-q"]);
      await git(history.root, ["config", "user.name", "Piecefarm"]);
      await git(history.root, ["config", "user.email", "piecefarm@localhost"]);
      await git(history.root, ["config", "commit.gpgsign", "false"]);
    }
    await history.refresh();
    try {
      const stored = JSON.parse(await readFile(history.archivePath, "utf8"));
      return { history, archive: NoveltyArchive.fromJSON(stored), restored: true };
    } catch (error) {
      if (error.code !== "ENOENT") throw error;
      return { history, archive: new NoveltyArchive({ seed }), restored: false };
    }
  }

  async refresh() {
    try {
      const [head, count] = await Promise.all([
        git(this.root, ["rev-parse", "HEAD"]),
        git(this.root, ["rev-list", "--count", "HEAD"]),
      ]);
      this.head = head.stdout.trim();
      this.editions = Number(count.stdout.trim()) || 0;
    } catch {
      this.head = null;
      this.editions = 0;
      this.lastEdition = null;
      return this.snapshot();
    }
    try { this.lastEdition = JSON.parse(await readFile(this.editionPath, "utf8")); }
    catch (error) { if (error.code !== "ENOENT") throw error; }
    return this.snapshot();
  }

  snapshot() {
    return {
      head: this.head,
      shortHead: this.head?.slice(0, 8) || null,
      editions: this.editions,
      lastEdition: this.lastEdition ? { ...this.lastEdition } : null,
    };
  }

  async pieceVmLineage(limit = 128) {
    if (!Number.isInteger(limit) || limit < 1 || limit > 512) throw new RangeError("PieceVM lineage limit must be 1..512");
    let commits;
    try {
      commits = (await git(this.root, ["log", `--max-count=${limit}`, "--format=%H", "--", "archive.json"]))
        .stdout.trim().split("\n").filter(Boolean).reverse();
    } catch {
      return [];
    }
    const lineage = new Map();
    for (const commit of commits) {
      try {
        const stored = JSON.parse((await git(this.root, ["show", `${commit}:archive.json`], { maxBuffer: 32 * 1024 * 1024 })).stdout);
        for (const candidate of stored.pieceVm?.lineage || stored.pieceVm?.residents || []) {
          if (candidate?.id && candidate?.source && !lineage.has(candidate.id)) lineage.set(candidate.id, candidate);
        }
      } catch { /* A pre-PieceVM edition contributes no lineage. */ }
    }
    return [...lineage.values()].slice(-limit);
  }

  async save(archive, { commit = false, reason = "checkpoint" } = {}) {
    await atomicWrite(this.archivePath, `${canonical(archive.toJSON())}\n`);
    if (!commit) return null;
    const snapshot = archive.snapshot();
    const edition = {
      schema: 1,
      reason,
      iteration: snapshot.iteration,
      accepted: snapshot.accepted,
      rejected: snapshot.rejected,
      coverage: snapshot.coverage,
      capacity: snapshot.capacity,
      selected: snapshot.selected,
      pieceVm: snapshot.pieceVm,
    };
    await atomicWrite(this.editionPath, `${canonical(edition)}\n`);
    await git(this.root, ["add", "--", "archive.json", "edition.json"]);
    try {
      await git(this.root, ["diff", "--cached", "--quiet"]);
      return null;
    } catch (error) {
      if (error.code !== 1) throw error;
    }
    const message = `season: ${reason} at iteration ${snapshot.iteration} coverage ${snapshot.coverage}`;
    await git(this.root, ["commit", "-q", "-m", message]);
    this.head = (await git(this.root, ["rev-parse", "HEAD"])).stdout.trim();
    this.editions += 1;
    this.lastEdition = edition;
    return this.head;
  }
}
