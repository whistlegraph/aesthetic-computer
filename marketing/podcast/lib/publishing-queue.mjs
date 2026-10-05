// Durable Buzzsprout uploads. A job owns a snapshot, so retries never narrate
// again or pick up a later edit. Receipts are written before jobs are removed.
import { copyFileSync, existsSync, mkdirSync, readFileSync, readdirSync, renameSync, rmSync, writeFileSync } from "node:fs";
import { resolve } from "node:path";
import { hosted } from "./hosted.mjs";

export const QUEUED_EXIT = 75;
const queueDir = (out) => resolve(out, "publishing-queue");
const jobDir = (out, slug) => resolve(queueDir(out), slug);
const receiptPath = (out, slug) => resolve(out, `${slug}.buzzsprout.json`);
const read = (path) => JSON.parse(readFileSync(path, "utf8"));
const atomicJson = (path, value) => {
  const temp = `${path}.${process.pid}.tmp`;
  writeFileSync(temp, JSON.stringify(value, null, 2) + "\n", { mode: 0o600 });
  renameSync(temp, path);
};

export function episodeDescription(slug, meta = {}) {
  const title = meta.title || slug;
  const essayUrl = `https://papers.aesthetic.computer/${hosted(slug) || `aesthetic-${slug}-essay`}.pdf`;
  const source = meta.link ? `Explore Aesthetic Computer: ${meta.link}` : `Read the essay: ${essayUrl}`;
  return `${meta.description || `A reading of "${title}" in @jeffrey's voice.`}\n\nWrite with questions and feedback: mail@aesthetic.computer. Unless you ask us not to, your letter may be read or mentioned on a future episode.\n\n${source}\nMore readings + papers: https://papers.aesthetic.computer`;
}

export function publishingQueue(out) {
  if (!existsSync(queueDir(out))) return [];
  return readdirSync(queueDir(out), { withFileTypes: true })
    .filter((entry) => entry.isDirectory() && !entry.name.startsWith("."))
    .map((entry) => read(resolve(queueDir(out), entry.name, "job.json")))
    .sort((a, b) => a.publishedAt.localeCompare(b.publishedAt) || a.slug.localeCompare(b.slug));
}

export function enqueueEpisode({ out, slug, podcast, privateEpisode = false, force = false, now = new Date() }) {
  if (!/^[a-z0-9][a-z0-9-]*$/.test(slug || "")) throw new Error("Invalid episode slug");
  if (!privateEpisode && !hosted(slug)) throw new Error(`${slug} is not on the publish allowlist`);
  if (!/^\d+$/.test(String(podcast || ""))) throw new Error("BUZZSPROUT_PODCAST_ID is required to queue an upload");
  const directory = jobDir(out, slug);
  const path = resolve(directory, "job.json");
  if (existsSync(path)) {
    const job = read(path);
    if (job.private !== privateEpisode || job.podcast !== String(podcast)) {
      throw new Error(`${slug} is already queued with a different destination or visibility`);
    }
    return job;
  }
  const receipt = existsSync(receiptPath(out, slug)) ? read(receiptPath(out, slug)) : null;
  if (receipt && !force) return { slug, status: "published", episodeId: receipt.id };
  const meta = read(resolve(out, `${slug}.json`));
  const job = {
    slug, podcast: String(podcast), status: "queued", attempts: 0,
    createdAt: now.toISOString(), publishedAt: new Date(meta.pubDate || now).toISOString(),
    private: privateEpisode, title: meta.title || slug, description: episodeDescription(slug, meta),
    previousEpisodeId: receipt?.id ?? null,
  };
  mkdirSync(queueDir(out), { recursive: true });
  const temp = resolve(queueDir(out), `.${slug}.${process.pid}.tmp`);
  mkdirSync(temp);
  try {
    copyFileSync(resolve(out, `${slug}.mp3`), resolve(temp, "audio.mp3"));
    const cover = [`${slug}-cover-1400.png`, `${slug}-cover.png`].map((f) => resolve(out, f)).find(existsSync);
    if (cover) { copyFileSync(cover, resolve(temp, "artwork.png")); job.artwork = true; }
    atomicJson(resolve(temp, "job.json"), job);
    renameSync(temp, directory);
  } finally { rmSync(temp, { recursive: true, force: true }); }
  return job;
}

export function isBillingFailure(status, body) {
  return status === 402 || ([400, 403, 422].includes(status) &&
    /unable to upload new episodes to this subscription|payment required|past[- ]due|billing|subscription.*(?:limit|expired|inactive)|(?:upload|storage).*limit/i.test(body));
}

// Only one uploader may POST at a time. A crash leaves its lock for inspection;
// deleting a stale lock cannot cause a repeat upload because 'uploading' jobs
// are held for reconciliation instead of automatically POSTed a second time.
export async function drainPublishingQueue({ out, podcast, token, fetch = globalThis.fetch, limit = 3, log = console.log }) {
  if (!Number.isInteger(limit) || limit < 1 || limit > 100) throw new Error("Retry limit must be 1..100");
  mkdirSync(queueDir(out), { recursive: true });
  const lock = resolve(queueDir(out), ".upload.lock");
  try { writeFileSync(lock, String(process.pid), { flag: "wx", mode: 0o600 }); }
  catch (error) {
    if (error.code === "EEXIST") throw new Error(`Publisher lock exists: ${lock}; check its process before removing a stale lock`);
    throw error;
  }
  let attempted = 0;
  try {
    for (const job of publishingQueue(out)) {
      const directory = jobDir(out, job.slug);
      const path = resolve(directory, "job.json");
      const receipt = existsSync(receiptPath(out, job.slug)) ? read(receiptPath(out, job.slug)) : null;
      if (receipt?.id && receipt.id !== job.previousEpisodeId) {
        rmSync(directory, { recursive: true });
        continue;
      }
      if (job.podcast !== String(podcast)) throw new Error(`${job.slug}: queued for another podcast`);
      if (!["queued", "billing"].includes(job.status)) {
        log(`! ${job.slug}: ${job.status}; inspect before retrying (${job.lastError || "upload outcome unknown"})`);
        continue;
      }
      if (attempted >= limit) break;
      if (!job.private && !hosted(job.slug)) throw new Error(`${job.slug} is no longer cleared to publish`);
      if (!token) throw new Error("BUZZSPROUT_TOKEN is required to upload; episodes remain queued");
      const fd = new FormData();
      fd.append("title", job.title);
      fd.append("description", job.description);
      fd.append("artist", "@jeffrey");
      if (job.private) fd.append("private", "true");
      else fd.append("published_at", job.publishedAt);
      fd.append("audio_file", new Blob([readFileSync(resolve(directory, "audio.mp3"))], { type: "audio/mpeg" }), `${job.slug}.mp3`);
      if (job.artwork) fd.append("artwork_file", new Blob([readFileSync(resolve(directory, "artwork.png"))], { type: "image/png" }), `${job.slug}-cover.png`);
      job.status = "uploading";
      job.attempts++;
      job.lastAttemptAt = new Date().toISOString();
      atomicJson(path, job);
      attempted++;
      let response, body;
      try {
        response = await fetch(`https://www.buzzsprout.com/api/${podcast}/episodes.json`, {
          method: "POST", headers: { Authorization: `Token token="${token}"` }, body: fd,
          signal: AbortSignal.timeout(120_000),
        });
        body = await response.text();
      } catch {
        job.status = "uncertain";
        job.lastError = "Upload response lost; check Buzzsprout before retrying to avoid a duplicate";
        atomicJson(path, job);
        throw new Error(`${job.slug}: ${job.lastError}`);
      }
      if (!response.ok) {
        const billing = isBillingFailure(response.status, body);
        job.status = billing ? "billing" : "failed";
        job.lastError = `HTTP ${response.status}: ${body.slice(0, 300)}`;
        atomicJson(path, job);
        log(`! ${job.slug} retained in publishing queue: ${job.lastError}`);
        if (billing) break; // One rejection is enough to know the subscription is blocked.
        throw new Error(`${job.slug}: ${job.lastError}`);
      }
      let episode;
      try { episode = JSON.parse(body); if (!episode.id) throw new Error("No episode ID"); }
      catch {
        job.status = "uncertain";
        job.lastError = "Successful upload returned no usable receipt; check Buzzsprout before retrying";
        atomicJson(path, job);
        throw new Error(`${job.slug}: ${job.lastError}`);
      }
      atomicJson(receiptPath(out, job.slug), episode);
      rmSync(directory, { recursive: true });
      log(`✓ ${job.slug} ${job.private ? "staged private" : "published"} · episode ${episode.id}`);
    }
    const remaining = publishingQueue(out);
    return { attempted, remaining: remaining.length, needsReview: remaining.some((job) => !["queued", "billing"].includes(job.status)) };
  } finally { rmSync(lock, { force: true }); }
}
