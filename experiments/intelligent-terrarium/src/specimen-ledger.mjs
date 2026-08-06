import { DatabaseSync } from "node:sqlite";
import { mkdir } from "node:fs/promises";
import { dirname, resolve } from "node:path";

const ADDRESSES = Array.from({ length: 12 }, (_, slot) => `${String.fromCharCode(65 + slot % 4)}${1 + Math.floor(slot / 4)}`);
const HARDWARE_PROFILES = Object.freeze(["quarter", "half", "standard", "double"]);

function hardwareProfileFromTags(value) {
  const tags = Array.isArray(value) ? value : JSON.parse(value || "[]");
  return HARDWARE_PROFILES.find((profile) => tags.includes(`hardware-${profile}`)) || null;
}

function inferredTag(value) {
  const tag = String(value || "").toLowerCase().trim().replace(/[^a-z0-9]+/g, "-").replace(/^-|-$/g, "").slice(0, 32);
  return tag ? `vision:${tag}` : null;
}

export class SpecimenLedger {
  constructor(path, database) {
    this.path = path;
    this.database = database;
  }

  static async open(path) {
    const absolute = resolve(path);
    await mkdir(dirname(absolute), { recursive: true });
    const database = new DatabaseSync(absolute);
    database.exec(`
      PRAGMA journal_mode = WAL;
      PRAGMA synchronous = NORMAL;
      CREATE TABLE IF NOT EXISTS specimens (
        id TEXT PRIMARY KEY,
        first_iteration INTEGER NOT NULL,
        last_iteration INTEGER NOT NULL,
        address TEXT UNIQUE,
        domain TEXT NOT NULL,
        source TEXT NOT NULL,
        parent TEXT,
        generation INTEGER NOT NULL,
        status TEXT NOT NULL,
        aliveness TEXT,
        quality REAL NOT NULL,
        novelty REAL NOT NULL,
        tags_json TEXT NOT NULL,
        visual_review_json TEXT,
        observations INTEGER NOT NULL DEFAULT 1,
        runtime_health REAL,
        runtime_health_at INTEGER,
        performance_strikes INTEGER NOT NULL DEFAULT 0,
        interventions INTEGER NOT NULL DEFAULT 0,
        last_intervention_json TEXT,
        culled_reason TEXT
      );
      CREATE TABLE IF NOT EXISTS observations (
        id INTEGER PRIMARY KEY AUTOINCREMENT,
        specimen_id TEXT NOT NULL,
        iteration INTEGER NOT NULL,
        actual REAL NOT NULL,
        potential REAL NOT NULL,
        variance REAL NOT NULL,
        spatial REAL NOT NULL,
        noise REAL NOT NULL,
        coherence REAL NOT NULL,
        muddiness REAL NOT NULL DEFAULT 0,
        category TEXT NOT NULL,
        FOREIGN KEY(specimen_id) REFERENCES specimens(id)
      );
      CREATE INDEX IF NOT EXISTS observations_specimen_iteration
        ON observations(specimen_id, iteration DESC);
      CREATE TABLE IF NOT EXISTS interventions (
        id INTEGER PRIMARY KEY AUTOINCREMENT,
        specimen_id TEXT NOT NULL,
        at_ms INTEGER NOT NULL,
        strategy TEXT NOT NULL,
        donor_id TEXT,
        before_hp REAL NOT NULL,
        outcome TEXT NOT NULL DEFAULT 'observing',
        FOREIGN KEY(specimen_id) REFERENCES specimens(id)
      );
      CREATE INDEX IF NOT EXISTS interventions_specimen_time
        ON interventions(specimen_id, at_ms DESC);
      CREATE TABLE IF NOT EXISTS visual_reviews (
        id INTEGER PRIMARY KEY AUTOINCREMENT,
        specimen_id TEXT NOT NULL,
        at_ms INTEGER NOT NULL,
        trigger TEXT NOT NULL,
        review_json TEXT NOT NULL,
        FOREIGN KEY(specimen_id) REFERENCES specimens(id)
      );
      CREATE INDEX IF NOT EXISTS visual_reviews_specimen_time
        ON visual_reviews(specimen_id, at_ms DESC);
      CREATE TABLE IF NOT EXISTS margin_probes (
        id INTEGER PRIMARY KEY AUTOINCREMENT,
        specimen_id TEXT NOT NULL,
        at_ms INTEGER NOT NULL,
        address INTEGER NOT NULL,
        track TEXT NOT NULL,
        capability TEXT,
        requested_by TEXT NOT NULL,
        record_json TEXT NOT NULL,
        FOREIGN KEY(specimen_id) REFERENCES specimens(id)
      );
      CREATE INDEX IF NOT EXISTS margin_probes_specimen_time
        ON margin_probes(specimen_id, at_ms DESC);
    `);
    const observationColumns = new Set(database.prepare("PRAGMA table_info(observations)").all().map((column) => column.name));
    if (!observationColumns.has("muddiness")) database.exec("ALTER TABLE observations ADD COLUMN muddiness REAL NOT NULL DEFAULT 0");
    const specimenColumns = new Set(database.prepare("PRAGMA table_info(specimens)").all().map((column) => column.name));
    const additions = {
      runtime_health: "REAL", runtime_health_at: "INTEGER",
      performance_strikes: "INTEGER NOT NULL DEFAULT 0",
      interventions: "INTEGER NOT NULL DEFAULT 0", last_intervention_json: "TEXT",
      culled_reason: "TEXT",
    };
    for (const [name, definition] of Object.entries(additions))
      if (!specimenColumns.has(name)) database.exec(`ALTER TABLE specimens ADD COLUMN ${name} ${definition}`);
    database.exec(`
      INSERT INTO visual_reviews (specimen_id, at_ms, trigger, review_json)
      SELECT specimens.id, COALESCE(specimens.runtime_health_at, 0), 'legacy', specimens.visual_review_json
      FROM specimens
      WHERE specimens.visual_review_json IS NOT NULL
        AND NOT EXISTS (SELECT 1 FROM visual_reviews WHERE visual_reviews.specimen_id=specimens.id)
    `);
    return new SpecimenLedger(absolute, database);
  }

  observe(candidate) {
    if (!candidate?.id) return null;
    const energy = candidate.sample?.energy?.at(-1) || {};
    const tags = candidate.tags || [candidate.aliveness || "unknown"];
    this.database.prepare(`
      INSERT INTO specimens (
        id, first_iteration, last_iteration, domain, source, parent, generation,
        status, aliveness, quality, novelty, tags_json, observations
      ) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, 1)
      ON CONFLICT(id) DO UPDATE SET
        last_iteration=excluded.last_iteration, status=excluded.status,
        aliveness=excluded.aliveness, quality=excluded.quality,
        novelty=excluded.novelty, tags_json=excluded.tags_json,
        observations=specimens.observations + 1
    `).run(candidate.id, candidate.iteration || 0, candidate.iteration || 0,
      candidate.domain || "sort", candidate.source || "", candidate.parent || null,
      candidate.generation || 0, candidate.status || "unknown", candidate.aliveness || null,
      candidate.quality || 0, candidate.novelty || 0, JSON.stringify(tags));
    if (candidate.domain === "raster" && candidate.status !== "rejected") {
      this.database.prepare(`
        INSERT INTO observations (
          specimen_id, iteration, actual, potential, variance, spatial, noise, coherence, muddiness, category
        ) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
      `).run(candidate.id, candidate.iteration || 0, energy.actual || 0, energy.potential || 0,
        energy.variance || 0, energy.spatial || 0, energy.noise || 0, energy.coherence || 0, energy.muddiness || 0,
        tags[0] || "unknown");
    }
    if (candidate.domain === "raster" && candidate.retained) this.#considerAddress(candidate.id, candidate.quality || 0);
    return this.get(candidate.id);
  }

  #considerAddress(id, quality) {
    const current = this.database.prepare("SELECT address FROM specimens WHERE id=?").get(id);
    if (current?.address) return current.address;
    const occupied = new Map(this.database.prepare("SELECT address, id, quality, tags_json FROM specimens WHERE address IS NOT NULL").all()
      .map((row) => [row.address, row]));
    const empty = ADDRESSES.find((address) => !occupied.has(address));
    if (empty) {
      this.database.prepare("UPDATE specimens SET address=? WHERE id=?").run(empty, id);
      return empty;
    }
    const incoming = this.database.prepare("SELECT tags_json FROM specimens WHERE id=?").get(id);
    const incomingProfile = hardwareProfileFromTags(incoming?.tags_json);
    const profileCounts = new Map(HARDWARE_PROFILES.map((profile) => [profile, 0]));
    for (const row of occupied.values()) {
      const profile = hardwareProfileFromTags(row.tags_json);
      if (profile) profileCounts.set(profile, profileCounts.get(profile) + 1);
    }
    const diversityCandidates = incomingProfile && profileCounts.get(incomingProfile) === 0
      ? [...occupied.values()].filter((row) => {
        const profile = hardwareProfileFromTags(row.tags_json);
        return !profile || profileCounts.get(profile) > 1;
      }) : [];
    const pool = diversityCandidates.length ? diversityCandidates : [...occupied.values()];
    const weakest = pool.sort((a, b) => a.quality - b.quality || a.address.localeCompare(b.address))[0];
    if (diversityCandidates.length || quality > weakest.quality * 1.12) {
      this.database.exec("BEGIN IMMEDIATE");
      try {
        this.database.prepare("UPDATE specimens SET address=NULL, status='archived' WHERE id=?").run(weakest.id);
        this.database.prepare("UPDATE specimens SET address=? WHERE id=?").run(weakest.address, id);
        this.database.exec("COMMIT");
      } catch (error) {
        this.database.exec("ROLLBACK");
        throw error;
      }
      return weakest.address;
    }
    return null;
  }

  reconcileResidents(ids) {
    const resident = new Set(ids);
    for (const row of this.database.prepare("SELECT id, status FROM specimens WHERE domain='raster'").all()) {
      if (!resident.has(row.id) && row.status === "resident")
        this.database.prepare("UPDATE specimens SET status='retired', address=NULL WHERE id=?").run(row.id);
    }
  }

  recordHealth(id, { hp, at, strikes = 0 }) {
    this.database.prepare(`
      UPDATE specimens SET runtime_health=?, runtime_health_at=?, performance_strikes=? WHERE id=?
    `).run(hp, at, strikes, id);
  }

  recordIntervention(id, intervention) {
    const record = {
      at: intervention.at, strategy: intervention.strategy,
      donorId: intervention.donorId || null, beforeHp: intervention.beforeHp,
    };
    this.database.exec("BEGIN IMMEDIATE");
    try {
      this.database.prepare(`
        INSERT INTO interventions (specimen_id, at_ms, strategy, donor_id, before_hp)
        VALUES (?, ?, ?, ?, ?)
      `).run(id, record.at, record.strategy, record.donorId, record.beforeHp);
      this.database.prepare(`
        UPDATE specimens SET interventions=interventions + 1,
          last_intervention_json=?, performance_strikes=0 WHERE id=?
      `).run(JSON.stringify(record), id);
      this.database.exec("COMMIT");
    } catch (error) {
      this.database.exec("ROLLBACK");
      throw error;
    }
  }

  recordOutcome(id, outcome) {
    this.database.prepare(`
      UPDATE interventions SET outcome=? WHERE id=(
        SELECT id FROM interventions WHERE specimen_id=? ORDER BY at_ms DESC LIMIT 1
      )
    `).run(outcome, id);
  }

  cull(id, reason) {
    this.database.prepare(`
      UPDATE specimens SET status='culled', address=NULL, culled_reason=?, performance_strikes=0 WHERE id=?
    `).run(reason, id);
  }

  fillVacancies(candidates) {
    const occupied = new Set(this.database.prepare("SELECT address FROM specimens WHERE address IS NOT NULL").all().map((row) => row.address));
    const addressedIds = new Set(this.database.prepare("SELECT id FROM specimens WHERE address IS NOT NULL").all().map((row) => row.id));
    const available = [...candidates]
      .filter((candidate) => candidate?.id && candidate.domain === "raster" && !addressedIds.has(candidate.id))
      .sort((a, b) => (b.quality || 0) - (a.quality || 0) || (b.novelty || 0) - (a.novelty || 0));
    const profileCounts = new Map(HARDWARE_PROFILES.map((profile) => [profile, 0]));
    for (const row of this.database.prepare("SELECT tags_json FROM specimens WHERE address IS NOT NULL").all()) {
      const profile = hardwareProfileFromTags(row.tags_json);
      if (profile) profileCounts.set(profile, profileCounts.get(profile) + 1);
    }
    const assigned = [];
    for (const address of ADDRESSES.filter((value) => !occupied.has(value))) {
      const diversityIndex = available.findIndex((candidate) => {
        const profile = hardwareProfileFromTags(candidate.tags);
        return profile && profileCounts.get(profile) === 0;
      });
      const candidate = diversityIndex >= 0 ? available.splice(diversityIndex, 1)[0] : available.shift();
      if (!candidate) break;
      this.database.prepare("UPDATE specimens SET address=?, status='resident' WHERE id=?").run(address, candidate.id);
      const profile = hardwareProfileFromTags(candidate.tags);
      if (profile) profileCounts.set(profile, profileCounts.get(profile) + 1);
      assigned.push({ address, id: candidate.id });
    }
    return assigned;
  }

  addressed() {
    return this.database.prepare(`
      SELECT id, address, status, aliveness, quality, novelty, tags_json, visual_review_json,
        runtime_health, runtime_health_at, performance_strikes, interventions,
        last_intervention_json, culled_reason
      FROM specimens WHERE address IS NOT NULL ORDER BY substr(address, 2, 1), substr(address, 1, 1)
    `).all().map((row) => ({ ...row, tags: JSON.parse(row.tags_json),
      visualReview: row.visual_review_json ? JSON.parse(row.visual_review_json) : null,
      lastIntervention: row.last_intervention_json ? JSON.parse(row.last_intervention_json) : null }));
  }

  recordVisualReview(id, review) {
    const row = this.database.prepare("SELECT tags_json FROM specimens WHERE id=?").get(id);
    if (!row) return false;
    const existing = JSON.parse(row.tags_json || "[]");
    const inferred = (review?.tags || []).map(inferredTag).filter(Boolean);
    if (review?.artifact && review.artifact !== "none") inferred.push(inferredTag(`artifact-${review.artifact}`));
    const tags = [...new Set([...existing, ...inferred])].slice(0, 16);
    const encoded = JSON.stringify(review);
    const at = Number.isFinite(Date.parse(review?.at)) ? Date.parse(review.at) : Date.now();
    this.database.exec("BEGIN IMMEDIATE");
    try {
      this.database.prepare("UPDATE specimens SET visual_review_json=?, tags_json=? WHERE id=?")
        .run(encoded, JSON.stringify(tags), id);
      this.database.prepare("INSERT INTO visual_reviews (specimen_id, at_ms, trigger, review_json) VALUES (?, ?, ?, ?)")
        .run(id, at, String(review?.trigger || "visual-novelty"), encoded);
      this.database.exec("COMMIT");
    } catch (error) {
      this.database.exec("ROLLBACK");
      throw error;
    }
    return true;
  }

  recordMarginProbe(id, probe) {
    if (!this.database.prepare("SELECT 1 FROM specimens WHERE id=?").get(id)) return false;
    const record = { ...probe, id };
    this.database.prepare(`
      INSERT INTO margin_probes (specimen_id, at_ms, address, track, capability, requested_by, record_json)
      VALUES (?, ?, ?, ?, ?, ?, ?)
    `).run(id, Number(probe.at) || Date.now(), probe.address, probe.track,
      probe.capability || null, probe.requestedBy || "loopback", JSON.stringify(record));
    return true;
  }

  latestMarginProbe() {
    const row = this.database.prepare("SELECT record_json FROM margin_probes ORDER BY id DESC LIMIT 1").get();
    if (!row) return null;
    try { return JSON.parse(row.record_json); }
    catch { return null; }
  }

  curationStats() {
    const specimenCount = Number(this.database.prepare("SELECT count(*) AS count FROM specimens").get().count);
    const observationCount = Number(this.database.prepare("SELECT count(*) AS count FROM observations").get().count);
    const reviews = this.database.prepare("SELECT review_json FROM visual_reviews").all();
    const recommendations = { retain: 0, watch: 0, reject: 0 };
    for (const row of reviews) {
      try {
        const recommendation = JSON.parse(row.review_json).recommendation;
        if (recommendation in recommendations) recommendations[recommendation] += 1;
      } catch { /* An old malformed row is evidence, not authority. */ }
    }
    return { specimens: specimenCount, observations: observationCount, reviews: reviews.length, recommendations };
  }

  get(id) {
    return this.database.prepare("SELECT * FROM specimens WHERE id=?").get(id) || null;
  }

  close() { this.database.close(); }
}

export { ADDRESSES };
