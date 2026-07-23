import { appendFile, mkdir, readFile, readdir, stat } from "node:fs/promises";
import { basename, join, relative } from "node:path";
import { atomicWrite, canonical, clone, hash } from "./canonical.mjs";
import { Terrarium } from "./core.mjs";

const ZERO_HASH = "0".repeat(64);

async function exists(path) {
  try { await stat(path); return true; } catch { return false; }
}

async function filesBelow(root) {
  if (!(await exists(root))) return [];
  const output = [];
  for (const entry of await readdir(root, { withFileTypes: true })) {
    const path = join(root, entry.name);
    if (entry.isDirectory()) output.push(...await filesBelow(path));
    else if (entry.isFile()) output.push(path);
  }
  return output.sort();
}

async function recoverTail(root, path) {
  const bytes = await readFile(path);
  let offset = 0;
  let validOffset = 0;
  while (offset < bytes.length) {
    const newline = bytes.indexOf(0x0a, offset);
    const end = newline === -1 ? bytes.length : newline;
    const line = bytes.subarray(offset, end).toString("utf8");
    if (!line.trim()) {
      validOffset = newline === -1 ? end : end + 1;
      offset = validOffset;
      continue;
    }
    try {
      JSON.parse(line);
      validOffset = newline === -1 ? end : end + 1;
      offset = validOffset;
    } catch (error) {
      const laterNewline = newline !== -1 && newline < bytes.length - 1;
      if (laterNewline) throw new Error(`invalid journal record before tail in ${path}: ${error.message}`);
      const tail = bytes.subarray(offset);
      const quarantineDir = join(root, "quarantine");
      await mkdir(quarantineDir, { recursive: true });
      const quarantine = join(quarantineDir, `${basename(path)}.${hash(tail.toString("hex")).slice(0, 12)}.tail`);
      await atomicWrite(quarantine, tail);
      await atomicWrite(path, bytes.subarray(0, validOffset));
      return { recoveredBytes: tail.length, quarantine: relative(root, quarantine) };
    }
  }
  return null;
}

function recordHash(record) {
  const { recordHash: ignored, ...body } = record;
  return hash(body);
}

export class StateRepository {
  constructor(root, seedDocument, terrarium, records, headRecordHash, segmentPath, recoveries) {
    this.root = root;
    this.seedDocument = seedDocument;
    this.terrarium = terrarium;
    this.records = records;
    this.headRecordHash = headRecordHash;
    this.segmentPath = segmentPath;
    this.recoveries = recoveries;
    this.writeTail = Promise.resolve();
  }

  static async create(root, { seed, terrariumId = "intelligent-terrarium", profile = "1gb" } = {}) {
    await mkdir(root, { recursive: true });
    const seedPath = join(root, "seed.json");
    if (!(await exists(seedPath))) {
      const document = { schema: 1, terrariumId, seed: String(seed), profile };
      await atomicWrite(seedPath, `${canonical(document)}\n`);
    }
    return this.open(root);
  }

  static async open(root, { segmentId = "wake" } = {}) {
    const seedDocument = JSON.parse(await readFile(join(root, "seed.json"), "utf8"));
    const journalRoot = join(root, "journal");
    const journalFiles = (await filesBelow(journalRoot)).filter((path) => path.endsWith(".ndjson"));
    const recoveries = [];
    for (const file of journalFiles) {
      const recovery = await recoverTail(root, file);
      if (recovery) recoveries.push(recovery);
    }

    const terrarium = new Terrarium(seedDocument.seed);
    const records = [];
    let previousHash = ZERO_HASH;
    for (const file of journalFiles) {
      const contents = await readFile(file, "utf8");
      for (const line of contents.split("\n")) {
        if (!line.trim()) continue;
        const record = JSON.parse(line);
        if (record.schema !== 1) throw new Error(`unsupported journal schema ${record.schema}`);
        if (record.prevHash !== previousHash) throw new Error(`journal hash chain broken at event ${record.seq}`);
        if (record.recordHash !== recordHash(record)) throw new Error(`journal record hash mismatch at event ${record.seq}`);
        terrarium.apply(record);
        if (terrarium.stateHash() !== record.stateHash) throw new Error(`state hash mismatch at event ${record.seq}`);
        previousHash = record.recordHash;
        records.push(record);
      }
    }

    const segmentPath = join(journalRoot, "segments", `${segmentId}.ndjson`);
    await mkdir(join(journalRoot, "segments"), { recursive: true });
    return new StateRepository(root, seedDocument, terrarium, records, previousHash, segmentPath, recoveries);
  }

  async transact(kind, payload) {
    const result = this.writeTail.then(() => this.#appendTransaction(kind, payload));
    this.writeTail = result.catch(() => {});
    return result;
  }

  async #appendTransaction(kind, payload) {
    const candidate = Terrarium.fromSnapshot(this.terrarium.snapshot());
    const record = {
      schema: 1,
      seq: candidate.state.lastSeq + 1,
      kind,
      payload: clone(payload || {}),
      prevHash: this.headRecordHash,
    };
    const outputs = candidate.apply(record);
    record.stateHash = candidate.stateHash();
    record.recordHash = recordHash(record);
    await appendFile(this.segmentPath, `${canonical(record)}\n`, { encoding: "utf8", mode: 0o600, flush: true });
    this.terrarium = candidate;
    this.records.push(record);
    this.headRecordHash = record.recordHash;
    return { record: clone(record), outputs: clone(outputs) };
  }

  async idle() {
    await this.writeTail;
  }

  stateHash() {
    return this.terrarium.stateHash();
  }
}

export async function verifyRepository(root) {
  const repository = await StateRepository.open(root, { segmentId: "verify" });
  return {
    stateHash: repository.stateHash(),
    lastSeq: repository.terrarium.state.lastSeq,
    headRecordHash: repository.headRecordHash,
    recoveries: repository.recoveries,
  };
}
