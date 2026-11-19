// Sack, 2026.11.18
// The parts of a sack that need no network: limits, file names, the storage
// sum a handle is charged, and the zip a sack downloads as.

import { crc32 } from "node:zlib";

export const SACKS = "sacks";
export const GB = 1e9; // decimal, as file sizes are shown
export const HANDLE_LIMIT = 10 * GB; // everything a handle keeps in sacks
export const FILE_LIMIT = 2 * GB; // one file
export const PART_BYTES = 16 * 1024 * 1024; // multipart piece size
export const FILES_PER_DROP = 200;
export const PENDING_MS = 24 * 60 * 60 * 1000; // an unfinished upload holds its space this long

// A dropped file's name, safe to keep and to put in a zip: no folders, no
// control characters, never empty, never a dot-path.
export function cleanName(name) {
  let clean = String(name ?? "").normalize("NFC").split(/[\\/]/).pop()
    .replace(/[\u0000-\u001f\u007f]/g, "").trim().slice(0, 200);
  if (!clean || /^\.+$/.test(clean)) clean = "file";
  return clean;
}

// `taken` is the names a sack already holds. A repeat becomes "name (2).ext".
export function uniqueName(name, taken) {
  if (!taken.has(name)) return name;
  const dot = name.lastIndexOf(".");
  const [stem, ext] = dot > 0 ? [name.slice(0, dot), name.slice(dot)] : [name, ""];
  for (let n = 2; ; n++) {
    const next = `${stem} (${n})${ext}`;
    if (!taken.has(next)) return next;
  }
}

export function cleanType(type) {
  const clean = String(type || "").toLowerCase().trim();
  return /^[a-z0-9][a-z0-9.+-]*\/[a-z0-9][a-z0-9.+-]*$/.test(clean) ? clean : "application/octet-stream";
}

// Only these play or show in the page; everything else is a download, so a
// dropped .html or .svg never runs from our storage.
export const showsInline = (type) =>
  /^(audio|video)\//.test(type) || ["image/png", "image/jpeg", "image/gif", "image/webp"].includes(type);

// Validate a drop into [{ name, bytes, type }]. Throws a message fit for a 400.
export function parseDrop(files, taken = new Set()) {
  if (!Array.isArray(files) || files.length === 0) throw new Error("Drop at least one file.");
  if (files.length > FILES_PER_DROP) throw new Error(`At most ${FILES_PER_DROP} files a drop.`);
  const names = new Set(taken);
  return files.map((file) => {
    const bytes = Number(file?.bytes);
    if (!Number.isInteger(bytes) || bytes < 1) throw new Error("Every file needs a size above zero.");
    if (bytes > FILE_LIMIT) throw new Error(`${cleanName(file?.name)} is over the 2 GB a file may be.`);
    const name = uniqueName(cleanName(file?.name), names);
    names.add(name);
    return { name, bytes, type: cleanType(file?.type) };
  });
}

// A file counts once it is done, or while its upload is young enough to finish.
export const fileCounts = (file, now = Date.now()) =>
  file.done || now - new Date(file.at).getTime() < PENDING_MS;

export const sackBytes = (sack, now = Date.now()) =>
  (sack.files || []).reduce((sum, file) => sum + (fileCounts(file, now) ? file.bytes : 0), 0);

// What a handle is charged across all its sacks, and how many it has.
export async function storageFor(db, user, now = Date.now()) {
  const sacks = await db.collection(SACKS)
    .find({ user, nuked: { $ne: true } }, { projection: { files: 1 } }).toArray();
  const kept = sacks.filter((sack) => (sack.files || []).some((file) => file.done));
  return {
    bytes: sacks.reduce((sum, sack) => sum + sackBytes(sack, now), 0),
    sacks: kept.length,
    files: kept.reduce((sum, sack) => sum + sack.files.filter((file) => file.done).length, 0),
  };
}

export const partCount = (bytes) => Math.max(1, Math.ceil(bytes / PART_BYTES));

// 📦 A stored (uncompressed) zip, written as it is read so a sack of any size
// streams through without being held. `entries` is [{ name, open }] where
// open() returns an async iterable of Buffers. Offsets past 4 GB use zip64.
export async function* zipStream(entries, when = new Date()) {
  const time = (when.getHours() << 11) | (when.getMinutes() << 5) | (when.getSeconds() >> 1);
  const date = ((when.getFullYear() - 1980) << 9) | ((when.getMonth() + 1) << 5) | when.getDate();
  const FLAGS = 0x0808; // sizes follow the data; names are UTF-8
  const U32 = 0xffffffff;
  const central = [];
  let offset = 0;

  for (const entry of entries) {
    const name = Buffer.from(entry.name, "utf8");
    const at = offset;
    const local = Buffer.alloc(30);
    local.writeUInt32LE(0x04034b50, 0);
    local.writeUInt16LE(20, 4);
    local.writeUInt16LE(FLAGS, 6);
    local.writeUInt16LE(0, 8); // stored
    local.writeUInt16LE(time, 10);
    local.writeUInt16LE(date, 12);
    local.writeUInt16LE(name.length, 26);
    yield local; yield name;
    offset += 30 + name.length;

    let crc = 0, size = 0;
    for await (const chunk of entry.open()) {
      crc = crc32(chunk, crc);
      size += chunk.length;
      yield chunk;
    }
    if (size > U32) throw new Error(`${entry.name} is too large to zip.`);
    offset += size;

    const after = Buffer.alloc(16);
    after.writeUInt32LE(0x08074b50, 0);
    after.writeUInt32LE(crc, 4);
    after.writeUInt32LE(size, 8);
    after.writeUInt32LE(size, 12);
    yield after;
    offset += 16;
    central.push({ name, crc, size, at });
  }

  const centralAt = offset;
  for (const { name, crc, size, at } of central) {
    const far = at >= U32;
    const head = Buffer.alloc(46);
    head.writeUInt32LE(0x02014b50, 0);
    head.writeUInt16LE(45, 4);
    head.writeUInt16LE(far ? 45 : 20, 6);
    head.writeUInt16LE(FLAGS, 8);
    head.writeUInt16LE(0, 10);
    head.writeUInt16LE(time, 12);
    head.writeUInt16LE(date, 14);
    head.writeUInt32LE(crc, 16);
    head.writeUInt32LE(size, 20);
    head.writeUInt32LE(size, 24);
    head.writeUInt16LE(name.length, 28);
    head.writeUInt16LE(far ? 12 : 0, 30);
    head.writeUInt32LE(far ? U32 : at, 42);
    yield head; yield name;
    offset += 46 + name.length;
    if (far) {
      const extra = Buffer.alloc(12);
      extra.writeUInt16LE(0x0001, 0);
      extra.writeUInt16LE(8, 2);
      extra.writeBigUInt64LE(BigInt(at), 4);
      yield extra;
      offset += 12;
    }
  }
  const centralSize = offset - centralAt;

  if (centralAt >= U32 || centralSize >= U32 || central.length >= 0xffff) {
    const record = Buffer.alloc(56);
    record.writeUInt32LE(0x06064b50, 0);
    record.writeBigUInt64LE(44n, 4);
    record.writeUInt16LE(45, 12);
    record.writeUInt16LE(45, 14);
    record.writeBigUInt64LE(BigInt(central.length), 24);
    record.writeBigUInt64LE(BigInt(central.length), 32);
    record.writeBigUInt64LE(BigInt(centralSize), 40);
    record.writeBigUInt64LE(BigInt(centralAt), 48);
    const locator = Buffer.alloc(20);
    locator.writeUInt32LE(0x07064b50, 0);
    locator.writeBigUInt64LE(BigInt(offset), 8);
    locator.writeUInt32LE(1, 16);
    yield record; yield locator;
  }

  const end = Buffer.alloc(22);
  end.writeUInt32LE(0x06054b50, 0);
  end.writeUInt16LE(Math.min(central.length, 0xffff), 8);
  end.writeUInt16LE(Math.min(central.length, 0xffff), 10);
  end.writeUInt32LE(Math.min(centralSize, U32), 12);
  end.writeUInt32LE(Math.min(centralAt, U32), 16);
  yield end;
}
