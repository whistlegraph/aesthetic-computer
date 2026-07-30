#!/usr/bin/env node
import { createHash, createPrivateKey, sign } from "node:crypto";
import { readFile, writeFile, mkdir } from "node:fs/promises";
import { basename, dirname, resolve } from "node:path";

const args = Object.fromEntries(process.argv.slice(2).map((value, i, all) =>
  value.startsWith("--") ? [value.slice(2), all[i + 1]] : null).filter(Boolean));
if (!args.source || !args.out || !args.key || !args.version || !args.sequence) {
  console.error("usage: release.mjs --source piece.js --out release/manifest.json --key ed25519.pem --version VERSION --sequence N [--channel stable] [--base-url URL]");
  process.exit(2);
}
const sourcePath = resolve(args.source);
const source = await readFile(sourcePath);
const file = basename(sourcePath);
const baseUrl = (args["base-url"] || "https://updates.aesthetic.computer/xbox").replace(/\/$/, "");
const payload = {
  schema: 1,
  channel: args.channel || "stable",
  slug: args.slug || file.replace(/\.m?js$/, ""),
  version: args.version,
  sourceUrl: `${baseUrl}/${encodeURIComponent(args.version)}/${encodeURIComponent(file)}`,
  sourceSha256: createHash("sha256").update(source).digest("hex"),
  sourceBytes: source.length,
  sequence: Number(args.sequence),
  expiresUnixMs: Number(args.expires || Date.now() + 30 * 24 * 60 * 60 * 1000),
  keyId: args["key-id"] || "xbox-release-1",
};
if (!Number.isSafeInteger(payload.sequence) || payload.sequence <= 0) throw new Error("sequence must be a positive integer");
// Insertion order above is the v1 canonical wire order. The BIOS verifies the
// UTF-8 bytes of this compact form, never a reserialized object.
const signedPayload = JSON.stringify(payload);
const privateKey = createPrivateKey(await readFile(resolve(args.key)));
const signature = sign(null, Buffer.from(signedPayload), privateKey).toString("base64");
const manifest = { ...payload, signedPayload, signature };
await mkdir(dirname(resolve(args.out)), { recursive: true });
await writeFile(resolve(args.out), JSON.stringify(manifest, null, 2) + "\n", { flag: "wx" });
await writeFile(resolve(dirname(args.out), file), source, { flag: "wx" });
console.log(`${args.out} ${payload.sourceSha256}`);
