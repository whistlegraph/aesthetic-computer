import { existsSync, readFileSync } from "node:fs";
import { homedir } from "node:os";
import { dirname, join, resolve } from "node:path";
import { fileURLToPath } from "node:url";

const repo = resolve(dirname(fileURLToPath(import.meta.url)), "../..");
export function loadRegistry() {
  const candidates = [
    process.env.FLEET_MACHINES,
    ...[join(repo, "aesthetic-computer-vault"), join(repo, "vault"), join(homedir(), "aesthetic-computer-vault")]
      .flatMap(root => [join(root, "machines.normalized.json"), join(root, "machines.json")]),
  ].filter(Boolean);
  const path = candidates.find(p => existsSync(p));
  if (!path) throw new Error(`No machine registry found (looked in: ${candidates.join(", ")})`);
  const data = JSON.parse(readFileSync(path, "utf8"));
  return { path, machines: data.machines || {}, schema: data._schema || null };
}
