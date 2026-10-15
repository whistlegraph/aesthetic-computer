import { existsSync, readFileSync } from "node:fs";
import { vaultPath } from "../vault/credentials.mjs";

export function registryCandidates(options = {}) {
  const env = options.env || process.env;
  return env.FLEET_MACHINES ? [env.FLEET_MACHINES] :
    ["machines.normalized.json", "machines.json"].map(file => vaultPath(file, options));
}

export function loadRegistry(options) {
  const candidates = registryCandidates(options);
  const path = candidates.find(p => existsSync(p));
  if (!path) throw new Error(`No machine registry found (looked in: ${candidates.join(", ")})`);
  const data = JSON.parse(readFileSync(path, "utf8"));
  return { path, machines: data.machines || {}, schema: data._schema || null };
}
