import { existsSync, readFileSync } from "node:fs";
import { resolve } from "node:path";

export const vaultRoot = resolve(process.env.AC_VAULT_ROOT || "/Users/jas/aesthetic-computer-vault");
export const klokkentalesVault = resolve(vaultRoot, "klokkentales");

function readEnv(path) {
  if (!existsSync(path)) return {};
  return Object.fromEntries(readFileSync(path, "utf8").split(/\r?\n/).flatMap((line) => {
    const match = line.match(/^\s*([A-Z][A-Z0-9_]*)\s*=\s*(.*?)\s*$/);
    if (!match) return [];
    return [[match[1], match[2].replace(/^['"]|['"]$/g, "")]];
  }));
}

export function loadKlokkentalesEnv() {
  const sharedElevenLabs = readEnv(resolve(vaultRoot, "lith", ".env"));
  const sharedBuzzsprout = readEnv(resolve(vaultRoot, "buzzsprout", ".env"));
  const project = readEnv(resolve(klokkentalesVault, ".env"));
  return { ...sharedElevenLabs, ...sharedBuzzsprout, ...project, ...process.env };
}
