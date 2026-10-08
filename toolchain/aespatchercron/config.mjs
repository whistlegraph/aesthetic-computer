import { homedir } from "node:os";
import { join } from "node:path";

export const defaultHome = join(homedir(), ".local/share/aespatchercron");
export function eligiblePiece(route) {
  return /^[a-z0-9-]+$/.test(route) && !/^(?:prompt|give|shop|keep|mint|print|mug|ticket|billing|wallet|auth|login|logout|signin|user|handle|device|delete|register|imnew|account|profile|token|password|email|publish)(?:-|$)/.test(route);
}
export function defaults(repo) {
  return { repo, fetch: true, collection: { hours: 24, minimum: 3, limit: 100, chatLimit: 200, logLimit: 2000 },
    lith: { host: "root@lith.aesthetic.computer", root: "/opt/ac", identity: join(repo, "aesthetic-computer-vault/home/.ssh/id_rsa") },
    bounds: { perDay: 2, maxActive: 4, maxFiles: 4, maxLines: 200 },
    worker: { executable: "codex", timeoutMs: 900000 }, cloudflare: { enabled: true },
    posthog: { enabled: true, projectId: null, organizationId: null }, hosting: { kind: "unconfigured" } };
}

export function validateConfig(config) {
  if (!config || typeof config.repo !== "string" || !config.repo.startsWith("/") || typeof config.fetch !== "boolean" ||
      !config.bounds || Object.entries({ perDay: 5, maxActive: 10, maxFiles: 8, maxLines: 500 }).some(([key, max]) =>
        !Number.isInteger(config.bounds[key]) || config.bounds[key] < 1 || config.bounds[key] > max) ||
      !config.worker || typeof config.worker.executable !== "string" || !Number.isInteger(config.worker.timeoutMs) || config.worker.timeoutMs < 1000 || config.worker.timeoutMs > 1800000)
    throw new Error("Invalid bounded runner config");
  return config;
}
