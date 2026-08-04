import { readFile, readdir } from "node:fs/promises";
import path from "node:path";
import { fileURLToPath } from "node:url";
import { classifyPostHogFunction } from "../../shared/posthog-policy.mjs";

const HANDLER_EXPORT =
  /export\s+(?:async\s+)?function\s+handler|export\s+const\s+handler|exports\.handler|export\s+default\s+(?:async\s+)?(?:function|\()/;

export const LITH_ROUTE_FAMILIES = Object.freeze([
  {
    surface: "function-api",
    routes: ["/api/:fn", "/api/:fn/*", "/.netlify/functions/:fn"],
    posthog: "per-function-policy",
  },
  {
    surface: "function-aliases",
    routes: [
      "/handle",
      "/user",
      "/run",
      "/reload/*",
      "/session/*",
      "/authorized",
      "/handles",
      "/redirect-proxy*",
      "/docs*",
      "/presigned-{upload,download}-url/*",
    ],
    posthog: "per-function-policy",
  },
  {
    surface: "media",
    routes: ["/media/*", "/frame/:piece"],
    posthog: "minimized-browser-or-aggregate",
  },
  {
    surface: "fedac-and-oven",
    routes: [
      "/api/os-release-upload",
      "/api/os-image",
      "/api/{pack-html,bundle-html,os}",
    ],
    posthog: "inventory-only",
  },
  {
    surface: "gym",
    routes: ["/api/{publish,history,rewind}-gym"],
    posthog: "aggregate-status-only",
  },
  {
    surface: "lith-operations",
    routes: ["/lith/deploy", "/lith", "/lith/{stats,errors,requests,traffic}"],
    posthog: "existing-lith-silo-only",
  },
  {
    surface: "local-and-host-tools",
    routes: ["/local-upload/:filename", "/local-uploads/*", "/menuband-logs"],
    posthog: "inventory-only",
  },
  {
    surface: "host-rewrites",
    routes: ["api.*", "data.*", "prompt.*", "notepat.com"],
    posthog: "per-destination-policy",
  },
  {
    surface: "site-and-pieces",
    routes: ["static files", "piece fallback", "index fallback"],
    posthog: "minimized-browser",
  },
]);

export function classifyFunction(name) {
  return classifyPostHogFunction(name);
}

export async function buildPostHogInventory(root = process.cwd()) {
  const functionDir = path.join(root, "system/netlify/functions");
  const files = (await readdir(functionDir)).filter((file) =>
    /\.(mjs|js)$/.test(file),
  );
  const byName = new Map();

  for (const file of files.sort()) {
    const name = file.replace(/\.(mjs|js)$/, "");
    const sourcePath = path.join(functionDir, file);
    const source = await readFile(sourcePath, "utf8");
    const current = byName.get(name) || {
      name,
      canonicalFunctionRoute: `/api/${name}`,
      kind: HANDLER_EXPORT.test(source) ? "handler" : "helper-or-script",
      sources: [],
      ...classifyFunction(name),
    };
    if (HANDLER_EXPORT.test(source)) current.kind = "handler";
    current.sources.push(`system/netlify/functions/${file}`);
    byName.set(name, current);
  }

  const functions = [...byName.values()];
  const classes = Object.fromEntries(
    [...new Set(functions.map((entry) => entry.class))]
      .sort()
      .map((className) => [
        className,
        functions.filter((entry) => entry.class === className).length,
      ]),
  );

  return {
    generatedFrom: "system/netlify/functions/*.{js,mjs}",
    sourceFileCount: files.length,
    handlerCount: functions.filter((entry) => entry.kind === "handler").length,
    helperOrScriptCount: functions.filter((entry) => entry.kind !== "handler")
      .length,
    classes,
    functions,
    lithRouteFamilies: LITH_ROUTE_FAMILIES,
  };
}

const isMain =
  process.argv[1] &&
  fileURLToPath(import.meta.url) === path.resolve(process.argv[1]);
if (isMain) console.log(JSON.stringify(await buildPostHogInventory(), null, 2));
