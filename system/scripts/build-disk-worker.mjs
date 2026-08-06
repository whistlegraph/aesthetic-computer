import { createHash } from "node:crypto";
import { mkdir, writeFile } from "node:fs/promises";
import path from "node:path";
import { fileURLToPath } from "node:url";
import { build } from "esbuild";

const scriptsDir = path.dirname(fileURLToPath(import.meta.url));
const systemDir = process.env.AC_DISK_WORKER_SYSTEM_DIR
  ? path.resolve(process.env.AC_DISK_WORKER_SYSTEM_DIR)
  : path.resolve(scriptsDir, "..");
const libDir = path.join(systemDir, "public/aesthetic.computer/lib");
const entry = "public/aesthetic.computer/lib/disk.mjs";

const preserveRuntimeImports = {
  name: "preserve-runtime-imports",
  setup(buildApi) {
    buildApi.onResolve({ filter: /.*/ }, (args) => {
      if (args.kind === "dynamic-import") {
        return { path: args.path, external: true };
      }
      if (["url", "module", "https"].includes(args.path)) {
        return { path: args.path, external: true };
      }
      return null;
    });
  },
};

const result = await build({
  absWorkingDir: systemDir,
  entryPoints: [entry],
  bundle: true,
  format: "esm",
  platform: "browser",
  target: ["es2022"],
  treeShaking: true,
  legalComments: "none",
  write: false,
  plugins: [preserveRuntimeImports],
});

const output = result.outputFiles?.[0]?.contents;
if (!output?.length) throw new Error("Disk worker build produced no output");

const sha256 = createHash("sha256").update(output).digest("hex");
const filename = `disk.worker.${sha256.slice(0, 12)}.mjs`;
const manifest = {
  filename,
  sha256,
  bytes: output.length,
};

await mkdir(libDir, { recursive: true });
await writeFile(path.join(libDir, filename), output);
await writeFile(
  path.join(libDir, "disk-worker-manifest.json"),
  `${JSON.stringify(manifest, null, 2)}\n`,
);

console.log(`${filename} ${output.length} bytes`);
