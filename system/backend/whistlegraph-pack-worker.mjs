import { parentPort, workerData } from 'node:worker_threads';
import { fileURLToPath } from 'node:url';
import { execFileSync } from 'node:child_process';
process.env.AC_SOURCE_DIR = fileURLToPath(new URL('../public/aesthetic.computer', import.meta.url));
const { createJSPieceBundleFromSource } = await import('../../oven/bundler.mjs');
const result = await createJSPieceBundleFromSource(`${workerData.code}-v${workerData.version}`, workerData.source, { density:workerData.density });
const version = execFileSync('git', ['rev-parse', 'HEAD'], { cwd:fileURLToPath(new URL('../../', import.meta.url)), encoding:'utf8' }).trim();
parentPort.postMessage({ html:result.html, version });
