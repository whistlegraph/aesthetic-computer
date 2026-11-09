// Renders the query-parameter ledger (lib/params.mjs) to docs/query-params.md.
//   node system/scripts/params-doc.mjs
import { writeFile } from "node:fs/promises";
import path from "node:path";
import { fileURLToPath } from "node:url";
import { PARAMS, renderDoc } from "../public/aesthetic.computer/lib/params.mjs";

const scriptsDir = path.dirname(fileURLToPath(import.meta.url));
const out = path.resolve(scriptsDir, "../../docs/query-params.md");

const doc = `# Query parameters

Generated from [\`lib/params.mjs\`](../system/public/aesthetic.computer/lib/params.mjs)
by \`node system/scripts/params-doc.mjs\`. Edit the ledger, not this file.

${Object.keys(PARAMS).length} parameters. A flag is on when present; \`=false\`
does not turn it off — drop the name. Keep query parameters before any
\`#fragment\`.

**Scope** — \`view\`: presentation; bios re-applies *carry: bar* names on every
history write and the worker re-appends *carry: jump* names when a piece jumps.
\`boot\`: consumed on the main thread at start. \`auth\`: session hand-off,
stripped before the worker sees it. \`piece\`: reaches the piece as \`$.query\`.
\`server\`: read by the HTML shell.

**Kind** — \`flag\`: present → true. \`number\`: Number(value). \`size\`: bare,
\`true\`, or \`WxH\`. \`string\`: as written. \`token\`: opaque; never logged.

Unknown names reach the piece as strings and are warned once per worker.
Boot telemetry records which names were present — names only, never values.

${renderDoc()}
`;

await writeFile(out, doc);
console.log(`${path.relative(process.cwd(), out)} (${Object.keys(PARAMS).length} parameters)`);
