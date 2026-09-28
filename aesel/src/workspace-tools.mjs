// workspace-tools.mjs — the general coding tools for Aesel's own agent loop.
//
// The hosted bridge was built for one file, the piece, and gives the model one
// way to touch it. A pro session is a repository, so when Aesel runs the loop
// itself against an open model it needs what a vendor CLI would have brought:
// read, search, edit, write and a shell. They are deliberately few and plain —
// an open model follows five obvious tools better than fifteen clever ones.
//
// Reads and searches run straight away. Writes and commands go through the
// interface's approval drawer (`approve`), which honours the session's
// auto-allow the same way it does for Claude and Codex.
import { spawn } from "node:child_process";
import { existsSync, mkdirSync, readFileSync, writeFileSync } from "node:fs";
import { dirname, isAbsolute, resolve } from "node:path";

// Cut every tool answer to this. A 9,000-line file read whole is a round that
// costs more than the rest of the turn; the model is told how to page instead.
const MAX_OUTPUT = 30000;
const DEFAULT_LINES = 400;
const COMMAND_TIMEOUT = 120000;

export const WORKSPACE_TOOLS = [
  {
    name: "read_file",
    description: `Read a text file, with line numbers. Returns at most ${DEFAULT_LINES} lines unless 'limit' says otherwise; use 'offset' (1-based line) to page through a long file.`,
    input_schema: {
      type: "object",
      properties: {
        path: { type: "string", description: "Path, absolute or relative to the working directory." },
        offset: { type: "integer", description: "First line to return, 1-based." },
        limit: { type: "integer", description: "How many lines to return." },
      },
      required: ["path"],
    },
  },
  {
    name: "search",
    description: "Search file contents with ripgrep. Returns matching lines as path:line:text. Respects .gitignore.",
    input_schema: {
      type: "object",
      properties: {
        pattern: { type: "string", description: "A ripgrep regular expression." },
        path: { type: "string", description: "Directory or file to search; defaults to the working directory." },
        glob: { type: "string", description: "Only files matching this glob, e.g. '*.mjs'." },
      },
      required: ["pattern"],
    },
  },
  {
    name: "edit_file",
    description: "Replace one exact occurrence of old_text with new_text in a file. old_text must match the file exactly, including indentation, and appear once; include surrounding lines to make it unique. Read the file first.",
    input_schema: {
      type: "object",
      properties: {
        path: { type: "string" },
        old_text: { type: "string" },
        new_text: { type: "string" },
      },
      required: ["path", "old_text", "new_text"],
    },
  },
  {
    name: "write_file",
    description: "Create a file, or replace one entirely. Prefer edit_file for changes to an existing file.",
    input_schema: {
      type: "object",
      properties: { path: { type: "string" }, content: { type: "string" } },
      required: ["path", "content"],
    },
  },
  {
    name: "bash",
    description: `Run a shell command in the working directory (bash -lc). Output is stdout and stderr together, cut to ${MAX_OUTPUT} characters. Times out after ${COMMAND_TIMEOUT / 1000} s. Use it for tests, git, builds and listing files.`,
    input_schema: {
      type: "object",
      properties: { command: { type: "string" } },
      required: ["command"],
    },
  },
];

export const WORKSPACE_TOOL_NAMES = new Set(WORKSPACE_TOOLS.map((tool) => tool.name));

export const WORKSPACE_INSTRUCTIONS =
  "Tools: read_file, search, edit_file, write_file and bash act on the working directory. Look before you change: search or read the code you are about to edit. Make the smallest change that does the job, then run the relevant test or check with bash and report what it actually printed. Do not claim something works without having run it.";

// The person's own command-line tools, named so the model reaches for them
// instead of searching the disk. A CLI costs nothing until it is run, works on
// every backend, and is something every model already knows how to drive —
// which is why the toolbox is commands, not a wall of tool schemas.
const TOOLBOX = [
  ["frame", "see any fleet Mac's screen: `frame <machine>` (pixels + OCR + accessibility tree; `frame --help`)"],
  ["puppet", "act on a Mac or browser: click, type, keys (`puppet --help`)"],
  ["slab-ledger", "the fleet's live sessions"],
  ["ac-os", "AC Native OS builds"],
  ["ac", "the person's AC account: `ac whoami`, `ac publish <file> [slug]`, `ac colors orange teal` (their @handle's letter colours, names or hex, cycling)"],
  ["rg", "ripgrep, for searching code — never grep -r"],
  ["fd", "find files by name, fast"],
  ["jq", "JSON"],
    ["gh", "GitHub"],
];

export function toolboxInstructions(env = process.env) {
  const dirs = String(env.PATH || "").split(":").filter(Boolean);
  const found = TOOLBOX.filter(([name]) => dirs.some((dir) => existsSync(`${dir}/${name}`)));
  if (!found.length) return "";
  return `Your toolbox (run with bash): ${found.map(([name, what]) => `${name} — ${what}`).join("; ")}. Machines in the fleet: run \`frame --help\` or \`frame list\` rather than guessing names. When asked to do several things (e.g. two machines), do all of them.`;
}

function clip(text) {
  if (text.length <= MAX_OUTPUT) return text;
  return `${text.slice(0, MAX_OUTPUT)}\n… cut at ${MAX_OUTPUT} of ${text.length} characters`;
}

function run(command, args, { cwd, signal, timeout = COMMAND_TIMEOUT }) {
  return new Promise((done) => {
    const child = spawn(command, args, { cwd, signal, env: process.env });
    let output = "";
    const collect = (chunk) => {
      if (output.length < MAX_OUTPUT * 2) output += chunk;
    };
    child.stdout.on("data", collect);
    child.stderr.on("data", collect);
    const timer = setTimeout(() => child.kill("SIGKILL"), timeout);
    child.on("error", (error) => {
      clearTimeout(timer);
      done({ code: -1, output: output + error.message });
    });
    child.on("close", (code, killed) => {
      clearTimeout(timer);
      done({ code: killed === "SIGKILL" ? "timeout" : code, output });
    });
  });
}

// Run one workspace tool. `approve(kind, subject)` resolves true when the
// person (or their auto-allow) lets it go ahead.
export async function runWorkspaceTool(name, input = {}, { cwd, signal, approve }) {
  const at = (path) => resolve(cwd, isAbsolute(String(path)) ? String(path) : String(path || "."));

  if (name === "read_file") {
    const lines = readFileSync(at(input.path), "utf8").split("\n");
    const start = Math.max(1, Number(input.offset) || 1);
    const count = Math.max(1, Number(input.limit) || DEFAULT_LINES);
    const shown = lines.slice(start - 1, start - 1 + count).map((line, i) => `${start + i}\t${line}`);
    const rest = lines.length - (start - 1 + shown.length);
    return clip(shown.join("\n") + (rest > 0 ? `\n… ${rest} more lines; read again with offset ${start + shown.length}` : ""));
  }

  if (name === "search") {
    const args = ["--line-number", "--no-heading", "--color", "never", "--max-columns", "300"];
    if (input.glob) args.push("--glob", String(input.glob));
    args.push("--", String(input.pattern), at(input.path));
    const { code, output } = await run("rg", args, { cwd, signal, timeout: 30000 });
    if (code === 1) return "No matches.";
    return clip(output.split(`${cwd}/`).join(""));
  }

  if (name === "edit_file") {
    const file = at(input.path);
    const before = readFileSync(file, "utf8");
    const old = String(input.old_text ?? "");
    const count = old ? before.split(old).length - 1 : 0;
    if (count === 0) throw new Error("old_text was not found in the file. Read it again and copy the text exactly.");
    if (count > 1) throw new Error(`old_text appears ${count} times. Include more surrounding lines so it is unique.`);
    if (!(await approve("file", file))) throw new Error("The person declined this edit.");
    writeFileSync(file, before.replace(old, () => String(input.new_text ?? "")));
    return `Edited ${input.path}.`;
  }

  if (name === "write_file") {
    const file = at(input.path);
    if (!(await approve("file", file))) throw new Error("The person declined this write.");
    mkdirSync(dirname(file), { recursive: true });
    writeFileSync(file, String(input.content ?? ""));
    return `Wrote ${input.path}.`;
  }

  if (name === "bash") {
    const command = String(input.command || "");
    if (!(await approve("command", command))) throw new Error("The person declined this command.");
    const { code, output } = await run("bash", ["-lc", command], { cwd, signal });
    return clip(`${output}${output.endsWith("\n") || !output ? "" : "\n"}[exit ${code}]`);
  }

  throw new Error(`No tool named ${name}.`);
}
