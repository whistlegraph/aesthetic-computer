import { McpServer } from "@modelcontextprotocol/sdk/server/mcp.js";
import { StdioServerTransport } from "@modelcontextprotocol/sdk/server/stdio.js";
import { z } from "zod";
import { execFile as execFileCb, spawn as spawnCb, execFileSync } from "node:child_process";
import { promisify } from "node:util";
import { access, mkdir, readFile, writeFile } from "node:fs/promises";
import { readFileSync } from "node:fs";
import { basename, dirname, join } from "node:path";
import { homedir } from "node:os";
import { fileURLToPath } from "node:url";
import {
  buildMessage, handleColors, newDraftId, planReply, previewHeight, previewPage,
  sha256, signatureHtml, textToHtml, wrapHtml,
} from "./compose.mjs";

const execFile = promisify(execFileCb);

function resolveBin(name, fallbacks) {
  if (process.env[`AC_MAIL_${name.toUpperCase()}`]) {
    return process.env[`AC_MAIL_${name.toUpperCase()}`];
  }
  try {
    return execFileSync("which", [name], { encoding: "utf8" }).trim() || fallbacks[0];
  } catch {
    for (const p of fallbacks) {
      try { execFileSync("test", ["-x", p]); return p; } catch {}
    }
    return name;
  }
}

const HOME = homedir();

// Where the mail actually lives. The maildir, the mu index, and the msmtp
// credentials all sit on jasellite — no laptop keeps a copy anymore. So when
// AC_MAIL_HOST is set (the default), mu/mbsync/msmtp are run *there* over ssh
// and this process only shuttles argv and stdin. Searching the local disk
// instead would quietly report an empty inbox, which is far worse than an
// error: it reads as "you have no mail" rather than "I looked in the wrong
// place". Set AC_MAIL_HOST="" to go local — that's how jasellite runs itself.
//
// ssh, not the HTTP daemon, is the default on purpose: the daemon is bearer-
// authed, and handing that token to every laptop widens who can read the mail.
// ssh keys are already per-machine and already scoped to @jeffrey.
const MAIL_HOST = process.env.AC_MAIL_HOST ?? "jas@24.144.92.66";
const REMOTE = MAIL_HOST !== "";

// Remote binaries come off jasellite's login PATH; only resolve local paths
// when we're actually going to exec locally.
const MU = REMOTE ? "mu" : resolveBin("mu", ["/opt/homebrew/bin/mu", "/usr/bin/mu"]);
const MBSYNC = REMOTE ? "mbsync" : resolveBin("mbsync", ["/opt/homebrew/bin/mbsync", "/usr/bin/mbsync"]);
const MSMTP = REMOTE ? "msmtp" : resolveBin("msmtp", ["/opt/homebrew/bin/msmtp", "/usr/bin/msmtp"]);
// The maildir path is used by whichever machine runs mu — so in remote mode
// it must be jasellite's home, not this machine's ($HOME here is /Users/…
// on a Mac, and a remote `mu init` with that path corrupts the index).
const MAILDIR =
  process.env.AC_MAIL_MAILDIR ||
  (REMOTE ? "/home/jas/.mail-all" : join(HOME, ".mail-all"));
const MU_DB = process.env.AC_MAIL_MU_DB || join(HOME, ".cache", "mu", "xapian");
// helper.py owns everything that must happen beside the mail: the draft
// store, the preview render, the send, and the Gmail IMAP verbs. Like mu, it
// runs on whichever machine holds the maildir.
const HELPER =
  process.env.AC_MAIL_HELPER ||
  (REMOTE
    ? "/home/jas/services/mail-mcp/helper.py"
    : join(dirname(fileURLToPath(import.meta.url)), "helper.py"));
// Preview images are copied here, on the machine the session runs on, so
// they can be opened and shown.
const PREVIEW_DIR = process.env.AC_MAIL_PREVIEWS || join(HOME, ".cache", "ac-mail", "previews");

// jasellite's login shell is fish, and ssh hands it one flat string — so every
// argument we send has to survive a shell re-split on the far side. Single
// quotes are literal in both fish and POSIX sh; the only character that needs
// care is the quote itself.
const shellQuote = (arg) => `'${String(arg).replaceAll("'", `'\\''`)}'`;

// Rewrite a local command into the ssh invocation that runs it on jasellite.
// BatchMode keeps a missing key failing fast instead of hanging on a password
// prompt (nothing is watching stdin here — we're an MCP server on a pipe).
const SSH_OPTS = ["-o", "BatchMode=yes", "-o", "ConnectTimeout=10"];
function remotely(cmd, args) {
  if (!REMOTE) return [cmd, args];
  return ["ssh", [...SSH_OPTS, MAIL_HOST, cmd, ...args.map(shellQuote)]];
}
const ACCOUNTS = {
  "ac-mail": "mail@aesthetic.computer",
  "jas-mail": "me@jas.life",
  "sotce-mail": "mail@sotce.net",
};
// Extra mail accounts (e.g. private client inboxes) are merged in from an
// untracked config file so their identities never live in this public repo —
// same convention as the Slab menubar's ~/.config/slab/mail-accounts.json.
// Each entry: { "account": "<mbsync-channel>", "email": "<from address>" }.
const EXTRA_ACCOUNTS_PATH =
  process.env.AC_MAIL_EXTRA_ACCOUNTS ||
  join(HOME, ".config", "slab", "mail-accounts.json");
try {
  for (const entry of JSON.parse(readFileSync(EXTRA_ACCOUNTS_PATH, "utf8"))) {
    if (entry?.account && entry?.email) ACCOUNTS[entry.account] = entry.email;
  }
} catch {
  // No extra accounts configured (or file unreadable/malformed) — fine.
}
const ACCOUNT_NAMES = Object.keys(ACCOUNTS);
const INBOX_QUERY_ALL = ACCOUNT_NAMES.map((a) => `maildir:/${a}/INBOX`).join(" OR ");
const DEFAULT_ACCOUNT = "ac-mail";
const FROM_ADDRESS = ACCOUNTS[DEFAULT_ACCOUNT];
const STYLE_GUIDE_PATH =
  process.env.AC_EMAIL_STYLE_GUIDE ||
  "/workspaces/aesthetic-computer/toolchain/email/style-guide.md";
const DEFAULT_EMAIL_STYLE = {
  forceLowercase: true,
  defaultSignature: "@jeffrey",
  appendSignature: true,
};

function parseBooleanField(content, fieldName, fallback) {
  const match = content.match(new RegExp(`^\\s*${fieldName}:\\s*(true|false)\\s*$`, "im"));
  if (!match) return fallback;
  return match[1].toLowerCase() === "true";
}

function parseStringField(content, fieldName, fallback) {
  const match = content.match(
    new RegExp(`^\\s*${fieldName}:\\s*["']?([^"'\n]+)["']?\\s*$`, "im"),
  );
  if (!match) return fallback;
  return match[1].trim();
}

async function loadEmailStyle() {
  try {
    const content = await readFile(STYLE_GUIDE_PATH, "utf8");
    return {
      forceLowercase: parseBooleanField(content, "force_lowercase", DEFAULT_EMAIL_STYLE.forceLowercase),
      defaultSignature: parseStringField(
        content,
        "default_signature",
        DEFAULT_EMAIL_STYLE.defaultSignature,
      ),
      appendSignature: parseBooleanField(
        content,
        "append_signature_if_missing",
        DEFAULT_EMAIL_STYLE.appendSignature,
      ),
      source: STYLE_GUIDE_PATH,
      loadedFromGuide: true,
    };
  } catch {
    return {
      ...DEFAULT_EMAIL_STYLE,
      source: STYLE_GUIDE_PATH,
      loadedFromGuide: false,
    };
  }
}

// URLs stay verbatim — Drive file ids and similar are case-sensitive,
// and a lowercased link is a dead link.
function lowercasePreservingUrls(text) {
  return text
    .split(/(https?:\/\/\S+|www\.\S+)/g)
    .map((segment, i) => (i % 2 ? segment : segment.toLowerCase()))
    .join("");
}

function applyEmailStyle({ subject, body, preserveCase, signature }, style) {
  let nextSubject = subject;
  let nextBody = body;
  const finalSignature = (signature || style.defaultSignature).trim();

  if (style.forceLowercase && !preserveCase) {
    nextSubject = lowercasePreservingUrls(nextSubject);
    nextBody = lowercasePreservingUrls(nextBody);
  }

  // The text part carries the sign-off as a bare last line; the HTML part
  // renders it separately, so hand back the body both with and without it.
  let bodyNoSig = nextBody.trimEnd();
  let signed = "";
  if (style.appendSignature && finalSignature) {
    signed = finalSignature;
    if (bodyNoSig.toLowerCase().endsWith(finalSignature.toLowerCase())) {
      bodyNoSig = bodyNoSig.slice(0, -finalSignature.length).trimEnd();
    }
    nextBody = `${bodyNoSig}\n\n${finalSignature}`;
  }

  return { subject: nextSubject, body: nextBody, bodyNoSig, signature: signed };
}

async function ensureMuIndex() {
  // The index lives next to the maildir, so when that's remote this question
  // can only be answered remotely — a local stat would say "missing" and send
  // us off to re-init an index that already exists on the other machine.
  if (REMOTE) {
    try {
      await run(MU, ["info"], { timeout: 15_000 });
      return;
    } catch {
      await run(MU, ["init", `--maildir=${MAILDIR}`], { timeout: 30_000 });
      await run(MU, ["index"], { timeout: 300_000 });
      return;
    }
  }
  try {
    await access(MU_DB);
  } catch {
    await execFile(MU, ["init", `--maildir=${MAILDIR}`]);
    await execFile(MU, ["index"]);
  }
}

async function run(rawCmd, rawArgs, { input, timeout = 30_000 } = {}) {
  const [cmd, args] = remotely(rawCmd, rawArgs);
  if (input !== undefined) {
    // execFile doesn't pipe input to stdin; use spawn instead.
    return new Promise((resolve, reject) => {
      const child = spawnCb(cmd, args, { timeout });
      let stdout = "", stderr = "", settled = false;
      const finish = (fn) => (arg) => { if (settled) return; settled = true; fn(arg); };
      const ok = finish(resolve);
      const fail = finish(reject);
      child.stdout.on("data", (d) => (stdout += d));
      child.stderr.on("data", (d) => (stderr += d));
      child.on("close", (code) => {
        if (code !== 0) return fail(new Error(`${cmd} exited ${code}: ${stderr.trim()}`));
        ok({ stdout: stdout.trim(), stderr: stderr.trim() });
      });
      child.on("error", fail);
      // Guard the pipe: if the child exits early (bad message, auth failure,
      // oversized attachment), writing to its stdin emits EPIPE. Without this
      // handler that error is thrown unhandled and crashes the whole process —
      // taking the MCP stdio connection down mid-request. Swallow EPIPE and let
      // the 'close' handler above report the real exit code/stderr instead.
      child.stdin.on("error", (err) => {
        if (err && err.code === "EPIPE") return;
        fail(err);
      });
      try {
        child.stdin.end(input);
      } catch (err) {
        fail(err);
      }
    });
  }
  const opts = { timeout, maxBuffer: 10 * 1024 * 1024 };
  const { stdout, stderr } = await execFile(cmd, args, opts);
  return { stdout: stdout.trim(), stderr: stderr.trim() };
}

const helper = (args, { input = "", timeout = 60_000 } = {}) =>
  run("python3", [HELPER, ...args], { input, timeout });

// Saved per-character colours for a handle, from the AC API. Cached for an
// hour; a miss or an outage falls back to the default palette in compose.
const colorCache = new Map();
async function savedHandleColors(handle) {
  const hit = colorCache.get(handle);
  if (hit && Date.now() - hit.at < 3_600_000) return hit.colors;
  try {
    const name = encodeURIComponent(handle.replace(/^@/, ""));
    const r = await fetch(`https://aesthetic.computer/api/handle-colors?handle=${name}`, {
      signal: AbortSignal.timeout(5000),
    });
    const colors = r.ok ? (await r.json()).colors : null;
    colorCache.set(handle, { at: Date.now(), colors });
    return colors;
  } catch {
    return null;
  }
}

// Every copy mu has of a message (Gmail files one message under several
// labels), and the copy that best says where it lives.
async function findCopies(messageId) {
  const msgid = messageId.replace(/^<|>$/g, "");
  try {
    const { stdout } = await run(MU, ["find", "--format=json", `msgid:${msgid}`]);
    return JSON.parse(stdout || "[]");
  } catch {
    return []; // mu exits 4 on no match
  }
}
const pickCopy = (hits) =>
  hits.find((h) => /\/INBOX$/.test(h[":maildir"] ?? "")) ??
  hits.find((h) => !(h[":maildir"] ?? "").includes("[Gmail]")) ??
  hits[0];

function describeDraft(meta, text) {
  const line = (label, value) => (value ? `${label}: ${value}\n` : "");
  return (
    `Draft ${meta.id} · sha ${meta.sha256.slice(0, 8)}\n` +
    line("From", meta.from) + line("To", meta.to) + line("Cc", meta.cc) + line("Bcc", meta.bcc) +
    line("Subject", meta.subject) + line("In-Reply-To", meta.inReplyTo) +
    line("Attachments", meta.attachments?.join(", ")) +
    `\n${text}`
  );
}

// Build a draft: style the text, render the HTML part, assemble the message,
// and hand the lot to the mail host to store and preview. Sends nothing.
async function composeDraft({
  to, cc, bcc, subject, body, in_reply_to, references, preserve_case, keep_subject_case,
  signature, from_account, attachments, revises,
}) {
  if (!to?.trim()) throw new Error("no recipient");
  if (!body?.trim()) throw new Error("empty body");
  const style = await loadEmailStyle();
  const styled = applyEmailStyle({ subject, body, preserveCase: preserve_case, signature }, style);
  if (keep_subject_case) styled.subject = subject;
  const account = from_account || DEFAULT_ACCOUNT;
  const from = ACCOUNTS[account];
  const files = [];
  for (const path of attachments ?? []) {
    files.push({ name: basename(path), data: await readFile(path) });
  }

  const sig = styled.signature;
  const isHandle = sig.startsWith("@");
  const html = wrapHtml(
    textToHtml(styled.bodyNoSig),
    signatureHtml(sig, {
      colors: isHandle ? handleColors(sig, await savedHandleColors(sig)) : undefined,
      link: isHandle ? `https://aesthetic.computer/${sig}` : undefined,
    }),
  );

  const id = newDraftId();
  const messageId = `${id}@${from.split("@")[1]}`;
  const eml = buildMessage({
    from, to, cc, bcc, subject: styled.subject, text: styled.body, html, messageId,
    inReplyTo: in_reply_to, references, attachments: files,
  });
  const meta = {
    id, status: "pending", account, from, to, cc, bcc, subject: styled.subject, messageId,
    inReplyTo: in_reply_to, sha256: sha256(eml), created: new Date().toISOString(),
    attachments: files.map((f) => f.name), revises,
  };
  const envelope = { from, to, cc, bcc, subject: styled.subject, attachments: files };
  const { stdout } = await helper(["draft-store"], {
    input: JSON.stringify({
      meta,
      eml: Buffer.from(eml).toString("base64"),
      text: styled.body,
      previewHtml: previewPage(envelope, html),
      previewHeight: previewHeight(envelope, styled.body),
    }),
    timeout: 180_000,
  });
  const stored = JSON.parse(stdout);

  const notes = [];
  const content = [];
  if (stored.png) {
    await mkdir(PREVIEW_DIR, { recursive: true });
    const local = join(PREVIEW_DIR, `${id}.png`);
    await writeFile(local, Buffer.from(stored.png, "base64"));
    notes.push(`Preview image: ${local}`);
    content.push({ type: "image", data: stored.png, mimeType: "image/png" });
  } else {
    notes.push(`No preview image (${stored.previewError}).`);
  }
  notes.push(
    stored.gmailDraft
      ? `Also in Gmail Drafts for ${from}.`
      : `Not mirrored to Gmail Drafts (${stored.gmailError}).`,
  );
  if (revises) notes.push(`Replaces ${revises}, which can no longer be sent.`);
  notes.push(`NOT SENT. Send with mail_send { draft_id: "${id}" } once @jeffrey has approved this draft.`);
  content.unshift({ type: "text", text: `${describeDraft(meta, styled.body)}\n\n${notes.join("\n")}` });
  return { content };
}

// Start a sync on the mail host. The host's own mail-sync unit does the
// work (mbsync for every account, then mu index), so by default this only
// asks for it and returns. Hosts without the unit fall back to running
// mbsync here, which blocks.
async function startSync(wait) {
  try {
    await run(
      "systemctl",
      ["--user", "start", ...(wait ? [] : ["--no-block"]), "mail-sync"],
      { timeout: wait ? 900_000 : 20_000 },
    );
    return wait ? "Sync finished." : "Sync started on the mail host; new mail is searchable when it finishes.";
  } catch {
    const results = [];
    for (const channel of ACCOUNT_NAMES) {
      try {
        await run(MBSYNC, [channel], { timeout: 300_000 });
        results.push(`${channel}: ok`);
      } catch (err) {
        results.push(`${channel}: ${err.message}`);
      }
    }
    await run(MU, ["index"], { timeout: 120_000 });
    return `Sync finished.\n${results.join("\n")}`;
  }
}

// Every tool registers on a fresh McpServer from this factory. A factory —
// not a module-level singleton — because the shared HTTP daemon mode at the
// bottom builds one server per request (the SDK's stateless pattern):
// parallel Claude sessions would collide JSON-RPC request ids on a shared
// instance. Registration only stores closures, so a build per call is cheap.
// The body keeps top-level indentation to leave the tool definitions' diff
// history readable.
function buildServer() {
const server = new McpServer({
  name: "mail",
  version: "2.0.0",
});

// --- mail_sync ---
server.tool(
  "mail_sync",
  "Fetch new mail from Gmail and re-index. Returns at once by default — the sync runs on the mail host and takes from seconds to a few minutes; pass wait=true to block until it has finished.",
  {
    wait: z
      .boolean()
      .optional()
      .default(false)
      .describe("Block until the sync has finished (default false)"),
  },
  async ({ wait }) => {
    try {
      return { content: [{ type: "text", text: await startSync(wait) }] };
    } catch (err) {
      return fail("Sync", err);
    }
  },
);

// --- mail_search ---
server.tool(
  "mail_search",
  "Search mail using mu find with query string (mu query syntax)",
  { query: z.string().describe("mu find query string, e.g. 'from:alice subject:hello'") },
  async ({ query }) => {
    try {
      await ensureMuIndex();
      const { stdout } = await run(MU, [
        "find",
        "--format=json",
        "--sortfield=date",
        "--reverse",
        query,
      ]);
      return { content: [{ type: "text", text: stdout || "No results." }] };
    } catch (err) {
      // mu returns exit code 4 for no matches
      if (err.code === 4 || (err.stderr && err.stderr.includes("no matches"))) {
        return { content: [{ type: "text", text: "No results found." }] };
      }
      return {
        content: [{ type: "text", text: `Search failed: ${err.message}` }],
        isError: true,
      };
    }
  },
);

// --- mail_read ---
server.tool(
  "mail_read",
  "Read a specific email by message-id or file path using mu view",
  {
    identifier: z
      .string()
      .describe("Message-ID (with or without angle brackets) or file path to the email"),
  },
  async ({ identifier }) => {
    try {
      await ensureMuIndex();
      let filePath = identifier;

      // If it looks like a message-id rather than a path, find the file first
      if (!identifier.startsWith("/")) {
        const msgid = identifier.replace(/^<|>$/g, "");
        const { stdout } = await run(MU, [
          "find",
          "--format=plain",
          "--fields=l",
          `msgid:${msgid}`,
        ]);
        filePath = stdout.split("\n")[0];
        if (!filePath) {
          return {
            content: [{ type: "text", text: "Message not found." }],
            isError: true,
          };
        }
      }

      let { stdout } = await run(MU, ["view", filePath]);
      // A message with only an HTML body reads as nothing through mu view;
      // give the HTML as text instead.
      if (stdout.includes("[No text body found]")) {
        const { stdout: text } = await helper(["html-text", filePath]);
        stdout = stdout.replace("[No text body found]", text);
      }
      return { content: [{ type: "text", text: stdout }] };
    } catch (err) {
      return {
        content: [{ type: "text", text: `Read failed: ${err.message}` }],
        isError: true,
      };
    }
  },
);

// --- mail_inbox ---
server.tool(
  "mail_inbox",
  `List recent inbox messages sorted by date descending. Default covers all accounts; pass account=${ACCOUNT_NAMES.map((a) => `'${a}'`).join(" | ")} to filter.`,
  {
    count: z
      .number()
      .optional()
      .default(20)
      .describe("Number of messages to return (default 20)"),
    account: z
      .enum([...ACCOUNT_NAMES, "all"])
      .optional()
      .default("all")
      .describe("Which account inbox to list (default all)"),
  },
  async ({ count, account }) => {
    try {
      await ensureMuIndex();
      const query =
        account === "all"
          ? `(${INBOX_QUERY_ALL})`
          : `maildir:/${account}/INBOX`;
      const { stdout } = await run(MU, [
        "find",
        "--format=json",
        "--sortfield=date",
        "--reverse",
        `--maxnum=${count}`,
        query,
      ]);
      return { content: [{ type: "text", text: stdout || "Inbox is empty." }] };
    } catch (err) {
      if (err.code === 4 || (err.stderr && err.stderr.includes("no matches"))) {
        return { content: [{ type: "text", text: "Inbox is empty." }] };
      }
      return {
        content: [{ type: "text", text: `Inbox failed: ${err.message}` }],
        isError: true,
      };
    }
  },
);

// --- drafts ---
// Nothing is sent in one step. mail_draft / mail_reply build the exact bytes,
// store them on the mail host, and return what the recipient will see;
// mail_send takes a draft id and sends those bytes. A change is a new draft.
const draftFields = {
  body: z.string().describe("Email body, plain text. Blank lines separate paragraphs; lines starting '- ' become a list. Do not add the sign-off."),
  bcc: z.string().optional().describe("BCC address(es), comma-separated"),
  preserve_case: z
    .boolean()
    .optional()
    .default(false)
    .describe("Keep subject/body casing as written; without it both are lowercased"),
  signature: z
    .string()
    .optional()
    .describe("Sign-off override; defaults to the style guide's @handle, rendered in handle colours and linked to the AC profile"),
  attachments: z
    .array(z.string())
    .optional()
    .describe("Absolute paths of files to attach"),
  revises: z
    .string()
    .optional()
    .describe("Id of a pending draft this one replaces; the old one can no longer be sent"),
};

const fail = (what, err) => ({
  content: [{ type: "text", text: `${what} failed: ${err.message}` }],
  isError: true,
});

server.tool(
  "mail_draft",
  "Compose a new email as a draft. Nothing is sent. Returns the draft id, the exact headers and text, and a rendered image of the email as the recipient will see it; show that to @jeffrey. To change a draft, call again with `revises`. Use mail_reply to answer an existing message.",
  {
    to: z.string().describe("Recipient address(es), comma-separated"),
    cc: z.string().optional().describe("CC address(es), comma-separated"),
    subject: z.string().describe("Email subject"),
    from_account: z
      .enum(ACCOUNT_NAMES)
      .optional()
      .default(DEFAULT_ACCOUNT)
      .describe(`Account to send from (${ACCOUNT_NAMES.join(" | ")})`),
    ...draftFields,
  },
  async (args) => {
    try {
      return await composeDraft(args);
    } catch (err) {
      return fail("Draft", err);
    }
  },
);

server.tool(
  "mail_reply",
  "Draft a reply to a message by its Message-ID. Nothing is sent. Threading, subject, recipients (reply-all by default) and the answering account are taken from the original; pass to/cc/from_account only to override them. Returns the same preview as mail_draft.",
  {
    message_id: z.string().describe("Message-ID of the email being answered"),
    reply_all: z
      .boolean()
      .optional()
      .default(true)
      .describe("Keep the original's other recipients on cc (default true)"),
    to: z.string().optional().describe("Override the recipient(s)"),
    cc: z.string().optional().describe("Override the cc list"),
    from_account: z
      .enum(ACCOUNT_NAMES)
      .optional()
      .describe("Override the account; defaults to the one the original was addressed to"),
    ...draftFields,
  },
  async ({ message_id, reply_all, to, cc, from_account, ...rest }) => {
    try {
      await ensureMuIndex();
      const original = pickCopy(await findCopies(message_id));
      if (!original) throw new Error(`no message with id ${message_id}`);
      const plan = planReply(original, { mine: Object.values(ACCOUNTS), replyAll: reply_all });
      const account =
        from_account ||
        ACCOUNT_NAMES.find((a) => ACCOUNTS[a].toLowerCase() === plan.answeredAs) ||
        ACCOUNT_NAMES.find((a) => original[":maildir"]?.startsWith(`/${a}/`)) ||
        DEFAULT_ACCOUNT;
      return await composeDraft({
        ...rest,
        to: to ?? plan.to,
        cc: cc ?? plan.cc,
        subject: plan.subject,
        in_reply_to: plan.inReplyTo,
        references: plan.references,
        from_account: account,
        // A reply keeps the thread's subject as its author cased it.
        keep_subject_case: true,
      });
    } catch (err) {
      return fail("Reply draft", err);
    }
  },
);

server.tool(
  "mail_send",
  "Send a draft made by mail_draft or mail_reply. Takes only the draft id: the bytes that were previewed are the bytes that go. Call it only after @jeffrey has seen this exact draft and said to send it — a revised draft has a new id and needs a new yes. Report the returned headers and body afterward.",
  { draft_id: z.string().describe("Id of a pending draft") },
  async ({ draft_id }) => {
    try {
      const { stdout } = await helper(["draft-send", draft_id], { timeout: 600_000 });
      const { meta, text } = JSON.parse(stdout);
      return {
        content: [{ type: "text", text: `SENT ${meta.sentAt}\n${describeDraft(meta, text)}` }],
      };
    } catch (err) {
      return fail("Send", err);
    }
  },
);

server.tool(
  "mail_drafts",
  "List drafts that are waiting to be sent or discarded. Pass draft_id to show one in full.",
  { draft_id: z.string().optional().describe("Show this draft's headers and text") },
  async ({ draft_id }) => {
    try {
      if (draft_id) {
        const { meta, text } = JSON.parse((await helper(["draft-show", draft_id])).stdout);
        return { content: [{ type: "text", text: `${meta.status.toUpperCase()}\n${describeDraft(meta, text)}` }] };
      }
      const { stdout } = await helper(["draft-list"]);
      const rows = stdout
        .split("\n")
        .filter(Boolean)
        .map((line) => JSON.parse(line))
        .map((m) => `${m.id}  ${m.created.slice(0, 16)}  ${m.from} → ${m.to}  "${m.subject}"`);
      return { content: [{ type: "text", text: rows.join("\n") || "No pending drafts." }] };
    } catch (err) {
      return fail("Drafts", err);
    }
  },
);

server.tool(
  "mail_discard",
  "Throw away a pending draft. It can no longer be sent, and its copy in Gmail Drafts is removed.",
  { draft_id: z.string().describe("Id of a pending draft") },
  async ({ draft_id }) => {
    try {
      await helper(["draft-discard", draft_id]);
      return { content: [{ type: "text", text: `Discarded ${draft_id}.` }] };
    } catch (err) {
      return fail("Discard", err);
    }
  },
);

// --- mail_archive ---
server.tool(
  "mail_archive",
  "Archive inbox messages by Message-ID: removes them from the inbox, keeps them in All Mail. The account is worked out from where each message sits.",
  { message_ids: z.array(z.string()).describe("Message-IDs to archive") },
  async ({ message_ids }) => {
    try {
      await ensureMuIndex();
      const byAccount = new Map();
      const lines = [];
      for (const id of message_ids) {
        const inbox = (await findCopies(id)).find((h) => /\/INBOX$/.test(h[":maildir"] ?? ""));
        if (!inbox) {
          lines.push(`not in an inbox: ${id}`);
          continue;
        }
        const account = inbox[":maildir"].split("/")[1];
        byAccount.set(account, [...(byAccount.get(account) ?? []), id.replace(/^<|>$/g, "")]);
      }
      for (const [account, ids] of byAccount) {
        lines.push((await helper(["archive", account, ...ids], { timeout: 120_000 })).stdout);
      }
      // Let the local index catch up without holding this call open.
      if (byAccount.size) lines.push(await startSync(false));
      return { content: [{ type: "text", text: lines.join("\n") }] };
    } catch (err) {
      return fail("Archive", err);
    }
  },
);

// --- mail_count ---
server.tool(
  "mail_count",
  "Return count of unread inbox messages (all accounts, avoids Gmail All Mail duplication)",
  {},
  async () => {
    try {
      await ensureMuIndex();
      const { stdout } = await run(MU, [
        "find",
        "--format=plain",
        "--fields=l",
        `flag:unread AND (${INBOX_QUERY_ALL})`,
      ]);
      const count = stdout ? stdout.split("\n").filter(Boolean).length : 0;
      return {
        content: [{ type: "text", text: `Unread messages: ${count}` }],
      };
    } catch (err) {
      if (err.code === 4 || (err.stderr && err.stderr.includes("no matches"))) {
        return { content: [{ type: "text", text: "Unread messages: 0" }] };
      }
      return {
        content: [{ type: "text", text: `Count failed: ${err.message}` }],
        isError: true,
      };
    }
  },
);

return server;
}

// Start — stdio by default (Claude spawns one process per session), or a
// shared daemon with `--http [port]` so any number of parallel sessions
// reuse ONE resident process (wired up by toolchain/mcp/install-daemons.sh
// + a local-scope http entry in ~/.claude.json). Stateless streamable
// HTTP: fresh server + transport per POST, closed when the response ends.
const httpFlag = process.argv.indexOf("--http");
if (httpFlag !== -1) {
  const port = Number(process.argv[httpFlag + 1]) || 7765;
  // Remote mode (jasellite): AC_MAIL_HTTP_HOST binds beyond loopback —
  // point it at the tailscale IP so the daemon is tailnet-only, never
  // the public interface. AC_MAIL_TOKEN adds bearer auth on top.
  const host = process.env.AC_MAIL_HTTP_HOST || "127.0.0.1";
  const token = process.env.AC_MAIL_TOKEN || "";
  const { StreamableHTTPServerTransport } = await import(
    "@modelcontextprotocol/sdk/server/streamableHttp.js"
  );
  const { timingSafeEqual } = await import("node:crypto");
  const { createServer } = await import("node:http");
  const authorized = (req) => {
    if (!token) return true;
    const header = req.headers.authorization || "";
    const given = Buffer.from(header.replace(/^Bearer\s+/i, ""));
    const want = Buffer.from(token);
    return given.length === want.length && timingSafeEqual(given, want);
  };
  createServer(async (req, res) => {
    if (!authorized(req)) {
      res.writeHead(401, { "Content-Type": "application/json" });
      res.end(JSON.stringify({
        jsonrpc: "2.0",
        error: { code: -32000, message: "unauthorized" },
        id: null,
      }));
      return;
    }
    if (req.method !== "POST") {
      res.writeHead(405, { "Content-Type": "application/json" });
      res.end(JSON.stringify({
        jsonrpc: "2.0",
        error: { code: -32000, message: "stateless server: POST only" },
        id: null,
      }));
      return;
    }
    let body = "";
    for await (const chunk of req) body += chunk;
    try {
      const server = buildServer();
      const transport = new StreamableHTTPServerTransport({
        sessionIdGenerator: undefined,
        enableJsonResponse: true,
      });
      res.on("close", () => {
        transport.close();
        server.close();
      });
      await server.connect(transport);
      await transport.handleRequest(req, res, JSON.parse(body));
    } catch (err) {
      if (!res.headersSent) {
        res.writeHead(500, { "Content-Type": "application/json" });
        res.end(JSON.stringify({
          jsonrpc: "2.0",
          error: { code: -32603, message: err.message },
          id: null,
        }));
      }
    }
  }).listen(port, host, () => {
    console.error(
      `📬 mail-mcp shared daemon on http://${host}:${port}` +
        (token ? " (bearer auth)" : ""),
    );
  });
} else {
  await buildServer().connect(new StdioServerTransport());
}
