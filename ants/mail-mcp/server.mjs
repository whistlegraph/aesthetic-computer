import { McpServer } from "@modelcontextprotocol/sdk/server/mcp.js";
import { StdioServerTransport } from "@modelcontextprotocol/sdk/server/stdio.js";
import { z } from "zod";
import { execFile as execFileCb } from "node:child_process";
import { promisify } from "node:util";
import { access, readFile } from "node:fs/promises";

const execFile = promisify(execFileCb);

const MU = "/usr/bin/mu";
const MBSYNC = "/usr/bin/mbsync";
const MSMTP = "/usr/bin/msmtp";
const MAILDIR = "/home/me/.mail";
const MU_DB = "/home/me/.cache/mu/xapian";
const FROM_ADDRESS = "mail@aesthetic.computer";
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

function applyEmailStyle({ subject, body, preserveCase, signature }, style) {
  let nextSubject = subject;
  let nextBody = body;
  const finalSignature = (signature || style.defaultSignature).trim();

  if (style.forceLowercase && !preserveCase) {
    nextSubject = nextSubject.toLowerCase();
    nextBody = nextBody.toLowerCase();
  }

  if (style.appendSignature && finalSignature) {
    const trimmedBody = nextBody.trimEnd();
    const endsWithSignature = trimmedBody
      .toLowerCase()
      .endsWith(finalSignature.toLowerCase());
    nextBody = endsWithSignature ? trimmedBody : `${trimmedBody}\n\n${finalSignature}`;
  }

  return { subject: nextSubject, body: nextBody };
}

async function ensureMuIndex() {
  try {
    await access(MU_DB);
  } catch {
    await execFile(MU, ["init", `--maildir=${MAILDIR}`]);
    await execFile(MU, ["index"]);
  }
}

async function run(cmd, args, { input, timeout = 30_000 } = {}) {
  const opts = { timeout, maxBuffer: 10 * 1024 * 1024 };
  if (input !== undefined) opts.input = input;
  const { stdout, stderr } = await execFile(cmd, args, opts);
  return { stdout: stdout.trim(), stderr: stderr.trim() };
}

const server = new McpServer({
  name: "mail",
  version: "1.0.0",
});

// --- mail_sync ---
server.tool("mail_sync", "Sync mail from Gmail using mbsync", {}, async () => {
  try {
    const { stdout, stderr } = await run(MBSYNC, ["ac-mail"], {
      timeout: 120_000,
    });
    // Re-index after sync
    await run(MU, ["index"], { timeout: 60_000 });
    return {
      content: [
        {
          type: "text",
          text: `Sync complete.\n${stdout}\n${stderr}`.trim(),
        },
      ],
    };
  } catch (err) {
    return {
      content: [{ type: "text", text: `Sync failed: ${err.message}` }],
      isError: true,
    };
  }
});

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

      const { stdout } = await run(MU, ["view", filePath]);
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
  "List recent inbox messages sorted by date descending",
  {
    count: z
      .number()
      .optional()
      .default(20)
      .describe("Number of messages to return (default 20)"),
  },
  async ({ count }) => {
    try {
      await ensureMuIndex();
      const { stdout } = await run(MU, [
        "find",
        "--format=json",
        "--sortfield=date",
        "--reverse",
        `--maxnum=${count}`,
        "maildir:/INBOX",
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

// --- mail_send ---
server.tool(
  "mail_send",
  "Send an email via msmtp. From is always mail@aesthetic.computer.",
  {
    to: z.string().describe("Recipient email address"),
    subject: z.string().describe("Email subject"),
    body: z.string().describe("Email body (plain text)"),
    preserve_case: z
      .boolean()
      .optional()
      .default(false)
      .describe("Keep original subject/body casing when true"),
    signature: z
      .string()
      .optional()
      .describe("Optional sign-off override; defaults to style-guide signature"),
  },
  async ({ to, subject, body, preserve_case, signature }) => {
    try {
      const style = await loadEmailStyle();
      const styled = applyEmailStyle({
        subject,
        body,
        preserveCase: preserve_case,
        signature,
      }, style);
      const date = new Date().toUTCString();
      const message = [
        `Date: ${date}`,
        `From: ${FROM_ADDRESS}`,
        `To: ${to}`,
        `Subject: ${styled.subject}`,
        "MIME-Version: 1.0",
        "Content-Type: text/plain; charset=UTF-8",
        "Content-Transfer-Encoding: 8bit",
        "",
        styled.body,
      ].join("\r\n");

      const { stdout, stderr } = await run(MSMTP, ["-t"], { input: message });
      return {
        content: [
          {
            type: "text",
            text: `Email sent to ${to}.\nStyle guide: ${style.source}${style.loadedFromGuide ? "" : " (defaults used)"}\n${stdout}\n${stderr}`.trim(),
          },
        ],
      };
    } catch (err) {
      return {
        content: [{ type: "text", text: `Send failed: ${err.message}` }],
        isError: true,
      };
    }
  },
);

// --- mail_count ---
server.tool(
  "mail_count",
  "Return count of unread messages",
  {},
  async () => {
    try {
      await ensureMuIndex();
      const { stdout } = await run(MU, [
        "find",
        "--format=plain",
        "--fields=l",
        "flag:unread",
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

// Start
const transport = new StdioServerTransport();
await server.connect(transport);
