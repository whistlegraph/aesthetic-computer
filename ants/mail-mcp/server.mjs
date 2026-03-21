import { McpServer } from "@modelcontextprotocol/sdk/server/mcp.js";
import { StdioServerTransport } from "@modelcontextprotocol/sdk/server/stdio.js";
import { z } from "zod";
import { execFile as execFileCb, spawn as spawnCb } from "node:child_process";
import { promisify } from "node:util";
import { access, readFile, stat } from "node:fs/promises";
import { basename } from "node:path";

const execFile = promisify(execFileCb);

const MU = "/usr/bin/mu";
const MBSYNC = "/usr/bin/mbsync";
const MSMTP = "/usr/bin/msmtp";
const MAILDIR = "/home/me/.mail-all";
const MU_DB = "/home/me/.cache/mu/xapian";
const ACCOUNTS = {
  "ac-mail": "mail@aesthetic.computer",
  "jas-mail": "me@jas.life",
};
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
  if (input !== undefined) {
    // execFile doesn't pipe input to stdin; use spawn instead.
    return new Promise((resolve, reject) => {
      const child = spawnCb(cmd, args, { timeout });
      let stdout = "", stderr = "";
      child.stdout.on("data", (d) => (stdout += d));
      child.stderr.on("data", (d) => (stderr += d));
      child.on("close", (code) => {
        if (code !== 0) return reject(new Error(`${cmd} exited ${code}: ${stderr.trim()}`));
        resolve({ stdout: stdout.trim(), stderr: stderr.trim() });
      });
      child.on("error", reject);
      child.stdin.write(input);
      child.stdin.end();
    });
  }
  const opts = { timeout, maxBuffer: 10 * 1024 * 1024 };
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
    const results = [];
    for (const channel of ["ac-mail", "jas-mail"]) {
      try {
        const { stdout, stderr } = await run(MBSYNC, [channel], {
          timeout: 120_000,
        });
        results.push(`${channel}: ok\n${stdout}\n${stderr}`.trim());
      } catch (err) {
        results.push(`${channel}: ${err.message}`);
      }
    }
    // Re-index after sync
    await run(MU, ["index"], { timeout: 60_000 });
    return {
      content: [
        {
          type: "text",
          text: `Sync complete.\n${results.join("\n")}`.trim(),
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
  "Send an email via msmtp. Defaults to mail@aesthetic.computer; use from_account to switch. For replies, set in_reply_to and references for proper threading. Supports file attachments via the attachments parameter (array of absolute paths).",
  {
    to: z.string().describe("Recipient email address"),
    cc: z.string().optional().describe("CC email address(es), comma-separated"),
    subject: z.string().describe("Email subject"),
    body: z.string().describe("Email body (plain text)"),
    in_reply_to: z
      .string()
      .optional()
      .describe("Message-ID of the email being replied to (for threading)"),
    references: z
      .string()
      .optional()
      .describe("Space-separated Message-IDs for the thread (for threading)"),
    preserve_case: z
      .boolean()
      .optional()
      .default(false)
      .describe("Keep original subject/body casing when true"),
    signature: z
      .string()
      .optional()
      .describe("Optional sign-off override; defaults to style-guide signature"),
    from_account: z
      .enum(["ac-mail", "jas-mail"])
      .optional()
      .default("ac-mail")
      .describe("Which msmtp account to send from (ac-mail or jas-mail)"),
    attachments: z
      .array(z.string())
      .optional()
      .describe("Array of absolute file paths to attach (e.g. [\"/path/to/file.pdf\"])"),
  },
  async ({ to, cc, subject, body, in_reply_to, references, preserve_case, signature, from_account, attachments }) => {
    try {
      const style = await loadEmailStyle();
      const styled = applyEmailStyle({
        subject,
        body,
        preserveCase: preserve_case,
        signature,
      }, style);
      const account = from_account || DEFAULT_ACCOUNT;
      const fromAddr = ACCOUNTS[account] || FROM_ADDRESS;
      const date = new Date().toUTCString();
      const headers = [
        `Date: ${date}`,
        `From: ${fromAddr}`,
        `To: ${to}`,
      ];
      if (cc) headers.push(`Cc: ${cc}`);
      if (in_reply_to) {
        const irt = in_reply_to.startsWith("<") ? in_reply_to : `<${in_reply_to}>`;
        headers.push(`In-Reply-To: ${irt}`);
      }
      if (references) {
        const refs = references
          .split(/\s+/)
          .map((r) => (r.startsWith("<") ? r : `<${r}>`))
          .join(" ");
        headers.push(`References: ${refs}`);
      }
      headers.push(`Subject: ${styled.subject}`, "MIME-Version: 1.0");

      let message;
      if (attachments && attachments.length > 0) {
        // MIME multipart with attachments
        const boundary = `----=_Part_${Date.now()}_${Math.random().toString(36).slice(2)}`;
        headers.push(
          `Content-Type: multipart/mixed; boundary="${boundary}"`,
        );
        const parts = [
          ...headers,
          "",
          `--${boundary}`,
          "Content-Type: text/plain; charset=UTF-8",
          "Content-Transfer-Encoding: 8bit",
          "",
          styled.body,
        ];
        for (const filePath of attachments) {
          await stat(filePath); // throws if missing
          const fileData = await readFile(filePath);
          const name = basename(filePath);
          const ext = name.split(".").pop()?.toLowerCase();
          const mime = ext === "pdf" ? "application/pdf"
            : ext === "png" ? "image/png"
            : ext === "jpg" || ext === "jpeg" ? "image/jpeg"
            : ext === "html" ? "text/html"
            : ext === "xlsx" ? "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet"
            : "application/octet-stream";
          parts.push(
            `--${boundary}`,
            `Content-Type: ${mime}; name="${name}"`,
            "Content-Transfer-Encoding: base64",
            `Content-Disposition: attachment; filename="${name}"`,
            "",
            fileData.toString("base64").replace(/(.{76})/g, "$1\r\n"),
          );
        }
        parts.push(`--${boundary}--`);
        message = parts.join("\r\n");
      } else {
        // Plain text (no attachments)
        headers.push(
          "Content-Type: text/plain; charset=UTF-8",
          "Content-Transfer-Encoding: 8bit",
        );
        message = [...headers, "", styled.body].join("\r\n");
      }

      const { stdout, stderr } = await run(MSMTP, ["-a", account, "-t"], { input: message });
      return {
        content: [
          {
            type: "text",
            text: `Email sent from ${fromAddr} to ${to}${cc ? ` (cc: ${cc})` : ""}.\nStyle guide: ${style.source}${style.loadedFromGuide ? "" : " (defaults used)"}\n${stdout}\n${stderr}`.trim(),
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
