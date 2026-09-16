#!/usr/bin/env node
import { emitKeypressEvents } from "node:readline";
import { spawn } from "node:child_process";
import { fileURLToPath } from "node:url";
import { createClient, MailError, safe, clip, wrap, graphemes, validateDraft, replyTo } from "./client.mjs";

const args = process.argv.slice(2);
if (args.includes("--help") || args.includes("-h")) {
  console.log(`ac-mail — Letters, in your terminal

ac-mail                Open the shared AC mailbox
ac-mail login          Run ac-login's browser sign-in
ac-mail status         Run ac-login status
ac-mail --demo         Explore with synthetic letters; no login or network

Inbox: ↑/↓ or j/k · Enter open · Tab inbox/sent · c compose · r refresh · q quit
Letter: ↑/↓ scroll · c reply · Esc back
Compose: Tab field · Enter new body line · Ctrl+S review · Esc return
Review: y send · Esc edit

Uses ~/.ac-token from ac-login. Drafts remain in memory, never temporary files.
The server currently returns the newest 50 letters in each mailbox.`);
  process.exit(0);
}
if (["login", "status"].includes(args[0])) {
  const child = spawn(process.execPath, [fileURLToPath(new URL("../../tezos/ac-login.mjs", import.meta.url)), ...(args[0] === "status" ? ["status"] : [])], { stdio: "inherit" });
  child.on("error", () => { console.error("Could not start ac-login."); process.exitCode = 1; });
  child.on("exit", (code) => { process.exitCode = code ?? 1; });
} else if (args.some((arg) => arg !== "--demo")) {
  console.error("Unknown option. Run ac-mail --help."); process.exitCode = 1;
} else if (!process.stdin.isTTY || !process.stdout.isTTY) {
  console.error("ac-mail needs an interactive terminal. Run ac-mail --help for usage."); process.exitCode = 1;
} else {
  await run(args.includes("--demo"));
}

async function run(demo) {
  const input = process.stdin, output = process.stdout;
  const blank = () => ({ to: "", subject: "", body: "" });
  let mailbox = { inbox: [], sent: [], unread: 0, addresses: [] };
  let tab = "inbox", mode = "list", selected = 0, scroll = 0, field = 0;
  let draft = blank(), notice = "", busy = false, closed = false, letter;
  const demoData = {
    addresses: ["demo@aesthetic.computer"], unread: 1,
    inbox: [
      { id: "demo1", from: "@friend", subject: "Hello", text: "A letter in your terminal.\n\nThis is a synthetic preview. Nothing here sends real mail.", when: new Date().toISOString(), read: false },
      { id: "demo2", from: "@neighbor", subject: "An afternoon drawing", text: "I made a drawing today.\nWhat have you been making?", when: new Date().toISOString(), read: true },
    ], sent: [],
  };
  const client = demo ? {
    inbox: async () => structuredClone(demoData),
    read: async (id) => { const m = demoData.inbox.find((m) => m.id === id); if (m && !m.read) { m.read = true; demoData.unread--; } return { read: 1 }; },
    send: async (d) => { demoData.sent.unshift({ id: `sent${demoData.sent.length}`, to: d.to, subject: d.subject, text: d.body, when: new Date().toISOString() }); return { status: "mailed" }; },
  } : createClient();

  function finish() {
    if (closed) return;
    closed = true;
    input.off("keypress", onKey); output.off("resize", paint);
    process.off("SIGINT", finish); process.off("SIGTERM", finish);
    input.setRawMode(false); input.pause();
    output.write("\x1b[0m\x1b[?25h\x1b[?1049l");
    draft = blank(); mailbox = {}; letter = null;
  }
  async function task(fn) {
    busy = true; paint();
    try { await fn(); } catch (error) {
      notice = error instanceof MailError ? error.message : "Could not complete mail request.";
    } finally { busy = false; paint(); }
  }
  async function refresh() {
    const next = await client.inbox();
    if (!Array.isArray(next.inbox) || !Array.isArray(next.sent)) throw new MailError("Invalid mailbox response.");
    if (closed) return;
    mailbox = next;
    selected = Math.max(0, Math.min(selected, mailbox[tab].length - 1));
  }
  function paint() {
    if (closed) return;
    const cols = Math.max(1, (output.columns || 80) - 1), rows = Math.max(1, output.rows || 24);
    const contentRows = Math.max(1, rows - 5);
    const lines = [];
    const add = (text = "", style = "") => lines.push(style + clip(text, cols) + "\x1b[0m");
    if (cols < 28 || rows < 9) {
      output.write("\x1b[H\x1b[2J" + clip("Resize terminal (28 × 9 minimum)", cols)); return;
    }
    const address = mailbox.addresses?.find((a) => !/^ac\d\d[a-z]{5}@/.test(a)) || mailbox.addresses?.[0];
    add(`Letters${demo ? " · DEMO" : ""}  ${address || "ac-login"}`, "\x1b[1;36m");
    add(`${tab === "inbox" ? "[Inbox]" : " Inbox "} ${mailbox.unread || 0} unread   ${tab === "sent" ? "[Sent]" : " Sent "}${mode === "compose" || mode === "review" ? "   Write" : ""}`);
    add("─".repeat(cols), "\x1b[90m");
    let help;
    if (mode === "list") {
      const entries = mailbox[tab] || [];
      const offset = Math.max(0, selected - contentRows + 1);
      if (!entries.length) add(busy ? "Loading letters…" : "No letters here yet.");
      entries.slice(offset, offset + contentRows).forEach((m, i) => {
        const day = new Date(m.when).toLocaleDateString(undefined, { month: "short", day: "numeric" });
        const who = tab === "inbox" ? m.from : m.to;
        add(`${m.read === false && tab === "inbox" ? "●" : " "} ${clip(who, 18).padEnd(18)} ${day}  ${m.subject || "(no subject)"}`, i + offset === selected ? "\x1b[7m" : "");
      });
      help = "↑↓ move  Enter open  Tab inbox/sent  c write  r refresh  q quit";
    } else if (mode === "letter") {
      const text = [
        `${tab === "inbox" ? "From" : "To"}: ${tab === "inbox" ? letter.from : letter.to}`,
        new Date(letter.when).toLocaleString(), letter.subject || "(no subject)", "",
        letter.text,
      ].flatMap((part) => wrap(part, cols));
      scroll = Math.max(0, Math.min(scroll, Math.max(0, text.length - contentRows)));
      text.slice(scroll, scroll + contentRows).forEach((line) => add(line));
      help = "↑↓ scroll  c reply  Esc back  q quit";
    } else if (mode === "compose") {
      const labels = ["To", "Subject", "Body"], keys = ["to", "subject", "body"];
      labels.forEach((name, i) => {
        if (i < 2) add(`${i === field ? "›" : " "} ${name}: ${draft[keys[i]]}${i === field ? "▏" : ""}`);
      });
      add(`${field === 2 ? "›" : " "} Body (${draft.body.length}/500)`);
      wrap(draft.body + (field === 2 ? "▏" : ""), cols).slice(-Math.max(1, contentRows - 3)).forEach((line) => add(line));
      help = "Tab field  Enter newline  Ctrl+S review  Esc keep draft";
    } else if (mode === "review") {
      const external = draft.to.includes("@") && !draft.to.startsWith("@") && !/@(?:mail\.)?aesthetic\.computer$/i.test(draft.to);
      const text = [`Send to ${draft.to}?`, external ? "Outside email — providers can read this letter." : "AC letter — currently readable by administrators.", draft.subject || "(no subject)", "", draft.body].flatMap((part) => wrap(part, cols));
      scroll = Math.max(0, Math.min(scroll, Math.max(0, text.length - contentRows)));
      text.slice(scroll, scroll + contentRows).forEach((line) => add(line));
      help = "y send  Esc edit  ↑↓ scroll";
    } else {
      add("Discard your draft and quit?"); help = "y discard and quit  Esc keep writing";
    }
    while (lines.length < rows - 2) add();
    add(busy ? "Working…" : notice, "\x1b[33m");
    add(help, "\x1b[90m");
    output.write("\x1b[H\x1b[2J" + lines.slice(0, rows).join("\r\n"));
  }
  function compose(to = "", subject = "") {
    // A pending draft is never overwritten by replying to another letter.
    if (!draft.to && !draft.subject && !draft.body) draft = { to, subject, body: "" };
    field = draft.to ? 2 : 0; mode = "compose"; notice = "";
  }
  function onKey(str, key = {}) {
    if (closed) return;
    if (key.ctrl && key.name === "c") { finish(); return; }
    if (busy) return;
    if (mode === "quit") {
      if (str === "y") finish();
      else if (key.name === "escape") mode = "compose";
      paint(); return;
    }
    if (mode === "compose") {
      if (key.name === "escape") { mode = "list"; notice = "Draft kept. Press c to resume."; }
      else if (key.ctrl && key.name === "s") {
        notice = validateDraft(draft) || "";
        if (!notice) { mode = "review"; scroll = 0; }
      } else if (key.name === "tab") field = (field + (key.shift ? 2 : 1)) % 3;
      else {
        const name = ["to", "subject", "body"][field];
        if (key.name === "backspace") draft[name] = graphemes(draft[name]).slice(0, -1).join("");
        else if (key.name === "return") { if (field === 2 && draft.body.length < 500) draft.body += "\n"; else if (field < 2) field++; }
        else if (!key.ctrl && !key.meta && str && !str.includes("\x1b")) {
          const value = safe(str).replace(/\n/g, field === 2 ? "\n" : " ");
          const limit = field === 2 ? 500 : field === 1 ? 80 : 254;
          if (draft[name].length + value.length <= limit) draft[name] += value;
          else notice = `Maximum ${limit} characters.`;
        }
      }
      paint(); return;
    }
    if (mode === "review") {
      if (key.name === "escape") mode = "compose";
      else if (key.name === "up") scroll--;
      else if (key.name === "down") scroll++;
      else if (str === "y") void task(async () => {
        const result = await client.send(draft);
        if (result.status !== "mailed") throw new MailError("Delivery unconfirmed. Refresh Sent before resending.");
        draft = blank(); tab = "sent"; mode = "list"; selected = 0;
        notice = demo ? "Demo letter sent. No real delivery." : "Letter sent.";
        await refresh();
      });
      paint(); return;
    }
    if (str === "q") { if (draft.to || draft.subject || draft.body) mode = "quit"; else finish(); }
    else if (str === "c") compose(mode === "letter" && tab === "inbox" ? replyTo(letter) : "", mode === "letter" && tab === "inbox" ? clip(`re: ${letter.subject || ""}`, 80) : "");
    else if (key.name === "escape") { mode = "list"; scroll = 0; }
    else if (mode === "letter") {
      if (key.name === "up" || str === "k") scroll--;
      if (key.name === "down" || str === "j") scroll++;
    } else if (key.name === "tab") { tab = tab === "inbox" ? "sent" : "inbox"; selected = 0; }
    else if (key.name === "up" || str === "k") selected = Math.max(0, selected - 1);
    else if (key.name === "down" || str === "j") selected = Math.max(0, Math.min(mailbox[tab].length - 1, selected + 1));
    else if (str === "r") void task(async () => { await refresh(); notice = "Refreshed."; });
    else if (key.name === "return" && mailbox[tab][selected]) {
      letter = mailbox[tab][selected]; mode = "letter"; scroll = 0;
      if (tab === "inbox" && !letter.read) void task(async () => {
        await client.read(letter.id);
        letter.read = true; mailbox.unread = Math.max(0, mailbox.unread - 1);
      });
    }
    paint();
  }
  output.write("\x1b[?1049h\x1b[?25l");
  input.setRawMode(true); emitKeypressEvents(input); input.on("keypress", onKey); input.resume();
  output.on("resize", paint); process.on("SIGINT", finish); process.on("SIGTERM", finish);
  process.once("exit", finish);
  await task(refresh);
}
