import assert from "node:assert/strict";
import test from "node:test";
import { cleanText, renderFrame, renderGenrePicker, textWidth, wrapText } from "../src/render.mjs";

test("cleans terminal control sequences", () => {
  assert.equal(cleanText("safe\x1b[2J\x00 text"), "safe text");
});

test("wraps content to the available width", () => {
  const lines = wrapText("aesthetic code owns the terminal interface", 12);
  assert.ok(lines.length > 1);
  assert.ok(lines.every((line) => Array.from(line).length <= 12));
});

test("renders the two starting genres as a keyboard choice", () => {
  const labels = ["AC piece (blank)", "nopaint.art brush"];
  const first = renderGenrePicker(labels, 0, 54, 16, false);
  const second = renderGenrePicker(labels, 1, 54, 16, false);
  assert.match(first, /› AC piece \(blank\)/);
  assert.match(second, /› nopaint\.art brush/);
  for (const label of labels) {
    assert.equal(first.split(label).length - 1, 1, `${label} has one visible label`);
  }
  assert.equal(first.split("\n").length, 16);
  for (const row of first.split("\n")) assert.equal(textWidth(row), 54);
});

test("renders one branded interface with privacy state and prompt", () => {
  const frame = renderFrame(
    {
      workspace: "/Users/jas/project",
      mode: "remote",
      status: "ready",
      busy: false,
      input: "make the tests pass",
      entries: [
        { kind: "notice", text: "REMOTE INFERENCE · prompt content may leave this machine" },
        { kind: "user", text: "inspect this repository" },
        { kind: "assistant", text: "I found the failing test." },
      ],
    },
    80,
    20,
    false,
  );
  assert.match(frame, /Aesel/);
  assert.match(frame, /REMOTE · READY/);
  assert.match(frame, /YOU  inspect this repository/);
  assert.match(frame, /AC   I found the failing test/);
  assert.match(frame, /› make the tests pass/);
  assert.equal(frame.split("\n").length, 20);
});

test("shows the signed-in handle and the current piece in the header", () => {
  const frame = renderFrame(
    {
      workspace: "/project",
      mode: "remote",
      status: "ready",
      busy: false,
      input: "",
      entries: [{ id: "p", kind: "publish", text: "https://aesthetic.computer/@tester/smiley" }],
      account: "@tester",
      piece: "smiley",
    },
    70,
    12,
    false,
  );
  assert.match(frame, /Aesel  @tester  smiley/);
  assert.match(frame, /REMOTE · READY/);
  assert.match(frame, /PUB  https:\/\/aesthetic\.computer\/@tester\/smiley/);
  assert.match(renderFrame({ workspace: "/p", mode: "remote", status: "ready", entries: [], input: "" }, 60, 12, false), /not signed in/);
});

test("keeps a QR column on the right and narrows the transcript around it", async () => {
  const { qrBlock } = await import("../src/qr.mjs");
  const qr = qrBlock("aesthetic.computer/prompt~channel%20Ab0-_9Zz~!autorun");
  const state = {
    workspace: "/project",
    mode: "remote",
    status: "ready",
    account: "@tester",
    piece: "movika.mjs",
    input: "",
    entries: [{ id: "a", kind: "user", text: "hello" }],
    qr,
  };
  const wide = renderFrame(state, 100, 26, true);
  const rows = wide.split("\n");
  assert.equal(rows.length, 26);
  for (const row of rows) assert.equal(textWidth(row), 100, "every row fills the frame exactly");
  assert.equal(rows.at(-6).includes("\u2580"), true, "the code sits on the bottom transcript rows");
  assert.equal(rows[2].includes("\u2580"), false, "and not at the top");

  // Too small a window, or no colour, drops the code rather than corrupting the frame.
  const narrow = renderFrame(state, 50, 26, true);
  assert.equal(narrow.includes("\u2580"), false);
  for (const row of narrow.split("\n")) assert.equal(textWidth(row), 50);
  assert.equal(renderFrame(state, 100, 26, false).includes("\u2580"), false, "NO_COLOR keeps the frame plain");

  // No window is too small to draw a whole frame in.
  for (const columns of [32, 40, 50, 64, 80, 140]) {
    for (const row of renderFrame(state, columns, 12, true).split("\n")) {
      assert.equal(textWidth(row), columns, `row overflows at ${columns} columns`);
    }
  }
});

test("no state can make a row wider than the window", async () => {
  const { qrBlock } = await import("../src/qr.mjs");
  const qr = qrBlock("aesthetic.computer/prompt~channel%20Ab0-_9Zz~!autorun");
  const path = "/private/tmp/claude-501/-Users-jas/8e2ce643-970e-478d-bf06-4e3e19671f15/scratchpad/puka.mjs";
  const base = { workspace: path, mode: "remote", status: "ready", account: "@tester", piece: "puka.mjs", input: "", qr, entries: [] };
  // One wrapped row scrolls the whole frame, and the QR code sits on the rows
  // that go first — so every one of these has to come back exactly as wide as
  // the window, escape codes and double-width glyphs and all.
  const states = [
    base,
    { ...base, status: "approval", approval: { subject: path } },
    { ...base, status: "approval", approval: { subject: "x".repeat(400) } },
    { ...base, entries: [{ id: "e", kind: "assistant", text: "Done ✅ it runs 🚀 now — check it ✨" }] },
    { ...base, entries: [{ id: "e", kind: "assistant", text: "这是一个测试 これはテストです 이것은 테스트입니다" }] },
    { ...base, entries: [{ id: "e", kind: "command", text: `${"█".repeat(120)}\n${"—".repeat(200)}` }] },
    { ...base, input: `${"🌈".repeat(60)}?`, cursor: 61 },
    { ...base, audience: { here: 3, peak: 12, arrivals: 14, online: 148 } },
    { ...base, audience: { here: 0, peak: 0, arrivals: 0, online: null } },
    { ...base, audience: { here: 9999, peak: 99999, arrivals: 0, online: 99999 } },
    { ...base, health: { frame: { blank: true, color: [12, 10, 24], colors: 1 } } },
    { ...base, audience: { here: 2, peak: 2, online: 9 }, health: { frame: { blank: true, color: [255, 255, 255], colors: 1 } } },
    { ...base, audience: { here: 2, peak: 2, online: 9 }, health: { frame: { blank: false, colors: 64 } } },
  ];
  for (const state of states) {
    for (const [columns, rows] of [[75, 42], [100, 40], [80, 24], [57, 24], [40, 20], [32, 10]]) {
      const frame = renderFrame(state, columns, rows, true);
      assert.equal(frame.split("\n").length, rows);
      for (const row of frame.split("\n")) assert.equal(textWidth(row), columns);
    }
  }
});

test("measures a row in terminal cells, not characters", () => {
  assert.equal(textWidth("ok"), 2);
  assert.equal(textWidth("🌈"), 2, "an emoji takes two columns");
  assert.equal(textWidth("测"), 2, "so does a CJK glyph");
  assert.equal(textWidth("é"), 1, "a combining mark takes none");
  assert.equal(textWidth("▀─·—…"), 5, "the interface's own glyphs stay narrow");
  assert.deepEqual(wrapText("🌈🌈🌈", 4), ["🌈🌈", "🌈"]);
  assert.equal(wrapText("🌈🌈", 1).filter(Boolean).length, 2, "a glyph wider than the column still advances");
});

test("renders approvals inside the interface", () => {
  const frame = renderFrame(
    {
      workspace: "/project",
      mode: "remote",
      status: "approval",
      busy: true,
      input: "",
      entries: [],
      approval: { subject: "npm test" },
    },
    60,
    14,
    false,
  );
  assert.match(frame, /ALLOW npm test/);
  assert.match(frame, /y once/);
  assert.match(frame, /a session/);
  assert.match(frame, /n deny/);
});

// The count is the only number in the interface that moves because of somebody
// else, so it has to be legible — and it has to be honest, which means saying
// nothing at all rather than zero when the server has not answered.
test("shows who is watching the piece, and says nothing when it cannot know", async () => {
  const { audienceReadout } = await import("../src/render.mjs");
  const base = {
    workspace: "/project",
    mode: "remote",
    status: "ready",
    account: "@tester",
    piece: "puka.mjs",
    input: "",
    entries: [],
  };

  const watched = renderFrame(
    { ...base, audience: { here: 3, peak: 12, arrivals: 14, online: 148 } },
    100,
    24,
    false,
  );
  assert.match(watched, /3 here/, "the live count is on the frame");
  assert.match(watched, /12 peak/, "and so is the high-water mark");
  assert.match(watched, /148 on AC/);

  const quiet = renderFrame({ ...base, audience: { here: 0, peak: 0, online: null } }, 100, 24, false);
  assert.match(quiet, /0 here/, "an answered zero is a real, reportable zero");

  const unknown = renderFrame({ ...base, audience: { here: null, peak: 0 } }, 100, 24, false);
  assert.ok(!/here/.test(unknown), "an unanswered query claims nothing at all");
  const absent = renderFrame(base, 100, 24, false);
  assert.ok(!/here/.test(absent), "and neither does a session with no audience yet");

  // The peak is only news while it is ahead of the present.
  const level = audienceReadout({ here: 4, peak: 4, online: null }, 80, false);
  assert.equal(level.plain, "4 here");

  // A narrow window keeps the fact that matters and drops the trimmings.
  const wide = audienceReadout({ here: 2, peak: 9, online: 100 }, 80, false);
  assert.equal(wide.plain, "2 here · 9 peak · 100 on AC");
  assert.equal(audienceReadout({ here: 2, peak: 9, online: 100 }, 16, false).plain, "2 here · 9 peak");
  assert.equal(audienceReadout({ here: 2, peak: 9, online: 100 }, 8, false).plain, "2 here");
  assert.equal(audienceReadout({ here: 2, peak: 9, online: 100 }, 3, false).plain, "", "and gives up rather than clipping a number in half");
});

// The failure the whole diagnostics pipe exists for: a piece that paints one
// flat colour and throws nothing. It has to reach the frame even when the
// viewer count never arrived, and it has to outrank the trimmings.
test("a blank frame is reported, and outranks the rest of the readout", async () => {
  const { audienceReadout } = await import("../src/render.mjs");
  const base = {
    workspace: "/project", mode: "remote", status: "ready",
    account: "@tester", piece: "kizide.mjs", input: "", entries: [],
  };

  const blank = renderFrame(
    { ...base, health: { frame: { blank: true, color: [12, 10, 24], colors: 1 } } },
    100, 24, false,
  );
  assert.match(blank, /blank 12,10,24/, "it says so even with no viewer count");

  const both = audienceReadout(
    { here: 2, peak: 9, online: 100, frame: { blank: true, color: [0, 0, 0], colors: 1 } },
    80, false,
  );
  assert.equal(both.plain, "blank 0,0,0 \u00b7 2 here \u00b7 9 peak \u00b7 100 on AC");
  // Squeezed, the bad news is what survives.
  assert.equal(
    audienceReadout({ here: 2, peak: 9, online: 100, frame: { blank: true, color: [0, 0, 0] } }, 12, false).plain,
    "blank 0,0,0",
  );

  const healthy = audienceReadout({ here: 2, peak: 2, frame: { blank: false, colors: 48 } }, 80, false);
  assert.equal(healthy.plain, "2 here \u00b7 48 colors", "a working piece reports its spread, not an alarm");

  const nothing = audienceReadout({ here: null, frame: null }, 80, false);
  assert.equal(nothing.plain, "", "and an unanswered session still claims nothing");
});
// The running electricity estimate shares the gauge row, and is the first thing
// that row gives up: an estimate is the least urgent number on it.
test("the energy estimate reaches the gauge row and drops first when squeezed", async () => {
  const { audienceReadout } = await import("../src/render.mjs");
  const { Energy } = await import("../src/energy.mjs");

  const energy = new Energy();
  energy.add("z-ai/glm-4.6", { input_tokens: 6200, output_tokens: 900, cache_read_input_tokens: 24000 });

  const frame = renderFrame(
    {
      workspace: "/project", mode: "remote", status: "ready",
      account: "@tester", piece: "kizide.mjs", input: "", entries: [], energy,
    },
    100, 24, false,
  );
  assert.match(frame, /~[\d.]+ Wh/, "the number wears a tilde, because it is an estimate");

  const full = audienceReadout({ here: 2, peak: 9, energy: 3600 }, 80, false);
  assert.equal(full.plain, "2 here · 9 peak · ~1.00 Wh");
  assert.equal(audienceReadout({ here: 2, peak: 9, energy: 3600 }, 16, false).plain, "2 here · 9 peak");
  assert.equal(audienceReadout({ energy: 0 }, 80, false).plain, "", "an unmetered session claims nothing");
});


test("an inbox line names its sender and cannot pass for a typed one", () => {
  const frame = renderFrame(
    {
      workspace: "/client",
      mode: "remote",
      status: "ready",
      busy: false,
      input: "",
      account: "@tester",
      model: "claude-sonnet-5",
      profile: { name: "pro" },
      entries: [
        { id: "u", kind: "user", text: "look at the diff" },
        { id: "i", kind: "inbox", from: "neo:sip", text: "the build finished" },
      ],
    },
    70,
    12,
    false,
  );
  // In pro a typed line wears the prompt glyph, not a badge; an inbox line
  // still wears its arrow and its sender, so the two never pass for each other.
  assert.match(frame, /^ look at the diff/m, "your line sits flush left with no badge");
  assert.doesNotMatch(frame, /YOU/);
  assert.match(frame, /^ ↓ neo:sip · the build finished/m, "an inbox line keeps its arrow");
  // Codex's shape: no header band, a bar with air on both sides, and one
  // line under it with the handle, the directory and the model.
  assert.doesNotMatch(frame, /REMOTE · READY/, "pro has no header band");
  assert.doesNotMatch(frame, /\/publish/);
  const rows = frame.split("\n");
  assert.equal(rows[rows.length - 4].trim(), "", "air above the bar");
  assert.match(rows[rows.length - 3], /^ {3,}$/, "the bar is empty: the terminal's own cursor stands there");
  assert.equal(rows[rows.length - 2].trim(), "", "air below the bar");
  assert.match(rows[rows.length - 1], /@tester · \/client · claude-sonnet-5/, "the facts sit under the bar");
  assert.doesNotMatch(rows[rows.length - 1], /remote/, "the mode is not a fact worth a word");
});

test("the pro frame takes its shape from the layout", () => {
  const frame = renderFrame(
    {
      workspace: "/client", mode: "remote", status: "ready", busy: false, input: "",
      account: "@tester", model: "gpt-6-astra", providerSettings: { backend: "codex", model: "gpt-6-astra" },
      profile: { name: "pro" },
      layout: { bottom: ["rule", "bar", "status"], status: ["model", "engine"], prompt: ">", separator: " | " },
      entries: [{ id: "u", kind: "user", text: "hi" }],
    },
    60, 10, false,
  );
  const rows = frame.split("\n");
  assert.equal(rows.length, 10);
  assert.match(rows[7], /^─+$/, "the rule is where the layout put it");
  assert.match(rows[8], /^> /, "the prompt glyph is the layout's, on the bar's first cell");
  assert.equal(rows[9].trim(), "gpt-6-astra | codex", "only the facts asked for, with the separator asked for");
});

test("in pro the model on the status line is the one thing to click, and it opens settings", async () => {
  const { headerAction, proStatus } = await import("../src/render.mjs");
  const state = {
    workspace: "/client", mode: "remote", status: "ready", busy: false, input: "",
    account: "@tester", model: "claude-sonnet-5", profile: { name: "pro" }, entries: [],
  };
  const { spans } = proStatus(state, 80, false);
  const model = spans.find((span) => span.name === "model");
  assert.ok(model, "the model is on the line");
  assert.equal(spans[0].name, "handle");
  assert.equal(spans[0].x, 1, "the line starts one cell in");
  // Terminal mouse coordinates are one-based; the status line is the last row.
  assert.equal(headerAction(state, 80, 24, model.x + 1, 24), "model");
  assert.equal(headerAction(state, 80, 24, model.x + model.width, 24), "model");
  assert.equal(headerAction(state, 80, 24, model.x + model.width + 2, 24), "", "past the model is nothing");
  assert.equal(headerAction(state, 80, 24, model.x + 1, 23), "", "the row above is the air below the bar");
  assert.equal(headerAction(state, 80, 24, 3, 22), "", "no header band to click in pro");
  const moved = { ...state, layout: { bottom: ["status", "gap", "bar"] } };
  const movedModel = proStatus(moved, 80, false, moved.layout).spans.find((span) => span.name === "model");
  assert.equal(headerAction(moved, 80, 24, movedModel.x + 1, 22), "model", "the click follows the layout");
});

test("the bottom line names the provider, abbreviates the path the way fish does, and says connecting", async () => {
  const { fishPath, providerLabel, proStatus, windowTitle } = await import("../src/render.mjs");
  assert.equal(fishPath("/Users/jas/aesthetic-computer/easel", "/Users/jas"), "~/a/easel");
  assert.equal(fishPath("/Users/jas/.config/easel", "/Users/jas"), "~/.c/easel");
  assert.equal(fishPath("/Users/jas", "/Users/jas"), "~");
  assert.equal(fishPath("/opt/homebrew/bin", "/Users/jas"), "/o/h/bin");
  assert.equal(providerLabel("ac"), "aesthetic");
  const state = {
    workspace: "/Users/jas/aesthetic-computer/easel", mode: "remote", status: "connecting", busy: false, input: "",
    account: "@tester", model: "claude-sonnet-5", providerSettings: { backend: "ac", model: "claude-sonnet-5" },
    profile: { name: "pro" }, entries: [],
  };
  const { line, spans } = proStatus(state, 100, false);
  assert.equal(line.trim(), "@tester · ~/a/easel · claude-sonnet-5 · connecting…");
  assert.deepEqual(spans.map((span) => span.name), ["handle", "workspace", "model", "activity"]);
  assert.equal(windowTitle(state), "🫏 aesel · ~/a/easel · aesthetic · ◌ connecting");
  assert.equal(windowTitle({ ...state, status: "ready", busy: true }), "🫏 aesel · ~/a/easel · aesthetic · ● working");
  assert.equal(windowTitle({ ...state, status: "ready" }), "🫏 aesel · ~/a/easel · aesthetic");
  assert.equal(proStatus({ ...state, modelLabel: "Claude Opus 5.5", status: "ready" }, 100, false).line.trim(), "@tester · ~/a/easel · Claude Opus 5.5", "the proper name when the list knows it");
  assert.equal(proStatus({ ...state, status: "ready" }, 34, false).line.trim(), "@tester · claude-sonnet-5", "short of room, the place goes before the model");
});

test("a drop-down stands on the fact that opened it, and a click on one of its rows picks", async () => {
  const { dropdownGeometry, headerAction, proStatus } = await import("../src/render.mjs");
  const base = {
    workspace: "/client", mode: "remote", status: "ready", busy: false, input: "",
    account: "@tester", model: "claude-sonnet-5", providerSettings: { backend: "claude", model: "claude-sonnet-5" },
    profile: { name: "pro" }, entries: [{ id: "u", kind: "user", text: "hi" }],
  };
  assert.equal(proStatus(base, 80, false).spans.find((s) => s.name === "engine"), undefined, "the provider is folded into the model");
  const items = [
    { header: true, label: "claude", detail: "your claude account" },
    { id: "claude-opus-5-5", label: "Claude Opus 5.5", detail: "claude-opus-5-5", provider: "claude" },
    { id: "claude-sonnet-5", label: "Claude Sonnet 5", detail: "claude-sonnet-5", provider: "claude" },
  ];
  const state = { ...base, dropdown: { kind: "model", items, index: 2, loading: false } };
  const g = dropdownGeometry(state, 80, 24);
  assert.equal(g.count, 3);
  assert.equal(g.top, 24 - 1 - 3 - 1, "title row, then the rows, all above the status line");
  const modelSpan = proStatus(state, 80, false).spans.find((s) => s.name === "model");
  assert.equal(g.x, modelSpan.x, "it stands on the model");
  const frame = renderFrame(state, 80, 24, false).split("\n");
  assert.match(frame[g.top], /▾ provider · model/);
  assert.match(frame[g.top + 1], /^ *claude {2,}/, "a provider heads its group");
  assert.match(frame[g.top + 2], /  Claude Opus 5.5 {2,}claude-opus-5-5/);
  assert.match(frame[g.top + 3], /› Claude Sonnet 5 {2,}claude-sonnet-5/, "the current model is marked");
  assert.equal(headerAction(state, 80, 24, g.x + 2, g.top + 3), "pick:1", "clicking a model row picks it");
  assert.equal(headerAction(state, 80, 24, g.x + 2, g.top + 4), "pick:2");
  assert.equal(headerAction(state, 80, 24, 2, 3), "dismiss", "anywhere else closes it");
  const loading = renderFrame({ ...base, dropdown: { kind: "model", items: [], index: 0, loading: true } }, 80, 24, false);
  assert.match(loading, /loading…/);
});

test("a reply's markdown is read, not shown, and pro's page is flush left", async () => {
  const { markdown } = await import("../src/render.mjs");
  const read = markdown("## Plan\n- **Airtable** needs `auth` first\n- see [docs](https://example.com/x)");
  assert.equal(read.text, "Plan\n• Airtable needs auth first\n• see docs");
  assert.deepEqual(read.spans.map((s) => [s.tone, read.text.slice(s.start, s.end)]), [["highlight bold", "Plan"], ["bold", "Airtable"], ["soft", "auth"], ["soft", "docs"]]);
  const frame = renderFrame(
    {
      workspace: "/client", mode: "remote", status: "ready", busy: false, input: "", account: "@tester",
      profile: { name: "pro" },
      entries: [
        { id: "u", kind: "user", text: "hi" },
        { id: "a", kind: "assistant", text: "Hi.\n\n- **Airtable** needs authorization before `/mcp` works." },
        { id: "n", kind: "notice", text: "a notice" },
      ],
    },
    70, 14, false,
  );
  assert.match(frame, /^ hi\s*$/m, "the typed line, alone");
  assert.match(frame, /^ Hi\.\s*$/m, "the reply flush left");
  assert.match(frame, /^ • Airtable needs authorization before \/mcp works\.\s*$/m, "markers gone, bullet kept");
  assert.match(frame, /^ · a notice\s*$/m, "a notice keeps a two-cell mark");
  assert.doesNotMatch(frame, /\*\*|AC {3}/);
});

test("while the machine works the handle breathes on the dance clock", async () => {
  const { proStatus } = await import("../src/render.mjs");
  const base = { workspace: "/c", mode: "remote", status: "ready", input: "", account: "@tester", model: "m", profile: { name: "pro" }, entries: [], busy: true, requestStartedAt: Date.now() };
  const bright = proStatus({ ...base, mascotMs: 100 }, 80, true).line;
  const dim = proStatus({ ...base, mascotMs: 700 }, 80, true).line;
  assert.notEqual(bright, dim, "the handle is painted differently across the beat");
  assert.equal(proStatus({ ...base, busy: false, mascotMs: 700 }, 80, true).line, proStatus({ ...base, busy: false, mascotMs: 100 }, 80, true).line, "and holds still when idle");
});

test("in pro a question takes the page with three answers, and a running tool rides the status line", async () => {
  const { approvalModal, proStatus } = await import("../src/render.mjs");
  const base = { workspace: "/c", mode: "remote", status: "approval", input: "", account: "@t", model: "m", profile: { name: "pro" }, entries: [{ id: "a", kind: "assistant", text: "I will list the files." }] };
  const frame = renderFrame({ ...base, approval: { id: 1, subject: "run: ls -la" }, approvalIndex: 1 }, 80, 20, false);
  assert.match(frame, /allow\?/);
  assert.match(frame, /run: ls -la/);
  assert.match(frame, /Allow once {2,}y/);
  assert.match(frame, /Allow every time this session {2,}a/);
  assert.match(frame, /Deny {2,}n/);
  assert.doesNotMatch(frame, /I will list the files/, "the conversation waits behind the question");
  const rows = approvalModal({ approval: { subject: "x" }, approvalIndex: 0 }, 80, 12, false);
  assert.equal(rows.length, 12, "the modal fills exactly the transcript rows");
  const busy = { ...base, status: "working", busy: true, requestStartedAt: Date.now(), toolNow: "/bin/zsh -lc \"git status --short\"" };
  assert.match(proStatus(busy, 100, false).line, /s… · \/bin\/zsh -lc "git status --short"/, "the tool shows beside the timer");
  assert.doesNotMatch(renderFrame(busy, 100, 20, false), /RUN/, "and not as a line of the conversation");
});

test("while the machine works, the line it is working on runs a colour wave, frame by frame", () => {
  const base = { workspace: "/c", mode: "remote", status: "working", input: "", account: "@t", model: "m", profile: { name: "pro" }, busy: true, requestStartedAt: Date.now(),
    entries: [{ id: "u0", kind: "user", text: "earlier line" }, { id: "a0", kind: "assistant", text: "ok" }, { id: "u1", kind: "user", text: "make it sing" }] };
  const a = renderFrame({ ...base, mascotMs: 0 }, 60, 12, true).split("\n");
  const b = renderFrame({ ...base, mascotMs: 240 }, 60, 12, true).split("\n");
  const row = (rows) => rows.find((r) => r.includes("make it sing".slice(-4)) && r.includes("\x1b[38;"));
  const wave = (rows) => rows.find((r) => /make it sing/.test(r.replace(/\x1b\[[0-9;]*m/g, "")) && new Set(r.match(/\x1b\[38;[0-9;]*m/g) || []).size >= 5);
  assert.ok(wave(a), "each letter of the working line carries its own hue");
  assert.notEqual(wave(a), wave(b), "and the hues move between frames");
  const distinctInks = (r) => new Set(r.match(/\x1b\[38;[0-9;]*m/g) || []).size;
  const earlier = a.find((r) => r.replace(/\x1b\[[0-9;]*m/g, "").includes("earlier line"));
  assert.ok(distinctInks(earlier) <= 3, "an earlier line keeps one ink");
  assert.ok(distinctInks(wave(a)) >= 5, "the working line wears many");
  const idle = renderFrame({ ...base, busy: false, mascotMs: 240 }, 60, 12, true).split("\n");
  assert.equal(wave(idle), undefined, "and so does the line once the answer has landed");
});

test("running tools are provisional rows at the foot of the page, and leave when they finish", () => {
  const base = { workspace: "/c", mode: "remote", status: "tool", input: "", account: "@t", model: "m", profile: { name: "pro" }, busy: true, requestStartedAt: Date.now(),
    entries: [{ id: "u", kind: "user", text: "check the tree" }] };
  const running = renderFrame({ ...base, toolsNow: new Map([["t1", "git status --short"], ["t2", "Task · look for the failing test"]]) }, 80, 16, false);
  assert.match(running, /⋯ git status --short/);
  assert.match(running, /⋯ Task · look for the failing test/);
  const done = renderFrame({ ...base, busy: false, toolsNow: new Map() }, 80, 16, false);
  assert.doesNotMatch(done, /⋯/, "nothing of them stays");
});
