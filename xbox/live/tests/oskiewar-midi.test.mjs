import assert from "node:assert/strict";
import { readFile } from "node:fs/promises";
import test from "node:test";
import { gunzipSync } from "node:zlib";
import createOskiewarMidi, {
  OSKIEWAR_MIDI_BASE_NOTE, OSKIEWAR_MIDI_NOTES,
  OSKIEWAR_THEME_BASE_NOTE, OSKIEWAR_THEME_NOTES, OSKIEWAR_THEMES,
  themePadName,
} from "../oskiewar-midi.mjs";
import { OSKIEWAR_SIGNAL_EVENTS } from "../oskiewar-sfx.mjs";

const webShell = await readFile(new URL("../mac-test.html", import.meta.url), "utf8");
const caddyfile = await readFile(
  new URL("../../../lith/Caddyfile", import.meta.url), "utf8");

// One fake output port, rewired per bridge so each test reads only its own
// traffic. Node ships a read-only `navigator`, hence defineProperty.
let sink = [];
Object.defineProperty(globalThis, "navigator", {
  configurable: true,
  value: {
    requestMIDIAccess: async () => ({
      outputs: new Map([["iac", {
        name: "IAC Driver Bus 1",
        send: (bytes) => sink.push([...bytes]),
      }]]),
      set onstatechange(_) {},
    }),
  },
});

// The greeting is off unless a test asks for it: its timers outlive the test
// that started them, and would otherwise rain notes into the next one's wire.
async function bridge(options = {}) {
  const wire = [];
  sink = wire;
  const midi = createOskiewarMidi({ noteMs: 5, handshake: false, ...options });
  assert.equal(await midi.enable(), true, midi.lastError);
  return { midi, wire };
}

const NOTE_ON = 0x90, NOTE_OFF = 0x80, CONTROL = 0xB0;

test("every signal event gets a pad, packed from note 36", () => {
  assert.equal(Object.keys(OSKIEWAR_MIDI_NOTES).length, OSKIEWAR_SIGNAL_EVENTS.length);
  OSKIEWAR_SIGNAL_EVENTS.forEach((event, index) => {
    assert.equal(OSKIEWAR_MIDI_NOTES[event], OSKIEWAR_MIDI_BASE_NOTE + index);
  });
  const notes = Object.values(OSKIEWAR_MIDI_NOTES);
  assert.ok(Math.max(...notes) <= 127, "signal pads must fit under note 127");
});

test("theme pads sit above the signals with room for the list to grow", () => {
  const lastSignal = OSKIEWAR_MIDI_BASE_NOTE + OSKIEWAR_SIGNAL_EVENTS.length - 1;
  assert.ok(OSKIEWAR_THEME_BASE_NOTE > lastSignal,
    "a new signal event would land on a theme pad");
  OSKIEWAR_THEMES.forEach((theme, index) => {
    assert.equal(OSKIEWAR_THEME_NOTES[theme], OSKIEWAR_THEME_BASE_NOTE + index);
  });
  assert.equal(themePadName("title"), "title theme");
});

test("a hit carries its intensity as velocity", async () => {
  const { midi, wire } = await bridge();
  midi.signal("punch", 0, 0.5);
  assert.deepEqual(wire.at(-1), [NOTE_ON, OSKIEWAR_MIDI_NOTES.punch, 64]);
});

test("a signal with no intensity plays in rather than landing on velocity 1", async () => {
  const { midi, wire } = await bridge();
  midi.signal("bodyhit", 1);
  assert.deepEqual(wire.at(-1), [NOTE_ON, OSKIEWAR_MIDI_NOTES.bodyhit, 100]);
});

test("wind is sent as a controller as well as a pad", async () => {
  const { midi, wire } = await bridge();
  midi.signal("wind", -1, 0.8);
  assert.deepEqual(wire[0], [CONTROL, 1, 102], "CC1 carries the strength");
  assert.ok(wire.some(([status, note]) =>
    status === NOTE_ON && note === OSKIEWAR_MIDI_NOTES.wind));
});

test("an unknown event sends nothing at all", async () => {
  const { midi, wire } = await bridge();
  assert.equal(midi.signal("notasignal"), false);
  assert.equal(wire.length, 0);
});

test("splitting players puts each fighter on its own channel", async () => {
  const { midi, wire } = await bridge({ splitPlayers: true });
  midi.signal("kick", 2, 1);
  assert.equal(wire.at(-1)[0], NOTE_ON | 2, "player 2 lands on channel 3");
});

test("hits release themselves so nothing is left held", async () => {
  const { midi, wire } = await bridge();
  midi.signal("blast", 0, 1);
  await new Promise((resolve) => setTimeout(resolve, 30));
  assert.ok(wire.some(([status, note]) =>
    status === NOTE_OFF && note === OSKIEWAR_MIDI_NOTES.blast));
});

test("the round's own events move the theme", async () => {
  const { midi, wire } = await bridge();
  midi.signal("hello");
  assert.equal(midi.currentTheme, "title");
  assert.ok(wire.some(([status, note]) =>
    status === NOTE_ON && note === OSKIEWAR_THEME_NOTES.title));

  wire.length = 0;
  midi.signal("countdown");
  assert.equal(midi.currentTheme, "round");
  const off = wire.findIndex(([status, note]) =>
    status === NOTE_OFF && note === OSKIEWAR_THEME_NOTES.title);
  const on = wire.findIndex(([status, note]) =>
    status === NOTE_ON && note === OSKIEWAR_THEME_NOTES.round);
  assert.ok(off >= 0 && on >= 0 && off < on, "the old bed stops before the new one starts");

  midi.signal("matchwin");
  assert.equal(midi.currentTheme, "result");
});

test("a theme is held, not restruck, while its section lasts", async () => {
  const { midi, wire } = await bridge();
  midi.signal("hello");
  wire.length = 0;
  midi.signal("select");
  midi.signal("fighters");
  assert.equal(midi.currentTheme, "title");
  assert.ok(!wire.some(([status, note]) => note === OSKIEWAR_THEME_NOTES.title
    && (status === NOTE_ON || status === NOTE_OFF)), "theme note was disturbed");
});

test("themes can be driven directly and switched off", async () => {
  const { midi, wire } = await bridge({ themes: false });
  midi.signal("hello");
  assert.equal(midi.currentTheme, null, "themes: false stops the follower");
  wire.length = 0;
  midi.theme("round", 1);
  assert.deepEqual(wire.at(-1), [NOTE_ON, OSKIEWAR_THEME_NOTES.round, 127]);
  midi.theme(null);
  assert.deepEqual(wire.at(-1), [NOTE_OFF, OSKIEWAR_THEME_NOTES.round, 0]);
});

test("panic releases the held theme", async () => {
  const { midi, wire } = await bridge();
  midi.signal("countdown");
  wire.length = 0;
  midi.panic();
  assert.equal(midi.currentTheme, null);
  assert.ok(wire.some(([status, note]) =>
    status === NOTE_OFF && note === OSKIEWAR_THEME_NOTES.round));
  assert.ok(wire.some(([status, control]) => status === CONTROL && control === 123));
});

test("the signal bus drives it, and unsubscribes cleanly", async () => {
  const { midi, wire } = await bridge();
  const bus = new EventTarget();
  const off = midi.connectSignals(bus);
  bus.dispatchEvent(new CustomEvent("oskiewar:signal",
    { detail: { event: "jump", player: 0, value: 1 } }));
  assert.deepEqual(wire[0], [NOTE_ON, OSKIEWAR_MIDI_NOTES.jump, 127]);
  off();
  wire.length = 0;
  bus.dispatchEvent(new CustomEvent("oskiewar:signal", { detail: { event: "jump" } }));
  assert.equal(wire.length, 0);
});

test("the chart names every pad in note order", async () => {
  const { midi } = await bridge();
  const chart = midi.chart();
  assert.equal(chart.length, OSKIEWAR_SIGNAL_EVENTS.length + OSKIEWAR_THEMES.length);
  assert.deepEqual(chart.map((pad) => pad.note),
    [...chart.map((pad) => pad.note)].sort((a, b) => a - b));
  assert.equal(chart.at(-1).pad, "result theme");
});

test("the port opens with a greeting so the DAW answers first", async () => {
  const { midi, wire } = await bridge({ handshake: true });
  await new Promise((resolve) => setTimeout(resolve, 500));
  const struck = wire.filter(([status]) => status === NOTE_ON).map(([, note]) => note);
  assert.deepEqual(struck, ["hello", "countdown", "roundwin", "matchwin"]
    .map((event) => OSKIEWAR_MIDI_NOTES[event]));
  assert.equal(midi.currentTheme, null, "the greeting must not move the theme");
});

test("the greeting can be declined", async () => {
  const { wire } = await bridge({ handshake: false });
  await new Promise((resolve) => setTimeout(resolve, 200));
  assert.equal(wire.length, 0);
});

test("panic cancels a greeting still in flight", async () => {
  const { midi, wire } = await bridge({ handshake: true });
  midi.panic();
  wire.length = 0;
  await new Promise((resolve) => setTimeout(resolve, 500));
  assert.ok(!wire.some(([status]) => status === NOTE_ON), "a note escaped after panic");
});

test("the subdomain turns midi on without a query string", () => {
  assert.match(webShell, /location\.hostname\.startsWith\("midi\."\)/);
  assert.match(caddyfile, /oskiewar\.com, midi\.oskiewar\.com \{/);
});

test("midi mode shuts the account door and lights the lamp", () => {
  assert.match(webShell, /body\.midi-out #logout \{ display: none; \}/,
    "logged-in play must be out of the way while scoring");
  assert.match(webShell, /midi: document\.body\.classList\.contains\("midi-out"\)/,
    "capabilities should carry the flag the game can gate on");
  assert.match(webShell, /classList\.add\("midi-out"\)/);
});

test("midi mode never outlives the address that asked for it", () => {
  assert.doesNotMatch(webShell, /localStorage\.getItem\("oskiewar-midi"\)/,
    "a remembered flag would mute the plain site's bank with nothing saying why");
  assert.doesNotMatch(webShell, /localStorage\.setItem\("oskiewar-midi"/);
  assert.match(webShell, /localStorage\.removeItem\("oskiewar-midi"\)/,
    "the flag anyone already stored has to be cleared, not merely ignored");
});

test("the lamp is drawn by the game, not floated over it", async () => {
  assert.doesNotMatch(webShell, /id="midi-out"/,
    "an overlay can never match icons that live in game units");
  assert.match(webShell, /midiPulse: globalThis\.__oskiewarMidiPulse \|\| 0/,
    "capabilities carries the pulse rather than adding a sandbox function");
  assert.match(webShell, /onSignal: \(\) => \{ globalThis\.__oskiewarMidiPulse = Date\.now\(\); \}/);

  const game = await readFile(new URL("../oskiewar.js", import.meta.url), "utf8");
  assert.match(game, /function hudStatusTray\(/, "the status lane must exist");
  assert.match(game, /function drawStatusPiano\(/);
  assert.match(game, /const statusCell = \d+/, "icons share one cell size");
  // The lane is right-aligned off the clock, which itself sits left of the QR.
  assert.match(game, /const right = clock\.left - \d+/);
  assert.match(game, /function hudClockBox\(/,
    "clock geometry is shared so the lane cannot drift from it");
  assert.match(game, /if \(debugHitboxes\) \{\s*\n\s*const pad/,
    "debug should draw the zone the icons live in");
  assert.doesNotMatch(game, /drawDebugBug\(safe\)/,
    "the bug belongs in the lane now, not the bottom of the screen");
});

test("the web shell keeps midi opt-in so it cannot silence the bank by surprise", () => {
  assert.match(webShell, /import createOskiewarMidi from "\/oskiewar-midi\.mjs"/);
  assert.match(webShell, /oskiewarMidi\.connectSignals\(globalThis\)/);
  assert.match(webShell, /oskiewarSfx\.mute\(true\)/, "the bank is muted unless layered");
  assert.match(webShell, /pagehide[\s\S]{0,60}panic\(\)/, "held notes are released on hide");
});

test("lith serves the module instead of proxying it away", () => {
  const host = caddyfile.slice(caddyfile.indexOf("oskiewar.com {"));
  const modules = host.indexOf("/oskiewar-midi.mjs");
  const proxy = host.indexOf("handle /oskiewar* {");
  assert.ok(modules > 0, "the bridge module has no file_server route");
  assert.ok(modules < proxy, "the proxy matcher would claim the module first");
});

// The rack is only a real check when it is installed; skip quietly elsewhere.
test("the installed rack agrees with the bridge, pad for pad", async (t) => {
  const rack = new URL("file:///Users/jas/Music/Ableton/User Library/" +
    "Presets/Instruments/Drum Rack/oskiewar pads.adg");
  let xml;
  try { xml = gunzipSync(await readFile(rack)).toString("utf8"); }
  catch { return t.skip("oskiewar pads.adg is not installed here"); }

  const pads = {};
  for (const segment of xml.split("<DrumBranchPreset").slice(1)) {
    const name = /<Name Value="([^"]+)"/.exec(segment)?.[1];
    const note = /<ReceivingNote Value="(\d+)"/.exec(segment)?.[1];
    if (name && note) pads[name] = Number(note);
  }
  const expected = Object.fromEntries(
    createOskiewarMidi().chart().map((pad) => [pad.pad, pad.note]));
  assert.deepEqual(pads, expected, "rack pads and bridge note map disagree");
});
