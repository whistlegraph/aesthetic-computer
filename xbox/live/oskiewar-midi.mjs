// oskiewar Web MIDI bridge.
//
// Every gameSignal is already broadcast as an `oskiewar:signal` CustomEvent so
// `oskiewar-sfx.mjs` can synthesize it (`oskiewar.js` `emitSignal`). This module is
// a second subscriber on the same bus that sends the stream out as MIDI, so a
// DAW on the same machine can be the instrument instead of the procedural bank.
// It never touches the sfx module; muting one is the host's decision.
//
// On macOS the destination is the IAC Driver, a loopback MIDI bus built into
// CoreMIDI. It ships offline and lives in a window most people never open:
// Audio MIDI Setup > Window > Show MIDI Studio, double-click IAC Driver, tick
// "Device is online". Measured round trip on that bus is 0.08 ms median, so all
// audible delay belongs to the DAW's own buffer rather than to this path.
//
// The note map is derived from OSKIEWAR_SIGNAL_EVENTS rather than written down a
// second time, so the pads in `oskiewar pads.adg` and this bridge cannot drift
// apart: a pad's note is 36 plus the event's index in that frozen list. Note 36
// is a Drum Rack's bottom-left pad, which is why the map starts there.

import { OSKIEWAR_SIGNAL_EVENTS } from "./oskiewar-sfx.mjs";

export const OSKIEWAR_MIDI_PORT = "IAC Driver Bus 1";
export const OSKIEWAR_MIDI_BASE_NOTE = 36;

const clamp = (value, low, high) => Math.max(low, Math.min(high, value));

export const OSKIEWAR_MIDI_NOTES = Object.freeze(Object.fromEntries(
  OSKIEWAR_SIGNAL_EVENTS
    .map((event, index) => [event, OSKIEWAR_MIDI_BASE_NOTE + index])
    .filter(([, note]) => note <= 127)));

// Themes sit at 84 rather than just past the last signal, leaving notes 75-83
// free. A new gameSignal event appends to the frozen list and shifts nothing
// below it, so that gap is what keeps the theme pads from moving underneath
// somebody's finished sound design the next time the vocabulary grows.
export const OSKIEWAR_THEME_BASE_NOTE = 84;
export const OSKIEWAR_THEMES = Object.freeze(["title", "round", "result"]);
export const OSKIEWAR_THEME_NOTES = Object.freeze(Object.fromEntries(
  OSKIEWAR_THEMES.map((theme, index) =>
    [theme, OSKIEWAR_THEME_BASE_NOTE + index])));

// The pad label carries the word, since "title" alone reads as a hit.
export const themePadName = (theme) => `${theme} theme`;

// Which signals move the music. The game never announces "the round started" as
// its own event, so the theme is inferred from the events that can only happen
// once it has: a countdown or a serve is round music, an outcome is result
// music, and anything on the title screen is the title bed.
const THEME_CUES = Object.freeze({
  hello: "title", select: "title", fighters: "title",
  countdown: "round", ballserve: "round",
  tie: "result", roundwin: "result", matchwin: "result",
});

// Wind is the one signal that reads better ridden than struck: it arrives when
// the sign changes and carries a strength. It is sent as a controller *and* a
// note so a filter can follow it while a pad still marks the change.
const CONTINUOUS = Object.freeze({ wind: 1 });

// Played once the port opens, so the DAW answers before any gameplay does.
// It strikes real pads rather than arbitrary notes, which makes it a check that
// sounds are actually loaded and not just that bytes are moving.
const HANDSHAKE = Object.freeze([
  { event: "hello", after: 0, value: .55 },
  { event: "countdown", after: 120, value: .7 },
  { event: "roundwin", after: 240, value: .85 },
  { event: "matchwin", after: 380, value: 1 },
]);

const NOTE_ON = 0x90, NOTE_OFF = 0x80, CONTROL = 0xB0;

export function createOskiewarMidi(options = {}) {
  const portName = options.portName || OSKIEWAR_MIDI_PORT;
  const baseChannel = clamp(Math.round(Number(options.channel) || 1), 1, 16);
  const noteMs = Math.max(4, Number(options.noteMs) || 60);
  const restVelocity = clamp(Math.round(Number(options.velocity) || 100), 1, 127);
  const notes = options.notes || OSKIEWAR_MIDI_NOTES;
  const continuous = { ...CONTINUOUS, ...(options.continuous || {}) };
  // Off by default: one MIDI track set to channel 1 receives the whole game.
  // Splitting puts player n on channel baseChannel + n, for a rack per fighter.
  const splitPlayers = Boolean(options.splitPlayers);
  const themeNotes = options.themeNotes || OSKIEWAR_THEME_NOTES;
  const themeCues = { ...THEME_CUES, ...(options.themeCues || {}) };
  const themesFollowPlay = options.themes !== false;
  // Themes stay on the base channel even when hits are split per player, since
  // the music belongs to the match rather than to a fighter.
  const themeChannel = clamp(Math.round(Number(options.themeChannel) || baseChannel), 1, 16);

  const timers = new Set();
  const channelsUsed = new Set();
  let access = null, port = null, enabled = false, muted = false;
  let lastError = null, sentCount = 0, eventCounter = 0, currentTheme = null;

  // Live shows the IAC bus as "IAC Driver Bus 1", but a different host may
  // prefix the manufacturer, so widen the match before giving up entirely.
  function resolvePort() {
    if (!access) return null;
    const outputs = [...access.outputs.values()];
    const label = (out) => `${out.manufacturer || ""} ${out.name || ""}`.trim();
    return outputs.find((out) => out.name === portName)
      || outputs.find((out) => label(out) === portName)
      || outputs.find((out) => (out.name || "").includes("IAC"))
      || (options.anyPort ? outputs[0] : null) || null;
  }

  async function enable() {
    if (enabled) return true;
    if (typeof navigator === "undefined" ||
        typeof navigator.requestMIDIAccess !== "function") {
      lastError = "web midi unavailable — needs a chromium browser on https";
      return false;
    }
    try {
      access = await navigator.requestMIDIAccess({ sysex: false });
    } catch (err) {
      lastError = `midi permission refused (${err?.name || err})`;
      return false;
    }
    // Ports come and go when the IAC bus is toggled or a device is unplugged.
    access.onstatechange = () => { port = resolvePort(); };
    port = resolvePort();
    if (!port) {
      lastError = `no midi output named "${portName}" — is the IAC Driver online?`;
      return false;
    }
    enabled = true;
    lastError = null;
    if (options.handshake !== false) handshake();
    return true;
  }

  function send(bytes) {
    if (!enabled || !port || muted) return false;
    try {
      port.send(bytes);
      sentCount += 1;
      return true;
    } catch (err) {
      lastError = `send failed (${err?.name || err})`;
      return false;
    }
  }

  function channelFor(player) {
    return splitPlayers && player >= 0
      ? clamp(baseChannel + player, 1, 16) : baseChannel;
  }

  // Most emitSignal calls pass value 0, meaning "no intensity given" rather than
  // "silent", so those land on a played-in default instead of velocity 1.
  function velocityFor(value) {
    const amount = Math.abs(Number(value) || 0);
    return amount > 0 ? clamp(Math.round(amount * 127), 1, 127) : restVelocity;
  }

  // Struck directly rather than through signal(), so the greeting cannot drag
  // the theme along with it or count as gameplay.
  function strike(note, value, when) {
    const timer = setTimeout(() => {
      timers.delete(timer);
      const status = baseChannel - 1;
      channelsUsed.add(baseChannel);
      send([NOTE_ON | status, note, velocityFor(value)]);
      const release = setTimeout(() => {
        timers.delete(release);
        send([NOTE_OFF | status, note, 0]);
      }, noteMs);
      timers.add(release);
    }, when);
    timers.add(timer);
  }

  function handshake() {
    if (!enabled) return false;
    for (const step of HANDSHAKE) {
      const note = notes[step.event];
      if (note !== undefined) strike(note, step.value, step.after);
    }
    return true;
  }

  // A theme is held rather than struck: its note stays down for as long as that
  // section lasts, so a sustaining instrument plays through and a gate-mode
  // Simpler loops. Switching sends the note-off first, so the two beds never
  // overlap and the DAW is never left holding one when the match moves on.
  function theme(name = null, value = 0) {
    const next = name !== null && themeNotes[name] !== undefined ? name : null;
    if (next === currentTheme) return false;
    if (currentTheme !== null) {
      send([NOTE_OFF | (themeChannel - 1), themeNotes[currentTheme], 0]);
    }
    currentTheme = next;
    if (next !== null) {
      channelsUsed.add(themeChannel);
      send([NOTE_ON | (themeChannel - 1), themeNotes[next], velocityFor(value)]);
    }
    return true;
  }

  function signal(event, player = -1, value = 0, value2 = 0) {
    const note = notes[event];
    if (note === undefined) return false;
    // The bed moves before the accent lands on top of it.
    if (themesFollowPlay && themeCues[event]) theme(themeCues[event]);
    const channel = channelFor(player);
    const status = channel - 1;
    channelsUsed.add(channel);

    const controller = continuous[event];
    if (controller !== undefined) {
      send([CONTROL | status, controller,
        clamp(Math.round(Math.abs(Number(value) || 0) * 127), 0, 127)]);
    }
    if (!send([NOTE_ON | status, note, velocityFor(value)])) return false;
    eventCounter += 1;

    // Drum Racks are one-shot, so the note-off is hygiene rather than gating —
    // but without it a held note stacks up in anything that does respect length.
    const timer = setTimeout(() => {
      timers.delete(timer);
      send([NOTE_OFF | status, note, 0]);
    }, noteMs);
    timers.add(timer);
    if (typeof options.onSignal === "function") {
      options.onSignal({ event, player, value, value2, note, channel });
    }
    return true;
  }

  function cc(controller, value, player = -1) {
    return send([CONTROL | (channelFor(player) - 1),
      clamp(Math.round(Number(controller) || 0), 0, 127),
      clamp(Math.round(Number(value) || 0), 0, 127)]);
  }

  function connectSignals(target = globalThis) {
    if (!target?.addEventListener) return () => {};
    const listener = ({ detail = {} }) =>
      signal(detail.event, detail.player, detail.value, detail.value2);
    target.addEventListener("oskiewar:signal", listener);
    return () => target.removeEventListener?.("oskiewar:signal", listener);
  }

  function panic() {
    for (const timer of timers) clearTimeout(timer);
    timers.clear();
    // A held theme is exactly the note most worth releasing here.
    if (currentTheme !== null) {
      send([NOTE_OFF | (themeChannel - 1), themeNotes[currentTheme], 0]);
      currentTheme = null;
    }
    for (const channel of channelsUsed) {
      send([CONTROL | (channel - 1), 123, 0]);   // all notes off
    }
    channelsUsed.clear();
  }

  function disable() {
    panic();
    enabled = false;
    if (access) access.onstatechange = null;
    access = null;
    port = null;
  }

  // The printable pad chart, so the rack and the reference sheet share a source.
  function chart() {
    return [
      ...Object.entries(notes).map(([event, note]) =>
        ({ event, note, kind: "signal", pad: event })),
      ...Object.entries(themeNotes).map(([event, note]) =>
        ({ event, note, kind: "theme", pad: themePadName(event) })),
    ].sort((a, b) => a.note - b.note);
  }

  return Object.freeze({
    enable, disable, signal, cc, connectSignals, panic, chart, theme, handshake,
    mute(value = true) { muted = Boolean(value); },
    get currentTheme() { return currentTheme; },
    get enabled() { return enabled; },
    get muted() { return muted; },
    get portName() { return port?.name || portName; },
    get outputs() {
      return access ? [...access.outputs.values()].map((o) => o.name) : [];
    },
    get lastError() { return lastError; },
    get sentCount() { return sentCount; },
    get playedEvents() { return eventCounter; },
    notes,
    themeNotes,
    events: OSKIEWAR_SIGNAL_EVENTS,
    themes: OSKIEWAR_THEMES,
  });
}

export default createOskiewarMidi;
