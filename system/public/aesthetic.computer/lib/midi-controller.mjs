// User-requested Web MIDI. Port names and IDs stay in this document.
export function createMidiController({ navigator = globalThis.navigator, message = () => {}, changed = () => {} } = {}) {
  let access, pending, epoch = 0, error = null, inputId = "all", outputId = "", channel = 0;
  const attached = new Map(), owned = new Map();
  const state = () => ({ supported: !!navigator?.requestMIDIAccess, connected: !!access, pending: !!pending, error,
    inputs: [...(access?.inputs.values() || [])].filter(p => p.state === "connected").map(p => ({ id: p.id, name: p.name || "MIDI input" })),
    outputs: [...(access?.outputs.values() || [])].filter(p => p.state === "connected").map(p => ({ id: p.id, name: p.name || "MIDI output" })),
    inputId, outputId, channel });
  const notify = () => changed(state());
  const output = () => access?.outputs.get(outputId);
  function send(data, port = output()) {
    if (port?.state !== "connected") return false;
    try { port.send(data); return true; } catch { error = "MIDI output is unavailable"; notify(); return false; }
  }
  function release(id) {
    const note = owned.get(id);
    if (!note) return;
    owned.delete(id);
    // Multiple pads may own the same pitch. Release it after the last owner.
    if (![...owned.values()].some(n => n.port === note.port && n.channel === note.channel && n.note === note.note))
      send([0x80 | note.channel, note.note, 0], note.port);
  }
  function panic() {
    for (const id of [...owned.keys()]) release(id);
  }
  function detach(port) {
    port.onmidimessage = null;
    attached.delete(port.id);
    message({ source: port.id, disconnected: true });
    try { Promise.resolve(port.close?.()).catch(() => {}); } catch {}
  }
  function refresh() {
    for (const port of attached.values()) {
      if (port.state !== "connected" || (inputId !== "all" && port.id !== inputId)) detach(port);
    }
    for (const port of access?.inputs.values() || []) {
      if (port.state !== "connected" || (inputId !== "all" && port.id !== inputId) || attached.has(port.id)) continue;
      attached.set(port.id, port);
      port.onmidimessage = event => {
        const data = [...(event.data || [])], status = data[0], kind = status & 0xf0;
        const length = kind === 0xc0 || kind === 0xd0 ? 2 : 3;
        if (status < 0x80 || status > 0xef || data.length !== length || data.slice(1).some(v => !Number.isInteger(v) || v < 0 || v > 127)) return;
        message({ source: port.id, data });
      };
      try { Promise.resolve(port.open?.()).catch(() => { error = "MIDI input could not open"; notify(); }); } catch { error = "MIDI input could not open"; }
    }
    // A removed output cannot keep ownership across a later reconnection.
    for (const [id, note] of owned) if (note.port.state !== "connected") owned.delete(id);
    notify();
  }
  async function connect() {
    if (access) return state();
    if (pending) return pending;
    if (!navigator?.requestMIDIAccess) { error = "MIDI is unavailable in this browser"; notify(); return state(); }
    const generation = epoch;
    error = null;
    pending = Promise.resolve().then(() => navigator.requestMIDIAccess({ sysex: false })).then(result => {
      if (epoch !== generation) return state();
      access = result;
      access.onstatechange = refresh;
      refresh();
      return state();
    }).catch(() => { if (epoch === generation) error = "MIDI permission was not granted"; return state(); })
      .finally(() => { if (epoch === generation) { pending = null; notify(); } });
    notify();
    return pending;
  }
  return {
    state, connect, panic,
    selectInput(id) { inputId = id === "all" || access?.inputs.has(id) ? id : "all"; refresh(); },
    selectOutput(id) { panic(); outputId = access?.outputs.has(id) ? id : ""; notify(); },
    selectChannel(value) { if (!Number.isInteger(value) || value < 0 || value > 15) return; panic(); channel = value; notify(); },
    noteOn(id, note, velocity = 100) {
      if (!Number.isInteger(note) || note < 0 || note > 127 || !Number.isInteger(velocity) || velocity < 1 || velocity > 127) return false;
      release(id);
      const port = output();
      const alreadyHeld = [...owned.values()].some(n => n.port === port && n.channel === channel && n.note === note);
      if (!alreadyHeld && !send([0x90 | channel, note, velocity], port)) return false;
      owned.set(id, { note, channel, port }); return true;
    },
    noteOff: release,
    program(program) { if (Number.isInteger(program) && program >= 0 && program < 128) return send([0xc0 | channel, program]); return false; },
    disconnect() {
      epoch++; pending = null; panic();
      if (access) access.onstatechange = null;
      for (const port of [...attached.values()]) detach(port);
      access = null; notify();
    },
  };
}
