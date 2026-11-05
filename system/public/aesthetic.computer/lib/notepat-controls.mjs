import { GM_PROGRAM_NAMES } from "./notepat-programs.mjs";
import { createMidiController } from "./midi-controller.mjs";
import { createGmBridge } from "./notepat-gm-bridge.mjs";

export function createNotepatControls({ send, context, destination, focus = () => {} }) {
  let disposed = false, current = { wave: "gm", program: 0, local: true }, midiState;
  const dialog = document.createElement("dialog");
  dialog.className = "notepat-controls";
  dialog.setAttribute("aria-label", "Notepat sound and MIDI");
  dialog.setAttribute("data-ac-no-track", "");
  dialog.innerHTML = `<form method="dialog"><header><strong>Sound & MIDI</strong><button value="close" aria-label="Close sound and MIDI">×</button></header></form>
    <label>Sound<select data-field="sound"></select></label>
    <label class="check"><input type="checkbox" data-field="local" checked> Play sound here</label>
    <p role="status" data-field="sound-status"></p>
    <button type="button" data-field="connect">Connect MIDI</button>
    <p role="status" data-field="midi-status"></p>
    <div class="ports"><label>Input<select data-field="input"></select></label><label>Output<select data-field="output"></select></label>
    <label>Output channel<select data-field="channel"></select></label></div>
    <button type="button" data-field="record">Record a phrase</button>
    <footer><button type="button" data-field="panic">Stop all notes</button><a href="https://notepat.com/amxd" data-field="ableton">Ableton device ↗</a></footer>`;
  const style = document.createElement("style");
  style.textContent = `.notepat-controls{box-sizing:border-box;width:min(420px,calc(100vw - 24px));max-height:calc(100dvh - 24px);overflow:auto;padding:20px;color:#f3efe4;background:#191e29;border:1px solid #73807c;border-radius:14px;font:16px/1.4 system-ui,sans-serif;box-shadow:0 18px 70px #0009}.notepat-controls,.notepat-controls *{pointer-events:auto}.notepat-controls::backdrop{background:#070b15b8}.notepat-controls header,.notepat-controls footer{display:flex;align-items:center;justify-content:space-between;gap:12px}.notepat-controls header{margin-bottom:14px}.notepat-controls strong{font-size:20px}.notepat-controls label{display:grid;gap:6px;margin:12px 0}.notepat-controls select,.notepat-controls button{font:inherit;min-height:44px;border:1px solid #64726f;border-radius:7px;background:#292f3e;color:#fff;padding:8px;max-width:100%}.notepat-controls button{cursor:pointer}.notepat-controls button:hover{background:#384535}.notepat-controls :focus-visible{outline:2px solid #c6f7a0;outline-offset:3px}.notepat-controls .check{display:flex;align-items:center;gap:9px}.notepat-controls input{width:20px;height:20px}.notepat-controls p{font-size:14px;color:#c1cbd0;margin:8px 0;min-height:0}.notepat-controls p:empty{display:none}.notepat-controls a{color:#c6f7a0}.notepat-controls footer{margin-top:18px}.notepat-controls .ports{display:grid;grid-template-columns:1fr 1fr;gap:0 12px}.notepat-controls .ports label{min-width:0}.notepat-controls [data-field=connect]{width:100%;background:#344332}@media(max-width:360px){.notepat-controls .ports{grid-template-columns:1fr}}`;
  document.head.append(style); document.body.append(dialog);
  const field = name => dialog.querySelector(`[data-field="${name}"]`);
  const option = (parent, value, label) => { const o = document.createElement("option"); o.value = value; o.textContent = label; parent.append(o); };
  const waves = ["sine", "composite", "triangle", "square", "whistle", "sawtooth", "harp", "stample", "drum"];
  const oscillatorGroup = document.createElement("optgroup"); oscillatorGroup.label = "Notepat";
  for (const wave of waves) option(oscillatorGroup, wave, wave === "stample" ? "Sample" : wave[0].toUpperCase() + wave.slice(1));
  field("sound").append(oscillatorGroup);
  const gmGroup = document.createElement("optgroup"); gmGroup.label = "General MIDI";
  GM_PROGRAM_NAMES.forEach((name, program) => option(gmGroup, `gm:${program}`, `${program + 1} · ${name}`));
  field("sound").append(gmGroup);
  for (let n = 0; n < 16; n++) option(field("channel"), n, n + 1);
  function renderMidi(state) {
    if (disposed) return;
    midiState = state;
    field("connect").disabled = state.pending || !state.supported;
    field("connect").textContent = state.pending ? "Connecting…" : state.connected ? "MIDI connected" : "Connect MIDI";
    field("midi-status").textContent = state.error || (!state.supported ? "This browser has no MIDI access. The touch grids and keyboard still play." : state.connected && !state.inputs.length && !state.outputs.length ? "No MIDI ports found. Connect a device to use it here." : "");
    for (const direction of ["input", "output"]) {
      const select = field(direction); select.replaceChildren();
      option(select, direction === "input" ? "all" : "", direction === "input" ? "All inputs" : "No MIDI output");
      for (const port of state[`${direction}s`]) option(select, port.id, port.name);
      select.value = state[`${direction}Id`]; select.disabled = !state.connected;
    }
    field("channel").value = state.channel;
    field("channel").disabled = !state.outputId;
    send({ type: "notepat:state", content: { hostReady: true, midiConnected: state.connected, midiOutput: !!state.outputId } });
  }
  const midi = createMidiController({ message: content => send({ type: "midi:keyboard", content }), changed: renderMidi });
  const gm = createGmBridge({ context, destination, status: content => {
    if (disposed) return;
    field("sound-status").textContent = content.error || (content.loading ? "Loading instrument…" : "");
    send({ type: "notepat:state", content: { gmReady: content.ready, gmLoading: !!content.loading } });
  } });
  let preparedProgram, preparedContext;
  function prepareInstrument() {
    if(current.wave !== "gm" || !current.local) return;
    const ctx=context();
    if(preparedProgram===current.program && preparedContext===ctx) return;
    preparedProgram=current.program; preparedContext=ctx;
    void gm.program(current.program);
  }
  const control = content => send({ type: "notepat:control", content });
  field("record").onclick = () => { control({ kind: "record" }); dialog.close(); };
  field("connect").onclick = () => midi.connect();
  field("input").onchange = e => midi.selectInput(e.target.value);
  field("output").onchange = e => { midi.selectOutput(e.target.value); if (current.wave === "gm") midi.program(current.program); };
  field("channel").onchange = e => { midi.selectChannel(Number(e.target.value)); if (current.wave === "gm") midi.program(current.program); };
  field("sound").onchange = e => {
    const value = e.target.value;
    control(value.startsWith("gm:") ? { kind: "program", program: Number(value.slice(3)) } : { kind: "wave", wave: value });
  };
  field("local").onchange = e => control({ kind: "local", enabled: e.target.checked });
  const panic = () => { midi.panic(); gm.panic(); control({ kind: "panic" }); };
  field("panic").onclick = panic;
  field("ableton").onclick = () => window.acVisits?.action("notepat_amxd_requested");
  for (const type of ["keydown", "keyup", "keypress"]) dialog.addEventListener(type, e => e.stopPropagation());
  dialog.addEventListener("close", () => { focus(false); send({ type: "notepat:state", content: { controlsOpen: false } }); });
  const blur = () => panic();
  window.addEventListener("blur", blur);
  const hidden = () => { if (document.hidden) panic(); };
  document.addEventListener("visibilitychange", hidden);
  renderMidi(midi.state());
  return {
    message(content) {
      if (disposed || !content) return;
      const { op } = content;
      prepareInstrument();
      if (op === "open") { panic(); focus(true); if (!dialog.open) dialog.showModal(); send({ type: "notepat:state", content: { controlsOpen: true } }); }
      else if (op === "state") {
        current = { ...current, ...content };
        prepareInstrument();
        field("sound").value = current.wave === "gm" ? `gm:${current.program}` : current.wave;
        field("local").checked = current.local;
        field("record").textContent = current.recording ? "Stop and play recording" : "Record a phrase";
      } else if (op === "program") { gm.program(content.program); if (!content.fromMidi) midi.program(content.program); }
      else if (op === "gm-on") gm.note(content);
      else if (op === "gm-off") gm.stop(content.id, content.fade);
      else if (op === "gm-bend") gm.update(content);
      else if (op === "midi-on") midi.noteOn(content.id, content.note, content.velocity);
      else if (op === "midi-off") midi.noteOff(content.id);
      else if (op === "panic") { midi.panic(); gm.panic(); }
    },
    dispose() { disposed = true; midi.disconnect(); gm.panic(); focus(false); dialog.remove(); style.remove(); window.removeEventListener("blur", blur); document.removeEventListener("visibilitychange", hidden); },
  };
}
