let midi = null;
const activeInputs = new Map();

const NOTE_ON = 0x90;
const NOTE_OFF = 0x80;
const PITCH_BEND = 0xe0;

function handleMidiMessage(message) {
  const [status, note, velocity] = message.data || [];
  if (status === undefined) return;

  const command = status & 0xf0;
  if (command !== NOTE_ON && command !== NOTE_OFF && command !== PITCH_BEND) {
    return;
  }

  // Forward both note-on and note-off so pieces can decide how to react.
  window.acSEND?.({
    type: "midi:keyboard",
    content: { data: [status, note, velocity] },
  });
}

function attachInput(port) {
  if (!port || port.type !== "input") return;

  try {
    port.onmidimessage = handleMidiMessage;
    if (port.state === "connected" && port.connection !== "open") {
      port.open?.().catch((err) =>
        console.warn("🎹 Unable to open MIDI input", port.name, err),
      );
    }
    activeInputs.set(port.id, port);
    console.log("🎹 MIDI input ready:", port.manufacturer, port.name);
  } catch (err) {
    console.warn("🎹 Failed to attach MIDI input", port, err);
  }
}

function detachInput(port) {
  if (!port) return;
  try {
    port.onmidimessage = null;
    activeInputs.delete(port.id);
    console.log("🎹 MIDI input removed:", port.manufacturer, port.name);
  } catch (err) {
    console.warn("🎹 Failed to detach MIDI input", port, err);
  }
}

function refreshInputs() {
  if (!midi) return;
  for (const port of midi.inputs.values()) {
    attachInput(port);
  }
}

function connectToMidi() {
  navigator
    .requestMIDIAccess({ sysex: true })
    .then(
      (access) => {
        midi = access;
        console.log("🎹 MIDI access granted");

        refreshInputs();

        midi.onstatechange = (event) => {
          const { port } = event;
          if (!port || port.type !== "input") return;

          if (port.state === "connected") {
            attachInput(port);
          } else if (port.state === "disconnected") {
            detachInput(port);
          }
        };
      },
      (error) => console.error("🎹 MIDI access error", error),
    );
}

function initialize() {
  if (typeof navigator === "undefined" || !navigator.requestMIDIAccess) {
    console.warn("🎹 MIDI Undetected.");
    return;
  }

  connectToMidi();
}

export { initialize };
