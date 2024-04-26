let midi = null;
let receiveDevice = null;

// Disconnect a device and clear the received MIDI message log.
// const resetInput = () => {
//   if (receiveDevice) {
//     receiveDevice.onmidimessage = null;
//     receiveDevice.close();
//     receiveDevice = null;
//   }
// };

const connectToMidi = () => {
  navigator
    .requestMIDIAccess({
      sysex: true,
    })
    .then(
      (access) => {
        midi = access;
        console.log("🎹 Connected to MIDI:", midi);

        midi.onstatechange = (event) => {
          console.log(event);
        };

        // resetInput();

        let keyboard;
        for (const port of midi.inputs.values()) {
          if (port.name === "reface YC MIDI 1") keyboard = port;
        }

        if (keyboard) {
          keyboard.onmidimessage = (message) => {
            if (message.data[0] === 144) {
              window.acSEND?.({
                type: "midi:keyboard",
                content: { data: message.data },
              });
            }
          };
        }
      },
      (error) => console.error(error),
    );
};

function initialize() {
  if (navigator.requestMIDIAccess) {
    connectToMidi();
  } else {
    console.warn("🎹 MIDI Undetected.");
  }
}

export { initialize };
