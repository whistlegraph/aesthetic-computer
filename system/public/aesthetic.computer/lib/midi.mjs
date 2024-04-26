let midi = null;

let receiveDevice = null;

// Disconnect a device and clear the received MIDI message log.
const resetInput = () => {
  if (receiveDevice) {
    receiveDevice.onmidimessage = null;
    receiveDevice.close();
    receiveDevice = null;
  }
};

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
          showEvent(event);
        };

        resetInput();

        const inputValues = midi.inputs.values();
        console.log(inputValues);

        receiveDevice = inputValues[0];
        receiveDevice.onmidimessage = (message) => {
          console.log(message);
        };
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
