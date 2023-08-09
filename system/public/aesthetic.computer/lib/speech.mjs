// Speech, 23.08.09.15.50
// A thin API over the web speech synthesis API.

const synth = window.speechSynthesis;

let voices = [];

function populateVoiceList() {
  voices = synth.getVoices().sort(function (a, b) {
    const aname = a.name.toUpperCase();
    const bname = b.name.toUpperCase();
    if (aname < bname) {
      return -1;
    } else if (aname == bname) {
      return 0;
    } else {
      return 1;
    }
  });
  // console.log("🗣️ Available voices:", voices);
}

populateVoiceList();

if (speechSynthesis.onvoiceschanged !== undefined) {
  speechSynthesis.onvoiceschanged = populateVoiceList;
}

function speak(words, voice) {
  if (synth.speaking) {
    console.error("🗣️ Already speaking...");
    return;
  }

  const utterance = new SpeechSynthesisUtterance(words);

  let voiceIndex = 11;
  if (voice === "female") voiceIndex = 9;
  if (voice === "male") voiceIndex = 10;

  utterance.voice = voices[voiceIndex];
  // console.log("Speaking:", words, utterance.voice);

  utterance.onend = function (event) {
    // console.log("🗣️ Speech completed:", event);
    window.acSEND({ type: "speech:completed" }); // TODO: Send a message to the disk that the speech has been completed.
  };

  utterance.onerror = function (event) {
    console.error("🗣️ Speech failure:", event);
  };

  synth.speak(utterance);
}

export { speak };
