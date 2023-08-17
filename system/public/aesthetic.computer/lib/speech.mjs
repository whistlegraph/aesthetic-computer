// Speech, 23.08.09.15.50
// A thin API over the web speech synthesis API,
// with cloud support.

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

// The mode can either be "local", which uses
// the web speech synth API or "server" which uses google cloud
// and returns a mp3 file.
function speak(words, voice, mode = "local", opts = {}) {
  if (mode === "local") {
    if (synth.speaking) {
      console.error("🗣️ Already speaking...");
      return;
    }

    const utterance = new SpeechSynthesisUtterance(words);

    let voiceIndex = 11;
    if (voice.startsWith("female")) voiceIndex = 9;
    if (voice.startsWith("male")) voiceIndex = 10;

    utterance.voice = voices[voiceIndex];
    // console.log("Speaking:", words, utterance.voice);

    if (!opts.skipCompleted) {
      utterance.onend = function (event) {
        // console.log("🗣️ Speech completed:", event);
        window.acSEND({ type: "speech:completed" }); // Send to piece.
      };
    }

    utterance.onerror = function (event) {
      console.error("🗣️ Speech failure:", event);
    };

    synth.speak(utterance);
  } else if (mode === "cloud") {
    const queryString = new URLSearchParams({ from: words, voice }).toString();
    fetch(`/tts?${queryString}`)
      .then((res) => res.blob()) // Convert the response to a Blob.
      .then((blob) => {
        const sfx = new Audio();
        sfx.src = URL.createObjectURL(blob);
        if (!opts.skipCompleted) {
          sfx.addEventListener("ended", () => {
            window.acSEND({ type: "speech:completed" });
          });
        }
        sfx.play();
      })
      .catch((err) => {
        console.error("Speech fetch failure:", err);
      });
  }
}

export { speak };
