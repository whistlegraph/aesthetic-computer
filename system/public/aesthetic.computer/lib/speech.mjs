// Speech, 23.08.09.15.50
// A thin API over the web speech synthesis API,
// with cloud support.

const synth = window.speechSynthesis;

const speakAPI = {}; // Will get `audioContext` and `playSfx`;

let voices = [];

import { utf8ToBase64 } from "./helpers.mjs";

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
    const label = `speech:${voice} - ${words}`;

    // Trigger speech playback.
    function play() {
      console.log("🗣️", label);
      const id = label + "_" + performance.now(); // An id for this sample.
      speakAPI.playSfx(
        id,
        label,
        { reverse: opts.reverse, pan: opts.pan },
        () => {
          if (!opts.skipCompleted) window.acSEND({ type: "speech:completed" });
        }
      );
    }

    // Add the label to the sfx library.
    // if (!speakAPI.sfx[label]) {
    // const queryString = new URLSearchParams({
    //   from: words,
    //   voice,
    // }).toString();

    const wordsEncoded = utf8ToBase64(words);
    const queryString = `from=${wordsEncoded}&voice=${voice}`;

    function fetchSpeech() {
      const controller = new AbortController();
      const id = setTimeout(() => controller.abort(), 8000);
      const host = window.acDEBUG
        ? `` // Just use current host, via `netlify.toml`.
        : "https://ai.aesthetic.computer";
      fetch(`${host}/api/say?${queryString}`, { signal: controller.signal })
        .then(async (res) => {
          clearTimeout(id);
          if (res.status === 200) {
            const blob = await res.blob(); // Convert the response to a Blob.
            speakAPI.sfx[label] ||= await blob.arrayBuffer();
            play();
          } else {
            console.log("🗣️ Speech fetch failure, retrying...", res.status);
            fetchSpeech();
          }
        })
        .catch((err) => {
          clearTimeout(id);
          console.error("🗣️ Speech fetch failure, retrying...", err);
          fetchSpeech();
        });
    }
    fetchSpeech();
    // } else {
    //  play(); // Or play it again if it's already present.
    // }
  }
}

export { speak, speakAPI };
