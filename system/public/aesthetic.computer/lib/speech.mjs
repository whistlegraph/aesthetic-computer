// Speech, 23.08.09.15.50
// A thin API over the web speech synthesis API,
// with cloud support.

const synth = window.speechSynthesis;

const speakAPI = {}; // Will get `audioContext` and `playSfx`;

let voices = [];

import { utf8ToBase64 } from "./helpers.mjs";

function populateVoiceList() {
  voices = synth?.getVoices().sort(function (a, b) {
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

if (synth?.onvoiceschanged !== undefined) {
  synth.onvoiceschanged = populateVoiceList;
}

// The mode can either be "local", which uses
// the web speech synth API or "server" which uses google cloud
// and returns a mp3 file.

function speak(words, voice, mode = "local", opts = {}) {
  if (!synth) console.warn("No speech is supported on this platform.");
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
    const label = `speech:${voice}:${opts.provider || "openai"} - ${words}`;

    // For preload-only mode, return a promise that resolves when cached
    let preloadResolve = null;
    const preloadPromise = opts.preloadOnly ? new Promise(resolve => { preloadResolve = resolve; }) : null;

    // Trigger speech playback.
    function play() {
      // If preload-only, just resolve the promise without playing
      if (opts.preloadOnly) {
        if (preloadResolve) preloadResolve(label);
        return;
      }
      
      console.log("🗣️", label);
      const id = label + "_" + performance.now(); // An id for this sample.
      
      // Calculate speed from pitch if provided (frequency in Hz)
      // For musical pitch shifting, we use C4 (261.63Hz) as the base pitch
      // so that note pitches directly correspond to playback speed ratios
      let speed = opts.reverse ? -1 : 1;
      if (isFinite(opts.pitch)) {
        const basePitch = opts.basePitch || 261.63; // C4 - middle C as musical reference
        speed = opts.pitch / basePitch;
        console.log("🗣️ Pitch shift:", { pitch: opts.pitch, basePitch, speed: speed.toFixed(3) });
      } else if (isFinite(opts.speed)) {
        speed = opts.speed;
      }
      
      const vol = isFinite(opts.volume) ? opts.volume : 1;
      console.log("🗣️ playSfx:", { speed: speed.toFixed(3), vol, pitch: opts.pitch });
      
      speakAPI.playSfx(
        id,
        label,
        { speed, pan: opts.pan, volume: vol, loop: opts.loop },
        () => {
          if (!opts.skipCompleted) window.acSEND({ type: "speech:completed" });
        },
      );
    }

    // Clear a specific cache entry (useful for corrupted samples)
    function clearCache(labelToClear) {
      if (speakAPI.sfx[labelToClear]) {
        delete speakAPI.sfx[labelToClear];
        console.log("🗣️ Cleared cache for:", labelToClear);
      }
    }

    // Check local cache first (in-memory, survives until page reload)
    // Skip local cache if bust option is set or if this label was marked for bust
    const needsBust = opts.bust || speakAPI.bustCache?.has(label);
    if (needsBust) {
      speakAPI.bustCache?.delete(label); // Clear the bust flag after using it
      console.log("🧹 Cache bust for:", label);
    }
    
    if (speakAPI.sfx[label] && !needsBust) {
      console.log("🗣️ Local cache hit:", label);
      // For preloadOnly, immediately resolve since already cached
      if (opts.preloadOnly) {
        return Promise.resolve(label);
      }
      play();
      return;
    }

    // Fetch from server (which has its own CDN cache)
    const payload = {
      from: words,
      voice: voice,
      provider: opts.provider || "openai", // "openai" (default) or "google"
      bust: needsBust, // Force regenerate on server if marked
    };

    function fetchSpeech() {
      const controller = new AbortController();
      const id = setTimeout(() => controller.abort(), 8000);
      const host = ``; //window.acDEBUG
      // ? `` // Just use current host, via `netlify.toml`.
      // : "https://ai.aesthetic.computer";

      fetch(`${host}/api/say`, {
        method: "POST",
        headers: {
          "Content-Type": "application/json",
        },
        body: JSON.stringify(payload),
        signal: controller.signal,
      })
        .then(async (res) => {
          clearTimeout(id);
          if (res.status === 200) {
            // console.log("🗣️ Speech response:", res);
            const blob = await res.blob(); // Convert the response to a Blob.
            speakAPI.sfx[label] = await blob.arrayBuffer(); // Cache locally
            console.log("🗣️ Cached locally:", label);
            play();
          } else {
            console.log("🗣️ Speech fetch failure, retrying...", res.status);
            setTimeout(() => {
              fetchSpeech();
            }, 1000);
          }
        })
        .catch((err) => {
          clearTimeout(id);
          console.error("🗣️ Speech fetch failure, retrying...", err);
          setTimeout(() => {
            fetchSpeech();
          }, 1000);
        });
    }

    fetchSpeech();
    
    // Return promise for preload-only mode
    if (preloadPromise) return preloadPromise;
  }
}

export { speak, speakAPI };
