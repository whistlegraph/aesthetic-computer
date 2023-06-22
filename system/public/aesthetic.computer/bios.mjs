// 💻 BIOS

// 📦 All Imports
import * as Loop from "./lib/loop.mjs";
import { Pen } from "./lib/pen.mjs";
import { Box } from "./lib/geo.mjs";
import { Keyboard } from "./lib/keyboard.mjs";
import * as UI from "./lib/ui.mjs";
import * as Glaze from "./lib/glaze.mjs";
import { apiObject, extension } from "./lib/helpers.mjs";
import { parse, slug } from "./lib/parse.mjs";
import * as Store from "./lib/store.mjs";
import { Desktop, MetaBrowser, Instagram, iOS } from "./lib/platform.mjs";
import { headers } from "./lib/console-headers.mjs";
import { logs } from "./lib/logs.mjs";
import { soundWhitelist } from "./lib/sound/sound-whitelist.mjs";

const { assign, keys } = Object;
const { round, floor, min, max } = Math;

// 💾 Boot the system and load a disk.
async function boot(parsed, bpm = 60, resolution, debug) {
  headers(); // Print console headers.

  if (debug) {
    if (window.isSecureContext) {
      console.log("🔒 Secure");
    } else {
      console.warn("🔓 Insecure");
    }
  }

  window.acCONTENT_EVENTS = [];

  let pen,
    keyboard,
    keyboardFocusLock = false;
  let handData; // Hand-tracking.

  // let frameCount = 0;
  // let timePassed = 0;
  let now = 0;

  let diskSupervisor;
  let currentPiece = null; // Gets set to a path after `loaded`.
  let currentPieceHasKeyboard = false;

  // Media Recorder
  let mediaRecorder, mediaRecorderDataHandler, mediaRecorderBlob; // Holds the last generated recording.

  // Clipboard
  let pastedText;

  // Events
  let whens = {};

  // 0. Video storage
  const videos = [];

  // 1. Rendering

  // Wrap everything in an #aesthetic-computer div.
  const wrapper = document.createElement("div");
  wrapper.id = "aesthetic-computer";

  // Our main display surface.
  const canvas = document.createElement("canvas");
  const ctx = canvas.getContext("2d", { willReadFrequently: true });

  // An extra canvas reference for passing through or buffering video recording streams.
  let streamCanvasContext;
  let resizeToStreamCanvas = false;

  // A layer for modal messages such as "audio engine is off".
  const modal = document.createElement("div");
  modal.id = "modal";

  // A ui canvas for rendering a native resolution ui on top of everything.
  const uiCanvas = document.createElement("canvas");
  const uiCtx = uiCanvas.getContext("2d");
  uiCanvas.dataset.type = "ui";

  // A buffer for nicer resolution switches, nice when moving from
  // low resolution back to high resolution. Could eventually be used
  // for transition effects.
  const freezeFrameCan = document.createElement("canvas");
  const ffCtx = freezeFrameCan.getContext("2d");
  freezeFrameCan.dataset.type = "freeze";

  let imageData;
  let fixedWidth, fixedHeight;
  let projectedWidth, projectedHeight;
  let canvasRect;

  let glaze = { on: false };
  let currentGlaze;

  const glazeComposite = document.createElement("canvas");
  const glazeCompositeCtx = glazeComposite.getContext("2d");

  let needsReframe = false;
  let needsReappearance = false;
  let freezeFrame = false,
    freezeFrameFrozen = false,
    freezeFrameGlaze = false;

  const screen = apiObject("pixels", "width", "height");

  const REFRAME_DELAY = 250;
  let curReframeDelay = REFRAME_DELAY;
  let lastGap = 0;
  let density = 2.2; // added to window.devicePixelRatio

  // *** External Library Dependency Injection ***

  // FFMPEG.WASM
  async function loadFFmpeg() {
    return new Promise((resolve, reject) => {
      const script = document.createElement("script");
      script.src = "aesthetic.computer/dep/ffmpeg/ffmpeg.min.js";

      script.onerror = function (err) {
        reject(err, s);
      };

      script.onload = function handleScriptLoaded() {
        if (debug) console.log("📼 FFmpeg has loaded.", FFmpeg);
        resolve(FFmpeg);
      };

      document.head.appendChild(script);
    });
  }

  // THREE.JS (With a thin wrapper called ThreeD).
  let ThreeD;
  let ThreeDBakeQueue = [];
  async function loadThreeD() {
    ThreeD = await import(`./lib/3d.mjs`);
    ThreeD.initialize(
      wrapper,
      Loop.mainLoop,
      receivedDownload,
      receivedUpload,
      send
    );
  }

  // Web3
  async function loadWeb3() {
    return new Promise((resolve, reject) => {
      const script = document.createElement("script");
      script.src = "aesthetic.computer/dep/web3/web3.min.js";

      script.onerror = (err) => reject(err, s);

      script.onload = function handleScriptLoaded() {
        if (debug) console.log("🕸️3️⃣ Ready...");
        resolve(Web3);
      };

      document.head.appendChild(script);
    });
  }

  // Used by `disk` to set the metatags by default when a piece loads. It can
  // be overridden using `meta` inside of `boot` for any given piece.
  function setMetatags(meta) {
    if (meta?.title) {
      document.title = meta.title;
      document.querySelector('meta[name="og:title"]').content = meta.title;
      document.querySelector('meta[name="twitter:title"]').content = meta.title;
    }
    if (meta?.desc) {
      document.querySelector('meta[name="og:description"]').content = meta.desc;
    }
    if (meta?.img?.og) {
      document.querySelector('meta[name="og:image"]').content = meta.img.og;
    }
    if (meta?.img?.twitter) {
      document.querySelector('meta[name="twitter:image"]').content =
        meta.img.twitter;
    }
    if (meta?.url) {
      // This might need to be conditional / opt-in?
      // document.querySelector('meta[name="twitter:player"').content = meta.url;
    }
  }

  function frame(width, height, gap = 8) {
    lastGap = gap;

    // Cache the current canvas if needed.
    if (
      freezeFrame &&
      imageData?.data.length > 0 &&
      !document.body.contains(freezeFrameCan)
    ) {
      if (debug && logs.frame) {
        console.log(
          "🥶 Freezing:",
          freezeFrame,
          imageData.width,
          imageData.height
        );
      }

      freezeFrameCan.style.width = canvas.getBoundingClientRect().width + "px";
      freezeFrameCan.style.height =
        canvas.getBoundingClientRect().height + "px";

      // TODO: Get margin of canvasRect or make freezeFrame work on top of everything...
      // Is this still relevant? 2022.4.09

      /*
      console.log(
        "Freezeframe offset",
        wrapper.offsetLeft,
        canvasRect.x,
        canvasRect.width - canvasRect.x
      );
      */

      freezeFrameCan.style.left = canvasRect.x + "px";
      freezeFrameCan.style.top = canvasRect.y + "px";

      // TODO: Save the Glaze canvas if glaze is enabled / figure out how to deal
      //       with Glaze.

      if (freezeFrameGlaze) {
        Glaze.freeze(ffCtx);
        // ffCtx.fillStyle = "lime";
        // ffCtx.fillRect(0, 0, ffCtx.canvas.width, ffCtx.canvas.height);
        freezeFrameGlaze = false;
      } else {
        ffCtx.drawImage(canvas, 0, 0);
        // ffCtx.putImageData(imageData, 0, 0); // TODO: Fix source data detached error here.
      }

      if (!wrapper.contains(freezeFrameCan)) wrapper.append(freezeFrameCan);
      else freezeFrameCan.style.removeProperty("opacity");
      canvas.style.opacity = 0;
      freezeFrameFrozen = true;
    }

    // Find the width and height of our default screen and native projection.
    width = width || fixedWidth;
    height = height || fixedHeight;

    const gapSize = gap * window.devicePixelRatio;

    let subdivisions = 1;

    if (width === undefined && height === undefined) {
      // Automatically set and frame a reasonable resolution.
      // Or pull from density.
      let ratio = density || window.devicePixelRatio;
      if (!density && window.devicePixelRatio === 1) ratio = 3; // Always force a screen density of 3 on non-retina displays.
      subdivisions = ratio;
      width = round(window.innerWidth / subdivisions);
      height = round(window.innerHeight / subdivisions);
      projectedWidth = round(width * subdivisions - gapSize);
      projectedHeight = round(height * subdivisions - gapSize);
    } else {
      // Or do it manually if both width and height are defined.
      fixedWidth = width;
      fixedHeight = height;

      const scale = min(window.innerWidth / width, window.innerHeight / height);

      projectedWidth = round(width * scale - gapSize);
      projectedHeight = round(height * scale - gapSize);
    }

    if (debug && logs.frame)
      console.info(
        "🖼 Frame:",
        width,
        height,
        "🖥 Window:",
        window.innerWidth,
        window.innerHeight
      );

    // Send a message about this new width and height to any hosting frames.
    // parent.postMessage({ width: projectedWidth, height: projectedHeight }, "*");

    canvas.width = width;
    canvas.height = height;

    glazeComposite.width = canvas.width;
    glazeComposite.height = canvas.height;

    uiCanvas.width = projectedWidth * window.devicePixelRatio;
    uiCanvas.height = projectedHeight * window.devicePixelRatio;

    // Horizontal and vertical offsetting of the wrapper.
    wrapper.style.top =
      round((window.innerHeight - projectedHeight) / 2) + "px";

    wrapper.style.left = round((window.innerWidth - projectedWidth) / 2) + "px";
    wrapper.style.width = projectedWidth + "px";
    wrapper.style.height = projectedHeight + "px";

    canvas.style.width = projectedWidth + "px";
    canvas.style.height = projectedHeight + "px";
    uiCanvas.style.width = projectedWidth + "px";
    uiCanvas.style.height = projectedHeight + "px";

    // Add some fancy ratios to the canvas and uiCanvas.
    /*
    canvas.style.width = `calc(100vw - ${gapSize}px)`;
    canvas.style.height = `calc(calc(${
      height / width
    } * 100vw) - ${gapSize}px)`;
    canvas.style.maxHeight = `calc(100vh - ${gapSize}px)`;
    canvas.style.maxWidth = `calc(calc(${
      width / height
    } * 100vh) - ${gapSize}px)`;

    uiCanvas.style.width = `calc(100vw - ${gapSize}px)`;
    uiCanvas.style.height = `calc(calc(${
      height / width
    } * 100vw) - ${gapSize}px)`;

    uiCanvas.style.maxHeight = `calc(100vh - ${gapSize}px)`;
    uiCanvas.style.maxWidth = `calc(calc(${
      width / height
    } * 100vh) - ${gapSize}px)`;
    */

    if (imageData?.length > 0) {
      ctx.putImageData(imageData, 0, 0);
    } else {
      imageData = ctx.getImageData(0, 0, canvas.width, canvas.height);
      // This will have zero alpha.
    }

    assign(screen, { pixels: imageData.data, width, height });

    // Add the canvas, modal, and uiCanvas when we first boot up.
    if (!wrapper.contains(canvas)) {
      wrapper.append(canvas);
      wrapper.append(modal);

      const bumper = document.createElement("div");
      bumper.id = "bumper";
      modal.append(bumper);

      wrapper.append(uiCanvas);
      document.body.append(wrapper);

      // Trigger it to re-draw whenever the window resizes.
      let timeout;
      window.addEventListener("resize", (e) => {
        // Check to see if we are in "native-cursor" mode and hide
        // #aesthetic.computer for the resize if we aren't.
        if (document.body.classList.contains("native-cursor") === false) {
          wrapper.classList.add("hidden");
        }

        window.clearTimeout(timeout); // Small timer to save on performance.

        timeout = setTimeout(() => {
          needsReframe = true; // This makes zooming work / not work.
          curReframeDelay = REFRAME_DELAY;
        }, curReframeDelay); // Is this needed?
      });

      // Prevent canvas touchstart events from triggering magnifying glass on
      // iOS Safari, unless a link is pressed.
      wrapper.addEventListener(
        "touchstart",
        function (event) {
          if (event.target.tagName !== "A") event.preventDefault();
        },
        false
      );
    }

    canvasRect = canvas.getBoundingClientRect();

    Glaze.clear();

    // A native resolution canvas for drawing cursors, system UI, and effects.
    if (glaze.on) {
      currentGlaze = Glaze.on(
        canvas.width,
        canvas.height,
        canvasRect,
        projectedWidth,
        projectedHeight,
        wrapper,
        glaze.type,
        () => {
          send({ type: "needs-paint" }); // Once all the glaze shaders load, render a single frame.
          // canvas.style.opacity = 0;
        }
      );
    } else {
      Glaze.off();
    }

    needsReframe = false;
    needsReappearance = true; // Only for `native-cursor` mode.
    send({ type: "needs-paint" });
    send({
      type: "reframed",
      content: {
        innerWidth: window.innerWidth,
        innerHeight: window.innerHeight,
        subdivisions,
      },
    });
  }

  // 2. 🔈 Audio
  const sound = {
    bpm: new Float32Array(1),
  };

  const sfx = {}; // Buffers of sound effects that have been loaded.
  // TODO: Some of these need to be kept (like system ones) and others need to
  // be destroyed after pieces change.

  let updateMetronome,
    triggerSound,
    updateBubble,
    updateSound,
    killSound,
    killAllSound,
    requestSpeakerWaveforms,
    requestSpeakerAmplitudes,
    attachMicrophone,
    detachMicrophone,
    audioContext,
    audioStreamDest;

  let requestMicrophoneAmplitude,
    requestMicrophoneWaveform,
    requestMicrophonePitch;

  // TODO: Eventually this would be replaced with a more dynamic system.

  const backgroundTrackURLs = [
    "0 - analog multiplication.m4a",
    "1 - castlecowards.m4a",
    "2 - epanodos clinamen.m4a",
    "3 - for not being able.m4a",
    "4 - pantoum chain rhyme.m4a",
    "5 - they sit so nicely.m4a",
    "6 - vociferatings witchbefooled.m4a",
    "7 - an accuracy which it seems as impossible to attain.m4a",
    "8 - bivariate beamforming.m4a",
    "9 - and the three of them began to make.m4a",
    "10 - or perhaps destroyed.m4a",
    "11 - sunsmidnought.m4a",
    "12 - improvements design.m4a",
    "13 - consideration.m4a",
    "14 - magellanic clouds.m4a",
    "15 - syncopation demotic.m4a",
    "16 - textual criticism ambiguity.m4a",
  ];

  const backgroundMusicEl = document.createElement("audio");
  backgroundMusicEl.id = "background-music";
  backgroundMusicEl.crossOrigin = "anonymous";
  wrapper.appendChild(backgroundMusicEl);

  let analyserCtx, analyserSrc, analyser, frequencyData;
  let currentBackgroundTrack;

  function playBackgroundMusic(n) {
    if (currentBackgroundTrack !== n && !isNaN(n)) {
      const origin = "https://bgm.aesthetic.computer/";
      backgroundMusicEl.src = origin + backgroundTrackURLs[n];
      if (audioContext) backgroundMusicEl.play();
      currentBackgroundTrack = n;
    }
  }

  function stopBackgroundMusic() {
    currentBackgroundTrack = null;
    backgroundMusicEl.src = "";
  }

  function startSound() {
    // BGM Analyser
    analyserCtx = new AudioContext();
    analyserSrc = analyserCtx.createMediaElementSource(backgroundMusicEl);
    analyser = analyserCtx.createAnalyser();
    analyser.fftSize = 256; // See also: https://developer.mozilla.org/en-US/docs/Web/API/AnalyserNode/frequencyBinCount

    analyserSrc.connect(analyser);
    analyser.connect(analyserCtx.destination);
    frequencyData = new Uint8Array(analyser.frequencyBinCount);

    // Main audio feed
    audioContext = new AudioContext({
      latencyHint: "interactive",
      // TODO: Eventually choose a good sample rate and/or make it settable via
      //       the current disk.
      // sampleRate: 44100,
      // sampleRate: 48000,
      // sampleRate: 96000,
      // sampleRate: 192000,
    });

    audioStreamDest = audioContext.createMediaStreamDestination();

    if (audioContext.state === "running") {
      audioContext.suspend();
    }

    // TODO: Check to see if there is support for AudioWorklet or not...
    //       and and use ScriptProcessorNode as a fallback. 2022.01.13.21.00

    // Microphone Input Processor
    // (Gets attached via a message from the running disk.)
    attachMicrophone = async (data) => {
      let micStream;
      try {
        micStream = await navigator.mediaDevices.getUserMedia({
          audio: {
            echoCancellation: true,
            latency: 0,
            noiseSuppression: true,
            autoGainControl: true,
          },
        });
      } catch (err) {
        if (debug) console.warn("🎙 Microphone disabled:", err);
      }

      if (!micStream) {
        send({ type: "microphone-connect:failure" });
        return;
      }

      const micNode = new MediaStreamAudioSourceNode(audioContext, {
        mediaStream: micStream,
      });

      // TODO: Why can't there be separate audioWorklet modules?
      await audioContext.audioWorklet.addModule(
        "/aesthetic.computer/lib/microphone.mjs"
      );

      const playerNode = new AudioWorkletNode(
        audioContext,
        "microphone-processor",
        {
          outputChannelCount: [2],
          processorOptions: { debug },
        }
      );

      micNode.connect(playerNode);

      // Receive messages from the microphone processor thread.
      playerNode.port.onmessage = (e) => {
        const msg = e.data;

        if (msg.type === "amplitude") {
          send({ type: "microphone-amplitude", content: msg.content });
        }

        if (msg.type === "waveform") {
          send({ type: "microphone-waveform", content: msg.content });
        }

        if (msg.type === "pitch") {
          send({ type: "microphone-pitch", content: msg.content });
        }
      };

      // Request data / send message to the mic processor thread.
      requestMicrophoneAmplitude = () => {
        playerNode.port.postMessage({ type: "get-amplitude" });
      };

      requestMicrophoneWaveform = () => {
        playerNode.port.postMessage({ type: "get-waveform" });
      };

      requestMicrophonePitch = () => {
        playerNode.port.postMessage({ type: "get-pitch" });
      };

      // Connect mic to the mediaStream.
      playerNode.connect(audioStreamDest);

      // Connect to the speaker if we are monitoring audio.
      if (data?.monitor === true) playerNode.connect(audioContext.destination);

      // Setup microphone detachment function.
      detachMicrophone = () => {
        playerNode.disconnect();
        micNode.disconnect();
        micStream.getTracks().forEach((t) => t.stop());
        if (debug) console.log("🎙💀 Microphone:", "Detached");
      };

      // Send a message back to `disk` saying the microphone is connected.
      send({ type: "microphone-connect:success" });
      if (debug) console.log("🎙 Microphone connected:", data);
    };

    // Sound Synthesis Processor
    try {
      (async () => {
        await audioContext.audioWorklet.addModule(
          "/aesthetic.computer/lib/speaker.mjs"
        );

        const soundProcessor = new AudioWorkletNode(
          audioContext,
          "sound-processor",
          {
            outputChannelCount: [2],
            processorOptions: { bpm: sound.bpm, debug },
          }
        );

        updateMetronome = function (newBPM) {
          soundProcessor.port.postMessage({ type: "new-bpm", data: newBPM });
        };

        triggerSound = function (sound) {
          soundProcessor.port.postMessage({ type: "sound", data: sound });
        };

        updateBubble = function (bubble) {
          soundProcessor.port.postMessage({ type: "bubble", data: bubble });
        };

        killSound = function (id) {
          soundProcessor.port.postMessage({ type: "kill", data: id });
        };

        updateSound = function (data) {
          soundProcessor.port.postMessage({ type: "update", data });
        };

        killAllSound = function () {
          soundProcessor.port.postMessage({ type: "kill:all" });
        };

        // Request data / send message to the mic processor thread.
        requestSpeakerWaveforms = function () {
          soundProcessor.port.postMessage({ type: "get-waveforms" });
        };

        requestSpeakerAmplitudes = function () {
          soundProcessor.port.postMessage({ type: "get-amplitudes" });
        };

        soundProcessor.port.onmessage = ({ data: msg }) => {
          if (msg.type === "waveforms") {
            send({ type: "speaker-waveforms", content: msg.content });
            return;
          }

          if (msg.type === "amplitudes") {
            send({ type: "speaker-amplitudes", content: msg.content });
            return;
          }

          if (msg.type === "metronome") {
            diskSupervisor.requestBeat?.(msg.content); // Update metronome.
            return;
          }
        };

        soundProcessor.connect(audioStreamDest); // Connect to the mediaStream.

        soundProcessor.connect(audioContext.destination);

        audioContext.resume();

        modal.classList.remove("on");
      })();
    } catch (e) {
      coneole.log("Sound failed to initialize:", e);
    }

    function enableAudioPlayback(skip = false) {
      if (backgroundMusicEl.paused && currentBackgroundTrack !== null) {
        backgroundMusicEl.play();
      }
      if (!skip && ["suspended", "interrupted"].includes(audioContext.state)) {
        audioContext.resume();
      }
    }

    enableAudioPlayback(true);
    //

    window.addEventListener("pointerdown", enableAudioPlayback);
    window.addEventListener("keydown", enableAudioPlayback);
  }

  // TODO: Add mute
  // function mute() {
  //   audioContext.suspend();
  //   // Or... audioContext.resume();
  // }

  // Try to load the disk boilerplate as a worker first.
  // Safari and FF support is coming for worker module imports: https://bugs.webkit.org/show_bug.cgi?id=164860
  //const worker = new Worker("./aesthetic.computer/lib/disk.js", {
  //  type: "module",
  //});
  const fullPath = "/aesthetic.computer/lib/disk.mjs" + "#" + Date.now(); // bust the cache. This prevents an error related to Safari loading workers from memory.

  const firstMessage = {
    type: "init-from-bios",
    content: {
      parsed,
      debug,
      rootPiece: window.acSTARTING_PIECE,
      user: window.acUSER,
    },
  };

  const onMessage = (m) => receivedChange(m);

  let send;

  // 🔥 Optionally use workers or not.
  // Always use workers if they are supported, except for
  // when we are in VR (MetaBrowser).
  const sandboxed = window.origin === "null";

  // Disable workers if we are in a sandboxed iframe.
  const workersEnabled = !sandboxed;
  // const workersEnabled = false;

  if (!MetaBrowser && workersEnabled) {
    const worker = new Worker(new URL(fullPath, window.location.href), {
      type: "module",
    });

    // Rewire things a bit if workers with modules are not supported (Firefox).
    worker.onerror = async (err) => {
      if (
        err.message ===
        "SyntaxError: import declarations may only appear at top level of a module"
      ) {
        console.warn(
          "🟡 Disk module workers unsupported in this browser. Using a dynamic import..."
        );
        // https://bugzilla.mozilla.org/show_bug.cgi?id=1247687
        const module = await import(`./lib/disk.mjs`);
        module.noWorker.postMessage = (e) => onMessage(e); // Define the disk's postMessage replacement.
        send = (e) => module.noWorker.onMessage(e); // Hook up our post method to disk's onmessage replacement.
        send(firstMessage);
      } else {
        console.error("🛑 Disk error:", err);
        // TODO: Try and save the crash here by restarting the worker
        //       without a full system reload?
      }
    };

    if (debug && worker.postMessage) console.log("🟢 Worker");

    send = (e, shared) => worker.postMessage(e, shared);

    worker.onmessage = onMessage;
  } else {
    // B. No Worker Mode
    if (debug) console.log("🔴 No Worker");
    const module = await import(`./lib/disk.mjs`);
    module.noWorker.postMessage = (e) => onMessage(e); // Define the disk's postMessage replacement.
    send = (e) => module.noWorker.onMessage(e); // Hook up our post method to disk's onmessage replacement.
  }

  // The initial message sends the path and host to load the disk.
  send(firstMessage);

  // Beat

  // Set the default bpm.
  sound.bpm = bpm;

  function requestBeat(time) {
    send(
      {
        type: "beat",
        content: {
          time,
          bpm: sound.bpm,
        },
      } //,
      //[sound.bpm] // TODO: Why not just send the number here?
    );
  }

  function receivedBeat(content) {
    // BPM
    if (sound.bpm !== content.bpm) {
      sound.bpm = content.bpm;
      updateMetronome(sound.bpm);
    }

    // SQUARE
    for (const sound of content.sounds) triggerSound(sound);
    for (const bubble of content.bubbles) updateBubble(bubble);
    for (const id of content.kills) killSound(id);
  }

  // Update & Render
  let frameAlreadyRequested = false;

  function requestFrame(needsRender, updateCount, nowUpdate) {
    now = nowUpdate;

    if (needsRender && needsReframe) {
      frame(undefined, undefined, lastGap);
      pen.retransformPosition();
      frameAlreadyRequested = false;
    }

    if (frameAlreadyRequested) return;

    frameAlreadyRequested = true;
    // frameCount += 1;

    // TODO: 📏 Measure performance of frame: test with different resolutions.

    // console.log("Sending frame...", frameCount, performance.now())

    // Grab a sample of any playing background music, calculate the frequency
    // and send as needed.
    let amplitude = 0;
    if (backgroundMusicEl.paused === false) {
      // Get the frequency data from the audio element
      analyser.getByteFrequencyData(frequencyData);

      // Calculate the maximum amplitude in the frequency data for this period.
      for (let i = 0; i < frequencyData.length; i += 1) {
        if (frequencyData[i] > amplitude) {
          amplitude = frequencyData[i];
        }
      }
    }

    // Transferrable objects
    const transferrableObjects = [screen.pixels.buffer];
    //const transferrableObjects = [];

    // TODO: Eventually make frequencyData transferrable?
    // if (frequencyData) {
    // transferrableObjects.push(frequencyData.buffer);
    // }

    send(
      {
        type: "frame",
        content: {
          needsRender,
          updateCount,
          pixels: screen.pixels.buffer,
          audioTime: audioContext?.currentTime,
          audioBpm: sound.bpm, // TODO: Turn this into a messaging thing.
          audioMusicAmplitude: amplitude,
          audioMusicSampleData: amplitude > 0 ? frequencyData : [],
          width: canvas.width,
          height: canvas.height,
          // TODO: Do all fields of `pointer` need to be sent? 22.09.19.23.30
          pen: { events: pen.events, pointers: pen.pointers },
          pen3d: ThreeD?.pollControllers(), // TODO: Implement pointers in 3D.
          hand: handData,
          keyboard: keyboard.events,
          clipboardText: pastedText,
        },
      },
      transferrableObjects
    );

    // if (Object.keys(pen.pointers).length > 1) {
    //   console.log(pen.events, pen.pointers);
    // }

    // Clear any pasted text.
    pastedText = undefined;

    pen.updatePastPositions();

    // Time budgeting stuff...
    //const updateDelta = performance.now() - updateNow;
    //console.log("Update Budget: ", round((updateDelta / updateRate) * 100));
    // TODO: Output this number graphically.

    //const renderNow = performance.now();
    //const renderDelta = performance.now() - renderNow;
    //console.log("Render Budget: ", round((renderDelta / renderRate) * 100));
    // TODO: Output this number graphically.

    //render3d();
    // Clear pen events.
    pen.events.length = 0;
    if (ThreeD?.penEvents) ThreeD.penEvents.length = 0;

    // Clear keyboard events.
    keyboard.events.length = 0;
  }

  let frameCached = false;
  let pixelsDidChange = false; // TODO: Can this whole thing be removed? 2021.11.28.03.50

  let contentFrame;
  let underlayFrame,
    underlayVideo = {};

  //const bakedCan = document.createElement("canvas", {
  //  willReadFrequently: true,
  //});

  // *** Received Frame ***
  async function receivedChange({ data: { type, content } }) {
    // Add a DOM event hitbox for the `Button Hitboxes`
    // event listener on the document.
    // 📓 Adding the same label multiple times will have no additional effect.
    if (type === "button:hitbox:add") {
      if (hitboxes[content.label] !== undefined) return;

      let state = "up";
      hitboxes[content.label] = async (e) => {
        const frame = canvas.getBoundingClientRect();
        const scale = projectedWidth / canvas.width;
        const hitbox = Box.from({
          x: frame.left + content.box.x * scale,
          y: frame.top + content.box.y * scale,
          w: content.box.w * scale,
          h: content.box.h * scale,
        });

        const hit = hitbox.contains({ x: e.x, y: e.y });

        if (e.type === "pointerup" && state === "down" && hit) {
          // This is pretty specific to the "copy" clipboard
          // stuff for now. 23.06.16.15.03
          try {
            await navigator.clipboard.writeText(content.message);
            send({ type: "copy:copied" });
          } catch (err) {
            send({ type: "copy:failed" });
          }
        } else if (e.type === "pointerdown" && hit) {
          state = "down";
        } else if (e.type === "pointerup" && !hit) {
          state = "up";
        }
      };

      return;
    }

    // Remove a hitbox via its label.
    if (type === "button:hitbox:remove") {
      delete hitboxes[content];
      return;
    }

    // Removed in favor of the above. 23.06.16.15.04
    // Copy text to clipboard.
    // if (type === "copy") {
    //   try {
    //     await navigator.clipboard.writeText(content);
    //     send({ type: "copy:copied" });
    //   } catch (err) {
    //     send({ type: "copy:failed" });
    //   }
    //   return;
    // }

    // Authenticate / signup or login a user.
    if (type === "login") {
      window.acLOGIN?.();
      return;
    }

    if (type === "signup") {
      window.acLOGIN?.("signup");
      return;
    }

    if (type === "logout") {
      window.acLOGOUT?.();
      window.flutter_inappwebview?.callHandler("closeWebview"); // Close A.C. webview on logout inside of Autonomy wallet.
      return;
    }

    // Send a locally opened file across the thread.
    if (type === "file-open:request") {
      const file = await openFile();
      send({
        type: "file-open:response",
        content: { data: file, result: file ? "success" : "error" },
      });
      return;
    }

    // Send a locally opened file across the thread.
    if (type === "file-encode:request") {
      let file;
      if (content.type === "png")
        file = await bufferToBlob(content.file, "image/png", content.modifiers);
      send({
        type: "file-encode:response",
        content: { data: file, result: file ? "success" : "error" },
      });
      return;
    }

    // Send a user authorization token (or undefined) across the thread.
    if (type === "authorization:request") {
      const token = await authorize();
      send({
        type: "authorization:response",
        content: { data: token, result: token ? "success" : "error" },
      });
      return;
    }

    // *** Route to different functions if this change is not a full frame update.
    if (type === "load-failure" && MetaBrowser) {
      document.querySelector("#software-keyboard-input")?.blur();
      return;
    }

    // if (type === "alert-popup:instagram" && Instagram) {
    //   window.alert(content);
    //   return;
    // }

    // Connect to an ethereum wallet extension.
    if (type === "web3-connect") {
      if (window.ethereum) {
        const addresses = await (typeof window.ethereum.request === "function"
          ? window.ethereum.request({ method: "eth_requestAccounts" })
          : window.ethereum.enable());

        const address = addresses[0];
        await loadWeb3(); // Load the web3.js library.
        // const w3 = new Web3(window.ethereum);

        // From: https://github.com/web3/web3.js/issues/2683#issuecomment-1304496119
        async function ensReverse(address) {
          const web3 = new Web3("https://eth.public-rpc.com/");
          const namehash = await web3.eth.call({
            to: "0x084b1c3c81545d370f3634392de611caabff8148", // ENS: Reverse Registrar
            data: web3.eth.abi.encodeFunctionCall(
              {
                name: "node",
                type: "function",
                inputs: [{ type: "address", name: "addr" }],
              },
              [address]
            ),
          });
          return web3.eth.abi.decodeParameter(
            "string",
            await web3.eth.call({
              to: "0xa2c122be93b0074270ebee7f6b7292c7deb45047", // ENS: Default Reverse Resolver
              data: web3.eth.abi.encodeFunctionCall(
                {
                  name: "name",
                  type: "function",
                  inputs: [{ type: "bytes32", name: "hash" }],
                },
                [namehash]
              ),
            })
          );
        }

        const ensName = await ensReverse(address);
        const id = ensName || address;
        if (debug) console.log("🕸️3️⃣ Connected to:", id);
        send({
          type: "web3-connect-response",
          content: { result: "success", id },
        });
      } else {
        send({ type: "web3-connect-response", content: { result: "error" } });
        console.warn(
          "🔴 Web3 is unavailable. Please install an Ethereum wallet or enable your extension."
        );
      }
      return;
    }

    if (type === "rewrite-url-path") {
      const newPath = content.path;
      if (window.origin !== "null")
        history.replaceState("", document.title, newPath);
      return;
    }

    if (type === "bgm-change") {
      playBackgroundMusic(content.trackNumber);
      return;
    }

    if (type === "bgm-stop") {
      stopBackgroundMusic();
      return;
    }

    if (type === "disk-defaults-loaded") {
      // Pen (also handles touch & pointer events)
      pen = new Pen((x, y) => {
        const p = {
          x: floor(((x - canvasRect.x) / projectedWidth) * screen.width),
          y: floor(((y - canvasRect.y) / projectedHeight) * screen.height),
        };
        return p;
      });

      // ⌨️ Keyboard
      keyboard = new Keyboard(() => currentPiece);
      {
        /**
         * Insert a hidden input element that is used to toggle the software
         * keyboard on touchscreen devices like iPhones and iPads.
         * *Only works in "disks/prompt".
         */
        const input = document.createElement("input");
        const form = document.createElement("form");
        form.id = "software-keyboard-input-form";
        form.style.opacity = 0;
        input.style.width = 0;
        input.style.height = 0;
        input.style.position = "absolute";

        input.id = "software-keyboard-input";
        input.type = "text";
        input.autocapitalize = "none";
        input.autocomplete = "off";
        input.style.opacity = 0;
        input.style.width = 0;
        input.style.height = 0;

        input.value = "_";

        form.append(input);
        wrapper.append(form);

        keyboard.input = input;

        // Generate an "Enter" keyboard event if the form was submitted.
        // - Don't use the submit event if we are sandboxed though!
        // - Which is required for the Meta Browser keyboard 23.02.08.12.30

        const enterEvent = {
          name: "keyboard:down:enter",
          key: "Enter",
          repeat: false,
          shift: false,
          alt: false,
          ctrl: false,
        };

        form.addEventListener("submit", (e) => {
          e.preventDefault();
          //console.log("SUBMIT", e);
          //if (!sandboxed) keyboard.events.push(enterEvent);
        });

        //if (sandboxed) {
        form.addEventListener("keydown", (e) => {
          if (e.key === "Enter") {
            e.preventDefault();
            const enter = { ...enterEvent };
            enter.shift = e.shiftKey;
            enter.alt = e.altKey;
            enter.ctrl = e.ctrlKey;
            keyboard.events.push(enter);
          }
        });
        //}

        input.addEventListener("input", (e) => {
          let input = e.data;

          const pressedKeys = [];

          if (e.inputType === "deleteContentBackward") {
            pressedKeys.push("Backspace");
          } else if (
            ["insertText", "insertCompositionText"].includes(e.inputType)
          ) {
            // Sanitize input if it arrives in chunks... like if it was dictated.
            // This is still basic, and is usable in the Meta Quest Browser. 22.10.24.17.07
            let sanitizedInput = input;
            if (input.length > 1) {
              sanitizedInput = input
                .trim()
                .toLowerCase()
                .replace(",", "")
                .replace(".", "");
              console.log("👄 Spoken / pasted input:", sanitizedInput);
            }

            [...sanitizedInput].forEach((chr) => pressedKeys.push(chr));
          }

          pressedKeys.forEach((pk) => {
            keyboard.events.push({
              name: "keyboard:down:" + pk.toLowerCase(),
              key: pk,
              repeat: false,
              shift: false,
              alt: false,
              ctrl: false,
            });
          });

          if (input === "Backspace") {
            e.target.value = e.target.value.slice(0, -1);
          } else {
            e.target.value += input;
          }
        });

        window.addEventListener("focusout", (e) => {
          input.blur();
        });

        window.addEventListener("focus", (e) => {
          // e.preventDefault();
        });

        window.addEventListener("pointerdown", (e) => {
          if (currentPieceHasKeyboard) e.preventDefault();
        });

        window.addEventListener("pointerup", (e) => {
          if (currentPieceHasKeyboard) e.preventDefault();
          if (currentPieceHasKeyboard && !keyboardFocusLock) {
            document.activeElement !== input ? input.focus() : input.blur();
          }
        });

        input.addEventListener("focus", (e) => {
          keyboard.events.push({ name: "keyboard:open" });
        });

        input.addEventListener("blur", (e) => {
          keyboard.events.push({ name: "keyboard:close" });
        });
      }

      // Turn off all layers onbeforeunload. (Prevents a white flicker in chrome.)
      window.addEventListener("beforeunload", (e) => {
        send({ type: "before-unload" });
        wrapper.remove();
      });

      // 🌒 Detect light or dark mode.
      // See also: https://flaviocopes.com/javascript-detect-dark-mode,
      //           https://developer.mozilla.org/en-US/docs/Web/CSS/@media/prefers-color-scheme

      if (
        window.matchMedia &&
        window.matchMedia("(prefers-color-scheme: dark)").matches
      ) {
        send({ type: "dark-mode", content: { enabled: true } });
      }

      window
        .matchMedia("(prefers-color-scheme: dark)")
        .addEventListener("change", (event) => {
          if (event.matches) {
            send({ type: "dark-mode", content: { enabled: true } });
          } else {
            send({ type: "dark-mode", content: { enabled: false } });
          }
        });

      // 📋 User pasting of content.
      window.addEventListener("paste", (event) => {
        pastedText = event.clipboardData.getData("text/plain");
      });

      // 🖥️ Display (Load the display, with 0 margin if sandboxed)
      frame(resolution?.width, resolution?.height, sandboxed ? 0 : undefined);

      // 🔊 Sound
      // TODO: Disable sound engine entirely... unless it is enabled by a disk. 2022.04.07.03.33
      // Only start this after a user-interaction to prevent warnings.
      window.addEventListener(
        "pointerdown",
        function down() {
          startSound();
        },
        { once: true }
      );

      diskSupervisor = { requestBeat, requestFrame };

      // ➰ Core Loops for User Input, Music, Object Updates, and Rendering
      Loop.start(
        () => {
          // TODO: What is this now?
          // pen.poll();
          // TODO: Key.input();
          // TODO: Voice.input();
        },
        function (needsRender, updateTimes, now) {
          // TODO: How can I get the pen data into the disk and back
          //       to Three.JS as fast as possible? 22.10.26.23.25
          diskSupervisor.requestFrame?.(needsRender, updateTimes, now);

          if (ThreeD?.status.alive === true && ThreeDBakeQueue.length > 0) {
            ThreeD.collectGarbage();
            // Bake all forms, while keeping track of baked forms, and any form that is missing after the queue ends needs to be cleared.
            const touchedForms = [];
            ThreeDBakeQueue.forEach((baker) => touchedForms.push(...baker()));
            ThreeD.checkForRemovedForms(touchedForms);
            ThreeDBakeQueue.length = 0;
            ThreeD?.render(now);
          }
        }
      );
    }

    // 💾 Disk Loading
    // Initialize some global stuff after the first piece loads.
    // Unload some already initialized stuff if this wasn't the first load.
    if (type === "disk-loaded") {
      currentPiece = content.path;
      currentPieceHasKeyboard = false;

      detachMicrophone?.(); // Remove any attached microphone.
      killAllSound?.(); // Kill any pervasive sounds in `speaker`.

      // ⚠️ Remove any sounds that aren't in the whitelist.
      keys(sfx).forEach((key) => {
        if (key !== sound) delete sfx[key];
      });
      if (logs.audio && debug) console.log("🔉 SFX Cleaned up:", sfx);

      // Reset preloading.
      window.waitForPreload = false;
      window.preloaded = false;

      // Clear any 3D content.
      ThreeD?.clear();

      // Kill the 3D engine.
      ThreeD?.kill();

      // Clear any DOM hitboxes added for Buttons that need
      // user interaction to trigger browser APIs. (Like clipboard)
      hitboxes = {};

      // Clear any DOM content that was added by a piece.
      contentFrame?.remove(); // Remove the contentFrame if it exists.
      contentFrame = undefined;

      underlayFrame?.remove(); // Remove the underlayFrame if it exists.
      underlayFrame = undefined;

      // Remove any event listeners added by the content frame.
      window?.acCONTENT_EVENTS.forEach((e) => e());
      window.acCONTENT_EVENTS = []; // And clear all events from the list.

      // Remove existing video tags.
      videos.forEach(({ video, buffer, getAnimationRequest }) => {
        console.log("🎥 Removing:", video, buffer, getAnimationRequest());

        if (video.srcObject) {
          const stream = video.srcObject;
          const tracks = stream.getTracks();
          tracks.forEach((track) => track.stop());
        }

        video.remove();

        // buffer.remove();
        cancelAnimationFrame(getAnimationRequest());
        handData = undefined; // Clear any handData.
      });

      videos.length = 0;
      // Note: Any other disk state cleanup that needs to take place on unload
      //       should happen here.

      // Reset the framing to a system default when unloading a disk if using
      // a customized resolution.
      // TODO: Do disks with custom resolutions need to be reset
      //       if they are being reloaded?

      if (fixedWidth && fixedHeight) {
        freezeFrame = true;
        freezeFrameGlaze = glaze.on;

        freezeFrameCan.width = imageData.width;
        freezeFrameCan.height = imageData.height;

        fixedWidth = undefined;
        fixedHeight = undefined;
        needsReframe = true;
      }

      if (lastGap !== 0) {
        // lastGap = 0; No longer needed... 22.10.04.15.28
        freezeFrame = true;
        freezeFrameCan.width = imageData.width;
        freezeFrameCan.height = imageData.height;
        needsReframe = true;
      }

      // Turn off glaze.
      glaze.on = false;

      canvas.style.removeProperty("opacity");

      // Clear pen events.
      pen.events.length = 0;

      // Clear keyboard events.
      keyboard.events.length = 0;

      // Clear when events.
      whens = {};

      // Close (defocus) software keyboard if we are NOT on the prompt.
      // debugger;
      if (content.text !== "prompt") {
        document.querySelector("#software-keyboard-input")?.blur();
      }
      keyboard.events.push({ name: "keyboard:close" });

      setMetatags(content.meta);

      // TODO: Make this automatic for pieces that use 3d.
      if (
        content.text === "wand" ||
        content.text.indexOf("wand") === 0 ||
        content.text === "oldwand" ||
        content.text.indexOf("oldwand") === 0
      ) {
        loadThreeD();
      }

      // Show an "audio engine: off" message.
      //if (content.noBeat === false && audioContext?.state !== "running") {
      //bumper.innerText = "audio engine off";
      //modal.classList.add("on");
      //}

      // Clear the ThreeD buffer.
      // ThreeD.clear();

      // Emit a push state for the old disk if it was not the first. This is so
      // a user can use browser history to switch between disks.
      if (content.pieceCount > 0) {
        if (content.fromHistory === false && window.origin !== "null") {
          history.pushState(
            "",
            document.title,
            content.text === "/prompt" ? "/" : "/" + content.text // Replace "prompt" with "/".
          );
        }

        // Replace the state if we are running an aliased `load` or `jump`.
        // (That doesn't avoids the history stack.)
        // Note: History state changes do not work in a sandboxed iframe.
        if (
          content.fromHistory === true &&
          content.alias === false &&
          window.origin !== "null"
        ) {
          history.replaceState(
            "",
            document.title,
            content.text === "/prompt" ? "/" : "/" + content.text // Replace "prompt" with "/".
          );
        }
      }

      UI.spinnerReset(); // Reset the timer on the yellow UI loading spinner.
      send({ type: "loading-complete" });
      return;
    }

    if (type === "forms") {
      const willBake = content.cam !== undefined;

      if (willBake) {
        // if (ThreeD?.status.alive === false) ThreeD.initialize(wrapper);

        // Add / update forms in a queue and then run all the bakes in render.
        ThreeDBakeQueue.push(() => {
          return ThreeD?.bake(content, screen, {
            width: projectedWidth,
            height: projectedHeight,
          });
        });

        //send({ type: "forms:baked", content: true });
      } else {
        //send({ type: "forms:baked", content: false });
      }

      // TODO: Measure the time this takes.
      //const pixels = ThreeD.bake(content, screen, {width: projectedWidth, height: projectedHeight});
      // bakedCan.width = screen.width;
      // bakedCan.height = screen.height;
      // bakedCtx.drawImage(ThreeD.domElement, 0, 0);
      // const pixels = bakedCtx.getImageData(0, 0, screen.width, screen.height).data;

      // send({
      //   type: "forms:baked",
      //   content: { width: screen.width, height: screen.height, pixels },
      // }, [pixels]);

      return;
    }

    if (type === "keyboard:enabled") {
      currentPieceHasKeyboard = true;
      return;
    }

    if (type === "keyboard:close") {
      keyboard?.input.blur();
      return;
    }

    if (type === "keyboard:lock") {
      keyboardFocusLock = true;
      console.log("⌨️ Virtual Keyboard: Locked");
      return;
    }

    if (type === "keyboard:unlock") {
      keyboardFocusLock = false;
      console.log("⌨️ Virtual Keyboard: Unlocked");
      return;
    }

    if (type === "gpu-event") {
      ThreeD?.handleEvent(content);
      return;
    }

    if (type === "content-create") {
      // Create a DOM container, if it doesn't already exist,
      // and add it here along with the requested content in the
      // template.
      if (!contentFrame) {
        contentFrame = document.createElement("div");
        contentFrame.id = "content";
        wrapper.appendChild(contentFrame);
        contentFrame.innerHTML += content.content; // Add content to contentFrame.
      } else {
        contentFrame.innerHTML += content.content; // Add content to contentFrame.
      }

      // Evaluate any added scripts inside of contentFrame.
      // TODO: This should only evaluate new scripts, as they are added...
      // It should also run if new scripts are added with the `html` function.
      const script = contentFrame.querySelector("script");

      if (script && !script.dataset.evaluated) {
        if (script?.src.length > 0) {
          const s = document.createElement("script");
          s.type = "module";
          // s.onload = callback; // s.onerror = callback;

          // The hash `time` parameter busts the cache so that the environment is
          // reset if a disk is re-entered while the system is running.
          // Why a hash? See also: https://github.com/denoland/deno/issues/6946#issuecomment-668230727
          s.src = script.src + "#" + Date.now();
          contentFrame.appendChild(s); // Re-insert the new script tag.
          script.remove(); // Remove old script element.
          s.dataset.evaluated = true;
        } else if (script?.innerText.length > 0) {
          window.eval(script.innerText);
          script.dataset.evaluated = true;
        }
      }

      send({
        type: "content-created",
        content: { id: content.id, response: "Content was made!" }, // TODO: Return an API / better object?
      });

      return;
    }

    if (type === "content-remove") {
      // Clear any DOM content that was added by a piece.
      contentFrame?.remove(); // Remove the contentFrame if it exists.
      contentFrame = undefined;
      // Remove any event listeners added by the content frame.
      window?.acCONTENT_EVENTS.forEach((e) => e());
      window.acCONTENT_EVENTS = []; // And clear all events from the list.
      return;
    }

    if (type === "signal") {
      if (debug) console.log("📻 Signal received:", content);
      if (whens[content]) {
        whens[content]();
        // delete whens[content]; // These shouldn't need to be deleted here. 22.10.04.23.04
      }
    }

    // I have a storage system where I can store data to localStorage (user settings),
    //                                                   indexedDB (large files)
    //                                                   remote (with user account or anonymous)

    // *** 🏪 Store: Persist ***
    if (type === "store:persist") {
      // Local Storage
      if (content.method === "local") {
        try {
          localStorage.setItem(content.key, JSON.stringify(content.data));
        } catch (e) {
          // console.warn(e);
        }

        if (debug && logs.store)
          console.log("📦 Persisted locally:", content, localStorage);
      }

      // IndexedDB
      // Potentially use this library: github.com/jakearchibald/idb
      // For images: https://hacks.mozilla.org/2012/02/storing-images-and-files-in-indexeddb/
      // Using: https://github.com/jakearchibald/idb
      // See also: web.dev/indexeddb-best-practices
      // TODO: Implement basic indexedDB storage and retrieval for the
      //       painting / array buffer.
      if (content.method === "local:db") {
        await Store.set(content.key, content.data);
        // const set = await Store.set(content.key, content.data);
        // const get = await Store.get(content.key);
        if (debug && logs.store)
          console.log("📦 Persisted on local:db:", content);
      }

      if (content.method === "remote:temporary") {
        // Upload to S3 with a code.
      }

      if (content.method === "remote:permanent") {
        // Combine token-gated web3 authentication here along
        // with IPFS storage.
        // This would be used as part of a `mint` function. 22.10.04.11.31
      }

      return;
    }

    // Store: Retrieve
    if (type === "store:retrieve") {
      if (content.method === "local") {
        let data;

        try {
          data = JSON.parse(localStorage.getItem(content.key));
        } catch (err) {
          // console.warn(err);
          // Probably in a sandboxed environment here...
        }

        if (debug && logs.store)
          console.log("📦 Retrieved local data:", content.key, data);
        send({
          type: "store:retrieved",
          content: data,
        });
      }

      if (content.method === "local:db") {
        const retrievedContent = await Store.get(content.key);
        if (debug && logs.store)
          console.log(
            "📦 Retrieved local:db data:",
            content.key,
            retrievedContent
          );
        send({ type: "store:retrieved", content: retrievedContent });
      }

      return;
    }

    // Store: Delete
    if (type === "store:delete") {
      if (content.method === "local") {
        if (debug && logs.store)
          console.log("📦 Delete local data:", content.key);
        send({
          type: "store:deleted",
          content: true,
        });
        // Just assume this is deleted.
      }

      if (content.method === "local:db") {
        const hasKey = (await Store.keys())?.includes(content.key);
        let deleted;

        if (hasKey) {
          await Store.del(content.key);
          const alteredKeys = await Store.keys();
          deleted = !alteredKeys.includes(content.key);
        } else {
          deleted = false;
        }

        if (debug && logs.store)
          console.log("📦 Delete local:db data:", content.key, deleted);
        send({
          type: "store:deleted",
          content: deleted,
        });
      }

      return;
    }

    if (type === "meta") {
      setMetatags(content);
      return;
    }

    if (type === "refresh") {
      window.location.reload();
      return;
    }

    if (type === "web") {
      // TODO: Eventually add pop-up support here. 23.05.08.17.08
      window.location.href = content;
      return;
    }

    if (type === "preload-ready") {
      window.preloaded = true;
      if (debug) console.log("⏳ Preloaded: ✅️");
      return;
    }

    if (type === "wait-for-preload") {
      window.waitForPreload = true;
      return;
    }

    if (type === "beat") {
      receivedBeat(content);
      return;
    }

    if (type === "beat:update") {
      updateSound(content);
      return;
    }

    if (type === "download") {
      receivedDownload(content);
      return;
    }

    if (type === "upload") {
      receivedUpload(content);
    }

    if (type === "import") {
      receivedImport(content);
      return;
    }

    if (type === "microphone") {
      receivedMicrophone(content);
      return;
    }

    if (type === "get-microphone-amplitude") {
      requestMicrophoneAmplitude?.();
      return;
    }

    if (type === "get-microphone-waveform") {
      requestMicrophoneWaveform?.();
      return;
    }

    if (type === "get-speaker-waveforms") {
      requestSpeakerWaveforms?.();
      return;
    }

    if (type === "get-speaker-amplitudes") {
      requestSpeakerAmplitudes?.();
      return;
    }

    if (type === "get-microphone-pitch") {
      requestMicrophonePitch?.();
      return;
    }

    if (type === "video") {
      receivedVideo(content);
      return;
    }

    if (type === "recorder:rolling") {
      if (mediaRecorder && mediaRecorder.state === "paused") {
        mediaRecorder.resume();
        return;
      }

      // TODO: To add it to a canvas...
      //       look into using "content" or options.

      // recorder.start();

      // https://developer.mozilla.org/en-US/docs/Web/API/HTMLCanvasElement/captureStream
      // console.log(content);
      // let audioTrack = dest.stream.getAudioTracks()[0];
      // add it to your canvas stream:
      // canvasStream.addTrack(audioTrack);
      // use your canvas stream like you would normally:
      // let recorder = new MediaRecorder(canvasStream);

      let mimeType;

      if (content === "audio" || content === "video") {
        if (MediaRecorder.isTypeSupported(content + "/mp4")) {
          mimeType = content + "/mp4"; // This is the setup for Safari.
        } else if (MediaRecorder.isTypeSupported(content + "/webm")) {
          mimeType = content + "/webm"; // And for Chrome & Firefox.
          // mimeType = content + "/webm; codecs=h264"; // Possible optimization to change the container to mp4 without re-encoding.
        } else {
          console.error("🔴 Mimetypes mp4 and webm are unsupported.");
        }
      } else {
        console.error("🔴 Option must be 'audio' or 'video'.");
      }

      let options;

      if (content === "audio") {
        options = {
          audioBitsPerSecond: 128000,
          mimeType,
        };
        mediaRecorder = new MediaRecorder(audioStreamDest.stream, options);
      } else if (content === "video") {
        // Currently always includes an audio track by default.
        options = {
          audioBitsPerSecond: 128000,
          // videoBitsPerSecond: 2500000,
          videoBitsPerSecond: 5000000,
          mimeType,
        };

        // *** streamCanvas Recording Settings ***
        // TODO: Examine the canvas stream resolution?

        // TODO: Duplicate / resize canvas as needed?
        // TODO: What happens upon resize?
        // TODO: Only set the streamCanvas here if it is below a certain
        //       resolution.
        streamCanvasContext = document.createElement("canvas").getContext("2d");

        // TODO: If we aren't using TikTok, then just find a good resolution / double
        // the pixels as needed. 22.08.11.03.38
        streamCanvasContext.canvas.width = canvas.width * 4;
        streamCanvasContext.canvas.height = canvas.height * 4;

        // Must be set after resize.
        streamCanvasContext.imageSmoothingEnabled = false;

        // Portrait Mode / TikTok (this is the default for recording)
        // This is hardcoded at 1080p for TikTok right now.
        // streamCanvasContext.canvas.width = 1080;
        // streamCanvasContext.canvas.height = 1920;

        // Draw into the streamCanvas buffer from the normal canvas,
        // Leaves black bars, resizing it to the frame of the streamCanvas.
        resizeToStreamCanvas = function () {
          const frameWidth = streamCanvasContext.canvas.width;
          const frameHeight = streamCanvasContext.canvas.height;
          const frameAspectRatio = frameHeight / frameWidth;
          const aspectRatio = canvas.height / canvas.width;

          if (frameAspectRatio > aspectRatio) {
            const height = streamCanvasContext.canvas.width * aspectRatio;
            streamCanvasContext.drawImage(
              canvas,
              0,
              frameHeight / 2 - height / 2,
              streamCanvasContext.canvas.width,
              height
            );
          } else {
            const width = streamCanvasContext.canvas.height / aspectRatio;
            streamCanvasContext.drawImage(
              canvas,
              frameWidth / 2 - width / 2,
              0,
              width,
              streamCanvasContext.canvas.height
            );
          }
        };

        const canvasStream = streamCanvasContext.canvas.captureStream(30);

        canvasStream.addTrack(audioStreamDest.stream.getAudioTracks()[0]);
        mediaRecorder = new MediaRecorder(canvasStream, options);
      }

      const chunks = []; // Store chunks of the recording.

      mediaRecorder.ondataavailable = (evt) => {
        if (evt.data.size > 0) {
          if (debug) console.log("🔴 Recorder: Data", evt.data);
          chunks.push(evt.data);
          mediaRecorderDataHandler?.(chunks);
        }
      };

      let recordingStartTime = 0;
      let recordingDuration;

      // 🗺️ mediaRecorder:Start
      mediaRecorder.onstart = function () {
        recordingStartTime = performance.now();
        send({ type: "recorder:rolling:started", content });
        if (debug) console.log("🔴 Recorder: Rolling", content);
      };

      // 🗺️ mediaRecorder:Stop (Recorder Printing)
      mediaRecorder.onstop = async function (evt) {
        recordingDuration = (performance.now() - recordingStartTime) / 1000;

        // Reset global streamCanvas state.
        streamCanvasContext = undefined;
        resizeToStreamCanvas = null;

        let blob = new Blob(chunks, {
          type: options.mimeType,
        });

        // Load FFmpeg so the recording can be transcribed to a proper video format.
        if (content === "video") {
          if (options.mimeType === "video/mp4") {
            console.warn("Encoding can be skipped!");
            // TODO: Skip encoding.
          }

          const { createFFmpeg, fetchFile } = await loadFFmpeg();

          let transcodeProgress = 0;

          const ffmpeg = createFFmpeg({
            log: debug,
            progress: (p) => {
              // Send a message to the piece that gives the transcode progress.
              let time = p.time;
              if (time === undefined) {
                if (transcodeProgress === 0) {
                  time = 0;
                } else {
                  time = recordingDuration;
                }
              }
              transcodeProgress = min(1, time / recordingDuration);
              send({
                type: "recorder:transcode-progress",
                content: transcodeProgress,
              });
            },
          });

          ffmpeg.setLogging(debug); // Enable ffmpeg logging only if we are in `debug` mode.

          await ffmpeg.load();
          ffmpeg.FS("writeFile", "input.video", await fetchFile(blob));

          await ffmpeg.run(
            "-i",
            "input.video",
            "-movflags",
            "+faststart",
            "-vf",
            // General shaving to make even sides (required by the pixel format)
            // "pad=ceil(iw/2)*2:ceil(ih/2)*2",
            // TikTok
            //"fps=30, scale=1080x1920:flags=neighbor:force_original_aspect_ratio=decrease, pad=1080:1920:(ow-iw)/2:(oh-ih)/2",
            "fps=30",
            "output.mp4"
          );
          // Notes on these options:
          // width expression: https://stackoverflow.com/a/20848224/8146077
          // scaling: https://trac.ffmpeg.org/wiki/Scaling
          // general info: https://avpres.net/FFmpeg/im_H264

          const file = ffmpeg.FS("readFile", "output.mp4");

          blob = new Blob([file.buffer], { type: "video/mp4" }); // Re-assign blob.
        }

        // Add the recording wrapper to the DOM, among other recordings that may exist.
        // const recordings = wrapper.querySelector("#recordings");
        //let recordingsWrapper;

        //if (recordings) {
        //  recordingsWrapper = recordings.querySelector("#recordings-wrapper");
        //} else {
        //  const recordingsEl = document.createElement("div");
        //  recordingsEl.id = "recordings";

        //  recordingsWrapper = document.createElement("div");
        //  recordingsWrapper.id = "recordings-wrapper";

        //  recordingsEl.append(recordingsWrapper); // Add wrapper.
        //  wrapper.append(recordingsEl); // Add recordings to DOM.
        //}

        // Add download link.
        //const download = document.createElement("a");
        //download.href = URL.createObjectURL(blob);
        //download.innerText = "DOWNLOAD VIDEO";
        //download.download = "test.mp4";
        //mediaRecorderDownload = download;
        //recordingsWrapper.append(download);

        // TODO: Clicking a button should trigger the download.

        // TODO: Add UI for downloading the file.

        // 🕸️ Upload the video to a bucket...
        // TODO: There needs to be a progress bar or spinner or button to
        //       upload the video.
        /*
        fetch("/presigned-upload-url/" + "mp4")
          .then(async (res) => {
            const presignedUrl = (await res.json()).uploadURL;
            if (debug) console.log("🔐 Presigned URL:", presignedUrl);

            const response = await fetch(presignedUrl, {
              method: "PUT",
              headers: {
                "Content-Type": "video/mp4",
                "x-amz-acl": "public-read",
              },
              body: blob,
            });

            if (debug) console.log("📼 Video uploaded:", response);
          })
          .catch((err) => {
            if (debug) console.log("⚠️ Failed to get presigned URL:", err);
          });
        */

        if (debug) console.log("📼 Recorder: Printed");

        mediaRecorderBlob = blob;

        send({ type: "recorder:printed" });
        // TODO: Can send the download code back here...
        // send({ type: "recorder:uploaded", code });

        mediaRecorder = undefined; // ❌ Trash the recorder.
      };

      mediaRecorder.start();
      return;
    }

    if (type === "recorder:cut") {
      if (!mediaRecorder) return;
      if (debug) console.log("✂️ Recorder: Cut");
      setTimeout(() => {
        mediaRecorder.pause();
        send({ type: "recorder:rolling:ended" });
      }, 250);
      return;
    }

    if (type === "recorder:present") {
      // TODO: Add a DOM preview here.
      if (mediaRecorder && mediaRecorder.state === "paused") {
        const type = mediaRecorder.mimeType.split("/")[0];

        if (type === "video") {
          underlayFrame = document.createElement("div");
          underlayFrame.id = "underlay";
          underlayVideo = {}; // Reset underlayVideo state.

          const el = document.createElement(type); // "audio" or "video"
          el.autoplay = true; // Allow video footage play automatically.
          el.setAttribute("playsinline", ""); // Only for iOS.
          el.loop = true;
          // el.setAttribute("muted", ""); // Only for iOS.
          // el.volume = 0;
          // el.controls = true;

          el.addEventListener("play", () => {
            send({ type: "recorder:present-playing" });
            underlayVideo.played = true;
          });

          el.addEventListener("pause", () => {
            send({ type: "recorder:present-paused" });
          });

          el.addEventListener(
            "pointerdown",
            () => {
              // Note: Just always leave this playing once it starts...
              if (el.paused) el.play();
              // TODO: This is hacky. Will do for now. 23.01.29.16.06
              /*
            if (underlayVideo.needsPlay || (el.paused && !underlayVideo.played)) {
              try {
                el.play();
              } catch (e) {
                console.warn(e);
              }
              delete underlayVideo.needsPlay;
            } else if (underlayVideo.needsPause) {
              el.pause();
              delete underlayVideo.needsPause;
            }
            */
            },
            { once: true }
          );

          // Try running everything through the audioContext even if
          // the video is muted.
          // if (audioContext) {
          // const source = audioContext.createMediaElementSource(el);
          // const gainNode = audioContext.createGain();
          // source.connect(gainNode);
          // source.connect(audioContext.destination);
          // }

          // Active recording...
          mediaRecorderDataHandler = (chunks) => {
            const blob = new Blob(chunks, {
              type: mediaRecorder.mimeType,
            });

            el.src = URL.createObjectURL(blob);

            // Report the progress of this element back to the `disk`.
            window.requestAnimationFrame(function update() {
              // Note: Reading el.currentTime seems a little delayed...
              //       and returns NaN early on.
              const content = el.currentTime / el.duration;
              send({ type: "recorder:present-progress", content });
              if (underlayFrame) window.requestAnimationFrame(update);
            });

            el.play(); // Attempt to play the video (won't work on mobile).
            // Note: Video plays via recorder:present:play and pause events.
          };

          mediaRecorder.requestData();

          // el.srcObject = mediaRecorder.stream; // Live feed.
          // 🧠 Eventually this could be useful for live feedback? 23.01.27.16.59

          underlayFrame.appendChild(el);

          wrapper.appendChild(underlayFrame);

          send({ type: "recorder:presented" });
        }
      }
      return;
    }

    if (type === "recorder:present:play") {
      underlayVideo.needsPlay = true;
      return;
    }

    if (type === "recorder:present:pause") {
      underlayVideo.needsPause = true;
      return;
    }

    if (type === "recorder:unpresent") {
      if (underlayFrame) {
        const media = underlayFrame.querySelector("video, audio");
        if (media?.src) URL.revokeObjectURL(media.src);

        underlayFrame?.remove(); // Remove the underlayFrame if it exists.
        underlayFrame = undefined;
        send({ type: "recorder:unpresented" });
      }
    }

    if (type === "recorder:print") {
      if (!mediaRecorder) return;
      mediaRecorder.stop(); // Render a video if a recording exists.
      // mediaRecorder = undefined;
      send({ type: "recorder:printing:started" });
      return;
    }

    if (type === "recorder:slate") {
      // mediaRecorder = undefined;
    }

    if (type === "load-bitmap") {
      const img = document.createElement("img");
      img.src = content;
      img.crossOrigin = "anonymous";
      img.addEventListener("load", async () => {
        const bitmap = await imgToBitmap(img);
        send(
          {
            type: "loaded-bitmap-success",
            content: { url: content, img: bitmap },
          },
          [bitmap.pixels.buffer]
        );
      });
      img.addEventListener("error", () => {
        send({ type: "loaded-bitmap-rejection", content: { url: content } });
      });

      return;
    }

    // Load a sound from a url.
    if (type === "load-sfx") {
      let internal = false;

      for (let wl of soundWhitelist) {
        if (content === wl) {
          internal = true;
          break;
        }
      }

      let url;
      if (internal) {
        url = `/sounds/AeCo_${content}.m4a`;
        if (window.production === true) {
          url = `https://assets.aesthetic.computer` + url;
        } else {
          url = `/assets` + url;
        }
      } else url = content;

      fetch(url)
        .then((response) => {
          return response.arrayBuffer();
        })
        .then(async (arrayBuffer) => {
          try {
            if (!audioContext) {
              sfx[content] = arrayBuffer;
              send({
                type: "loaded-sfx-success",
                content: { url, sfx: content },
              });
            } else {
              const audioBuffer = await audioContext.decodeAudioData(
                arrayBuffer
              );
              sfx[content] = audioBuffer;
              send({
                type: "loaded-sfx-success",
                content: { url, sfx: content /*buffer: audioBuffer*/ },
              });
            }
          } catch (error) {
            send({
              type: "loaded-sfx-rejection",
              content: { sfx: content },
            });
          }
        })
        .catch((error) => {
          send({ type: "loaded-sfx-rejection", content: { sfx: content } });
        });

      return;
    }

    // Trigger a sound to playback.
    if (type === "play-sfx") {
      if (audioContext) {
        // Instantly decode the audio before playback if it hasn't been already.
        if (sfx[content.sfx] instanceof ArrayBuffer) {
          let audioBuffer;
          try {
            audioBuffer = await audioContext.decodeAudioData(sfx[content.sfx]);
            if (debug && logs.audio) console.log("🔈 Decoded:", content.sfx);
            sfx[content.sfx] = audioBuffer;
          } catch (err) {
            console.error("🔉 Error: ", err);
          }
        }

        if (sfx[content.sfx] instanceof ArrayBuffer) return;
        // If decoding has failed or no sound is present then silently fail.

        const source = audioContext.createBufferSource();
        source.buffer = sfx[content.sfx]; // 'content.buffer' is supposed to be the AudioBuffer you've received in 'loaded-sfx-success' message
        source.connect(audioContext.destination);
        source.addEventListener("ended", () => source.disconnect());
        if (debug && logs.audio) console.log("🔈 Playing:", content.sfx);
        source.start();
      }
    }

    // This method does not load remote images from different origins.
    // if (type === "load-bitmap") {
    //   fetch(content, { mode: "no-cors" }).then(async (response) => {
    //     if (!response.ok) {
    //       send({
    //         type: "loaded-bitmap-rejection",
    //         content: { url: content },
    //       });
    //     } else {
    //       const blob = await response.blob();
    //       const img = await toBitmap(blob);

    //       send(
    //         {
    //           type: "loaded-bitmap-success",
    //           content: {
    //             url: content,
    //             img,
    //           },
    //         },
    //         [img.pixels.buffer]
    //       );
    //     }
    //   });
    //   return;
    // }

    if (type === "fullscreen-toggle") {
      curReframeDelay = 0;
      toggleFullscreen();
      return;
    }

    if (type === "fps-change") {
      console.log("🎞️ FPS:", content);
      Loop.frameRate(content);
      return;
    }

    // TODO: How can these updates be made more
    //       instant? 22.09.25.15.43
    // So that after calling them in the API, drawing can still happen...

    // TODO: They need to be synchronous / just use basic numbers
    //       that the disks already have.
    //       The pixel buffers can be updated instantly in the thread...

    //       And glaze and other functions can stay asynchronous, so no
    //       pixels are lost.

    /*
    if (type === "gap-change") {
      if (gap !== content) {
        if (debug) console.log("🕳️ Gap:", content);
        gap = content;
        needsReframe = true;
      }
      return;
    }
    */

    if (type === "glaze") {
      if (debug) {
        console.log("🪟 Glaze:", content, "Type:", content.type || "prompt");
      }
      glaze = content;
      if (glaze.on === false) {
        Glaze.off();
        canvas.style.removeProperty("opacity");
      }
      // Note: Glaze gets turned on only on a call to `resize` or `gap` via a piece.
      return;
    }

    if (type === "disk-loaded-and-booted") {
      // Skip preload marker on default init piece, and toggle it if necessary.
      if (currentPiece !== null && !window.waitForPreload)
        window.preloaded = true;
      if (debug && logs.loading)
        console.log("⏳ Preloaded:", window.preloaded ? "✅" : "❌");
      return;
    }

    if (type === "back-to-piece") {
      history.back();
      return false;
    }

    if (type === "disk-unload") {
      return;
    }

    // BIOS:RENDER
    // 🌟 Assume `type === render` from now on.
    if (!content) return;

    // This is a bit messy compared to what happens inside of content.reframe -> frame below. 22.10.27.02.05
    if (content.pixels?.byteLength > 0) {
      screen.pixels = new Uint8ClampedArray(content.pixels);
      let width = screen.width;
      let height = screen.height;
      if (content.reframe && content.reframe.width && content.reframe.height) {
        width = content.reframe.width;
        height = content.reframe.height;
      }
      imageData = new ImageData(screen.pixels, width, height);
      // console.log("cp", content.pixels, content, type);
    }

    // old threed garbage collection (remove)

    // Check for a change in resolution.
    if (content.reframe) {
      // Reframe the captured pixels.
      frame(content.reframe.width, content.reframe.height, content.reframe.gap);
      pen.retransformPosition();
    }

    if (content.cursorCode) pen.setCursorCode(content.cursorCode);

    // Abort the render if pixels don't match.
    if (
      content.dirtyBox === undefined &&
      content.pixels?.length !== undefined &&
      content.pixels?.length !== screen.pixels.length
    ) {
      console.warn("Aborted render. Pixel buffers did not match.");
      console.log(
        "Content pixels:",
        content.pixels.length,
        "Screen:",
        screen.pixels.length,
        content.didntRender,
        content.reframe,
        "Freeze:",
        freezeFrame
      );
      frameAlreadyRequested = false; // 🗨️ Tell the system we are ready for another frame.
      return;
    }

    let dirtyBoxBitmapCan;

    // 👌 Otherwise, grab all the pixels, or some, if `dirtyBox` is present.
    if (content.dirtyBox) {
      // 🅰️ Cropped update.
      const imageData = new ImageData(
        new Uint8ClampedArray(content.pixels), // Is this the only necessary part?
        content.dirtyBox.w,
        content.dirtyBox.h
      );

      // Paint everything to a secondary canvas buffer.
      // TODO: Maybe this should be instantiated when the system starts to better
      //       optimize things? (Only if it's ever slow...)
      // TODO: Use ImageBitmap objects to make this faster once it lands in Safari.
      dirtyBoxBitmapCan = document.createElement("canvas");
      dirtyBoxBitmapCan.width = imageData.width;
      dirtyBoxBitmapCan.height = imageData.height;

      const dbCtx = dirtyBoxBitmapCan.getContext("2d");
      dbCtx.putImageData(imageData, 0, 0);

      // Use this alternative once it's faster. 2022.01.29.02.46
      // const dbCtx = dirtyBoxBitmapCan.getContext("bitmaprenderer");
      // dbCtx.transferFromImageBitmap(dirtyBoxBitmap);
    } else if (content.paintChanged && content.pixels) {
      // 🅱️ Normal full-screen update.
      imageData = new ImageData(
        new Uint8ClampedArray(content.pixels),
        canvas.width,
        canvas.height
      );
    }

    pixelsDidChange = content.paintChanged || false;

    // UI Overlay (Composite) Layer
    let paintOverlay;
    if (content.label) {
      const overlay = new ImageData(
        content.label.img.pixels,
        content.label.img.width,
        content.label.img.height
      );

      // TODO: This could be instantiated elsewhere if it's ever slow... similar to dirtyBoxBitmapCam's potential optimization above. 23.01.30.21.32
      const overlayCan = document.createElement("canvas");
      const octx = overlayCan.getContext("2d");
      overlayCan.width = content.label.img.width;
      overlayCan.height = content.label.img.height;
      octx.imageSmoothingEnabled = false;
      octx.putImageData(overlay, 0, 0);

      paintOverlay = () => {
        ctx.drawImage(overlayCan, content.label.x, content.label.y);
      };
    }

    function draw() {
      // 🅰️ Draw updated content from the piece.

      const db = content.dirtyBox;
      if (db) {
        ctx.drawImage(dirtyBoxBitmapCan, db.x, db.y);
        paintOverlay?.();
        if (glaze.on) Glaze.update(dirtyBoxBitmapCan, db.x, db.y);
      } else if (pixelsDidChange) {
        ctx.putImageData(imageData, 0, 0); // Comment out for a `dirtyBox` visualization.

        paintOverlay?.();

        if (glaze.on) {
          ThreeD?.pasteTo(glazeCompositeCtx);
          glazeCompositeCtx.drawImage(canvas, 0, 0);
          Glaze.update(glazeComposite);
        }

        // TODO: Is this actually updating with a blank image at first? How to prevent the glaze.clear flicker? 2022.6.8
      }

      if (glaze.on) {
        Glaze.render(now, pen.normalizedPosition(canvasRect));
      } else {
        Glaze.off();
      }

      // 🅱️ Draw anything from the system UI layer on top.

      const dpi = window.devicePixelRatio;

      uiCtx.scale(dpi, dpi);

      uiCtx.clearRect(0, 0, 64, 64); // Clear 64 pixels from the top left to remove any
      //                                previously rendered corner icons.

      uiCtx.clearRect(0, uiCtx.canvas.height / dpi - 64, 64, 64); // Clear 64 pixels from the bottom left to remove any
      //                                previously rendered corner icons.

      uiCtx.clearRect(uiCtx.canvas.width / dpi - 64, 0, 64, 64);
      // Also clear 64 pixels from the top right to remove any previously rendered corner icons.

      pen.render(uiCtx, canvasRect); // ️ 🐭 Draw the cursor.

      // Show the spinner on any piece other than the first.
      if (content.loading === true && currentPiece !== null) {
        UI.spinner(uiCtx, now);
      }

      if (debug && frameCached && content.loading !== true) UI.cached(uiCtx); // Pause icon.

      uiCtx.resetTransform();

      if (typeof resizeToStreamCanvas === "function") {
        resizeToStreamCanvas();
      }
    }

    if (pixelsDidChange || pen.changedInPiece) {
      frameCached = false;
      pen.changedInPiece = false;
      draw();
    } else if (frameCached === false) {
      frameCached = true;
      draw();
      //console.log("Caching frame...");
      // } else if (content.loading === true && debug === true) {
    } else if (content.loading === true) {
      draw();
    } else if (frameCached === true) {
      //draw(); // TODO: This is causing stuttering.
      // console.log("Cached...");
    }

    // Hide the freezeFrame.
    if (freezeFrame && freezeFrameFrozen) {
      if (glaze.on === false) {
        //canvas.style.removeProperty("opacity");
      }
      //freezeFrameCan.style.opacity = 0;
      freezeFrameCan.remove();
      freezeFrame = false;
      freezeFrameGlaze = false;
      freezeFrameFrozen = false;
    }

    if (glaze.on) {
      Glaze.unfreeze();
    } else {
      canvas.style.removeProperty("opacity");
    }

    // TODO: Put this in a budget / progress bar system, related to the current refresh rate.

    if (needsReappearance && wrapper.classList.contains("hidden")) {
      wrapper.classList.remove("hidden");
      needsReappearance = false;
    }

    //frameCount += 1;
    frameAlreadyRequested = false; // 🗨️ Tell the system we are ready for another frame.

    //if (needsRender)

    //if (lastRender) {
    //console.log(performance.now() - lastRender)
    //}
    //lastRender = performance.now()
  }

  // Reads a file and uploads it to the server.
  async function receivedUpload(
    { filename, data, bucket },
    callbackMessage = "upload"
  ) {
    console.log("📤 Uploading globally:", filename, typeof data || "...");
    const ext = extension(filename);
    let MIME = "application/octet-stream"; // Default content type.

    if (ext === "json") {
      // JSON
      MIME = "application/json";

      // Parse JSON strings if they haven't been already, including support for `bigints`.
      if (typeof data !== "string") {
        data = JSON.stringify(
          data,
          (k, v) => (typeof v === "bigint" ? v.toString() : v)
          // 2 // Also make sure we indent by 2 spaces so it's nicely formatted.
        );
      }
    }

    if (ext === "obj") MIME = "application/object";
    if (ext === "glb") MIME = "model/gltf-binary";

    if (ext === "mp4") {
      MIME = "video/mp4";
      data = mediaRecorderBlob;
    }

    if (ext === "png") {
      MIME = "image/png";
      data = await bufferToBlob(data, MIME); // Could be adding modifiers here...
    }

    let prefetchURL = "/presigned-upload-url/" + ext;

    if (bucket === "wand") prefetchURL += "/" + filename + "/" + bucket; // Add filename info.

    // if (bucket === undefined) prefetchURL += "/" + filename; // "art" bucket.
    // 📓 This is handled on the server if an empty bucket is sent.

    // Authorization: Check to see if we will use a user or a guest bucket.
    const headers = {};
    const token = await authorize();
    if (token) {
      bucket = "user";
      headers.Authorization = `Bearer ${token}`;
      // This filename gets sorted into the user bucket via their own
      // directory upon uploading.
      prefetchURL += "/" + filename + "/" + bucket; // Add filename info.
    }

    function error(err) {
      send({
        type: callbackMessage,
        content: { result: "error", data: err },
      });
    }

    // Now send a request to the server...
    fetch(prefetchURL, { headers })
      .then(async (res) => {
        const resData = await res.json();

        const presignedUrl = resData.uploadURL;

        // Probably the download code... maybe something else if a custom
        // name is used.
        const url = new URL(presignedUrl);
        const filename = url.pathname.split("/").pop();
        const slug = filename.substring(0, filename.lastIndexOf("."));
        const path = url.pathname;

        if (debug) console.log("🔐 Presigned URL:", presignedUrl);

        const xhr = new XMLHttpRequest();
        xhr.open("PUT", presignedUrl, true);
        xhr.setRequestHeader("Content-Type", MIME);
        xhr.setRequestHeader("Content-Disposition", "inline");
        xhr.setRequestHeader("x-amz-acl", "public-read");

        xhr.upload.addEventListener("progress", (event) => {
          console.log(`Uploaded ${event.loaded} of ${event.total} bytes...`);
          send({
            type: "upload:progress",
            content: event.loaded / event.total,
          }); // Send a progress callback.
        });

        // Browser is online, send the request
        xhr.onerror = error;

        xhr.onreadystatechange = function () {
          if (xhr.readyState === XMLHttpRequest.DONE && xhr.status === 200) {
            send({
              type: callbackMessage,
              content: { result: "success", data: { slug, path } },
            });

            if (debug) console.log("✔️ File uploaded:", xhr.responseURL);
          }
        };

        try {
          xhr.send(new Blob([data], { type: MIME }));
        } catch (err) {
          error(err);
        }
      })
      .catch((err) => {
        if (debug) console.log("⚠️ Failed to get presigned URL:", err);
        error(err);
      });
  }

  // Request and open local file from the user.
  // TODO: Only supports images for now.
  // TODO: Make sure this works on mobile platforms.
  async function openFile() {
    pen?.up(); // Synthesize a pen `up` event so it doesn't stick
    //            due to the modal.
    const input = document.createElement("input");
    input.type = "file";
    input.accept = "image/*";
    input.style.position = "absolute";
    input.style.left = "-9999px"; // Position off-screen

    return new Promise((resolve, reject) => {
      // Simulate click event on a visible element
      const button = document.createElement("button");
      button.style.opacity = 0;
      button.onclick = () => {
        input.click();
        document.body.removeChild(button);
      };

      document.body.appendChild(button);
      button.click();

      input.onchange = () => {
        const file = input.files[0];
        if (!file) {
          reject("No file was selected!");
        } else if (!file.type.startsWith("image/")) {
          reject("Selected file is not an image.");
        } else {
          const reader = new FileReader();

          reader.onload = async () => {
            const blob = new Blob([reader.result], { type: file.type });
            resolve(await toBitmap(blob));
          };
          reader.onerror = (error) => {
            reject(error);
          };
          reader.readAsArrayBuffer(file);
        }
      };
    });
  }

  // Request and open local file from the user.
  // TODO: Only supports images for now.
  // async function openFile() {
  //   pen?.up(); // Synthesize a pen `up` event so it doesn't stick
  //   //            due to the modal.
  //   const input = document.createElement("input");
  //   input.type = "file";
  //   input.accept = "image/*";
  //   input.click();

  //   return new Promise((resolve, reject) => {
  //     input.onchange = () => {
  //       const file = input.files[0];
  //       if (!file) {
  //         reject("No file was selected!");
  //       } else if (!file.type.startsWith("image/")) {
  //         reject("Selected file is not an image.");
  //       } else {
  //         const reader = new FileReader();

  //         reader.onload = async () => {
  //           const blob = new Blob([reader.result], { type: file.type });
  //           resolve(await toBitmap(blob));
  //         };
  //         reader.onerror = (error) => {
  //           reject(error);
  //         };
  //         reader.readAsArrayBuffer(file);
  //       }
  //     };
  //   });
  // }

  // Gets an authorization token for the logged in user,
  // which can be passed onto the server for further verification.
  async function authorize() {
    let token;
    try {
      token = await window.auth0Client.getTokenSilently();
      console.log("🔐 Authorized");
    } catch (err) {
      if (debug) console.log("🔐️ ❌ Unauthorized");
    }
    return token;
  }

  // Reads the extension off of filename to determine the mimetype and then
  // handles the data accordingly and downloads the file in the browser.
  // Downloads both cached files via `data` and network stored files for
  // users and guests.
  async function receivedDownload({ filename, data, modifiers }) {
    console.log("💾 Downloading:", filename, typeof data);

    let object;
    let MIME = "application/octet-stream"; // Default content type.

    if (extension(filename) === "glb") {
      MIME = "model/gltf+binary";
      object = URL.createObjectURL(new Blob([data], { type: MIME }));
    } else if (
      extension(filename) === "json" ||
      extension(filename) === "gltf"
    ) {
      // ✍️ Text + 3D
      // JSON
      MIME = "application/json";
      // GLTF
      if (extension(filename === "gltf")) MIME = "model/gltf+json"; // Hacky conditional above...

      // Parse JSON strings if they haven't been already, including support for `bigints`.
      if (typeof data !== "string") {
        data = JSON.stringify(
          data,
          (k, v) => (typeof v === "bigint" ? v.toString() : v)
          // 2 // Also make sure we indent by 2 spaces so it's nicely formatted.
        );
      }
      object = URL.createObjectURL(new Blob([data], { type: MIME }));
    } else if (
      extension(filename) === "png" ||
      extension(filename) === "webp"
    ) {
      // 🖼️ Images
      MIME = "image/png"; // PNG

      if (extension(filename) === "webp") {
        MIME = "image/webp";
      }

      if (data) {
        // Download locally if data is provided.
        const blob = await bufferToBlob(data, MIME, modifiers);
        object = URL.createObjectURL(blob, { type: MIME });
      } else {
        // Or from the storage network.
        // Check to see if filename has user handle data.
        const hasEmailOrHandle = filename.split("/")[0].indexOf("@") > -1;
        object = hasEmailOrHandle
          ? `/media/${filename}`
          : `https://art.aesthetic.computer/${filename}`;
      }
    } else if (extension(filename) === "mp4") {
      // 🎥 Video
      // Use `data` from the global Media Recorder.
      if (mediaRecorderBlob) {
        object = URL.createObjectURL(mediaRecorderBlob);
      } else {
        console.warn(
          "🕸️ No local video available... Trying art bucket:",
          filename
        );
        object = `https://art.aesthetic.computer/${filename}`;
      }
    }

    // Fetch download url from `/presigned-download-url?for=${filename}` if we
    // don't already have a blob string.

    if (!object.startsWith("blob:")) {
      try {
        const response = await fetch(`/presigned-download-url?for=${filename}`);
        const json = await response.json();
        object = json.url;
      } catch (err) {
        console.log(err);
      }
    }

    const a = document.createElement("a");
    a.href = object;
    a.target = "_blank";
    a.download = filename.split("/").pop(); // Remove any extra paths.

    a.click();
    if (typeof a.href !== "string") URL.revokeObjectURL(a.href);

    // Picture in Picture: Image Download UI? 22.11.24.08.51
    //const container = document.createElement('div');
    //const iframe = document.createElement('iframe');

    //container.id = "pip-wrapper";
    //iframe.id = "pip";
    //iframe.src = "/blank";

    //container.append(iframe);
    //wrapper.append(container);
  }

  // Used above in `receivedUpload` and `receivedDownload` to generate image files.

  // Add a crop to square modifier.

  async function bufferToBlob(data, MIME, modifiers) {
    let can;

    // Encode a pixel buffer as a png.
    // See also: https://stackoverflow.com/questions/11112321/how-to-save-canvas-as-png-image
    const img = data;
    const imageData = new ImageData(img.pixels, img.width, img.height);

    can = document.createElement("canvas");
    const ctx = can.getContext("2d");

    if (modifiers?.cropToScreen) {
      can.width = screen.width;
      can.height = screen.height;
    } else {
      can.width = img.width;
      can.height = img.height;
    }

    if (modifiers?.crop === "square") {
      // debugger;
    }

    ctx.putImageData(imageData, 0, 0);

    // Scale or modify the image as needed.
    if ((modifiers?.scale !== 1 && modifiers?.scale > 0) || modifiers?.flipY) {
      const scale = modifiers?.scale || 1;
      const flipY = modifiers?.flipY;
      const can2 = document.createElement("canvas");
      const ctx2 = can2.getContext("2d");
      can2.width = can.width * scale;
      can2.height = can.height * scale;
      ctx2.imageSmoothingEnabled = false;
      if (flipY) {
        ctx2.scale(1, -1);
        ctx2.drawImage(can, 0, 0, can2.width, -can2.height);
      } else {
        ctx2.drawImage(can, 0, 0, can2.width, can2.height);
      }
      can = can2;
    }

    const blob = await new Promise((resolve) => can.toBlob(resolve, MIME, 100));
    return blob;
  }

  // Opens a file chooser that is filtered by a given extension / mimetype list.
  // And sends the text contents of an individual file back to the disk.
  function receivedImport(type) {
    const input = document.createElement("input");
    input.type = "file";
    input.accept = type;

    input.onchange = (e) => {
      // Grab the only selected file in the file input.
      const file = e.target.files[0];

      // Does type match nothing in the comma separated `input.accept` list?
      const noMatch = type.split(",").every((t) => {
        return t !== file.type && t !== `.${extension(file.name)}`;
      });

      // Relay error if chosen file does not match the `input.accept` list.
      if (noMatch) {
        send({
          type: "import",
          content: {
            result: "error",
            data: `Chosen file was not of type "${type}"`,
          },
        });
        return;
      }

      // Read the file.
      const reader = new FileReader();
      reader.readAsText(file);

      // Send the content back to the disk once the file loads.
      reader.onload = (e) => {
        send({
          type: "import",
          content: { result: "success", data: e.target.result },
        });
      };

      // Relay an error if the file fails to load for any reason.
      reader.onerror = () => {
        send({
          type: "import",
          content: { result: "error", data: reader.error },
        });
      };
    };

    input.click();
  }

  // Connects the Microphone to the current audioContext.
  function receivedMicrophone(data = {}) {
    if (data.detach) {
      detachMicrophone?.();
    } else {
      attachMicrophone?.(data);
    }
  }

  // Takes a request for a video and then either uses a media query (for a camera)
  // or loads a video file from a given url (unimplemented).

  // Then it puts that into a new video tag and starts playing it,
  // sending the disk the thread frames as they update (optional).

  // This module also is used to pull data from frames for
  // features like hand-tracking.
  let videoResize; // Holds a function defined after initialization.
  let handAPI = {};
  //let handLandmarker, HandLandmarker, FilesetResolver, vision;
  async function receivedVideo({ type, options }) {
    // if (debug) console.log("🎥 Type:", type, options);

    if (type === "camera:update") videoResize?.(options);

    if (type === "camera") {
      // TODO: Give video and canvas a unique identifier that
      //       will create a link in the worker so that frame updates
      //       for multiple videos can be routed simultaneously.
      const video = document.createElement("video");

      // Camera properties.
      let facingMode = options.facing || "user",
        zoom = 1;

      video.id = "camera-feed";
      video.autoplay = true; // Allow video footage to play automatically.
      video.setAttribute("playsinline", ""); // Only for iOS.
      video.setAttribute("muted", ""); // Don't include audio with video.

      const hands = options.hands === true; // Hand-tracking globals.
      let handVideoTime = -1;
      const useLegacyHandsAPI = true; // Performance of both libraries is
      //                                 equivalent on iPhone 14 Pro but vastly
      //                                 different on iPhone 13 Pro. 23.05.12.14.23

      const buffer = document.createElement("canvas");
      let animationRequest;

      function getAnimationRequest() {
        return animationRequest;
      }

      videos.push({ video, buffer, getAnimationRequest });

      buffer.width = options.width || 1280;
      buffer.height = options.height || 720;

      const bufferCtx = buffer.getContext("2d", { willReadFrequently: true });

      wrapper.appendChild(video);

      video.style = `position: absolute;
                     top: 0;
                     left: 0;
                     opacity: 0;
                     transform: scaleX(${facingMode === "user" ? -1 : 1});
                     width: 100%;`;

      buffer.style = `position: absolute;
                      opacity: 0;`;

      // List the user's potential video devices. (Front & Back Camera)
      // try {
      // const devices = await navigator.mediaDevices.enumerateDevices();
      // const videoInputDevices = devices.filter(
      //   (device) => device.kind === "videoinput"
      // );
      // if (debug)
      //   console.log(
      //     "🎥 Available constraints: ",
      //     navigator.mediaDevices.getSupportedConstraints()
      //   );
      // if (debug)
      //   console.log("🎥 Available video devices: ", videoInputDevices);
      // } catch (error) {
      // console.log(error.name + ": " + error.message);
      // }

      // Swap width and height on iOS. (Implementation default differences.)
      // Set a height / width aspect ratio on iOS because
      // of implementation differences.
      if (iOS) {
        const temp = options.width;
        options.width = options.height;
        options.height = temp;
      }

      try {
        // Grab video from the user using a requested width and height based
        // on the frame size.
        let cWidth = options.width,
          cHeight = options.height;

        const stream = await navigator.mediaDevices.getUserMedia({
          video: {
            facingMode,
            width: { ideal: cWidth },
            height: { ideal: cHeight },
            frameRate: { ideal: 30 },
          },
          audio: false,
        });

        video.srcObject = stream;
        const videoTrack = stream.getVideoTracks()[0];
        // const capabilities = videoTrack.getCapabilities();

        // Update the global facingMode in case it's different from
        // what was requested.
        facingMode = videoTrack.getConstraints().facingMode;
        // console.log(videoTrack.getConstraints());

        video.addEventListener(
          "loadedmetadata",
          () => {
            video.play();
            if (debug)
              console.log("🎥 Resolution:", buffer.width, buffer.height);
          },
          { once: true }
        );

        // Resizing the video after creation. (Window resize or device rotate.)
        videoResize = async function ({ width, height }) {
          cancelAnimationFrame(getAnimationRequest());

          try {
            if (iOS) {
              const temp = width;
              width = height;
              height = temp;
            }

            video.srcObject = null; // Refresh the video `srcObject`.

            await videoTrack.applyConstraints({
              width: { ideal: width },
              height: { ideal: height },
            });

            video.srcObject = stream;

            video.addEventListener(
              "loadedmetadata",
              () => {
                buffer.width = width;
                buffer.height = height;
                process();
                if (debug)
                  console.log("🎥 Resolution:", buffer.width, buffer.height);
              },
              { once: true }
            );
          } catch (error) {
            process();
            if (debug) console.warn("🎥 Resolution update failed.");
          }
        };

        process(); // Start processing frames.

        // ✋ Optional Hand-tracking (only load once)
        if (hands === true) {
          if (useLegacyHandsAPI && !window.Hands) {
            // Load older mediapipe lib.
            const script = document.createElement("script");
            script.src = "aesthetic.computer/dep/@mediapipe/hands/hands.js";
            script.crossOrigin = "anonymous";

            script.onload = function () {
              const config = {
                locateFile: (file) => {
                  return `aesthetic.computer/dep/@mediapipe/hands/${file}`;
                },
              };

              handAPI.hands = new Hands(config);

              handAPI.hands.setOptions({
                selfieMode: false,
                maxNumHands: 1,
                modelComplexity: 0,
                minDetectionConfidence: 0.5,
                minTrackingConfidence: 0.5,
              });

              handAPI.hands.onResults((data) => {
                diagram({
                  screen: data.multiHandLandmarks[0] || [],
                  world: data.multiHandWorldLandmarks[0] || [],
                  hand: data.multiHandedness[0]?.label.toLowerCase() || "none",
                });
              });
            };

            document.head.appendChild(script);
          } else {
            if (!handAPI.HandLandmarker) {
              const { HandLandmarker, FilesetResolver } = await import(
                "/aesthetic.computer/dep/@mediapipe/tasks-vision/vision_bundle.js"
              );

              const vision = await FilesetResolver.forVisionTasks(
                "/aesthetic.computer/dep/@mediapipe/tasks-vision/wasm"
              );

              handAPI.HandLandmarker = HandLandmarker;
              handAPI.vision = vision;
            }

            if (!handAPI.hl) {
              handAPI.hl = await handAPI.HandLandmarker.createFromOptions(
                handAPI.vision,
                {
                  baseOptions: {
                    modelAssetPath: "../models/hand_landmarker.task",
                    delegate: "GPU", // or "CPU"
                  },
                  canvas: document.createElement("canvas"),
                  runningMode: "VIDEO",
                  //runningMode: "LIVE_STREAM",
                  minHandDetectionConfidence: 0.25,
                  minHandPresenceConfidence: 0.25,
                  minTrackingConfidence: 0.25,
                  numHands: 1,
                }
              );
            }
          }
        }
      } catch (err) {
        console.log(err);
      }

      function diagram(hand) {
        if (facingMode === "user") {
          hand.screen.forEach((l) => (l.x = 1 - l.x));
          // Reverse handedness because our data is mirrored.
          if (hand.handedness === "left") {
            hand.handedness = "right";
          } else if (hand.handedness === "right") {
            hand.handedness = "left";
          }
        }
        handData = hand;
      }

      function process() {
        // TODO: Video effects / filter kernels could be added here...
        // zoom += 0.001;

        // 💡 For potentially higher quality visuals. 23.04.29.20.47 ...
        // Drawing a video frame to the buffer (mirrored, proportion adjusted).
        // const videoAR = video.videoWidth / video.videoHeight;
        // const bufferAR = buffer.width / buffer.height;
        // let outWidth, outHeight, outX = 0, outY = 0;

        // if (videoAR <= bufferAR) {
        //   // Tall to wide.
        //   outWidth = buffer.width;
        //   outHeight = outWidth / videoAR;
        // } else {
        //   // Wide to tall.
        //   outHeight = buffer.height;
        //   outWidth = outHeight * videoAR;
        // }

        // outY = ((buffer.height - outHeight) / 2); // Adjusting position.
        // outX = ((buffer.width - outWidth) / 2);

        // bufferCtx.save();
        // bufferCtx.scale(-1, 1); // Draw mirrored.
        // bufferCtx.drawImage(video, -outX - outWidth, outY, outWidth, outHeight);
        // bufferCtx.restore();

        // 🤚 Track Hands on the GPU if flagged.
        if (hands === true) {
          if (handVideoTime !== video.currentTime && video.videoWidth > 0) {
            handVideoTime = video.currentTime;
            if (useLegacyHandsAPI && !handAPI.legacyProcessing) {
              handAPI.hands?.send({ image: video }).then(() => {
                handAPI.legacyProcessing = false;
                // Don't process more than one frame at a time.
              });
              handAPI.legacyProcessing = true;
            } else {
              const data = handAPI.hl?.detectForVideo(video, handVideoTime);
              // TODO: This will no longer work. 23.5.24
              //       Check the other `diagram` call.
              // diagram(data?.landmarks[0] || []);
            }
            // send({type: "hand-tracking-data", content: landmarks});
          }
        }

        // Send frames by default.
        if (options.hidden !== true) {
          if (facingMode === "user") {
            bufferCtx.translate(buffer.width / 2, buffer.height / 2);
            bufferCtx.scale(-zoom, zoom);
            bufferCtx.translate(-buffer.width / 2, -buffer.height / 2);
          }
          bufferCtx.drawImage(video, 0, 0, buffer.width, buffer.height);
          bufferCtx.resetTransform();
          // if (facingMode === "user") {
          //   bufferCtx.translate(buffer.width / 2, buffer.height / 2);
          //   bufferCtx.scale(-zoom, zoom);
          //   bufferCtx.translate(-buffer.width / 2, -buffer.height / 2);
          // }
          // bufferCtx.drawImage(video, 0, 0, buffer.width, buffer.height);
          // bufferCtx.resetTransform();
          const pixels = bufferCtx.getImageData(
            0,
            0,
            buffer.width,
            buffer.height
          );

          send(
            {
              type: "video-frame",
              content: {
                width: pixels.width,
                height: pixels.height,
                pixels: pixels.data,
              },
            },
            [pixels.data.buffer]
          );
        }

        animationRequest = requestAnimationFrame(process);
      }
    }
  }

  // Window Focus
  window.addEventListener("focus", function (e) {
    send({ type: "focus-change", content: true });
  });

  // Window Blur
  window.addEventListener("blur", function (e) {
    send({ type: "focus-change", content: false });
  });

  // Window Visibility
  document.addEventListener("visibilitychange", function () {
    send({
      type: "visibility-change",
      content: !document.hidden,
    });
  });

  // 🚨 Signal (Used to pass messages via window... important for embedded HTML
  //           `content` used within pieces that needs communication with the
  //           main system)

  // Send signals to the running piece.
  window.signal = function (message) {
    if (debug) console.log("🚨 Signal sent:", message);
    send({
      type: "signal",
      content: message,
    });
  };

  // Receive signals from the piece & assign callbacks.
  // These get flushed between pieces.
  window.when = function (message, callback) {
    whens[message] = callback;
  };

  // 📚 History
  // TODO: Extract all the history features into a class of some kind?
  // TODO: Eventually add an API so that a disk can list all the history of
  //       a user's session. This could also be used for autocompletion of
  //       pieces / up + down arrow prev-next etc.
  window.onpopstate = function (e) {
    if (
      document.location.hash === "#debug" ||
      document.location.hash === "#nodebug"
    ) {
      document.location.reload();
    }

    send({
      type: "history-load",
      content: parse(slug(document.location.href) || window.acSTARTING_PIECE),
    });
  };

  // Fullscreen
  // Note: This doesn't work in Safari because you can't fullscreen the body element.
  //       (Or anything other than a video element?) 22.2.13

  const requestFullscreen =
    document.body.requestFullscreen || wrapper.webkitRequestFullscreen;

  const exitFullscreen =
    document.exitFullscreen || document.webkitExitFullscreen;

  // Tries to toggle fullscreen. Must be called within a user interaction.
  function toggleFullscreen() {
    const fullscreenElement =
      document.fullscreenElement || document.webkitFullscreenElement;

    if (!fullscreenElement) {
      requestFullscreen.apply(document.body)?.catch((e) => console.error(e));
    } else {
      exitFullscreen();
    }
  }

  document.body.onfullscreenchange = (event) => {
    const fullscreenElement =
      document.fullscreenElement || document.webkitFullscreenElement;

    if (fullscreenElement) {
      console.log("😱 Entered fullscreen mode!", fullscreenElement);
    } else {
      console.log("😱 Leaving fullscreen mode!");
    }
  };

  // 🔘 Button Hitboxes
  // (Created for 📋 Clipboard Events)
  let hitboxes = {};
  window.addEventListener("pointerup", async (e) => {
    keys(hitboxes).forEach((key) => hitboxes[key]?.(e));
  });

  window.addEventListener("pointerdown", async (e) => {
    keys(hitboxes).forEach((key) => hitboxes[key]?.(e));
  });
}

// Utilities
async function toBitmap(imgOrBlob) {
  const img = await createImageBitmap(imgOrBlob);
  const canvas = document.createElement("canvas");
  canvas.width = img.width;
  canvas.height = img.height;
  const ctx = canvas.getContext("2d");
  ctx.drawImage(img, 0, 0);
  const imageData = ctx.getImageData(0, 0, canvas.width, canvas.height);
  return {
    width: imageData.width,
    height: imageData.height,
    pixels: imageData.data,
  };
}

export { boot };
