// 💻 BIOS

// 📦 All Imports
import * as Loop from "./lib/loop.mjs";
import { Pen } from "./lib/pen.mjs";
import { Keyboard } from "./lib/keyboard.mjs";
import * as UI from "./lib/ui.mjs";
import * as Glaze from "./lib/glaze.mjs";
import { apiObject, extension } from "./lib/helpers.mjs";
import { dist } from "./lib/num.mjs";
import { parse, slug } from "./lib/parse.mjs";
import * as Store from "./lib/store.mjs";
import { Desktop, MetaBrowser, Instagram, iOS } from "./lib/platform.mjs";
import { headers } from "./lib/console-headers.mjs";

const { assign } = Object;
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

  let pen, keyboard;
  // let frameCount = 0;
  // let timePassed = 0;
  let now = 0;

  let diskSupervisor;
  let currentPiece = null; // Gets set to a path after `loaded`.
  let currentPieceHasTextInput = false;

  // Media Recorder
  let mediaRecorder; // See "recorder-rolling" below.

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
    // console.log("framing...", imageData);

    lastGap = gap;

    // Cache the current canvas if needed.
    if (freezeFrame && imageData && !document.body.contains(freezeFrameCan)) {
      if (debug) {
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
        console.log("Freeze glaze!");
        Glaze.freeze(ffCtx);
        // ffCtx.fillStyle = "lime";
        // ffCtx.fillRect(0, 0, ffCtx.canvas.width, ffCtx.canvas.height);
        freezeFrameGlaze = false;
      } else {
        ffCtx.putImageData(imageData, 0, 0);
      }

      if (!wrapper.contains(freezeFrameCan)) wrapper.append(freezeFrameCan);
      else freezeFrameCan.style.removeProperty("opacity");
      canvas.style.opacity = 0;
    }

    // Find the width and height of our default screen and native projection.
    width = width || fixedWidth;
    height = height || fixedHeight;

    const gapSize = gap * window.devicePixelRatio;

    // console.log("INNER HEIGHT", window.innerHeight);

    let subdivisions = 1;

    if (width === undefined && height === undefined) {
      // Automatically set and frame a reasonable resolution.
      // Or pull from density.
      let ratio = density || window.devicePixelRatio;
      if (!density && window.devicePixelRatio === 1) ratio = 3; // Always force a screen density of 3 on non-retina displays.
      subdivisions = ratio;
      width = round(window.innerWidth / subdivisions);
      height = round(window.innerHeight / subdivisions);
      projectedWidth = width * subdivisions - gapSize;
      projectedHeight = height * subdivisions - gapSize;
    } else {
      // Or do it manually if both width and height are defined.
      fixedWidth = width;
      fixedHeight = height;

      const scale = min(window.innerWidth / width, window.innerHeight / height);
      // console.log(window.innerWidth, window.innerHeight);

      projectedWidth = round(width * scale - gapSize);
      projectedHeight = round(height * scale - gapSize);
    }

    if (debug)
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

    if (imageData) ctx.putImageData(imageData, 0, 0);

    imageData = ctx.getImageData(0, 0, canvas.width, canvas.height);

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

  // 2. Audio
  const sound = {
    bpm: new Float32Array(1),
  };

  let updateMetronome,
    updateSquare,
    updateBubble,
    attachMicrophone,
    detachMicrophone,
    audioContext,
    audioStreamDest;

  let requestMicrophoneAmplitude, requestMicrophoneWaveform;

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
      sampleRate: 44100,
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
      if (debug) console.log("🎙 Microphone:", data);

      const micStream = await navigator.mediaDevices.getUserMedia({
        audio: {
          echoCancellation: false,
          latency: 0,
          noiseSuppression: false,
          autoGainControl: false,
        },
      });

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
      };

      // Request data / send message to the mic processor thread.
      requestMicrophoneAmplitude = () => {
        playerNode.port.postMessage({ type: "get-amplitude" });
      };

      requestMicrophoneWaveform = () => {
        playerNode.port.postMessage({ type: "get-waveform" });
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
    };

    // Sound Synthesis Processor
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
        soundProcessor.port.postMessage({
          type: "new-bpm",
          data: newBPM,
        });
      };

      updateSquare = function (square) {
        soundProcessor.port.postMessage({
          type: "square",
          data: square,
        });
      };

      updateBubble = function (bubble) {
        soundProcessor.port.postMessage({
          type: "bubble",
          data: bubble,
        });
      };

      soundProcessor.port.onmessage = (e) => {
        const time = e.data;
        diskSupervisor.requestBeat?.(time);
      };

      //soundProcessor.connect()

      // Connect soundProcessor to the mediaStream.
      soundProcessor.connect(audioStreamDest);

      soundProcessor.connect(audioContext.destination);

      audioContext.resume();

      modal.classList.remove("on");
      bumper.innerText = "";
    })();

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
    },
  };

  const onMessage = (m) => receivedChange(m);

  let send;

  // 🔥 Optionally use workers or not.
  // Always use workers if they are supported, except for
  // when we are in VR (MetaBrowser).
  const workersEnabled = true;

  //if (workersEnabled) {
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
    for (const square of content.squares) updateSquare(square);
    for (const bubble of content.bubbles) updateBubble(bubble);
  }

  // Update & Render
  let frameAlreadyRequested = false;

  function requestFrame(needsRender, updateCount, nowUpdate) {
    now = nowUpdate;

    if (needsReframe) {
      // console.log("NEEDS REFRAME:", needsReframe)
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
          inFocus: true, // document.hasFocus(),
          audioTime: audioContext?.currentTime,
          audioBpm: sound.bpm, // TODO: Turn this into a messaging thing.
          audioMusicAmplitude: amplitude,
          audioMusicSampleData: amplitude > 0 ? frequencyData : [],
          width: canvas.width,
          height: canvas.height,
          // TODO: Do all fields of `pointer` need to be sent? 22.09.19.23.30
          pen: { events: pen.events, pointers: pen.pointers },
          pen3d: ThreeD?.pollControllers(), // TODO: Implement pointers in 3D.
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

  let frameResolution;

  let frameCached = false;
  let pixelsDidChange = false; // TODO: Can this whole thing be removed? 2021.11.28.03.50

  let contentFrame;

  const bakedCan = document.createElement("canvas", {
    willReadFrequently: true,
  });

  // *** Received Frame ***
  async function receivedChange({ data: { type, content } }) {
    // *** Route to different functions if this change is not a full frame update.
    if (type === "load-failure" && MetaBrowser) {
      document.querySelector("#software-keyboard-input")?.blur();
      return;
    }

    // if (type === "alert-popup:instagram" && Instagram) {
    //   window.alert(content);
    //   return;
    // }

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

    if (type === "disk-loaded") {
      currentPiece = content.path;
      currentPieceHasTextInput = false;

      // Initialize some global stuff after the first piece loads.
      if (content.pieceCount === 0) {
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

          form.addEventListener("submit", (e) => {
            // Generate a keyboard event if the form was submitted.
            // (Enter keypressed.)
            keyboard.events.push({
              name: "keyboard:down:enter",
              key: "Enter",
              repeat: false,
              shift: false,
              alt: false,
              ctrl: false,
            });

            e.preventDefault();
          });

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

          let touching = false;
          let keyboardOpen = false;

          // TODO: The input element could be created and added to the DOM here
          //       if it didn't already exist?
          window.addEventListener("touchstart", () => (touching = true));

          window.addEventListener("focusout", (e) => {
            if (keyboardOpen) {
              keyboard.events.push({ name: "keyboard:close" });
              keyboardOpen = false;
            }
          });

          // Make a pointer "tap" gesture with an `inTime` window of 250ms to
          // trigger the keyboard on all browsers.
          let down = false;
          // let downPos;
          let inTime = false;

          window.addEventListener("pointerdown", (e) => {
            if (currentPieceHasTextInput) {
              down = true;
              // downPos = { x: e.x, y: e.y };
              inTime = true;
              setTimeout(() => (inTime = false), 500);
              e.preventDefault();
            }
          });

          window.addEventListener("pointerup", (e) => {
            if (
              down &&
              // dist(downPos.x, downPos.y, e.x, e.y) < 32 && // Distance threshold for opening keyboard.
              inTime &&
              currentPieceHasTextInput
              // Refactoring the above could allow iframes to capture keyboard events.
              // via sending things from input... 22.10.24.17.16, 2022.04.07.02.10
            ) {
              if (document.activeElement === input) {
                input.blur();
              } else {
                input.focus();
              }

              if (touching) {
                touching = false;
                keyboard.events.push({ name: "keyboard:open" });
                keyboardOpen = true;
              }
              down = false;
              e.preventDefault();
            }
          });

          input.addEventListener("focus", (e) => {
            keyboard.events.push({ name: "typing-input-ready" });
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

        // 🖥️ Display
        frame(resolution?.width, resolution?.height);

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
          function (needsRender, updateTimes, nowUpdate) {
            // TODO: How can I get the pen data into the disk and back
            //       to Three.JS as fast as possible? 22.10.26.23.25
            // console.log(nowUpdate);

            diskSupervisor.requestFrame?.(needsRender, updateTimes, nowUpdate);

            if (ThreeD?.status.alive === true && ThreeDBakeQueue.length > 0) {
              ThreeD.collectGarbage();
              // Bake all forms, while keeping track of baked forms, and any form that is missing after the queue ends needs to be cleared.
              const touchedForms = [];
              ThreeDBakeQueue.forEach((baker) => touchedForms.push(...baker()));
              ThreeD.checkForRemovedForms(touchedForms);
              ThreeDBakeQueue.length = 0;
              ThreeD?.render();
            }
          }
        );
      } else {
        // Unload some already initialized stuff if this wasn't the first load.

        // Remove any attached microphone.
        detachMicrophone?.();

        // Reset preloading.
        window.waitForPreload = false;
        window.preloaded = false;

        // Clear any 3D content.
        ThreeD?.clear();

        // Kill the 3D engine.
        ThreeD?.kill();

        // Clear any DOM content that was added by a piece.
        contentFrame?.remove(); // Remove the contentFrame if it exists.
        contentFrame = undefined;
        // Remove any event listeners added by the content frame.
        window?.acCONTENT_EVENTS.forEach((e) => e());
        window.acCONTENT_EVENTS = []; // And clear all events from the list.

        // Remove existing video tags.
        videos.forEach(({ video, buffer, getAnimationRequest }) => {
          console.log("🎥 Removing:", video, buffer, getAnimationRequest());
          video.remove();
          buffer.remove();
          cancelAnimationFrame(getAnimationRequest());
        });
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
        document.querySelector("#software-keyboard-input")?.blur();
        keyboard.events.push({ name: "keyboard:close" });
      }

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
        if (content.fromHistory === false) {
          history.pushState(
            "",
            document.title,
            content.text === "/prompt" ? "/" : "/" + content.text // Replace "prompt" with "/".
          );
        }

        // Replace the state if we are running an aliased `load` or `jump`.
        // (THat doesn't avoids the history stack.)
        if (content.fromHistory === true && content.alias === false) {
          history.replaceState(
            "",
            document.title,
            content.text === "/prompt" ? "/" : "/" + content.text // Replace "prompt" with "/".
          );
        }
      }

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

    if (type === "text-input-enabled") {
      currentPieceHasTextInput = true;
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
        localStorage.setItem(content.key, JSON.stringify(content.data));
        if (debug) console.log("📦 Persisted locally:", content, localStorage);
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
        if (debug) console.log("📦 Persisted on local:db:", content);
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
        const data = JSON.parse(localStorage.getItem(content.key));
        if (debug) console.log("📦 Retrieved local data:", content.key, data);
        send({
          type: "store:retrieved",
          content: data,
        });
      }

      if (content.method === "local:db") {
        const retrievedContent = await Store.get(content.key);
        if (debug)
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
        if (debug) console.log("📦 Delete local data:", content.key);
        send({
          type: "store:deleted",
          content: true,
        });
        // Just assume this is deleted.
      }

      if (content.method === "local:db") {
        const hasKey = (await Store.keys()).includes(content.key);
        let deleted;

        if (hasKey) {
          await Store.del(content.key);
          const alteredKeys = await Store.keys();
          deleted = !alteredKeys.includes(content.key);
        } else {
          deleted = false;
        }

        if (debug)
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

    if (type === "video") {
      receivedVideo(content);
      return;
    }

    if (type === "recorder-rolling") {
      if (debug) console.log("🔴 Recorder: Rolling", content);

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
          videoBitsPerSecond: 2500000,
          mimeType,
        };

        // *** streamCanvas Recording Settings ***
        // TODO: Examine the canvas stream resolution?

        // TODO: Duplicate / resize canvas as needed?
        // TODO: What happens upon resize?
        // TODO: Only set the streamCanvas here if it is below a certain
        //       resolution.
        streamCanvasContext = document.createElement("canvas").getContext("2d");
        streamCanvasContext.imageSmoothingEnabled = false;

        // TODO: If we aren't using TikTok, then just find a good resolution / double
        // the pixels as needed. 22.08.11.03.38
        // streamCanvasContext.canvas.width = canvas.width * 2;
        // streamCanvasContext.canvas.height = canvas.height * 2;

        // Portrait Mode / TikTok (this is the default for recording)
        // This is hardcoded at 1080p for TikTok right now.
        streamCanvasContext.canvas.width = 1080;
        streamCanvasContext.canvas.height = 1920;

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
      mediaRecorder.ondataavailable = (evt) => chunks.push(evt.data);

      let recordingStartTime = 0;
      let recordingDuration;

      // 🗺️ mediaRecorder:Start
      mediaRecorder.onstart = function () {
        recordingStartTime = performance.now();
      };

      // 🗺️ mediaRecorder:Stop
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
          // TODO: Check to see if encoding can be skipped? 22.08.11.03.41

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
                type: "transcode-progress",
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

          blob = new Blob([file.buffer], { type: "video/mp4" });
        }

        // Make an appropriate element to store the recording.
        const el = document.createElement(content); // "audio" or "video"

        el.width = 100;

        el.src = URL.createObjectURL(blob);
        el.controls = true;

        // Add the recording to the dom, among other recordings that may exist.
        const recordings = wrapper.querySelector("#recordings");

        if (recordings) {
          const recordingsWrapper = recordings.querySelector(
            "#recordings-wrapper"
          );
          recordingsWrapper.append(el);
        } else {
          const recordingsEl = document.createElement("div");
          recordingsEl.id = "recordings";

          const recordingsWrapper = document.createElement("div");
          recordingsWrapper.id = "recordings-wrapper";

          recordingsEl.append(recordingsWrapper); // Add wrapper.
          recordingsWrapper.append(el); // Add video element.

          // Add download link.
          const download = document.createElement("a");
          download.href = el.src;
          download.innerText = "DOWNLOAD VIDEO";
          download.download = "test.mp4";
          recordingsWrapper.append(download);

          // Add close button.
          const close = document.createElement("div");
          close.innerText = "AGAIN"; // "CLOSE"
          close.id = "recordings-close";
          recordingsWrapper.append(close);

          close.onpointerdown = () => {
            recordingsEl.remove();
            signal("recordings:close");
          };

          // TODO: There needs to be a progress bar or spinner or button to
          //       upload the video.
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

          // TODO: Add UI for downloading the file.

          wrapper.append(recordingsEl);
        }

        // TODO: Figure out what to do with these...
        el.play();

        if (debug) console.log("📼 Recorder: Printed");
      };

      mediaRecorder.start();
      return;
    }

    if (type === "recorder-cut") {
      if (debug) console.log("✂️ Recorder: Cut");
      mediaRecorder?.pause();
      return;
    }

    if (type === "recorder-print") {
      mediaRecorder?.stop();
      mediaRecorder = undefined;
      return;
    }

    if (type === "load-bitmap") {
      fetch(content).then(async (response) => {
        if (!response.ok) {
          send({
            type: "loaded-bitmap-rejection",
            content: { url: content },
          });
        } else {
          const blob = await response.blob();
          const bitmap = await createImageBitmap(blob);

          const ctx = document.createElement("canvas").getContext("2d");
          ctx.canvas.width = bitmap.width;
          ctx.canvas.height = bitmap.height;
          ctx.drawImage(bitmap, 0, 0);
          const iD = ctx.getImageData(0, 0, bitmap.width, bitmap.height);

          send(
            {
              type: "loaded-bitmap-success",
              content: {
                url: content,
                img: {
                  width: iD.width,
                  height: iD.height,
                  pixels: iD.data,
                },
              },
            },
            [iD.data]
          );
        }
      });
      return;
    }

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
      if (!window.waitForPreload) window.preloaded = true;
      if (debug) console.log("⏳ Preloaded:", window.preloaded ? "✅" : "❌");
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
    if (content.pixels) {
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

    // About the render if pixels don't match.
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

    function draw() {
      // 🅰️ Draw updated content from the piece.

      const db = content.dirtyBox;
      if (db) {
        ctx.drawImage(dirtyBoxBitmapCan, db.x, db.y);
        if (glaze.on) Glaze.update(dirtyBoxBitmapCan, db.x, db.y);
      } else if (pixelsDidChange) {
        ctx.putImageData(imageData, 0, 0); // Comment out for a `dirtyBox` visualization.
        if (glaze.on) {
          ThreeD?.pasteTo(glazeCompositeCtx);
          glazeCompositeCtx.drawImage(canvas, 0, 0);
          Glaze.update(glazeComposite);
          //Glaze.update(imageData);
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

      if (content.loading === true) {
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

    if (freezeFrame) {
      if (glaze.on === false) {
        //canvas.style.removeProperty("opacity");
      }
      //freezeFrameCan.style.opacity = 0;
      freezeFrameCan.remove();
      freezeFrame = false;
      freezeFrameGlaze = false;
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
    console.log("📤 Uploading globally: ", filename, typeof data);
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

    let prefetchURL = "/presigned-upload-url/" + ext;

    if (bucket === "wand") prefetchURL += "/" + filename + "/" + bucket; // Add filename info.

    // Now send a request to the server...
    fetch(prefetchURL)
      .then(async (res) => {
        const presignedUrl = (await res.json()).uploadURL;
        if (debug) console.log("🔐 Presigned URL:", presignedUrl);

        const response = await fetch(presignedUrl, {
          method: "PUT",
          headers: {
            "Content-Type": MIME,
            "x-amz-acl": "public-read",
          },
          body: new Blob([data], { type: MIME }),
        });

        if (debug) console.log("✔️ File uploaded:", response);
        send({
          type: callbackMessage,
          content: { result: "success", data: response },
        });
      })
      .catch((err) => {
        if (debug) console.log("⚠️ Failed to get presigned URL:", err);
        send({
          type: callbackMessage,
          content: { result: "error", data: err },
        });
      });
  }

  // Reads the extension off of filename to determine the mimetype and then
  // handles the data accordingly and downloads the file in the browser.
  async function receivedDownload({ filename, data, modifiers }) {
    console.log("💾 Downloading locally:", filename, typeof data);

    let object;
    let MIME = "application/octet-stream"; // Default content type.

    if (extension(filename) === "glb") {
      MIME = "model/gltf+binary";
      object = URL.createObjectURL(new Blob([data], { type: MIME }));
    } else if (
      extension(filename) === "json" ||
      extension(filename) === "gltf"
    ) {
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
      // PNG
      MIME = "image/png";
      if (extension(filename) === "webp") {
        MIME = "image/webp";
      }

      let can;
      if (data.pixels) {
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

        ctx.putImageData(imageData, 0, 0);

        // Scale or modify the image as needed.
        if (
          (modifiers?.scale !== 1 && modifiers?.scale > 0) ||
          modifiers?.flipY
        ) {
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
      }

      const blob = await new Promise((resolve) =>
        can.toBlob(resolve, MIME, 100)
      );
      object = URL.createObjectURL(blob, { type: MIME });
    }

    const a = document.createElement("a");
    a.href = object;
    a.download = filename;
    a.click();
    URL.revokeObjectURL(a.href);

    // Picture in Picture: Image Download UI? 22.11.24.08.51
    //const container = document.createElement('div');
    //const iframe = document.createElement('iframe');

    //container.id = "pip-wrapper";
    //iframe.id = "pip";
    //iframe.src = "/blank";

    //container.append(iframe);
    //wrapper.append(container);
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
  // or loads a video file from a given url.

  // Then it puts that into a new video tag and starts playing it,
  // sending the disk the thread frames as they update.

  let videoResize; // Holds a function defined after initialization.
  async function receivedVideo({ type, options }) {
    // if (debug) console.log("🎥 Type:", type, options);

    if (type === "camera:update") videoResize?.(options);

    if (type === "camera") {
      // TODO: Give video and canvas a unique identifier that
      //       will create a link in the worker so that frame updates
      //       for multiple videos can be routed simultaneously.
      const video = document.createElement("video");

      // Camera properties.
      let facingMode = "user",
        zoom = 1;

      video.id = "camera-feed";
      video.autoplay = true; // Allow video footage play automatically.
      video.setAttribute("playsinline", ""); // Only for iOS.
      video.setAttribute("muted", ""); // Don't include audio with video.

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
      wrapper.appendChild(buffer);

      video.style = `position: absolute;
                     top: 0;
                     left: 0;
                     opacity: 0;
                     width: 300px;`;

      buffer.style = `position: absolute;
                      opacity: 0;`;

      // List the user's potential video devices. (Front & Back Camera)
      try {
        const devices = await navigator.mediaDevices.enumerateDevices();
        const videoInputDevices = devices.filter(
          (device) => device.kind === "videoinput"
        );
        // if (debug)
        //   console.log(
        //     "🎥 Available constraints: ",
        //     navigator.mediaDevices.getSupportedConstraints()
        //   );
        // if (debug)
        //   console.log("🎥 Available video devices: ", videoInputDevices);
      } catch (error) {
        console.log(error.name + ": " + error.message);
      }

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
        const stream = await navigator.mediaDevices.getUserMedia({
          video: {
            facingMode,
            // Double the ideal resolution so there is a bit of downscaling
            // which makes for a sharper over-all image.
            width: { ideal: options.width * 2 },
            height: { ideal: options.height * 2 },
            frameRate: { ideal: 60 },
          },
          audio: false,
        });

        video.srcObject = stream;
        const videoTrack = stream.getVideoTracks()[0];

        // Update the global facingMode in case it's different from
        // what was requested.
        facingMode = videoTrack.getConstraints().facingMode;

        video.addEventListener(
          "loadedmetadata",
          () => {
            video.play();

            if (debug)
              console.log("🎥 Resolution:", buffer.width, buffer.height);
          },
          { once: true }
        );

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
              width: { ideal: width * 2 },
              height: { ideal: height * 2 },
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

        process();
      } catch (err) {
        console.log(err);
      }

      function process() {
        // TODO: Video effects / filter kernels could be added here...

        // zoom += 0.001;

        if (facingMode === "user") {
          bufferCtx.translate(buffer.width / 2, buffer.height / 2);
          bufferCtx.scale(-zoom, zoom);
          bufferCtx.translate(-buffer.width / 2, -buffer.height / 2);
        }

        bufferCtx.drawImage(video, 0, 0, buffer.width, buffer.height);

        bufferCtx.resetTransform();

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

        animationRequest = requestAnimationFrame(process);
      }
    }
  }

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
}

export { boot };
