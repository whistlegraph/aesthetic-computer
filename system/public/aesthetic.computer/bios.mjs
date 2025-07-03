// 💻 BIOS

// 📦 All Imports
import * as Loop from "./lib/loop.mjs";
import { Pen } from "./lib/pen.mjs";
import { Box } from "./lib/geo.mjs";
import { Keyboard } from "./lib/keyboard.mjs";
import { Gamepad } from "./lib/gamepad.mjs";
import { startCapturingMotion, stopCapturingMotion } from "./lib/motion.mjs";
import { speak, speakAPI } from "./lib/speech.mjs";
import * as UI from "./lib/ui.mjs";
import * as Glaze from "./lib/glaze.mjs";
import { apiObject, extension } from "./lib/helpers.mjs";
import { choose } from "./lib/help.mjs";
import { parse, slug } from "./lib/parse.mjs";
import { isKidlispSource, encodeKidlispForUrl } from "./lib/kidlisp.mjs";
import * as Store from "./lib/store.mjs";
import * as MIDI from "./lib/midi.mjs";
import * as USB from "./lib/usb.mjs";
import {
  MetaBrowser,
  iOS,
  Android,
  TikTok,
  Safari,
  Aesthetic,
  AestheticExtension,
} from "./lib/platform.mjs";
import { headers } from "./lib/headers.mjs";
import { logs } from "./lib/logs.mjs";
import { soundWhitelist } from "./lib/sound/sound-whitelist.mjs";
import { timestamp, radians } from "./lib/num.mjs";
import { UDP } from "./lib/udp.mjs";

// import * as TwoD from "./lib/2d.mjs"; // 🆕 2D GPU Renderer.
const TwoD = undefined;

const { assign, keys } = Object;
const { round, floor, min, max } = Math;
const { isFinite } = Number;

const diskSends = [];
let diskSendsConsumed = false;

// Store original URL parameters for refresh functionality
let preservedParams = {};

window.acDISK_SEND = function (message) {
  !diskSendsConsumed ? diskSends.push(message) : window.acSEND(message);
};

function consumeDiskSends(send) {
  if (diskSendsConsumed) return;
  diskSends.forEach((message) => send(message));
  diskSends.length = 0;
  diskSendsConsumed = true;
}

// 🔌 USB
USB.initialize();

// 💾 Boot the system and load a disk.
async function boot(parsed, bpm = 60, resolution, debug) {
  headers(); // Print console headers.

  // Store original URL parameters for refresh functionality from the resolution object
  preservedParams = {};
  if (resolution.gap === 0) preservedParams.nogap = "true"; // gap: 0 means nogap was true
  if (resolution.nolabel === true) preservedParams.nolabel = "true";

  if (debug) {
    if (window.isSecureContext) {
      // console.log("🔒 Secure");
    } else {
      console.warn("🔓 Insecure");
    }
  }

  window.acCONTENT_EVENTS = [];

  let HANDLE; // Populated with the user's handle from `disk`.

  let pen,
    keyboard,
    gamepad,
    keyboardFocusLock = false,
    keyboardSoftLock = false;
  let handData; // Hand-tracking.

  let frameCount = 0n;
  let now = 0;

  let diskSupervisor;
  let currentPiece = null; // Gets set to a path after `loaded`.
  let currentPieceHasKeyboard = false;

  // Media Recorder
  let mediaRecorder;
  let recordedFrames = [];
  const mediaRecorderChunks = [];
  let mediaRecorderDuration = 0,
    mediaRecorderStartTime;
  let needs$creenshot = false; // Flag when a capture is requested.

  // Events
  let whens = {};

  // Video storage
  const videos = [];

  // Media preloading tracker (for cancellations).
  const mediaPathsLoading = {};

  // Rendering

  // Wrap everything in an #aesthetic-computer div.
  const wrapper = document.createElement("div");
  wrapper.id = "aesthetic-computer";

  // 🖥️ Our main display surface. (Software Renderer)
  const canvas = document.createElement("canvas");
  const ctx = canvas.getContext("2d", { willReadFrequently: true });

  // 🖥️🔌 Secondary main display surface. (GPU Renderer)
  // TODO: Eventually deprecate the software renderer in favor of this?
  //       (Or even reuse the same tag if pieces swap.)
  //       TODO: Would it be possible for pieces to use both... and why?
  //             (Probably Not)
  TwoD?.initialize(wrapper);

  // An extra canvas reference for passing through or buffering video recording streams.
  let streamCanCtx;
  let paintToStreamCanvas = false;
  let startTapePlayback,
    stopTapePlayback,
    pauseTapePlayback,
    resumeTapePlayback;

  let shareFile, shareFileCallback; // A temporary storage container for a pre-prepped
  // file download to use on a user interaction.

  // A layer for modal messages such as "audio engine is off".
  const modal = document.createElement("div");
  modal.id = "modal";

  // A ui canvas for rendering a native resolution ui on top of everything.
  const uiCanvas = document.createElement("canvas");
  const uiCtx = uiCanvas.getContext("2d");
  uiCanvas.dataset.type = "ui";

  const debugCanvas = document.createElement("canvas");
  const debugCtx = debugCanvas.getContext("2d");
  debugCanvas.dataset.type = "debug";

  // A buffer for nicer resolution switches, nice when moving from
  // low resolution back to high resolution. Could eventually be used
  // for transition effects.
  const freezeFrameCan = document.createElement("canvas");
  const ffCtx = freezeFrameCan.getContext("2d");
  freezeFrameCan.dataset.type = "freeze";

  // A buffer for corner label overlays.
  const overlayCan = document.createElement("canvas");
  const octx = overlayCan.getContext("2d");

  let imageData;
  let fixedWidth, fixedHeight;
  let projectedWidth, projectedHeight;
  let canvasRect;

  // A post-process / effects layer.
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
  let subdivisions = 1; // Gets set in frame.

  const REFRAME_DELAY = 80; //250;
  let curReframeDelay = REFRAME_DELAY;
  let lastGap = undefined;
  let density = 2; // 1.333333; // window.acVSCODE ? 1.3333 : 2; // added to window.devicePixelRatio

  const startGap =
    location.host.indexOf("botce") > -1 || AestheticExtension ? 0 : 8;

  // Runs one on boot & every time display resizes to adjust the framebuffer.
  function frame(width, height, gap) {
    gap === 0
      ? document.body.classList.add("nogap")
      : document.body.classList.remove("nogap");

    if (gap === undefined) gap = lastGap ?? startGap;
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
          imageData.height,
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
      // console.log("Setting canvas opacity to 0...");

      freezeFrameFrozen = true;
    }

    // Find the width and height of our default screen and native projection.
    width = width || fixedWidth;
    height = height || fixedHeight;

    const gapSize = gap * window.devicePixelRatio;

    subdivisions = 1;

    if (width === undefined && height === undefined) {
      // Automatically set and frame a reasonable resolution.
      // Or pull from density.
      let ratio = density || window.devicePixelRatio;
      if (!density && window.devicePixelRatio === 1) ratio = 3; // Always force a screen density of 3 on non-retina displays.
      subdivisions = ratio;

      width =
        floor(window.innerWidth / subdivisions) - floor(gapSize / subdivisions);
      height =
        floor(window.innerHeight / subdivisions) -
        floor(gapSize / subdivisions);

      if (TikTok) height -= gap * 3;

      projectedWidth = round(width * density);
      projectedHeight = round(height * density);
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
        window.innerHeight,
      );

    // Send a message about this new width and height to any hosting frames.
    // parent.postMessage({ width: projectedWidth, height: projectedHeight }, "*");

    // TODO: Changing this width and height here will clear the canvas... which is no good...

    // ffCtx.drawImage(canvas, 0, 0);

    // TODO: How can I copy the pixels from canvas before changing it's width
    //       and height, and then copy them back after changing it here?
    // ctx.drawImage(canvas, 0, 0);

    // Create a temporary canvas
    const tempCanvas = document.createElement("canvas");
    const tempCtx = tempCanvas.getContext("2d");

    // Copy existing canvas contents
    tempCanvas.width = canvas.width;
    tempCanvas.height = canvas.height;
    tempCtx.drawImage(canvas, 0, 0);

    // Resize the original canvas
    canvas.width = width;
    canvas.height = height;

    // Restore the pixels onto the resized canvas
    ctx.drawImage(tempCanvas, 0, 0);

    tempCanvas.width = glazeComposite.width;
    tempCanvas.height = glazeComposite.height;

    tempCtx.drawImage(glazeComposite, 0, 0);

    glazeComposite.width = canvas.width;
    glazeComposite.height = canvas.height;
    glazeCompositeCtx.drawImage(tempCanvas, 0, 0);

    uiCanvas.width = projectedWidth * window.devicePixelRatio;
    uiCanvas.height = projectedHeight * window.devicePixelRatio;

    // Horizontal and vertical offsetting of the wrapper.

    if (TikTok) {
      wrapper.style.top = `${8 * 1.5}px`;
    } else {
      wrapper.style.top = (window.innerHeight - projectedHeight) / 2 + "px";
    }

    wrapper.style.left = (window.innerWidth - projectedWidth) / 2 + "px";
    wrapper.style.width = projectedWidth + "px";
    wrapper.style.height = projectedHeight + "px";

    canvas.style.width = projectedWidth + "px";
    canvas.style.height = projectedHeight + "px";
    uiCanvas.style.width = projectedWidth + "px";
    uiCanvas.style.height = projectedHeight + "px";

    if (debug) {
      debugCanvas.width = projectedWidth;
      debugCanvas.height = projectedHeight;
      debugCanvas.style.width = projectedWidth + "px";
      debugCanvas.style.height = projectedHeight + "px";
    }

    if (imageData?.length > 0) {
      ctx.putImageData(imageData, 0, 0);
    } else {
      imageData = ctx.getImageData(0, 0, canvas.width, canvas.height);
      // This will have zero alpha.
    }

    assign(screen, { pixels: imageData.data, width, height });

    TwoD?.frame(width, height, wrapper); // Reframe the 2D GPU layer.

    // Add the canvas, modal, and uiCanvas when we first boot up.
    if (!wrapper.contains(canvas)) {
      wrapper.append(canvas);
      wrapper.append(modal);

      const bumper = document.createElement("div");
      bumper.id = "bumper";
      modal.append(bumper);

      wrapper.append(uiCanvas);
      if (debug) wrapper.append(debugCanvas);
      document.body.append(wrapper);

      const fonts = [
        "berkeley-mono-variable.css",
        "ywft-processing-regular.css",
        "ywft-processing-light.css",
        "ywft-processing-bold.css",
      ];

      // Load fonts post-boot.
      fonts.forEach((font) => {
        const link = document.createElement("link");
        link.rel = "stylesheet";
        link.crossOrigin = "anonymous";
        link.href = "/type/webfonts/" + font;
        document.body.append(link);
      });

      if ("fonts" in document) {
        //  await document.fonts.load("1em YWFTProcessing-Light");
        //  await document.fonts.load("1em YWFTProcessing-Regular");
        // document.fonts.load("1em Berkeley Mono Variable").then(() => {
        //   setTimeout(() => {
        //     document.getElementById("console")?.classList.remove("hidden");
        //   }, 250);
        // });
      }

      // document.fonts.ready.then(function () {});

      // Trigger it to re-draw whenever the window resizes.
      let timeout;
      let lastWidth = window.innerWidth;
      let lastHeight = window.innerHeight;

      window.addEventListener("resize", (e) => {
        if (
          lastWidth === window.innerWidth &&
          lastHeight === window.innerHeight
        ) {
          return;
        }

        lastWidth = window.innerWidth;
        lastHeight = window.innerHeight;

        // Check to see if we are in "native-cursor" mode and hide
        // #aesthetic.computer for the resize if we aren't.
        if (document.body.classList.contains("native-cursor") === false) {
          // wrapper.classList.add("hidden");
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
          if (
            (document.hasFocus() &&
              !ticketWrapper &&
              event.target.tagName !== "A" &&
              event.target.tagName !== "IMG") ||
            event.touches.length > 2 // Prevent undo pop-up in Mobile Safari.
          ) {
            event.preventDefault();
          }
        },
        false,
      );
    }

    canvasRect = canvas.getBoundingClientRect();

    // Glaze.clear();

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
        },
      );
    } else {
      Glaze.off();
      glaze.on = false;
      canvas.style.removeProperty("opacity");
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

  // Used by `disk` to set the metatags by default when a piece loads. It can
  // be overridden using `meta` inside of `boot` for any given piece.
  // TODO: Some meta tags in practice use image_url & icon_url it seems.
  //       (Like in `hell_-world` or `freaky-flowers`) 23.10.25.20.32
  function setMetatags(meta) {
    if (meta?.title) {
      document.title = meta.title;
      const ogTitle = document.querySelector('meta[name="og:title"]');
      if (ogTitle) ogTitle.content = meta.title;
      const twitterTitle = document.querySelector('meta[name="twitter:title"]');
      if (twitterTitle) twitterTitle.content = meta.title;
    }
    if (meta?.desc) {
      const ogDesc = document.querySelector('meta[name="og:description"]');
      if (ogDesc) ogDesc.content = meta.desc;
      const desc = document.querySelector('meta[name="description"]');
      if (desc) desc.content = meta.desc;
    }
    if (meta?.img?.og) {
      const ogImg = document.querySelector('meta[name="og:image"]');
      if (ogImg) ogImg.content = meta.img.og;
    }
    if (meta?.img?.twitter) {
      const twitterImg = document.querySelector('meta[name="twitter:image"]');
      if (twitterImg) twitterImg.content = meta.img.twitter;
    }

    const icon = document.querySelector('link[rel="icon"]');

    if (icon) {
      if (meta?.icon_url) {
        if (icon.href !== meta.icon_url) icon.href = meta.icon_url;
      } else if (meta?.img?.icon) {
        if (icon.href !== meta.img.icon) icon.href = meta.img.icon;
      }
    }

    if (meta?.url) {
      // This might need to be conditional / opt-in?
      // document.querySelector('meta[name="twitter:player"').content = meta.url;
    }
  }

  // *** External Library Dependency Injection ***

  // FFMPEG.WASM
  async function loadFFmpeg() {
    return new Promise((resolve, reject) => {
      const script = document.createElement("script");
      script.src = "/aesthetic.computer/dep/ffmpeg/ffmpeg.min.js";

      script.onerror = function (err) {
        reject(err, s);
      };

      script.onload = function handleScriptLoaded() {
        if (debug && logs.deps) console.log("📼 FFmpeg has loaded.", FFmpeg);
        resolve(FFmpeg);
      };
      document.head.appendChild(script);
    });
    // return await import(`/aesthetic.computer/dep/@ffmpeg/ffmpeg-core.js`);
  }

  async function loadJSZip() {
    return new Promise((resolve, reject) => {
      const script = document.createElement("script");
      script.src = "/aesthetic.computer/dep/jszip.min.js";

      script.onerror = function (err) {
        reject(err, s);
      };

      script.onload = function handleScriptLoaded() {
        if (debug && logs.deps)
          console.log("🤐 JSZip has loaded.", window.JSZip);
        resolve(window.JSZip);
      };

      document.head.appendChild(script);
    });
  }

  async function loadStripe() {
    return new Promise((resolve, reject) => {
      const script = document.createElement("script");
      script.src = "https://js.stripe.com/v3/";
      script.crossOrigin = "anonymous";

      script.onerror = function (err) {
        reject(err, s);
      };

      script.onload = function handleScriptLoaded() {
        if (debug && logs.deps)
          console.log("🦓 Stripe has loaded.", window.Stripe);
        resolve(window.Stripe);
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
      send,
    );
  }

  // Web3
  async function loadWeb3() {
    return new Promise((resolve, reject) => {
      const script = document.createElement("script");
      script.src = "/aesthetic.computer/dep/web3/web3.min.js";

      script.onerror = (err) => reject(err, s);

      script.onload = function handleScriptLoaded() {
        if (debug) console.log("🕸️3️⃣ Ready...");
        resolve(Web3);
      };

      document.head.appendChild(script);
    });
  }

  // 2. 🔈 Audio
  const sound = {
    bpm: new Float32Array(1),
  };

  const sfx = {}; // Buffers of sound effects that have been loaded.
  const sfxPlaying = {}; // Sound sources that are currently playing.
  const sfxLoaded = {}; // Sound sources that have been buffered and loaded.

  // const sfX // ...

  const sfxCancel = [];
  speakAPI.sfx = sfx;
  // TODO: Some of these need to be kept (like system ones) and others need to
  // be destroyed after pieces change.

  let updateMetronome,
    beatSkip,
    activateSound,
    activatedSoundCallback,
    triggerSound,
    updateBubble,
    updateSound,
    killSound,
    killAllSound,
    clearSoundSampleCache,
    requestSpeakerWaveforms,
    requestSpeakerAmplitudes,
    attachMicrophone,
    detachMicrophone,
    audioContext,
    audioStreamDest,
    sfxStreamGain,
    micStreamGain,
    micGainNode,
    speakerGain;

  let requestMicrophoneAmplitude,
    requestMicrophoneWaveform,
    requestMicrophonePitch,
    requestMicrophoneRecordingStart,
    requestMicrophoneRecordingStop;

  // TODO: Eventually this would be replaced with a more dynamic system.

  const backgroundTrackURLs = [
    "0 - analog multiplication",
    "1 - castlecowards",
    "2 - epanodos clinamen",
    "3 - for not being able",
    "4 - pantoum chain rhyme",
    "5 - they sit so nicely",
    "6 - vociferatings witchbefooled",
    "7 - an accuracy which it seems as impossible to attain",
    "8 - bivariate beamforming",
    "9 - and the three of them began to make",
    "10 - or perhaps destroyed",
    "11 - sunsmidnought",
    "12 - improvements design",
    "13 - consideration",
    "14 - magellanic clouds",
    "15 - syncopation demotic",
    "16 - textual criticism ambiguity",
  ];

  const backgroundMusicEl = document.createElement("audio");
  backgroundMusicEl.id = "background-music";
  backgroundMusicEl.crossOrigin = "anonymous";
  wrapper.appendChild(backgroundMusicEl);

  let analyserCtx, analyserSrc, analyser, frequencyData;
  let currentBackgroundTrack;

  function playBackgroundMusic(n, volume) {
    if (currentBackgroundTrack !== n && !isNaN(n)) {
      let origin;

      if (window.production === true) {
        origin = "https://assets.aesthetic.computer/bgm/";
      } else {
        origin = "/assets/bgm/";
      }

      const ext = Safari ? "m4a" : "ogg";
      backgroundMusicEl.src = origin + backgroundTrackURLs[n] + "." + ext;
      backgroundMusicEl.volume = parseFloat(volume);
      if (audioContext) {
        backgroundMusicEl.play();
      }
      currentBackgroundTrack = n;
    }
  }

  function stopBackgroundMusic() {
    currentBackgroundTrack = null;
    backgroundMusicEl.src = "";
  }

  function startSound() {
    if (navigator.audioSession) navigator.audioSession.type = "ambient";

    // Main audio feed
    audioContext = new AudioContext({
      latencyHint: "interactive",
      // TODO: Eventually choose a good sample rate and/or make it settable via
      //       the current disk.
      // sampleRate: 44100,
      // sampleRate: 48000,
      sampleRate: 48000, //iOS || Aesthetic || Android ? 48000 : 192000,
    });

    acDISK_SEND({
      type: "audio:sample-rate",
      content: audioContext.sampleRate,
    });

    // Process any queued sounds now that audioContext is available
    processPendingSfx();

    // Main audio feed
    // audioContext = new AudioContext({
    //   latencyHint: "interactive",
    //   sampleRate: Math.min(
    //     192000,
    //     new (window.AudioContext || window.webkitAudioContext)().sampleRate,
    //   ),
    // });

    // BGM Analyser
    analyserCtx = new AudioContext();
    analyserSrc = analyserCtx.createMediaElementSource(backgroundMusicEl);
    analyser = analyserCtx.createAnalyser();
    analyser.fftSize = 256; // See also: https://developer.mozilla.org/en-US/docs/Web/API/AnalyserNode/frequencyBinCount

    analyserSrc.connect(analyser);
    analyser.connect(analyserCtx.destination);
    frequencyData = new Uint8Array(analyser.frequencyBinCount);

    // console.log("Sound started.");

    speakAPI.audioContext = audioContext; // Make audioContext global, for
    //                                       `speech` and perhaps other
    //                                       modules. 23.08.17.12.31

    audioStreamDest = audioContext.createMediaStreamDestination();
    sfxStreamGain = audioContext.createGain();
    sfxStreamGain.gain.value = 1;
    sfxStreamGain.connect(audioStreamDest);

    speakerGain = audioContext.createGain();
    speakerGain.gain.value = 1;
    speakerGain.connect(audioContext.destination);

    if (audioContext.state === "running") {
      // audioContext.suspend();
    }

    // TODO: Check to see if there is support for AudioWorklet or not...
    //       and and use ScriptProcessorNode as a fallback. 2022.01.13.21.00

    // Microphone Input Processor
    // (Gets attached via a message from the running disk.)
    attachMicrophone = async (data) => {
      if (navigator.audioSession)
        navigator.audioSession.type = "play-and-record"; // play-and-record";

      let micStream;
      let reason;
      try {
        micStream = await navigator.mediaDevices.getUserMedia({
          audio: {
            echocancellation: false, // put this behind a flag?
            latency: 0,
            noisesuppression: false,
            autogaincontrol: false,
          },
        });
      } catch (err) {
        if (debug) console.warn("🎙 Microphone disabled:", err);
        reason = err.message;
      }

      // return;

      if (!micStream) {
        send({ type: "microphone-connect:failure", content: { reason } });
        return;
      }

      const micNode = new MediaStreamAudioSourceNode(audioContext, {
        mediaStream: micStream,
      });

      // TODO: Why can't there be separate audioWorklet modules?
      await audioContext.audioWorklet.addModule(
        "/aesthetic.computer/lib/microphone.mjs",
      );

      const micProcessorNode = new AudioWorkletNode(
        audioContext,
        "microphone-processor",
        {
          outputChannelCount: [2],
          processorOptions: { debug },
        },
      );

      // Receive messages from the microphone processor thread.
      micProcessorNode.port.onmessage = (e) => {
        const msg = e.data;

        if (msg.type === "amplitude") {
          send({ type: "microphone-amplitude", content: msg.content });
        }

        if (msg.type === "waveform") {
          // console.log("Mic waveform:", msg.content);
          send({ type: "microphone-waveform", content: msg.content });
        }

        if (msg.type === "pitch") {
          send({ type: "microphone-pitch", content: msg.content });
        }

        if (msg.type === "recording:complete") {
          // console.log("Recording complete:", msg.content);
          // Turn this into a sample with a playback ID here and send
          // the sample ID back.
          const id = "microphone-recording" + "_" + performance.now();

          //if (debug)
          //  console.log("🔈 Buffer length:", msg.content.recording?.length);

          // Create an empty mono AudioBuffer (1 channel)
          const buffer = audioContext.createBuffer(
            1,
            msg.content.recording.length,
            audioContext.sampleRate,
          );
          const channel = buffer.getChannelData(0); // Ref to the first channel.
          channel.set(msg.content.recording);
          // Copy your Float32Array data into the buffer's channel

          // console.log("Recording:", msg.content.recording);

          sfx[id] = buffer; // Set the sfx id so the sfx system
          //                   can play back the sample.

          send({
            type: "microphone-recording:complete",
            content: { id, data: msg.content.recording },
          });
        }
      };

      requestMicrophoneRecordingStart = () => {
        micProcessorNode.port.postMessage({ type: "record:start" });
      };

      requestMicrophoneRecordingStop = () => {
        micProcessorNode.port.postMessage({ type: "record:stop" });
      };

      // Request data / send message to the mic processor thread.
      requestMicrophoneAmplitude = () => {
        micProcessorNode.port.postMessage({ type: "get-amplitude" });
      };

      requestMicrophoneWaveform = () => {
        micProcessorNode.port.postMessage({ type: "get-waveform" });
      };

      requestMicrophonePitch = () => {
        micProcessorNode.port.postMessage({ type: "get-pitch" });
      };

      micStreamGain = audioContext.createGain();
      micGainNode = audioContext.createGain();

      // Connect mic to the mediaStream.

      // TODO: How to automatically dip the mic gain node?

      // micNode.connect(micProcessorNode);
      micNode.connect(micGainNode);
      micGainNode.connect(micProcessorNode);

      micProcessorNode.connect(micStreamGain);
      micStreamGain.connect(audioStreamDest);
      // speakerGain.gain.value = 0.35;
      // micStreamGain.gain.value = 1.5;

      micGainNode.gain.value = 1;
      micStreamGain.gain.value = 1;
      sfxStreamGain.gain.value = 1;

      // Connect to the speaker if we are monitoring audio.
      if (data?.monitor === true)
        micProcessorNode.connect(audioContext.destination);

      // Setup microphone detachment function.
      detachMicrophone = () => {
        if (navigator.audioSession) navigator.audioSession.type = "ambient";
        micProcessorNode.disconnect();
        micNode.disconnect();
        micStream.getTracks().forEach((t) => t.stop());
        // speakerGain.gain.value = 1;
        sfxStreamGain.gain.value = 1;
        if (debug) console.log("🎙💀 Microphone:", "Detached");
        send({ type: "microphone-disconnect" });
      };

      // Send a message back to `disk` saying the microphone is connected.
      send({ type: "microphone-connect:success" });
      if (debug) console.log("🎙 Microphone connected:", data);
    };

    // Sound Synthesis Processor
    try {
      (async () => {
        const baseUrl = "/aesthetic.computer/lib/speaker.mjs";
        const cacheBuster = /*debug ?*/ `?time=${new Date().getTime()}`; // : "";
        await audioContext.audioWorklet.addModule(baseUrl + cacheBuster);

        const speakerProcessor = new AudioWorkletNode(
          audioContext,
          "speaker-processor",
          {
            outputChannelCount: [2],
            processorOptions: { bpm: sound.bpm, debug: true },
          },
        );

        beatSkip = function () {
          speakerProcessor.port.postMessage({ type: "beat:skip" });
        };

        updateMetronome = function (newBPM) {
          speakerProcessor.port.postMessage({ type: "new-bpm", data: newBPM });
        };

        triggerSound = function (sound) {
          speakerProcessor.port.postMessage({ type: "sound", data: sound });

          return {
            progress: () => {
              // console.log("🟠 Get progress of sound...", sound);
              speakerProcessor.port.postMessage({
                type: "get-progress",
                content: sound.id,
              });
            },
            kill: (fade) => {
              killSound(sound.id, fade);
            },
            update: (properties) => {
              // console.log(
              //   "Update needed for sound:",
              //   sound,
              //   "With:",
              //   properties,
              // );
              updateSound?.({ id: sound.id, properties });
            },
          };
        };

        updateBubble = function (bubble) {
          speakerProcessor.port.postMessage({ type: "bubble", data: bubble });
        };

        killSound = function (id, fade) {
          // console.log("📢 Kill:", id, "Fade:", fade);
          delete sfxPlaying[id];
          speakerProcessor.port.postMessage({
            type: "kill",
            data: { id, fade },
          });
        };

        updateSound = function (data) {
          speakerProcessor.port.postMessage({ type: "update", data });
        };

        killAllSound = function () {
          speakerProcessor.port.postMessage({ type: "kill:all" });
        };

        clearSoundSampleCache = function () {
          speakerProcessor.port.postMessage({ type: "cache:clear" });
          for (const k in sfxLoaded) delete sfxLoaded[k];
        };

        // Request data / send message to the mic processor thread.
        requestSpeakerWaveforms = function () {
          speakerProcessor.port.postMessage({ type: "get-waveforms" });
        };

        requestSpeakerAmplitudes = function () {
          speakerProcessor.port.postMessage({ type: "get-amplitudes" });
        };

        speakerProcessor.port.onmessage = ({ data: msg }) => {
          if (msg.type === "waveforms") {
            send({ type: "waveforms", content: msg.content });
            return;
          }

          if (msg.type === "amplitudes") {
            send({ type: "amplitudes", content: msg.content });
            return;
          }

          if (msg.type === "metronome") {
            diskSupervisor.requestBeat?.(msg.content); // Update metronome.
            return;
          }

          if (msg.type === "progress") {
            // Send sound progress to the disk.
            // console.log("Received progress for:", msg);
            send({
              type: "sfx:progress:report",
              content: msg.content,
            });
            return;
          }

          if (msg.type === "killed") {
            send({ type: "sfx:killed", content: msg.content });
            return;
          }
        };

        speakerProcessor.connect(sfxStreamGain); // Connect to the mediaStream.
        speakerProcessor.connect(speakerGain);

        activatedSoundCallback?.();

        // Process any queued sound effects now that audioContext is available
        processPendingSfx();

        modal.classList.remove("on");
      })();
    } catch (e) {
      console.log("Sound failed to initialize:", e);
    }

    function enableAudioPlayback(skip = false) {
      if (backgroundMusicEl.paused && currentBackgroundTrack !== null) {
        backgroundMusicEl.play();
      }
      if (!skip && ["suspended", "interrupted"].includes(audioContext.state)) {
        audioContext.resume();
      }
    }

    //enableAudioPlayback(true);
    window.addEventListener("pointerdown", () => enableAudioPlayback());
    window.addEventListener("keydown", () => enableAudioPlayback());
  }

  // Play a sound back through the sfx system.
  // 🌡️ TODO: `sfx` could be scraped for things that need to be decoded
  //          upon audio activation. This would probably be helpful
  //          in terms of creating a sampler and asynchronously
  //          decoding all the sounds after an initial tap.

  async function playSfx(id, soundData, options, completed) {
    if (audioContext) {
      if (sfxCancel.includes(id)) {
        sfxCancel.length = 0;
        return;
      }

      // Instantly decode the audio before playback if it hasn't been already.
      await decodeSfx(soundData);

      if (sfx[soundData] instanceof ArrayBuffer) {
        return;
      }

      if (!sfx[soundData]) {
        // Queue the sound effect to be played once it's loaded
        pendingSfxQueue.push({ id, soundData, options, completed, queuedAt: Date.now() });
        return;
      }

      const channels = [];
      for (let i = 0; i < sfx[soundData].numberOfChannels; i += 1) {
        channels.push(sfx[soundData].getChannelData(i)); // Get raw Float32Array.
      }

      const sample = {
        channels,
        sampleRate: sfx[soundData].sampleRate,
        length: sfx[soundData].length,
      };

      // TODO: ⏰ Memoize the buffer data after first playback so it doesn't have to
      //          keep being sent on every playthrough. 25.02.15.08.22

      // console.log("👮 Sample ID:", id, "Sound data:", soundData);

      sfxPlaying[id] = triggerSound?.({
        id,
        type: "sample",
        options: {
          buffer: sfxLoaded[soundData] ? soundData : sample, // Alternatively send a memoized code using a lookup table.
          label: soundData, // TODO: 🚩 This needs to be invalidated by `tape`.
          // TODO: 🚩 Cached speaker sounds need to be dumped on a piece swap.
          from: isFinite(options?.from) ? options.from : 0,
          to: isFinite(options?.to) ? options.to : 1,
          speed: isFinite(options?.speed) ? options.speed : 1,
          loop: options?.loop || false,
        },
        // options: { buffer: sample },
        // ⏰ TODO: If duration / 'beats' is not specified then use speed.
        // beats: undefined, // ((sample.length / sample.sampleRate) * sound.bpm / 60),
        // attack: 0, // 🩷 TODO: These should have saner defaults.
        // decay: 0,
      });

      if (triggerSound) sfxLoaded[soundData] = true;
    } else {
      // Queue the sound effect to be played once audioContext is available
      pendingSfxQueue.push({ id, soundData, options, queuedAt: Date.now() });
    }
  }

  speakAPI.playSfx = playSfx;

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
  const fullPath =
    "/aesthetic.computer/lib/disk.mjs" +
    window.location.search +
    "#" +
    Date.now(); // bust the cache. This prevents an error related to Safari loading workers from memory.

  const sandboxed =
    (window.origin === "null" || !window.origin) && !window.acVSCODE;

  const microphonePermission = await checkMicrophonePermission();

  const firstMessage = {
    type: "init-from-bios",
    content: {
      parsed,
      debug,
      rootPiece: window.acSTARTING_PIECE,
      user: window.acUSER,
      lanHost: window.acLAN_HOST,
      iframe: window.self !== window.top,
      sandboxed,
      shareSupported: (iOS || Android) && navigator.share !== undefined,
      previewOrIcon: window.acPREVIEW_OR_ICON,
      vscode: window.acVSCODE,
      microphonePermission,
      resolution,
    },
  };

  const onMessage = (m) => receivedChange(m);

  let send = (msg) => {
    console.warn("Send has not been wired yet!", msg);
  };

  //  👷️ Always use workers if they are supported, except for
  //     when we are in VR (MetaBrowser).
  // Disable workers if we are in a sandboxed iframe.
  const workersEnabled = !sandboxed;
  // const workersEnabled = false;

  if (/*!MetaBrowser &&*/ workersEnabled) {
    const worker = new Worker(new URL(fullPath, window.location.href), {
      type: "module",
    });

    // Rewire things a bit if workers with modules are not supported (Firefox).
    worker.onerror = async (err) => {
      // if (
      //   err.message ===
      //   "SyntaxError: import declarations may only appear at top level of a module"
      // ) {
      console.error("🛑 Disk error:", err);
      // console.error("🚨 Error message:", err.message);
      // console.error("🚨 Error filename:", err.filename);
      // console.error("🚨 Error lineno:", err.lineno);
      // console.error("🚨 Error colno:", err.colno);

      console.warn("🟡 Attempting a dynamic import...");
      // https://bugzilla.mozilla.org/show_bug.cgi?id=1247687
      const module = await import(`./lib/disk.mjs`);
      module.noWorker.postMessage = (e) => onMessage(e); // Define the disk's postMessage replacement.
      send = (e) => module.noWorker.onMessage(e); // Hook up our post method to disk's onmessage replacement.
      window.acSEND = send; // Make the message handler global, used in `speech.mjs` and also useful for debugging.
      send(firstMessage);
      consumeDiskSends(send);
      // } else {
      // TODO: Try and save the crash here by restarting the worker
      //       without a full system reload?
      // }
    };

    if (worker.postMessage) {
      // console.log("🟢 Worker");
      send = (e, shared) => worker.postMessage(e, shared);
      window.acSEND = send; // Make the message handler global, used in `speech.mjs` and also useful for debugging.
      worker.onmessage = onMessage;
    }
  } else {
    // B. No Worker Mode
    if (debug) console.log("🔴 No Worker");
    let module;
    try {
      module = await import(`./lib/disk.mjs`);
    } catch (err) {
      console.warn("Module load error:", err);
    }
    module.noWorker.postMessage = (e) => onMessage(e); // Define the disk's postMessage replacement.
    send = (e) => module.noWorker.onMessage(e); // Hook up our post method to disk's onmessage replacement.
    window.acSEND = send; // Make the message handler global, used in `speech.mjs` and also useful for debugging.
  }

  // The initial message sends the path and host to load the disk.
  send(firstMessage);
  consumeDiskSends(send);

  // Beat

  // Set the default bpm.
  sound.bpm = bpm;

  function requestBeat(time) {
    send({
      type: "beat",
      content: { time, bpm: sound.bpm },
    });
  }

  // Called inside of `requestFrame`, and on the `beat` message.
  function updateSynths(content) {
    function beat() {
      if (sound.bpm !== content.bpm) {
        sound.bpm = content.bpm;
        updateMetronome(sound.bpm);
      }
      for (const sound of content.sounds) triggerSound(sound);
      for (const bubble of content.bubbles) updateBubble(bubble);
      for (const item of content.kills) killSound(item.id, item.fade);
    }

    if (
      (!triggerSound || audioContext?.state !== "running") &&
      (sound.bpm !== content.bpm ||
        content.sounds.length > 0 ||
        content.bubbles.length > 0 ||
        content.kills.length > 0)
    ) {
      activatedSoundCallback = beat;
      // 📓 Hold a single update frame if audio cuts out or is just beginning.
      return;
    } else beat();
  }

  // Update & Render
  let frameAlreadyRequested = false;

  function requestFrame(needsRender, updateCount, nowUpdate) {
    now = nowUpdate;

    if (needsRender && needsReframe) {
      frame(undefined, undefined, lastGap);
      pen.retransformPosition();
      //frameAlreadyRequested = false; // Deprecated. 23.09.17.01.20
      return;
    }

    if (frameAlreadyRequested) return;

    frameAlreadyRequested = true;
    frameCount += 1n;

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
          audioTime: audioContext?.currentTime || 0,
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
          gamepad: gamepad.events,
          // clipboardText: pastedText,
        },
      },
      transferrableObjects,
    );

    // if (Object.keys(pen.pointers).length > 1) {
    //   console.log(pen.events, pen.pointers);
    // }

    //pastedText = undefined; // Clear any pasted text.

    pen.updatePastPositions();

    // Time budgeting stuff...
    //const updateDelta = performance.now() - updateNow;
    //console.log("Update Budget: ", round((updateDelta / updateRate) * 100));
    // TODO: Output this number graphically.

    // const renderNow = performance.now();
    // const renderDelta = performance.now() - renderNow;
    // console.log("Render Budget: ", round((renderDelta / renderRate) * 100));

    // TODO: Output this number graphically.

    //render3d();
    // Clear pen events.
    pen.events.length = 0;
    if (ThreeD?.penEvents) ThreeD.penEvents.length = 0;

    // Clear keyboard and gamepad events.
    keyboard.events.length = 0;
    gamepad.events.length = 0;
  }

  let frameCached = false;
  let pixelsDidChange = false; // TODO: Can this whole thing be removed? 2021.11.28.03.50

  let contentFrame;
  let ticketWrapper;
  let underlayFrame;

  // const bakedCan = document.createElement("canvas", {
  //  willReadFrequently: true,
  // });

  // *** Received Frame ***
  async function receivedChange({ data: { type, content } }) {
    if (type === "pen:lock") {
      console.log("🖋️ Request pen lock...");
      wrapper.requestPointerLock();
      return;
    }

    if (type === "midi:connect") {
      MIDI.initialize(); // Start 🎹 Detection.
      return;
    }

    // Respond to a request to send a message through to the iMessage extension.
    if (type === "imessage-extension:send") {
      let body = content.body.pixels
        ? await bufferToBlob(content.body.pixels, undefined, {
            scale: 6,
            dataURL: true,
          })
        : content.body;
      const message = { type: content.type, body };
      const packedMessage = JSON.stringify(message);
      if (debug) console.log("🗨️ Sending to iMessage:", packedMessage);
      window.webkit?.messageHandlers?.iMessageExtension.postMessage(
        packedMessage,
      );
      return;
    }

    if (type === "ios:send") {
      iOSAppSend({ type: content.type, body: content.body });
      return;
    }

    // Post a message to a potential iframe parent, like in the VSCode extension.
    if (type === "post-to-parent") {
      // if (debug) console.log("🏃‍♂️ Posting up to parent...", content);
      if (window.parent) window.parent.postMessage(content, "*");
      return;
    }

    // Connect to a UDP server,
    // which will pass messages to the disk runner.
    if (type === "udp:connect") {
      UDP.connect(content.port, content.url, send);
      return;
    }

    // Send a message to the UDP server.
    if (type === "udp:send") {
      UDP.send(content);
      return;
    }    // Disconect from the UDP server.
    if (type === "udp:disconnect") {
      UDP.disconnect(content.outageSeconds);
      return;
    }

    if (type === "ticket-wall") {
      if (!window.Stripe) await loadStripe();
      let pretext = ``;
      let button = `buy ticket`;
      let color = `white;`;
      let desc = `Chat now or check email for tickets.<br><br><span id="stripe">Processed by <a href="https://stripe.com">Stripe</a></span>`;

      if (content.item === "botce") {
        pretext = `
        <div id="pretext">
          <img style="image-rendering: pixelated;" cross-origin="anonymous" src="https://sotce-media.aesthetic.computer/botce-b.gif">
          <div id="pretext-bullets">
            <span id="desc">visit with <code>botce</code></span>
            <ul id="features">
              <li><code>botce</code> is a helpful ai</li>
              <li><code>botce</code> has advice</li>
              <!--<li><code>botce</code> is here until 11/25</li>-->
            </ul>
          </div>
        </div>
        <style>
         #pretext {
           display: flex;
           flex-direction: row;
           padding-bottom: 1.5em;
         }
         #pretext-bullets {
          display: flex;
           flex-direction: column;
           color: black;
         }
         #pretext img {
          /* border: 2px solid white; */
          border-radius: 10%;
          max-width: 30%;
          width: 100%;
          height: 100%;
         }
         #features {
          padding-left: 1.7em;
          margin-top: 0.4em;
          list-style-type: none;
         }
         #desc {
          color: #30313D;
          margin: 0 auto 0 1em;
          background: rgba(255, 100, 100, 0.5);me
          border-radius: 6px;
          padding: 8px 12px;
         }
         code {
          font-weight: bold;
         }
        </style>
        `;

        button = "$6";
        color = "rgb(255, 200, 200, 0.95)";
      }

      const template = `
        <link rel="stylesheet" href="aesthetic.computer/checkout.css" />
        <form style="background: ${color};" id="payment-form" class=${content.item}>
          ${pretext}
          <div id="link-authentication-element">
          </div>
          <div id="payment-element">
          </div>
          <!-- <div id="payment-request-button"></div> -->
          <button id="submit">
            <div class="spinner hidden" id="spinner">Processing...</div>
            <span id="button-text">${button}</span>
          </button>
          <div id="payment-description">${desc}</div>
          <div id="payment-message-wrapper">
            <div id="payment-message" class="hidden"></div>
          </div>
        </form>
      `;
      ticketWrapper = document.createElement("div");
      ticketWrapper.id = "ticket";
      ticketWrapper.innerHTML = template;
      ticketWrapper.classList.add("hidden");
      wrapper.appendChild(ticketWrapper);
      const { ticket } = await import(`./lib/ticket.mjs`);
      ticket(content.from, content.item, () => {
        ticketWrapper.classList.remove("hidden");
      }); // Open the ticket overlay.
      return;
    }

    if (type === "handle") {
      HANDLE = content; // Set the global HANDLE const to the user handle.
      return;
    }

    // Zip up some data and download it.
    if (type === "zip") {
      if (!window.JSZip) await loadJSZip();
      const zip = new window.JSZip(); // https://github.com/Stuk/jszip

      if (content.painting) {
        const steps = [];
        const images = {};

        // Encode `painting:recording` format.
        content.painting.record.forEach((step) => {
          const format = `${step.timestamp} - ${step.label}`;
          const encodedStep = { step: format };
          if (step.gesture?.length > 0) encodedStep.gesture = step.gesture;
          steps.push(encodedStep);
          if (step.painting) {
            images[format] = bufferToBlob(step.painting, "image/png");
          }
        });

        const stepFile = JSON.stringify(steps); // Encode a JSON file for steps.

        zip.file("painting.json", stepFile);

        // Add all images based on step and index.
        keys(images).forEach((label) => {
          zip.file(`${label}.png`, images[label]);
        });

        const finalTimestamp =
          content.painting.record[content.painting.record.length - 1].timestamp;

        const zipped = await zip.generateAsync({ type: "blob" });
        const filename = `painting-${finalTimestamp}.zip`;

        if (content.destination === "download") {
          // See also: `receivedDownload`.
          const a = document.createElement("a");
          a.href = URL.createObjectURL(zipped);
          a.target = "_blank";
          a.download = filename; // Remove any extra paths.
          a.click();
          URL.revokeObjectURL(a.href);
          send({ type: "zipped", content: { result: "success", data: true } });
        } else if (content.destination === "upload") {
          // TODO: Put this on the S3 server somewhere...
          console.log("🤐 Uploading zip...", zipped);
          receivedUpload({ filename, data: zipped }, "zipped");
        }
      } else {
        send({ type: "zipped", content: { result: "error", data: false } });
      }

      return;
    }

    // Load a zip from a URL and return its unpacked contents to the piece.
    if (type === "zip:load") {
      console.log("Load zip remotely...", content);
      fetch(decodeURI(content))
        .then((response) => {
          // console.log("Response", response);
          if (response.status === 200 || response.status === 304) {
            return response.arrayBuffer();
          } else {
            throw new Error(`Zip not found. Status: ${response.status}`);
          }
        })
        .then(async (buffer) => {
          if (!window.JSZip) await loadJSZip();
          const record = await unzip(buffer);
          if (record.length === 0) throw new Error("Record is an empty array");
          send({
            type: "loaded-zip-success",
            content: { url: content, data: record },
          });
        })
        .catch((error) => {
          send({ type: "loaded-zip-rejection", content: { url: content } });
        });
      return;
    }

    // Capture device motion.
    if (type === "motion:start") {
      startCapturingMotion();
      return;
    }

    if (type === "motion:stop") {
      stopCapturingMotion();
      return;
    }

    // Speech synthesis. (local and remote)
    if (type === "speak") {
      speak(content.utterance, content.voice, content.mode, content.opts);
      return;
    }

    // Show a classic DOM / window style alert box.
    if (type === "alert") {
      window.alert(content);
      return;
    }

    // Add a DOM event hitbox for the `Button Hitboxes`
    // event listener on the document.
    // 📓 Adding the same label multiple times will have no additional effect.
    if (type === "button:hitbox:add") {
      if (hitboxes[content.label] !== undefined) return;

      let state = "up";
      // Event handler for each button press.
      hitboxes[content.label] = async (e) => {
        const frame = canvas.getBoundingClientRect();
        const xscale = projectedWidth / canvas.width;
        const yscale = projectedHeight / canvas.height;
        const hitbox = Box.from({
          x: frame.left + content.box.x * xscale,
          y: frame.top + content.box.y * yscale,
          w: content.box.w * xscale,
          h: content.box.h * yscale,
        });

        // 📓 Uncomment to debug the hitboxes and see how they line up.
        // const dbg = Box.from({
        //   x: content.box.x * xscale,
        //   y: content.box.y * yscale,
        //   w: content.box.w * xscale,
        //   h: content.box.h * yscale,
        // });
        // debugCtx.fillStyle = "red";
        // debugCtx.globalAlpha = 0.5;
        // debugCtx.fillRect(dbg.x, dbg.y, dbg.w, dbg.h);

        const hit = hitbox.contains({ x: e.x, y: e.y });

        if (e.type === "pointerup" && state === "down" && hit) {
          // This is pretty specific to the "copy" clipboard
          // stuff for now. 23.06.16.15.03
          // console.log("🔘 Button tap label:", content.label);

          if (content.label === "copy") {
            try {
              await navigator.clipboard.writeText(content.message);
              send({ type: "copy:copied" });
            } catch (err) {
              console.warn("📋 Clipboard copy failed:", err);
              if (window.parent) {
                console.log("📋 Trying via message...");
                window.parent.postMessage(
                  { type: "clipboard:copy", value: content.message },
                  "*",
                );
              } else {
                send({ type: "copy:failed" });
              }
            }
          }

          // Paste should always happen on a pointerdown.
          if (content.label === "paste") {
            try {
              const pastedText = await navigator.clipboard.readText();
              // This routes through to the `pasted:text` event in `disk`.
              // where `pastedText` is sent on the next frame.
              if (pastedText.length === 0) {
                send({ type: "paste:pasted:empty" });
              } else {
                // Insert pasted text at current caret position.
                if (keyboard) {
                  const start = keyboard.input.selectionStart;
                  const end = keyboard.input.selectionEnd;
                  const selLen = end - start;

                  const beforeCursor = keyboard.input.value.substring(0, start);
                  const afterCursor = keyboard.input.value.substring(
                    selLen > 0 ? end : start,
                  );

                  keyboard.input.value =
                    beforeCursor + pastedText + afterCursor;

                  const newCursorPosition = start + pastedText.length;
                  keyboard.input.setSelectionRange(
                    newCursorPosition,
                    newCursorPosition,
                  );

                  send({
                    type: "prompt:text:replace",
                    content: {
                      text: keyboard.input.value,
                      cursor: keyboard.input.selectionStart,
                    },
                  });

                  if (document.activeElement !== keyboard.input) {
                    keyboard.input.focus();
                  }
                }
              }
              // send({
              //   type:
              //     pastedText.length > 0 ? "paste:pasted" : "paste:pasted:empty",
              // });
            } catch (err) {
              console.warn(err);
              send({ type: "paste:failed" });
            }
          }

          state = "up";
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
      if (window.self !== window.top) {
        window.parent.postMessage({ type: "login" }, "*");
      } else {
        window.acLOGIN?.();
      }
      return;
    }

    if (type === "signup") {
      if (window.self === window.top) {
        window.acLOGIN?.("signup");
      } else {
        console.log("🟠 Cannot sign up in an embedded view.");
      }
      return;
    }

    if (type === "logout") {
      if (window.acTOKEN) {
        if (window.parent) {
          window.parent.postMessage({ type: "logout" }, "*");
          localStorage.removeItem("session-aesthetic");
        }
        // Just use the logout services of the host.
      } else {
        window.acLOGOUT?.();
        window.flutter_inappwebview?.callHandler("closeWebview"); // Close A.C. webview on logout inside of Autonomy wallet.
      }
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
      // console.log("Getting token...");
      const token = await authorize();
      // console.log("Failure token:", token);
      send({
        type: "authorization:response",
        content: { data: token || null, result: token ? "success" : "error" },
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
              [address],
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
                [namehash],
              ),
            }),
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
          "🔴 Web3 is unavailable. Please install an Ethereum wallet or enable your extension.",
        );
      }
      return;
    }

    if (type === "rewrite-url-path") {
      const newPath = content.path;
      // if (window.origin !== "null") {
      if (content.historical) {
        console.log("Rewriting to:", newPath);
        history.pushState("", document.title, newPath);
      } else {
        history.replaceState("", document.title, newPath);
      }
      // }
      return;
    }

    if (type === "bgm-change") {
      playBackgroundMusic(content.trackNumber, content.volume || 1);
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
      keyboard = new Keyboard();
      {
        // console.log("⌨️ 🤖 Initializing Virtual Keyboard");
        /**
         * Insert a hidden input element that is used to toggle the software
         * keyboard on touchscreen devices like iPhones and iPads.
         * *Only works in "disks/prompt".
         */
        let keyboardOpen = false;
        let keyboardOpenMethod;
        const input = document.createElement("textarea");
        const form = document.createElement("form");
        form.id = "software-keyboard-input-form";
        form.style.opacity = 0;
        input.style.position = "absolute";
        input.id = "software-keyboard-input";
        input.autocapitalize = "none";
        // input.autofocus = true;
        // input.type = "text";
        input.autocomplete = "off";
        input.style.opacity = 0;
        input.style.width = 0 + "px";
        input.style.height = 0 + "px";
        // input.setAttribute("readonly", true);
        // input.enterkeyhint = "go"; // Why doesn't this work?

        // 📓 Uncomment to debug text editing form synchronization.
        // form.style.opacity = 1;
        // input.style.zIndex = 100;
        // input.style.top = "50px";
        // input.style.left = "100px";
        // input.style.opacity = 1;
        // input.style.width = 200 + "px";
        // input.style.height = 50 + "px";

        form.append(input);
        wrapper.append(form);

        keyboard.focusHandler = function (e) {
          if (!currentPieceHasKeyboard) return;
          if (keyboardFocusLock || keyboardSoftLock) return;
          if (
            document.activeElement !== input &&
            e.key !== "`" &&
            e.key !== "Escape"
          ) {
            keyboardOpenMethod = "keyboard";
            input.focus();

            if (e.key.length !== 1 || e.ctrl) {
              send({
                type: "prompt:text:replace",
                content: { text: "", cursor: 0, mute: true },
              });
            }

            return true;
          } else if (e.key === "Enter" && e.shiftKey === false) {
            // input.blur(); // Deprecated 23.07.29.17.44
            return false;
          }
        };

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
        });

        form.addEventListener("keydown", (e) => {
          if (e.key === "Enter") {
            e.preventDefault();

            const enter = { ...enterEvent };
            enter.shift = e.shiftKey;
            enter.alt = e.altKey;
            enter.ctrl = e.ctrlKey;
            keyboard.events.push(enter);

            if (
              (input.value === "dl" || input.value === "download") &&
              shareFile
            ) {
              const share = () => {
                navigator
                  .share({
                    files: [shareFile],
                    title: "Share Painting",
                    // text: "Share your painting!",
                  })
                  .then(() => {
                    console.log("📥 Share was successful.");
                    shareFile = null;
                  })
                  .catch((error) => {
                    console.log("📥 Sharing failed:", error);
                    shareFile = null;
                  });
                shareFileCallback = null;
              };

              if (shareFile) {
                share();
              } else {
                shareFileCallback = share;
              }
            }
          } else if (e.key === "Home") {
            e.preventDefault();
            const home = { name: "keyboard:down:home", key: "Home" };
            home.shift = e.shiftKey;
            home.alt = e.altKey;
            home.ctrl = e.ctrlKey;
            keyboard.events.push(home);
          } else if (e.key === "Tab") {
            e.preventDefault();
            const tab = { name: "keyboard:down:tab", key: "Tab" };
            tab.shift = e.shiftKey;
            tab.alt = e.altKey;
            tab.ctrl = e.ctrlKey;
            keyboard.events.push(tab);
          } /*else if (
            // Don't send the backtick unless we are on the prompt.
            e.key === "`" &&
            currentPiece.split("/").pop() !== "prompt"
          ) {
            e.preventDefault();
            keyboard.events.push({ name: "keyboard:down:`", key: "`" });
          }*/
        });

        input.addEventListener("beforeinput", (e) => {
          // console.log("Input Type:", e.inputType, input, e);

          const pressedKeys = [];

          if (e.inputType === "deleteContentBackward") {
            // console.log(e.inputType, e.target.value);
            // alert(e.inputType);
            // pressedKeys.push("Backspace");
          } else if (
            ["insertText", "insertCompositionText"].includes(e.inputType)
          ) {
            // Sanitize input if it arrives in chunks... like if it was dictated.
            // This is still basic, and is usable in the Meta Quest Browser. 22.10.24.17.07
            // let sanitizedInput = input;
            // if (input.length > 1) {
            //   sanitizedInput = input
            //     .trim()
            //     .toLowerCase()
            //     .replace(",", "")
            //     .replace(".", "");
            //   console.log("👄 Spoken / pasted input:", sanitizedInput);
            // }
            // [...sanitizedInput].forEach((chr) => pressedKeys.push(chr));
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
        });

        function handleInput(e) {
          input.removeEventListener("input", handleInput);

          let text = e.target.value;

          // Replace curly single and double quotes with straight quotes
          text = text
            .replace(/[\u2018\u2019]/g, "'")
            .replace(/[\u201C\u201D]/g, '"');

          e.target.value = text;

          send({
            type: "prompt:text:replace",
            content: {
              text: text,
              cursor: input.selectionStart,
            },
          });

          input.addEventListener("input", handleInput);
        }

        input.addEventListener("input", handleInput);

        input.addEventListener("keydown", (e) => {
          if (keyboardFocusLock) {
            e.preventDefault();
            return;
          }

          if (e.key === "ArrowLeft" || e.key === "ArrowRight") {
            let cursor =
              input.selectionDirection === "backward"
                ? input.selectionStart
                : input.selectionEnd;

            const selectionLength = input.selectionEnd - input.selectionStart;

            if (!e.shiftKey && selectionLength > 1) {
              cursor =
                e.key === "ArrowLeft"
                  ? max(0, input.selectionStart)
                  : input.selectionEnd;
            } else {
              cursor += e.key === "ArrowLeft" ? -1 : 1;
              cursor = max(0, min(input.value.length, cursor));
            }

            let start, end;
            if (e.shiftKey) {
              if (e.key === "ArrowLeft") {
                if (input.selectionDirection === "backward") {
                  start = cursor;
                  end = input.selectionEnd;
                } else {
                  start = input.selectionStart;
                  end =
                    cursor > input.selectionStart ? cursor : input.selectionEnd;
                }
              } else {
                if (input.selectionDirection === "forward") {
                  start = input.selectionStart;
                  end = cursor;
                } else {
                  end =
                    cursor < input.selectionEnd ? cursor : input.selectionStart;
                  start = cursor;
                }
              }
            }

            send({
              type: "prompt:text:cursor",
              content: {
                cursor,
                start,
                end,
              },
            });
          }
        });

        window.addEventListener("blur", (e) => {
          input.blur();
        });

        window.addEventListener("pointerdown", (e) => {
          if (currentPieceHasKeyboard) {
            e.preventDefault();
          }
          if (!document.hasFocus()) window.focus();
        });

        window.addEventListener("pointerup", (e) => {
          //if (keyboard.needsImmediateOpen) {
          //  keyboard.needsImmediateOpen = false;
          //  return;
          //}
          // console.log("🌬️ Blurrred", "Target:", e.target);

          // return;

          if (e.target === window) {
            // console.log("WINDOW BLURRED");
            e.preventDefault();
            return; // This prevents.
          }

          if (currentPieceHasKeyboard) e.preventDefault();

          // console.log(e.target);

          //console.log(currentPieceHasKeyboard, !keyboardFocusLock, !keyboardSoftLock)

          if (
            currentPieceHasKeyboard &&
            !keyboardFocusLock &&
            !keyboardSoftLock
          ) {
            if (keyboardOpen) {
              // console.log("Target:", e.target);
              if (MetaBrowser && e.target !== window) {
                // Skip dragging the finger outside of the Meta Browser.
              } else input.blur();
            } else {
              keyboardOpenMethod = "pointer";
              // input.removeAttribute("readonly");
              window.focus();
              input.focus();
            }
          }
        });

        input.addEventListener("focus", (e) => {
          if (keyboardOpen) return;
          // input.removeAttribute("readonly");
          keyboardOpen = true;
          keyboard.events.push({
            name: "keyboard:open",
            method: keyboardOpenMethod,
          });
          keyboardOpenMethod = undefined;
        });

        input.addEventListener("blur", (e) => {
          // input.setAttribute("readonly", true);
          // const temp = input.value;
          // input.value = "";
          // input.value = temp;
          keyboardOpen = false;
          keyboard.events.push({ name: "keyboard:close" });
        });

        window.addEventListener("blur", (e) => {
          // console.log("blurred window...");
          // keyboardOpen = false;
          // keyboard.events.push({ name: "keyboard:close" });
        });
      }

      // 🎮 Gamepad
      gamepad = new Gamepad();

      // Turn off all layers onbeforeunload. (Prevents a white flicker in chrome.)
      window.addEventListener("beforeunload", (e) => {
        send({ type: "before-unload" });
        wrapper.classList.add("reloading");
      });

      // Listen for resize events on the visual viewport
      window.visualViewport.addEventListener("resize", () => {
        const y = window.visualViewport.height;
        window.acDISK_SEND({ type: "viewport-height:changed", content: { y } });
      });      // 🌒 Detect light or dark mode.
      // See also: https://flaviocopes.com/javascript-detect-dark-mode,
      //           https://developer.mozilla.org/en-US/docs/Web/CSS/@media/prefers-color-scheme

      if (window.matchMedia) {
        if (window.matchMedia("(prefers-color-scheme: dark)").matches) {
          document.documentElement.style.setProperty('color-scheme', 'dark');
          send({ type: "dark-mode", content: { enabled: true } });
        } else {
          document.documentElement.style.setProperty('color-scheme', 'light');
          send({ type: "dark-mode", content: { enabled: false } });
        }

        window
          .matchMedia("(prefers-color-scheme: dark)")
          .addEventListener("change", (event) => {
            if (event.matches) {
              document.documentElement.style.setProperty('color-scheme', 'dark');
              send({ type: "dark-mode", content: { enabled: true } });
            } else {
              document.documentElement.style.setProperty('color-scheme', 'light');
              send({ type: "dark-mode", content: { enabled: false } });
            }
          });
      }

      // 📋 User pasting of content.
      //window.addEventListener("paste", (event) => {
      // pastedText = event.clipboardData.getData("text/plain");
      //});

      // 🖥️ Display (Load the display, with 0 margin if sandboxed)
      frame(
        resolution?.width,
        resolution?.height,
        resolution?.gap ?? (sandboxed ? 0 : undefined),
      );

      // 🔊 Sound
      // TODO: Disable sound engine entirely... unless it is enabled by a disk. 2022.04.07.03.33
      // Only start this after a user-interaction to prevent warnings.

      activateSound = () => {
        startSound();
        window.removeEventListener("keydown", activateSound);
        window.removeEventListener("pointerdown", activateSound);
      };

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

          TwoD?.render();
        },
      );
    } // 💾 Disk Loading
    // Initialize some global stuff after the first piece loads.
    // Unload some already initialized stuff if this wasn't the first load.
    if (type === "disk-loaded") {      // Clear any active parameters once the disk has been loaded.
      // Special handling for prompt piece with kidlisp content
      if (content.path === "aesthetic.computer/disks/prompt" && 
          content.params && content.params.length > 0 && 
          isKidlispSource(content.params[0])) {
        // For prompt piece with kidlisp parameters, preserve the prompt~ URL structure
        const encodedContent = encodeKidlispForUrl(content.params[0]);
        const encodedPath = "/prompt~" + encodedContent;
        // Use pushState instead of replaceState to preserve history navigation
        if (!content.fromHistory) {
          window.history.pushState({}, "", encodedPath);
        } else {
          window.history.replaceState({}, "", encodedPath);
        }
      } else if (content.text && isKidlispSource(content.text)) {
        // For standalone kidlisp pieces, use centralized URL encoding
        const encodedPath = "/" + encodeKidlispForUrl(content.text);
        // Use pushState instead of replaceState to preserve history navigation
        if (!content.fromHistory) {
          window.history.pushState({}, "", encodedPath);
        } else {
          window.history.replaceState({}, "", encodedPath);
        }
      } else {
        // For regular pieces, clear parameters but keep the basic path structure
        window.history.replaceState({}, "", window.location.pathname);
      }

      // if (currentPiece !== null) firstPiece = false;
      currentPiece = content.path;
      currentPieceHasKeyboard = false;
      if (keyboard) keyboard.input.value = "";

      if (!content.taping) {
        detachMicrophone?.(); // Remove any attached microphone unless we
        //                       are taping 📼.
      }

      // Kill any previously loading media.
      keys(mediaPathsLoading).forEach((key) => mediaPathsLoading[key].abort());

      killAllSound?.(); // Kill any pervasive sounds in `speaker`.
      clearSoundSampleCache?.();

      // ⚠️ Remove any sounds that aren't in the whitelist.
      const sfxKeys = keys(sfx);
      sfxKeys.forEach((key) => {
        if (key !== sound) delete sfx[key];
      });
      if (sfxKeys.length > 0 && logs.audio && debug)
        console.log("🔉 SFX Cleaned up:", sfx);

      // Stop any playing samples.
      keys(sfxPlaying).forEach((sfx) => sfxPlaying[sfx]?.kill());

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

      // Clear any ticket overlay that was added by a piece.
      ticketWrapper?.remove();
      ticketWrapper = undefined;

      underlayFrame?.remove(); // Remove the underlayFrame if it exists.
      underlayFrame = undefined;
      stopTapePlayback?.();

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

      pen.events.length = 0; // Clear pen events.
      keyboard.events.length = 0; // Clear keyboard events.
      gamepad.events.length = 0; // Clear gamepad events.

      // Clear when events.
      whens = {};

      // Close (defocus) software keyboard if we are NOT on the prompt.
      // debugger;
      if (content.text !== "prompt") {
        document.querySelector("#software-keyboard-input")?.blur();
      }
      // keyboard.events.push({ name: "keyboard:close" });

      setMetatags(content.meta);

      // TODO: Make this automatic for pieces that use 3d.
      if (
        content.text === "wand" ||
        content.text?.indexOf("wand") === 0 ||
        content.text === "oldwand" ||
        content.text?.indexOf("oldwand") === 0
      ) {
        loadThreeD();
      }

      // Show an "audio engine: off" message.
      //if (content.noBeat === false && audioContext?.state !== "running") {
      //bumper.innerText = "audio engine off";
      //modal.classList.add("on");
      //}

      // Clear the ThreeD buffer.
      // ThreeD.clear();      // Emit a push state for the old disk if it was not the first. This is so
      // a user can use browser history to switch between disks.
      if (content.pieceCount > 0 || content.alias === true) {
        if (content.fromHistory === false /*&& window.origin !== "null"*/) {
          // Handle URL encoding for different piece types
          let urlPath;
          if (content.text === "/prompt") {
            urlPath = "/";
          } else if (content.path === "aesthetic.computer/disks/prompt" && 
                     content.params && content.params.length > 0 && 
                     isKidlispSource(content.params[0])) {
            // For prompt piece with kidlisp parameters, preserve the prompt~ URL structure
            urlPath = "/prompt~" + encodeKidlispForUrl(content.params[0]);
          } else if (isKidlispSource(content.text)) {
            // For standalone kidlisp pieces, use centralized URL encoding
            urlPath = "/" + encodeKidlispForUrl(content.text);
          } else {
            // For regular pieces, use normal text
            urlPath = "/" + content.text;
          }

          history.pushState(
            "",
            document.title,
            urlPath, // Replace "prompt" with "/".
          );
          window.parent?.postMessage(
            {
              type: "url:updated",
              slug: (() => {
                if (content.text?.startsWith("/")) {
                  return content.text.slice(1);
                } else if (content.path === "aesthetic.computer/disks/prompt" && 
                          content.params && content.params.length > 0 && 
                          isKidlispSource(content.params[0])) {
                  return "prompt~" + encodeKidlispForUrl(content.params[0]);
                } else if (isKidlispSource(content.text)) {
                  return encodeKidlispForUrl(content.text);
                } else {
                  return content.text;
                }
              })(),
            },
            "*",
          );
        } // Replace the state if we are running an aliased `load` or `jump`.
        // (That doesn't avoid the history stack.)
        // Note: History state changes do not work in a sandboxed iframe!
        if (
          content.fromHistory === true &&
          content.alias === false //&&
          // window.origin !== "null"
        ) {
          try {
            // Handle URL encoding for different piece types
            let urlPath;
            if (content.text === "/prompt") {
              urlPath = "/";
            } else if (content.path === "aesthetic.computer/disks/prompt" && 
                       content.params && content.params.length > 0 && 
                       isKidlispSource(content.params[0])) {
              // For prompt piece with kidlisp parameters, preserve the prompt~ URL structure
              urlPath = "/prompt~" + encodeKidlispForUrl(content.params[0]);
            } else if (isKidlispSource(content.text)) {
              // For standalone kidlisp pieces, use centralized URL encoding
              urlPath = "/" + encodeKidlispForUrl(content.text);
            } else {
              // For regular pieces, use normal encoding
              urlPath = "/" + content.text;
            }

            history.replaceState("", document.title, urlPath);
          } catch (err) {
            console.warn("⚠️ Couldn't change url state. Going too fast!? ➿🚗");
          }
        }
      }

      UI.spinnerReset(); // Reset the timer on the yellow UI loading spinner.

      if (content.pieceHasSound && !audioContext) {
        // Enable sound engine on interaction.
        window.addEventListener("keydown", activateSound, { once: true });
        window.addEventListener("pointerdown", activateSound, { once: true });
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

    if (type === "$creenshot") {
      needs$creenshot = (data) => receivedDownload({ ...content, data });
      return;
    }

    if (type === "keyboard:enabled") {
      currentPieceHasKeyboard = true;
      keyboardFocusLock = false;
      keyboardSoftLock = false;
      // console.log("enabling keyboard...");
      return;
    }

    if (type === "keyboard:disabled") {
      currentPieceHasKeyboard = false;
      return;
    }

    if (type === "keyboard:close") {
      // if (keyboardFocusLock) return; // Deprecated: 23.10.02.23.18
      // console.log("⌨️ Keyboard closing...");
      keyboard?.input.blur();
      return;
    }

    if (type === "keyboard:open") {
      // console.log("⌨️ Keyboard opening...");
      if (keyboardFocusLock) return;
      keyboardFocusLock = false;
      currentPieceHasKeyboard = true;
      keyboard?.input.focus();
      // if (keyboard) keyboard.needsImmediateOpen = true; // For iOS.
      return;
    }

    // Prevents any touch or keyboard activation events directly on the input.
    if (type === "keyboard:soft-lock") {
      keyboardSoftLock = true;
      if (logs.hid && debug) console.log("⌨️ Virtual Keyboard: Soft-locked.");
    }

    if (type === "keyboard:soft-unlock") {
      keyboardSoftLock = false;
      keyboardFocusLock = false;
      if (logs.hid && debug) console.log("⌨️ Virtual Keyboard: Soft-unlocked.");
    }

    if (type === "keyboard:lock") {
      keyboardFocusLock = true;
      if (logs.hid && debug) console.log("⌨️ Virtual Keyboard: Locked");
      return;
    }

    if (type === "keyboard:unlock") {
      keyboardFocusLock = false;
      if (logs.hid && debug) console.log("⌨️ Virtual Keyboard: Unlocked");
      return;
    }

    if (type === "keyboard:cursor") {
      const input = keyboard.input;
      // Get the current position of the caret

      // Quit if keyboard is not open.
      if (document.activeElement !== keyboard.input) return;

      if (typeof content === "number") {
        const currentPosition =
          input.selectionDirection === "backward"
            ? input.selectionStart
            : input.selectionEnd;
        let newPosition = currentPosition + content;
        if (newPosition < 0) newPosition = 0;
        if (newPosition > input.value.length) newPosition = input.value.length;
        input.setSelectionRange(newPosition, newPosition);
      } else {
        // Set based on a specific value.
        input.setSelectionRange(content.cursor, content.cursor);
      }
      return;
    }

    if (type === "keyboard:text:replace") {
      const input = keyboard.input;
      input.value = content.text;
      if (content.cursor && document.activeElement === keyboard.input)
        input.setSelectionRange(content.cursor, content.cursor);
      return;
    }

    if (type === "gpu-event") {
      ThreeD?.handleEvent(content);
      return;
    }

    // Adding custom DOM content.
    if (type === "content-create") {
      // Create a DOM container, if it doesn't already exist,
      // and add it here along with the requested content in the
      // template
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

    // Removing custom DOM content.
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
      if (typeof content === "string") content = { type: content };
      if (whens[content.type]) whens[content.type](content.content);
    }

    // 📦 Storage

    // Can store data to localStorage (user settings),
    //                   indexedDB (large files)
    //                   remote (with user account or anonymous)

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

        if (!sandboxed) {
          try {
            data = JSON.parse(localStorage.getItem(content.key));
          } catch (err) {
            console.warn("📦 Retrieval error:", err);
            // Probably in a sandboxed environment here...
          }
        }

        if (debug && logs.store)
          console.log("📦 Retrieved local data:", content.key, data);

        send({
          type: "store:retrieved",
          content: { key: content.key, data },
        });
      }

      if (content.method === "local:db") {
        const retrievedContent = await Store.get(content.key);
        if (debug && logs.store)
          console.log(
            "📦 Retrieved local:db data:",
            content.key,
            retrievedContent,
          );
        send({
          type: "store:retrieved",
          content: { key: content.key, data: retrievedContent },
        });
      }

      return;
    }

    // Store: Delete
    if (type === "store:delete") {
      if (content.method === "local") {
        if (debug && logs.store)
          console.log("📦 Delete local data:", content.key);
        localStorage.removeItem(content.key);
        send({
          type: "store:deleted",
          content: { key: content.key, data: true },
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
          content: { key: content.key, data: true },
        });
      }
      return;
    }

    if (type === "meta") {
      setMetatags(content);
      return;
    }

    if (type === "refresh") {
      // Reconstruct URL with preserved parameters (nogap, nolabel)
      const currentUrl = new URL(window.location);
      
      // Add preserved parameters back to the URL
      if (preservedParams.nogap) {
        currentUrl.searchParams.set("nogap", preservedParams.nogap);
      }
      if (preservedParams.nolabel) {
        currentUrl.searchParams.set("nolabel", preservedParams.nolabel);
      }
      
      // Update the URL and reload
      window.location.href = currentUrl.toString();
      return;
    }

    if (type === "web") {
      // console.log("Jumping to:", content.url, content.blank);
      if (content.blank === true) {
        if (Aesthetic) {
          iOSAppSend({ type: "url", body: content.url });
        } else if (window.acVSCODE && window.parent !== window) {
          // In VSCode extension, send message to parent to handle external URL
          window.parent.postMessage({ 
            type: "openExternal", 
            url: content.url 
          }, "*");
        } else {
          window.open(content.url); // Open URL in a new tab
        }
      } else {
        window.location.href = content.url; // Redirect in the current tab
      }
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

    // Deprecated in favor of frame-synced communication.
    if (type === "beat") {
      updateSynths(content);
      return;
    }

    if (type === "beat:skip") {
      beatSkip();
      return;
    }
    if (type === "synth:update") {
      updateSound?.(content);
      return;
    }

    if (type === "bubble:update") {
      updateSound?.(content);
      return;
    }

    if (type === "download") {
      receivedDownload(content);
      return;
    }

    if (type === "upload") {
      receivedUpload(content);
      return;
    }

    if (type === "import") {
      receivedImport(content);
      return;
    }

    if (type === "microphone") {
      receivedMicrophone(content);
      return;
    }

    if (type === "microphone-record") {
      requestMicrophoneRecordingStart?.();
      return;
    }

    if (type === "microphone-cut") {
      requestMicrophoneRecordingStop?.();
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

    if (type === "get-waveforms") {
      requestSpeakerWaveforms?.();
      return;
    }

    if (type === "get-amplitudes") {
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

    // Audio-visual recording of the main audio track and microphone.
    if (type === "recorder:rolling") {
      // mediaRecorderBlob = null; // Clear the current blob when we start recording.

      const colonSplit = content.split(":");
      // tiktokVideo = colonSplit[1] === "tiktok";
      content = colonSplit[0];

      if (mediaRecorder && mediaRecorder.state === "paused") {
        mediaRecorder.resume();
        mediaRecorderStartTime = performance.now();
        send({
          type: "recorder:rolling:resumed",
          content: {
            mime: mediaRecorder.mimeType,
            time: audioContext?.currentTime,
          },
        });
        if (debug && logs.recorder)
          console.log("🔴 Recorder: Resumed", content);
        return;
      }

      if (mediaRecorder && mediaRecorder.state !== "paused") {
        stop();
      }

      function stop() {
        recordedFrames.length = 0;
        startTapePlayback = undefined;
        mediaRecorder = undefined; // ❌ Trash the recorder.
        mediaRecorderStartTime = undefined;
        mediaRecorderDuration = null;
        mediaRecorderChunks.length = 0;
      }

      let mimeType;

      // if (content === "audio" || content === "video") {

      // if (MediaRecorder.isTypeSupported(content + "/mp4")) {
      //   mimeType = content + "/mp4"; // This is the setup for Safari.
      // } else if (MediaRecorder.isTypeSupported(content + "/webm")) {
      //   mimeType = content + "/webm"; // And for Chrome & Firefox.
      //   // mimeType = content + "/webm;codecs=h264"; // Possible optimization.
      // } else {
      //   console.error("🔴 Mimetypes mp4 and webm are unsupported.");
      // }

      // } else {
      //   console.error("🔴 Option must be 'audio' or 'video'.");
      // }

      // Set the audio recorder mimetypes.
      // TODO: Should WAV also be here?
      if (MediaRecorder.isTypeSupported("audio/webm;codecs=opus")) {
        mimeType = "audio/webm;codecs=opus";
      } else if (MediaRecorder.isTypeSupported("audio/ogg;codecs=vorbis")) {
        mimeType = "audio/ogg;codecs=vorbis";
      } else if (MediaRecorder.isTypeSupported("audio/aac")) {
        mimeType = "audio/aac";
      }

      let options = { mimeType };

      if (content === "video") {
        // Start recording audio.
        mediaRecorder = new MediaRecorder(audioStreamDest.stream, options);
      }

      // 🗺️ mediaRecorder:Start
      mediaRecorder.onstart = function () {
        // mediaRecorderResized = false;
        mediaRecorderStartTime = performance.now();
        send({
          type: "recorder:rolling:started",
          content: {
            mime: mediaRecorder.mimeType,
            time: audioContext?.currentTime,
          },
        });
        if (debug && logs.recorder)
          console.log("🔴 Recorder: Rolling", content);

        // window.addEventListener("resize", () => (mediaRecorderResized = true), {
        // once: true,
        // });
      };

      // 🗺️ mediaRecorder:Stop (Recorder Printing)
      mediaRecorder.onstop = async function (evt) {
        stop();
        // recordingDuration = (performance.now() - recordingStartTime) / 1000;

        // Reset global streamCanvas state.
        // streamCanvasContext = undefined;
        // resizeToStreamCanvas = null;

        // let blob = new Blob(chunks, {
        //  type: options.mimeType,
        // });

        // Load FFmpeg so the recording can be transcribed to a proper video format.
        // if (content === "video") {
        //   if (options.mimeType === "video/mp4") {
        //     console.warn("Encoding can be skipped!");
        //     // TODO: Skip encoding.
        //   }

        //   const { createFFmpeg, fetchFile } = await loadFFmpeg();

        //   let transcodeProgress = 0;

        //   const ffmpeg = createFFmpeg({
        //     log: debug,
        //     progress: (p) => {
        //       // Send a message to the piece that gives the transcode progress.
        //       let time = p.time;
        //       if (time === undefined) {
        //         if (transcodeProgress === 0) {
        //           time = 0;
        //         } else {
        //           time = recordingDuration;
        //         }
        //       }
        //       transcodeProgress = min(1, time / recordingDuration);
        //       send({
        //         type: "recorder:transcode-progress",
        //         content: transcodeProgress,
        //       });
        //     },
        //   });

        //   ffmpeg.setLogging(debug); // Enable ffmpeg logging only if we are in `debug` mode.

        //   await ffmpeg.load();
        //   ffmpeg.FS("writeFile", "input.video", await fetchFile(blob));

        //   await ffmpeg.run(
        //     "-i",
        //     "input.video",
        //     "-movflags",
        //     "+faststart",
        //     "-vf",
        //     // General shaving to make even sides (required by the pixel format)
        //     // "pad=ceil(iw/2)*2:ceil(ih/2)*2",
        //     // TikTok
        //     //"fps=30, scale=1080x1920:flags=neighbor:force_original_aspect_ratio=decrease, pad=1080:1920:(ow-iw)/2:(oh-ih)/2",
        //     "fps=30",
        //     "output.mp4",
        //   );
        //   // Notes on these options:
        //   // width expression: https://stackoverflow.com/a/20848224/8146077
        //   // scaling: https://trac.ffmpeg.org/wiki/Scaling
        //   // general info: https://avpres.net/FFmpeg/im_H264

        //   const file = ffmpeg.FS("readFile", "output.mp4");

        //   blob = new Blob([file.buffer], { type: "video/mp4" }); // Re-assign blob.
        // }

        // Add the recording wrapper to the DOM, among other recordings that may exist.

        // if (debug) console.log("📼 Recorder: Printed");

        // mediaRecorderBlob = blob;

        // TODO: Store an index into the blob if its an audio clip or loaded sample.

        // send({ type: "recorder:printed", content: { id: "test-sample-id" } });
        // TODO: Can send the download code back here...
        // send({ type: "recorder:uploaded", code });

        // mediaRecorderBlob = new Blob(mediaRecorderChunks, {
        //   type: mediaRecorder.mimeType,
        // });

        // if (content === "video") {
        //   await receivedChange({
        //     data: {
        //       type: "store:persist",
        //       content: {
        //         key: "tape",
        //         method: "local:db",
        //         data: {
        //           blob: mediaRecorderBlob,
        //           duration: mediaRecorderDuration,
        //         },
        //       },
        //     },
        //   });
        // }

        // send({ type: "recorder:rolling:ended" });
      };

      mediaRecorder.ondataavailable = function (e) {
        if (e.data && e.data.size > 0) mediaRecorderChunks.push(e.data);
      };

      // Always cut off mediaRecorders on unload.
      window.addEventListener("unload", function () {
        mediaRecorder?.stop();
      });

      window.addEventListener("beforeunload", function () {
        mediaRecorder?.stop();
      });

      //if (content === "video") {
      // Start media recorder once svg loads.
      //svgCursor.onload = function (e) {
      // Use small chunk sizes. (`1000` broke TikTok)
      //  mediaRecorder.start(100);
      //};
      //svgCursor.src = "/aesthetic.computer/cursors/precise.svg";
      //} else {
      // console.log("Start audio recording...");
      mediaRecorder.start(100);
      //}
      return;
    }

    if (type === "recorder:cut") {
      if (!mediaRecorder) return;
      if (debug && logs.recorder) console.log("✂️ Recorder: Cut");
      mediaRecorderDuration += performance.now() - mediaRecorderStartTime;
      // mediaRecorder?.stop();
      mediaRecorder?.pause(); // Single clips for now.
      send({ type: "recorder:rolling:ended" });
      return;
    }

    if (type === "recorder:present") {
      if (mediaRecorder && mediaRecorder.state === "paused") {
        const blob = new Blob(mediaRecorderChunks, {
          type: mediaRecorder.mimeType,
        });
        sfx["tape:audio"] = await blobToArrayBuffer(blob);
        // console.log("Update tape audio with new blob!");

        underlayFrame = document.createElement("div");
        underlayFrame.id = "underlay";

        const frameCan = document.createElement("canvas");
        const fctx = frameCan.getContext("2d");

        frameCan.width = recordedFrames[0][1].width;
        frameCan.height = recordedFrames[0][1].height;

        startTapePlayback = (
          transmitProgress = true,
          doneCb,
          stream,
          render,
        ) => {
          let f = 0;
          let playbackStart;
          let playbackProgress = 0;
          let continuePlaying = true;
          let stopped = false;

          let tapeSoundId;

          stopTapePlayback = () => {
            continuePlaying = false;
            stopped = true;
            sfxPlaying[tapeSoundId]?.kill();
          };

          let pauseStart;

          pauseTapePlayback = () => {
            continuePlaying = false;
            pauseStart = performance.now();
            console.log("📼 Tape Sound:", tapeSoundId, sfxPlaying[tapeSoundId]);
            sfxPlaying[tapeSoundId]?.pause();
            send({ type: "recorder:present-paused" });
          };

          resumeTapePlayback = () => {
            if (stopped) {
              send({ type: "recorder:present-playing" });
              return startTapePlayback(true);
            }
            continuePlaying = true;
            window.requestAnimationFrame(update);
            sfxPlaying[tapeSoundId]?.resume();
            playbackStart += performance.now() - pauseStart;
            send({ type: "recorder:present-playing" });
          };

          async function update() {
            if (!continuePlaying || !underlayFrame) return;

            if (f === 0) {
              tapeSoundId = "tape:audio_" + performance.now();
              await playSfx(tapeSoundId, "tape:audio", { stream });
              // Will be silent if stream is here. ^
              playbackStart = performance.now();
              playbackProgress = 0;
            }

            // Resize fctx here if the width and
            // height is different.
            const pic = recordedFrames[f][1];
            if (
              fctx.canvas.width !== pic.width ||
              fctx.canvas.height !== pic.height
            ) {
              fctx.canvas.width = pic.width;
              fctx.canvas.height = pic.height;
            }

            fctx.putImageData(recordedFrames[f][1], 0, 0);

            render?.(frameCan, playbackProgress / mediaRecorderDuration); // Render a video as needed, using this canvas.

            playbackProgress = performance.now() - playbackStart;

            // Advance frames.
            if (f === 0) f += 1;
            while (playbackProgress > recordedFrames[f][0] && f !== 0) {
              f = (f + 1) % recordedFrames.length;
            } // Skip any necessary frames to better match the audio.

            if (f === 0 && doneCb) {
              send({ type: "recorder:present-progress", content: 1 });
              return doneCb(); // Completed a cycle.
            }

            if (transmitProgress) {
              send({
                type: "recorder:present-progress",
                content: playbackProgress / mediaRecorderDuration,
              });
            }

            window.requestAnimationFrame(update);
          }

          update();
        };

        startTapePlayback();

        underlayFrame.appendChild(frameCan);
        wrapper.appendChild(underlayFrame);
        send({ type: "recorder:presented" });
        send({ type: "recorder:present-playing" });
      } else {
        if (debug && logs.recorder)
          console.error("📼 No media recorder to present from!");
        send({ type: "recorder:presented:failure" });
      }
      return;
    }

    if (type === "recorder:present:play") {
      if (underlayFrame) resumeTapePlayback?.();
      return;
    }

    if (type === "recorder:present:pause") {
      if (underlayFrame) {
        pauseTapePlayback?.();
      }
      return;
    }

    if (type === "recorder:unpresent") {
      if (underlayFrame) {
        const media = underlayFrame.querySelector("video, audio");
        if (media?.src) URL.revokeObjectURL(media.src);
        underlayFrame?.remove();
        underlayFrame = undefined;
        send({ type: "recorder:unpresented" });
      }
      return;
    }

    // 🎞️ 🎥 Exporting stamped media.
    if (type === "recorder:print") {
      if (!mediaRecorder) return;
      send({ type: "recorder:present-playing" });

      let mimeType;
      const content = "video";
      if (MediaRecorder.isTypeSupported(content + "/mp4")) {
        mimeType = content + "/mp4"; // This is the setup for Safari.
      } else if (MediaRecorder.isTypeSupported(content + "/webm")) {
        mimeType = content + "/webm"; // And for Chrome & Firefox.
      } else {
        console.error("🔴 Mimetypes mp4 and webm are unsupported.");
      }

      streamCanCtx = document.createElement("canvas").getContext("2d", {
        alpha: false,
        willReadFrequently: true,
      });
      const sctx = streamCanCtx;

      // TODO: `tiktokVideo` could eventually be defined by an export option
      //        instead of in `recorder:rolling`.
      //if (tiktokVideo) {
      // Portrait Mode / TikTok (this is the default for recording)
      // This is hardcoded at half 720p for TikTok right now.
      // sctx.canvas.width = 1080; //720 / 2;
      // sctx.canvas.height = 1920; //1280 / 2;
      //} else {
      const originalWidth = canvas.width;
      const originalHeight = canvas.height;
      const aspectRatio = originalWidth / originalHeight;

      let newWidth = originalWidth * 4;
      let newHeight = originalHeight * 4;

      if (newWidth > 1080) {
        newWidth = originalWidth * 2;
        newHeight = newWidth / aspectRatio;
      }

      if (newHeight > 1920) {
        newHeight = originalHeight * 2;
        newWidth = newHeight * aspectRatio;
      }
      sctx.canvas.width = newWidth;
      sctx.canvas.height = newHeight;
      //}

      sctx.imageSmoothingEnabled = false; // Must be set after resize.

      const canvasStream = sctx.canvas.captureStream();
      const tapeRenderStreamDest = audioContext.createMediaStreamDestination();

      tapeRenderStreamDest.stream.getAudioTracks().forEach((track) => {
        canvasStream.addTrack(track);
      });

      const options = {
        mimeType,
        videoBitsPerSecond: 5000000,
        audioBitsPerSecond: 256000,
      };

      const videoRecorder = new MediaRecorder(canvasStream, options);

      const chunks = [];

      videoRecorder.ondataavailable = function (e) {
        if (e.data && e.data.size > 0) chunks.push(e.data);
      };

      function startRendering() {
        stopTapePlayback?.();
        videoRecorder.start(100);
        startTapePlayback(
          true,
          () => {
            setTimeout(function () {
              videoRecorder.stop();
            }, 500); // Seems like this is necessary or recordings get cut-off.
          },
          tapeRenderStreamDest,
          // 🎫 Renders and watermarks the frames for export.
          function renderTape(can, progress) {
            const frameWidth = sctx.canvas.width;
            const frameHeight = sctx.canvas.height;
            const frameAspectRatio = frameHeight / frameWidth;
            const aspectRatio = can.height / can.width;

            sctx.clearRect(0, 0, frameWidth, frameHeight);

            // if (glaze.on) can = Glaze.getCan();

            let x = 0,
              y = 0,
              width,
              height;

            if (frameAspectRatio > aspectRatio) {
              height = sctx.canvas.width * aspectRatio;
              y = floor(frameHeight / 2 - height / 2);
              width = sctx.canvas.width;
            } else {
              width = sctx.canvas.height / aspectRatio;
              x = floor(frameWidth / 2 - width / 2);
              height = sctx.canvas.height;
            }

            if (ThreeD)
              sctx.drawImage(
                ThreeD.getCan(),
                x,
                y,
                floor(width),
                floor(height),
              );

            sctx.drawImage(can, x, y, floor(width), floor(height));

            if (pen.pointers[1]) {
              const originalX = pen.pointers[1].x;
              const originalY = pen.pointers[1].y;
              const scaledX = (originalX / canvas.width) * width + x;
              const scaledY = (originalY / canvas.height) * height + y;

              if (pen.pointers[1].device === "mouse") {
                sctx.drawImage(
                  svgCursor,
                  floor(scaledX - 12),
                  floor(scaledY - 12),
                  svgCursor.naturalWidth,
                  svgCursor.naturalHeight,
                );
              } else {
                // Draw a soft tap.
                // const circleRadius = 16; // example value, adjust as needed
                // shuffleInPlace(["magenta", "lime", "white"]).forEach((color) => {
                //   const ox = choose(-4, -2, 0, 2, 4);
                //   const oy = choose(-4, -2, 0, 2, 4);
                //   sctx.globalAlpha = 0.15 + Math.random() * 0.25;
                //   sctx.beginPath();
                //   sctx.arc(
                //     scaledX + ox,
                //     scaledY + oy,
                //     circleRadius,
                //     0,
                //     2 * Math.PI,
                //   );
                //   sctx.fillStyle = color; // or any desired color
                //   sctx.fill();
                //   sctx.closePath();
                // });
                // sctx.globalAlpha = 1;
              }
            }

            // if (pen.pointers[1]) {
            // console.log(pen.pointers[1]);

            // TODO: Draw a circle based on pen.points[1].x and y
            //       that fits inside of the scaled can drawImage
            //       below, because these ranges are within
            //       the original width and height before the aspect
            //       ratio scale.
            // }

            // 2. Set up the font.
            const typeSize = min(32, max(24, floor(sctx.canvas.height / 20)));
            // const textHeight = typeSize;
            const gap = typeSize * 0.75;

            sctx.save(); // Save the current state of the canvas

            drawTextAtPosition(0, 90); // Left
            drawTextAtPosition(sctx.canvas.width, -90); // Right

            sctx.restore();
            sctx.globalAlpha = 1;

            function drawTextAtPosition(positionX, deg) {
              sctx.save();
              sctx.translate(positionX, 0);
              sctx.rotate(radians(deg));
              const yDist = 0.05;

              sctx.font = `${typeSize}px YWFTProcessing-Regular`;
              const text = "aesthetic.computer";
              const measured = sctx.measureText(text);
              const textWidth = measured.width;

              ["red", "lime", "blue", "white"].forEach((color) => {
                let offsetX, offsetY;
                if (color !== "white") {
                  sctx.globalAlpha = 0.45;
                  offsetX = choose(-2, -4, 0, 2, 4);
                  offsetY = choose(-2, -4, 0, 2, 4);
                } else {
                  sctx.globalAlpha = choose(0.5, 0.4, 0.6);
                  offsetX = choose(-1, 0, 1);
                  offsetY = choose(-1, 0, 1);
                  color = choose(
                    "white",
                    "white",
                    "white",
                    "magenta",
                    "yellow",
                  );
                }

                sctx.fillStyle = color;

                if (deg === 90) {
                  sctx.fillText(
                    text,
                    floor(
                      sctx.canvas.height * (1 - yDist) - textWidth + offsetY,
                    ),
                    -floor(offsetX + gap),
                  );
                } else if (deg === -90) {
                  sctx.fillText(
                    text,
                    -floor(sctx.canvas.height * yDist + textWidth + offsetY),
                    floor(offsetX - gap),
                  );
                }
              });

              if (HANDLE) {
                sctx.font = `${typeSize * 1.25}px YWFTProcessing-Light`;
                sctx.fillStyle = choose("yellow", "red", "blue");
                let offsetX, offsetY;
                const handleWidth =
                  textWidth / 2 + sctx.measureText(HANDLE).width / 2;
                const handleSpace = typeSize * 1.35;
                offsetX = choose(-1, 0, 1);
                offsetY = choose(-1, 0, 1);

                if (deg === 90) {
                  // Handle
                  sctx.fillText(
                    HANDLE,
                    floor(
                      sctx.canvas.height * (1 - yDist) - handleWidth + offsetY,
                    ),
                    -floor(offsetX + handleSpace + gap),
                  );
                } else if (deg === -90) {
                  sctx.fillText(
                    HANDLE,
                    -floor(sctx.canvas.height * yDist + handleWidth + offsetY),
                    floor(offsetX - handleSpace - gap),
                  );
                }
              }

              sctx.restore();
            }

            send({
              type: "recorder:transcode-progress",
              content: progress,
            });
          },
        );
      }

      const svgCursor = new Image();
      svgCursor.onload = (e) => startRendering();
      svgCursor.src = "/aesthetic.computer/cursors/precise.svg";

      // Video rendered, now download...
      videoRecorder.onstop = async function (e) {
        const blob = new Blob(chunks, { type: videoRecorder.mimeType });
        const filename = `tape-${timestamp()}.mp4`;

        await receivedChange({
          data: {
            type: "store:persist",
            content: {
              key: "tape",
              method: "local:db",
              data: { blob, duration: mediaRecorderDuration },
            },
          },
        });

        // 📥 Download the video.
        receivedDownload({ filename, data: blob });
        send({ type: "recorder:printed", content: { id: filename } });
        stopTapePlayback?.();
        send({ type: "recorder:present-paused" });
        // startTapePlayback(true);
        // pauseTapePlayback?.();
      };

      send({ type: "recorder:printing:started" });
      return;
    }

    if (type === "recorder:slate") {
      if (mediaRecorder?.mimeType.indexOf("video") !== -1) {
        await Store.del("tape");
      }
      mediaRecorder?.stop();
      return;
    }

    // Load a bitmap off the network.
    if (type === "load-bitmap") {
      const controller = new AbortController();
      mediaPathsLoading[content] = controller;

      const img = document.createElement("img");
      img.src = content;
      img.crossOrigin = "anonymous";

      const onLoad = async () => {
        const bitmap = await toBitmap(img);
        send(
          {
            type: "loaded-bitmap-success",
            content: { url: content, img: bitmap },
          },
          [bitmap.pixels.buffer],
        );
        img.removeEventListener("error", onError);
        // Clean up the loading tracker since the load completed successfully
        delete mediaPathsLoading[content];
      };

      const onError = (err) => {
        // console.error(err);
        send({ type: "loaded-bitmap-rejection", content: { url: content } });
        img.removeEventListener("load", onLoad); // Remove the other listener too
        // Clean up the loading tracker since the load failed
        delete mediaPathsLoading[content];
      };

      img.addEventListener("load", onLoad);
      img.addEventListener("error", onError);

      controller.signal.addEventListener("abort", () => {
        if (debug) console.log("🖼️ Aborted image load:", content);
        img.removeEventListener("load", onLoad);
        img.removeEventListener("error", onError);
        // Update src to stop current loading.
        img.src =
          "data:image/gif;base64,R0lGODlhAQABAIAAAAAAAP///yH5BAEAAAAALAAAAAABAAEAAAIBRAA7";
        // Clean up the loading tracker since the load was aborted
        delete mediaPathsLoading[content];
      });

      return;
    }

    // Abort a loading piece of media if it exists.
    // TODO: Only implemented on bitmaps for now. 23.10.02.15.13
    if (type === "load:abort") {
      mediaPathsLoading[content]?.abort();
      return;
    }

    // Load a sound from a url with instant playback support.
    if (type === "sfx:load") {
      if (debug && logs.audio) console.log("🔈 BIOS received sfx:load request for:", content);
      
      let internal = false;

      for (let wl of soundWhitelist) {
        if (content === wl) {
          internal = true;
          break;
        }
      }

      let url;
      if (internal) {
        const ext = Safari ? "m4a" : "ogg";
        url = `/sounds/AeCo_${content}.${ext}`;
        if (window.production === true) {
          url = `https://assets.aesthetic.computer` + url;
        } else {
          url = `/assets` + url;
        }
      } else url = content;

      if (debug && logs.audio) console.log("🔈 BIOS loading sound from URL:", url);

      // Enhanced instant playback audio loading strategy
      const audioId = content;
      
      // Strategy 1: Immediate HTML5 Audio for instant playback
      const htmlAudioElement = new Audio();
      htmlAudioElement.crossOrigin = "anonymous";
      htmlAudioElement.preload = "auto";
      htmlAudioElement.src = url;
      
      // Store the HTML5 audio for instant playback
      sfx[audioId + "_html5"] = htmlAudioElement;
      
      if (debug && logs.audio) console.log("🔈 BIOS stored HTML5 audio element for:", audioId + "_html5");
      
      // Strategy 2: Background fetch and decode for high-quality playback
      fetch(url)
        .then((response) => {
          return response.arrayBuffer();
        })
        .then(async (arrayBuffer) => {
          if (debug && logs.audio) console.log("🔈 BIOS fetched audio data for:", audioId);
          try {
            if (!audioContext) {
              sfx[audioId] = arrayBuffer;
              if (debug && logs.audio) console.log("🔈 BIOS stored raw audio buffer (no audioContext):", audioId);
              // Process any queued sounds that might be waiting for this file
              processPendingSfx();
            } else {
              // Background decode the buffer
              const audioBuffer = await audioContext.decodeAudioData(arrayBuffer);
              sfx[audioId] = audioBuffer;
              
              if (debug && logs.audio) console.log("🔈 BIOS decoded and stored audio buffer:", audioId);
              
              // Clean up HTML5 audio once high-quality buffer is ready
              if (sfx[audioId + "_html5"]) {
                delete sfx[audioId + "_html5"];
              }
              
              if (debug && logs.audio)
                console.log("🔈 Background decoded for high-quality playback:", audioId);
              
              // Process any queued sounds that might be waiting for this file
              processPendingSfx();
            }
            
            send({
              type: "loaded-sfx-success",
              content: { url, sfx: audioId },
            });
            if (debug && logs.audio) console.log("🔈 BIOS sent loaded-sfx-success (background):", audioId);
          } catch (error) {
            if (debug && logs.audio)
              console.error("Background audio decoding failed:", error);
            // Keep the HTML5 audio as fallback
            send({
              type: "loaded-sfx-success",
              content: { url, sfx: audioId },
            });
          }
        })
        .catch((error) => {
          // Keep the HTML5 audio as fallback
          send({
            type: "loaded-sfx-success", 
            content: { url, sfx: audioId },
          });
        });

      // Immediately signal success with instant playback capability
      send({
        type: "loaded-sfx-success",
        content: { url, sfx: audioId, instantPlayback: true },
      });
      if (debug && logs.audio) console.log("🔈 BIOS sent loaded-sfx-success (instant):", audioId);

      return;
    }

    // Trigger a sound to playback.
    if (type === "sfx:play") {
      playSfx(content.id, content.sfx, content.options);
      return;
    }

    if (type === "sfx:update") {
      sfxPlaying[content.id]?.update(content.properties);
      return;
    }

    if (type === "sfx:get-sample-data") {
      async function checkForSampleData() {
        if (audioContext) {
          const audioBuffer = await decodeSfx(content.id);

          if (!audioBuffer) {
            setTimeout(checkForSampleData, 100);
          } else {
            const dataFloat32 = audioBuffer.getChannelData(0);
            const data = Array.from(dataFloat32);

            send({
              type: "sfx:got-sample-data",
              content: { id: content.id, data },
            });
          }
        } else {
          setTimeout(checkForSampleData, 100);
          // await checkForSampleData();
        }
      }

      checkForSampleData();
      return;
    }

    if (type === "sfx:get-duration") {
      async function checkForDuration() {
        if (audioContext) {
          const audioBuffer = await decodeSfx(content.id);

          if (!audioBuffer) {
            setTimeout(checkForDuration, 100);
          } else {
            // Just send duration, not the expensive sample data
            send({
              type: "sfx:got-duration",
              content: { id: content.id, duration: audioBuffer.duration },
            });
          }
        } else {
          setTimeout(checkForDuration, 100);
        }
      }

      checkForDuration();
      return;
    }

    // Stop a playing sound or sample if it exists,
    // with an optional 'after' parameter for a fade out.
    if (type === "sfx:kill") {
      sfxPlaying[content.id]?.kill(content.fade);
      return;
    }

    //if (type === "sfx:fade") {
    // sfxPlaying[content.id]?.kill();
    //  return;
    //}

    // Report progress of a playing sound back to the disk.
    if (type === "sfx:progress") {
      sfxPlaying[content.id]?.progress();
      return;
    }

    if (type === "fullscreen-enable") {
      curReframeDelay = 0;
      enableFullscreen();
      return;
    }

    if (type === "fps-change") {
      Loop.frameRate(content);
      return;
    }

    if (type === "glaze") {
      if (debug && logs.glaze) {
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
      //if (debug && logs.loading)
      //  console.log("⏳ Preloaded:", window.preloaded ? "✅" : "❌");
      consumeDiskSends(send);
      return;
    }    if (type === "back-to-piece") {
      console.log("🔙 Browser history.back() called from URL:", window.location.href);
      console.log("🔙 Browser history length:", window.history.length);
      history.back();
      return false;
    }

    if (type === "disk-unload") {
      return;
    }

    // 🌟 Update & Render (Compositing)
    if (!(type === "render" || type === "update")) return;
    if (!content) return;

    if (content.TwoD) {
      TwoD?.pack(content.TwoD);
    }

    updateSynths(content.sound); // 🔈 Trigger any audio that was called upon.

    // 🖥️ Compositing

    // This is a bit messy compared to what happens inside of content.reframe -> frame below. 22.10.27.02.05
    if (
      content.pixels?.byteLength > 0 &&
      content.width === screen.width &&
      content.height === screen.height
    ) {
      screen.pixels = new Uint8ClampedArray(content.pixels);
      // screen.width = content.width;
      // screen.height = content.height;
      let width = screen.width;
      let height = screen.height;

      if (content.reframe && content.reframe.width && content.reframe.height) {
        width = content.reframe.width;
        height = content.reframe.height;
      }

      imageData = new ImageData(screen.pixels, width, height);
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
        freezeFrame,
      );
      //frameAlreadyRequested = false; // 🗨️ Tell the system we are ready for another frame.
      // ^ Deprecated: 23.09.17.01.20
      return;
    }

    let dirtyBoxBitmapCan;

    // 👌 Otherwise, grab all the pixels, or some, if `dirtyBox` is present.
    if (content.dirtyBox) {
      // 🅰️ Cropped update.
      const imageData = new ImageData(
        new Uint8ClampedArray(content.pixels), // Is this the only necessary part?
        content.dirtyBox.w,
        content.dirtyBox.h,
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
        content.width,
        content.height,
      );
    }

    pixelsDidChange = content.paintChanged || false;

    // ✨ UI Overlay (Composite) Layer
    // This currently paints corner labels and tape progress bars only.
    // (So they can be skipped for recordings?)
    let paintOverlays = {};
    function buildOverlay(name, o) {
      if (!o) return;

      paintOverlays[name] = () => {
        octx.imageSmoothingEnabled = false;

        overlayCan.width = o.img.width;
        overlayCan.height = o.img.height;

        octx.putImageData(
          new ImageData(o.img.pixels, o.img.width, o.img.height),
          0,
          0,
        );
        ctx.drawImage(overlayCan, o.x, o.y);
      };
    }

    buildOverlay("label", content.label);
    buildOverlay("tapeProgressBar", content.tapeProgressBar);

    function draw() {
      // 🅰️ Draw updated content from the piece.

      const db = content.dirtyBox;
      if (db) {
        ctx.drawImage(dirtyBoxBitmapCan, db.x, db.y);
        if (glaze.on) Glaze.update(dirtyBoxBitmapCan, db.x, db.y);
      } else if (
        pixelsDidChange ||
        needs$creenshot ||
        mediaRecorder?.state === "recording"
      ) {
        ctx.putImageData(imageData, 0, 0); // Comment out for a `dirtyBox` visualization.

        paintOverlays["label"]?.(); // label

        //
        if (
          // typeof paintToStreamCanvas === "function" &&
          mediaRecorder?.state === "recording" &&
          mediaRecorderStartTime !== undefined
          // frameCount % 2n === 0n
        ) {
          // Dump each frame frame if we are recording.
          recordedFrames.push([
            performance.now() - mediaRecorderStartTime,
            ctx.getImageData(0, 0, ctx.canvas.width, ctx.canvas.height),
          ]);
        }

        if (needs$creenshot) {
          needs$creenshot(
            ctx.getImageData(0, 0, ctx.canvas.width, ctx.canvas.height),
          );
          needs$creenshot = null;
        }

        paintOverlays["tapeProgressBar"]?.(); // tape progress

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
        canvas.style.removeProperty("opacity");
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

      // Show the spinner on any piece other than the first, and never
      // on the prompt.
      if (
        content.loading === true &&
        currentPiece !== null &&
        currentPiece !== "aesthetic.computer/disks/prompt"
      ) {
        UI.spinner(uiCtx, now);
      }

      if (debug && frameCached && content.loading !== true) UI.cached(uiCtx); // Pause icon.
      uiCtx.resetTransform();
    }

    if (
      pixelsDidChange ||
      needs$creenshot ||
      mediaRecorder?.state === "recording" ||
      pen.changedInPiece
    ) {
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
        canvas.style.removeProperty("opacity");
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

    if (needsReappearance /* && wrapper.classList.contains("hidden")*/) {
      // wrapper.classList.remove("hidden");
      needsReappearance = false;
    }

    frameAlreadyRequested = false; // 🗨️ Signal readiness for the next frame.
    // if (lastRender) console.log(performance.now() - lastRender)
    // lastRender = performance.now()
  }

  // 📤 Reads a file and uploads it to the server.
  async function receivedUpload(
    { filename, data, bucket },
    callbackMessage = "upload",
  ) {
    console.log("📤 Uploading file:", filename, typeof data || "...");
    const ext = extension(filename);
    let MIME = "application/octet-stream"; // Default content type.

    if (ext === "json") {
      // JSON
      MIME = "application/json";

      // Parse JSON strings if they haven't been already, including support for `bigints`.
      if (typeof data !== "string") {
        data = JSON.stringify(
          data,
          (k, v) => (typeof v === "bigint" ? v.toString() : v),
          // 2 // Also make sure we indent by 2 spaces so it's nicely formatted.
        );
      }
    }

    if (ext === "mjs") MIME = "application/javascript";
    if (ext === "lisp") MIME = "text/x-lisp";

    if (ext === "obj") MIME = "application/object";
    if (ext === "glb") MIME = "model/gltf-binary";

    if (ext === "mp4") {
      MIME = "video/mp4";
      // data = mediaRecorderBlob;
    }

    if (ext === "png") {
      MIME = "image/png";
      data = await bufferToBlob(data, MIME); // Could be adding modifiers here...
    }

    if (ext === "zip") MIME = "application/zip";

    let prefetchURL = "/presigned-upload-url/" + ext;

    if (bucket === "wand") prefetchURL += "/" + filename + "/" + bucket; // Add filename info.

    // if (bucket === undefined) prefetchURL += "/" + filename; // "art" bucket.
    // 📓 This is handled on the server if an empty bucket is sent.

    // Authorization: Check to see if we will use a user or a guest bucket.
    const headers = {};

    // If no bucket is specified, then try and use the "user" bucket.
    let userMedia = false,
      token;
    if (!bucket) {
      token = await authorize();
      if (token) {
        userMedia = true;
        bucket = "user";
        headers.Authorization = `Bearer ${token}`;
        // This filename gets sorted into the user bucket via their own
        // directory upon uploading.
        // Otherwise if there is no authorization, we just send an empty filename
        // slug with an extension and an identifier gets generated via nanoid on
        // the server.
        prefetchURL += "/" + filename + "/" + bucket; // Add filename info.
      }
    }

    function error(err) {
      send({
        type: callbackMessage,
        content: {
          result: "error",
          data: null,
        },
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
        const path = url.pathname.slice(1); // Remove prepending "/";

        if (debug) console.log("🔐 Presigned URL:", presignedUrl);

        const xhr = new XMLHttpRequest();
        xhr.open("PUT", presignedUrl, true);
        xhr.setRequestHeader("Content-Type", MIME);
        xhr.setRequestHeader("Content-Disposition", "inline");
        xhr.setRequestHeader("x-amz-acl", "public-read");

        const blob = new Blob([data]);

        xhr.upload.addEventListener("progress", (event) => {
          console.log(`Uploaded ${event.loaded} of ${blob.size} bytes...`);
          send({
            type: "upload:progress",
            content: event.loaded / event.total,
          }); // Send a progress callback.
        });

        // Browser is online, send the request
        xhr.onerror = error;

        xhr.onreadystatechange = async function () {
          if (xhr.readyState === XMLHttpRequest.DONE && xhr.status === 200) {
            if (
              (userMedia && token && ext === "png") ||
              ext === "mjs" ||
              ext === "lisp"
            ) {
              // TODO: Go ahead and add this media to the database.
              if (debug) {
                console.log(
                  "🗞️ Adding media to the database:",
                  slug,
                  path,
                  ext,
                );
              }

              // TODO: Write an authorized POST request that contains the slug
              //       to "api/track-media"
              const headers = {
                Authorization: `Bearer ${token}`,
                "Content-Type": "application/json",
              };

              const options = { method: "POST", headers };
              options.body = JSON.stringify({ slug, ext });
              const added = await fetch("api/track-media", options);
              if (debug) console.log("🗞️ Added to database...", added);
            }

            let data = { slug, url: url.toString(), ext };

            if (!userMedia && (ext === "mjs" || ext === "lisp")) {
              data.url =
                "https://art.aesthetic.computer/" + data.slug + "." + data.ext;
            }

            send({
              type: callbackMessage,
              content: { result: "success", data },
            });

            if (debug) console.log("✔️ File uploaded:", xhr.responseURL);
          }
        };

        try {
          xhr.send(blob, { type: MIME });
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

  async function authorize() {
    let token;
    try {
      token = window.acTOKEN;

      if (token) {
        // console.log("🔐 Hosted token found...");

        try {
          // Attempt to fetch user info using the token
          window.auth0Client.token = token;
          await window.auth0Client.getUser();
          // console.log("✅🔐 Token is valid!");
        } catch (error) {
          console.error("🔴🔐 Token is invalid or expired:", token);
          if (window.parent) window.parent.postMessage({ type: "logout" }, "*");
        }
      } else {
        // If acTOKEN is not available, get a new one
        // console.log("🔐 Retrieving auth token...");
        token = await window.auth0Client.getTokenSilently();
        // await window.auth0Client.getUser();
        // console.log("✅ Token is valid");
      }

      // console.log("🔐 Authorized");
    } catch (err) {
      // console.log("🔐️ ❌ Unauthorized", err);
    }
    return token;
  }

  // Reads the extension off of filename to determine the mimetype and then
  // handles the data accordingly and downloads the file in the browser.
  // Downloads both cached files via `data` and network stored files for
  // users and guests.
  async function receivedDownload({ filename, data, modifiers }) {
    console.log("💾 Downloading:", filename);
    // if (data) console.log("Data:", typeof data);
    // if (modifiers.sharing === true) presharingFile = true;

    let object, blob;
    let MIME = "application/octet-stream"; // Default content type.
    const ext = extension(filename);

    if (ext === "glb") {
      MIME = "model/gltf+binary";
      object = URL.createObjectURL(new Blob([data], { type: MIME }));
    } else if (ext === "json" || ext === "gltf") {
      // ✍️ Text + 3D
      // JSON
      MIME = "application/json";
      // GLTF
      if (extension(filename === "gltf")) MIME = "model/gltf+json"; // Hacky conditional above...

      // Parse JSON strings if they haven't been already, including support for `bigints`.
      if (typeof data !== "string") {
        data = JSON.stringify(
          data,
          (k, v) => (typeof v === "bigint" ? v.toString() : v),
          // 2 // Also make sure we indent by 2 spaces so it's nicely formatted.
        );
      }
      object = URL.createObjectURL(new Blob([data], { type: MIME }));
    } else if (ext === "png" || ext === "webp") {
      // 🖼️ Images
      MIME = "image/png"; // PNG

      if (extension(filename) === "webp") {
        MIME = "image/webp";
      }

      if (data) {
        // Download locally if data is provided.
        blob = await bufferToBlob(data, MIME, modifiers);
        object = URL.createObjectURL(blob, { type: MIME });
      } else {
        // Or from the storage network.
        // Check to see if filename has user handle data.
        const hasEmailOrHandle = filename.split("/")[0].indexOf("@") > -1;
        object = hasEmailOrHandle
          ? `/media/${filename}`
          : `https://art.aesthetic.computer/${filename}`;
      }
    } else if (ext === "mp4" || ext === "webm") {
      // TODO: ⚠️ `webm` could eventually mean audio here...
      // 🎥 Video
      // Use stored data from the global Media Recorder.
      const tape = data || (await Store.get("tape")).blob;

      // 🫲 Make sure the container matches the extension.
      const tapeMIME = tape.type; // Check the tape's blob's type.
      if (tapeMIME.indexOf("webm") > -1) {
        filename = filename.replace(".mp4", ".webm"); // Replaces ".mp4" set from `video`.
      } else {
        filename = filename.replace(".webm", ".mp4");
      }

      if (tape) {
        object = URL.createObjectURL(tape);
      } else {
        // console.warn(
        //   "🕸️ No local video available... Trying art bucket:",
        //   filename,
        // );
        // object = `https://art.aesthetic.computer/${filename}`;
      }
    } else if (ext === "mjs") {
      MIME = "application/javascript; charset=utf-8";
      object = URL.createObjectURL(new Blob([data], { type: MIME }));
    } else if (extension === "zip") {
      object = URL.createObjectURL(data, { type: MIME });
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

    // Check if navigator.share is supported and we are either on
    // iOS or Android
    // console.log("🧑‍🤝‍🧑 Sharing:", modifiers?.sharing, "Capable:", navigator.share);
    if ((iOS || Android) && modifiers?.sharing === true && navigator.share) {
      shareFile = new File(
        [blob || new Blob([data], { type: MIME })],
        filename.split("/").pop(),
        { type: MIME, lastModified: new Date().getTime() },
      );
      shareFileCallback?.(); // Run the callback if necessary, which should
      // prevent any special race conditions.
    } else {
      // Fallback to download if navigator.share is not supported
      const a = document.createElement("a");
      a.href = object;
      a.target = "_blank";
      a.download = filename.split("/").pop(); // Remove any extra paths.
      a.click();
      if (typeof a.href !== "string") URL.revokeObjectURL(a.href);
    }

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

  async function bufferToBlob(data, MIME = "image/png", modifiers) {
    let can;
    // Encode a pixel buffer as a png.
    // See also: https://stackoverflow.com/questions/11112321/how-to-save-canvas-as-png-image

    const imageData = data.data
      ? data
      : new ImageData(data.pixels, data.width, data.height);
    // Convert to imageData if it isn't already.

    can = document.createElement("canvas");
    const ctx = can.getContext("2d");

    if (modifiers?.cropToScreen) {
      can.width = screen.width;
      can.height = screen.height;
    } else {
      can.width = imageData.width;
      can.height = imageData.height;
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

    const blob = await new Promise((resolve) => {
      if (modifiers?.dataURL === true) {
        resolve(can.toDataURL(MIME));
      } else {
        can.toBlob(resolve, MIME, 100);
      }
    });
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
  let handAPI;
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

      let settings, stream, videoTrack;
      let facingModeChange = false;

      try {
        // Grab video from the user using a requested width and height based
        // on the frame size.
        let cWidth = options.width,
          cHeight = options.height;

        async function getDevice(facingModeChoice) {
          // Swap width and height on iOS. (Implementation default differences.)
          // Set a height / width aspect ratio on iOS because
          // of implementation differences.

          // console.log("Trying: Width:", cWidth, "Height:", cHeight);

          const constraints = {
            facingMode: facingModeChoice,
            frameRate: { ideal: 30 },
          };

          if (
            (iOS || Android) &&
            window.matchMedia("(orientation: portrait)").matches &&
            (facingModeChoice === "environment" || facingModeChoice === "user")
            // &&
            // firstVideo
          ) {
            const temp = cWidth;
            cWidth = cHeight;
            cHeight = temp;
            // firstVideo = false;
          }

          // alert(cWidth + " " + cHeight);

          constraints.width = { ideal: cWidth };
          constraints.height = { ideal: cHeight };

          stream = await navigator.mediaDevices.getUserMedia({
            video: { ...constraints },
            audio: false,
          });

          video.srcObject = stream;
          videoTrack = stream.getVideoTracks()[0];
          // const capabilities = videoTrack.getCapabilities();
          settings = videoTrack.getSettings();

          // console.log(
          //   "Got: Width:",
          //   settings.width,
          //   "Height:",
          //   settings.height,
          // ); // ❤️‍🔥

          // Update global facingMode in case different from requested.
          facingMode = videoTrack.getConstraints().facingMode;

          const devices = await navigator.mediaDevices.enumerateDevices();
          const videoDevices = devices.filter(
            (device) => device.kind === "videoinput",
          );

          send({ type: "video-devices", content: videoDevices.length });

          if (debug) {
            videoDevices.forEach((device, index) => {
              if (index === 0) {
                console.log(
                  `Camera ${index + 1} (usually environment):`,
                  device.label,
                  device,
                );
              } else if (index === 1) {
                console.log(`Camera ${index + 1} (usually user):`, device);
              } else {
                console.log(`Camera ${index + 1} (additional camera):`, device);
              }
            });
          }
        }

        await getDevice(facingMode);

        video.addEventListener(
          "loadedmetadata",
          () => {
            video.play();
            if (debug)
              console.log("🎥 Resolution:", buffer.width, buffer.height);
          },
          { once: true },
        );

        // Resizing the video after creation. (Window resize or device rotate.)
        videoResize = async function ({ width, height, facing }) {
          cancelAnimationFrame(getAnimationRequest());

          try {
            const sizeChange = !isNaN(width) && !isNaN(height);

            if (sizeChange) {
              video.addEventListener(
                "loadedmetadata",
                () => {
                  buffer.width = cWidth;
                  buffer.height = cHeight;
                  process();
                  send({ type: "camera:updated" });
                  if (debug)
                    console.log("🎥 Resolution:", buffer.width, buffer.height);
                },
                { once: true },
              );

              if (iOS || Android) {
                await getDevice(facing);
              } else {
                video.srcObject = null; // Refresh the video `srcObject`.
                await videoTrack.applyConstraints({
                  width: { ideal: cWidth },
                  height: { ideal: cHeight },
                });
              }
            }

            if (settings.facingMode !== facing) {
              facingModeChange = true;
              await getDevice(facing);
              facingModeChange = false;

              video.addEventListener(
                "canplay",
                () => {
                  process();
                  send({ type: "camera:updated", content: facingMode });
                },
                { once: true },
              );
            } else {
            }

            // video.srcObject = stream;
            // if (!sizeChange && !facingModeChange) process();
          } catch (error) {
            process();
            if (debug) console.warn("🎥 Resolution update failed.", error);
          }
        };

        // ✋ Optional Hand-tracking (only load once)
        if (hands === true && !handAPI) {
          if (useLegacyHandsAPI) {
            // Load older mediapipe lib.
            const script = document.createElement("script");
            script.src = "/aesthetic.computer/dep/@mediapipe/hands/hands.js";
            script.crossOrigin = "anonymous";

            script.onload = function () {
              const config = {
                locateFile: (file) => {
                  return `aesthetic.computer/dep/@mediapipe/hands/${file}`;
                },
              };

              handAPI = { hands: new Hands(config) }; // Globally def. handAPI.
              window.handAPI = handAPI; // For production debugging.

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
                "/aesthetic.computer/dep/@mediapipe/tasks-vision/wasm",
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
                },
              );
            }
          }
        }

        process(); // Start processing data.
      } catch (err) {
        send({ type: "camera:denied" });
        console.log(err);
      }

      function diagram(hand) {
        if (facingMode === "user") {
          // hand.screen.forEach((l) => (l.x = 1 - l.x));
          // Reverse handedness because our data is mirrored.
          // if (hand.handedness === "left") {
          //   hand.handedness = "right";
          // } else if (hand.handedness === "right") {
          //   hand.handedness = "left";
          // }
        }
        handData = hand;
      }

      function process() {
        cancelAnimationFrame(getAnimationRequest());
        if (facingModeChange) return;
        // cancelAnimationFrame(getAnimationRequest());
        // TODO: Video effects / filter kernels could be added here...
        // 💡 For GPU backed visuals. 23.04.29.20.47

        // Send frames by default.
        if (facingMode === "user" || (!iOS && !Android)) {
          bufferCtx.translate(buffer.width / 2, buffer.height / 2);
          const zoom = 1;
          // if (hands) {
          // bufferCtx.scale(zoom, zoom);
          // } else {
          bufferCtx.scale(-zoom, zoom);
          // }
          bufferCtx.translate(-buffer.width / 2, -buffer.height / 2);
        }

        // 🤚 Track Hands on the GPU if flagged.
        if (hands === true && handAPI) {
          if (handVideoTime !== video.currentTime && video.videoWidth > 0) {
            handVideoTime = video.currentTime;
            if (useLegacyHandsAPI && !handAPI?.legacyProcessing) {
              handAPI.hands?.send({ image: bufferCtx.canvas }).then(() => {
                handAPI.legacyProcessing = false;
                // Don't process more than one frame at a time.
              });
              handAPI.legacyProcessing = true;
            } else {
              // const data = handAPI.hl?.detectForVideo(video, handVideoTime);
              // TODO: This will no longer work. 23.5.24
              //       Check the other `diagram` call.
              // diagram(data?.landmarks[0] || []);
            }
            // send({type: "hand-tracking-data", content: landmarks});
          }
        }

        // Drawing a video frame to the buffer (mirrored, proportion adjusted).
        const videoAR = video.videoWidth / video.videoHeight;
        const bufferAR = buffer.width / buffer.height;
        let outWidth,
          outHeight,
          outX = 0,
          outY = 0;

        if (videoAR <= bufferAR) {
          // Tall to wide.
          outWidth = buffer.width;
          outHeight = outWidth / videoAR;
        } else {
          // Wide to tall.
          outHeight = buffer.height;
          outWidth = outHeight * videoAR;
        }

        outY = (buffer.height - outHeight) / 2; // Adjusting position.
        outX = (buffer.width - outWidth) / 2;

        bufferCtx.drawImage(video, outX, outY, outWidth, outHeight);
        bufferCtx.resetTransform();

        if (options.hidden !== true) {
          const pixels = bufferCtx.getImageData(
            0,
            0,
            buffer.width,
            buffer.height,
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
            [pixels.data.buffer],
          );
        }

        animationRequest = requestAnimationFrame(process);
      }
    }
  }

  // Pointer Lock 🔫
  document.addEventListener("pointerlockchange", () => {
    send({
      type:
        document.pointerLockElement === wrapper ? "pen:locked" : "pen:unlocked",
    });
  });

  // document.addEventListener("pointerlockerror", () => {
  // console.error("Pointer lock failed!");
  //});

  // Window Scroll 📜
  window.addEventListener("wheel", function (event) {
    send({
      type: "scroll",
      content: {
        x: event.deltaX / subdivisions,
        y: event.deltaY / subdivisions,
      },
    });
  });

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
    if (!document.hidden) wrapper.classList.remove("reloading");
    // if (document.hidden) mediaRecorder?.stop();
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
  // Note: These are useful for
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

    const sluggy = slug(document.location.href);
    if (sluggy === "prompt") keyboard?.input.focus();

    send({
      type: "history-load",
      content: parse(sluggy || window.acSTARTING_PIECE),
    });
  };

  // Fullscreen
  // Note: This doesn't work in Safari because you can't fullscreen the body element.
  //       (Or anything other than a video element?) 22.2.13

  const requestFullscreen =
    document.body.requestFullscreen || wrapper.webkitRequestFullscreen;

  // const exitFullscreen =
  //   document.exitFullscreen || document.webkitExitFullscreen;

  // Tries to toggle fullscreen. Must be called within a user interaction.
  function enableFullscreen() {
    const fullscreenElement =
      document.fullscreenElement || document.webkitFullscreenElement;

    if (!fullscreenElement) {
      requestFullscreen.apply(document.body)?.catch((e) => console.error(e));
    } else {
      // exitFullscreen();
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

  // 📄 Drag and Drop File API

  // Drag over...
  document.body.addEventListener("dragover", function (e) {
    e.stopPropagation();
    e.preventDefault();
    e.dataTransfer.dropEffect = "copy"; // Show as copy
    // copy, move, link, or none
  });

  document.body.addEventListener("drop", async function (e) {
    e.stopPropagation();
    e.preventDefault();
    const files = e.dataTransfer.files; // Get the file(s).
    // Check if a file was dropped and process only the first one.
    if (files.length > 0) {
      const file = files[0];
      const ext = extension(file.name);
      console.log("💧 Dropped:", file.name, ext);
      // 🗒️ Source code file.
      if (ext === "mjs") {
        const reader = new FileReader();
        reader.onload = function (e) {
          send({
            type: "dropped:piece",
            content: {
              name: file.name.replace(".mjs", ""),
              source: e.target.result,
            },
          });
        };

        reader.readAsText(file);
        // 🖼️ Image file
      } else if (
        ext === "png" ||
        ext === "jpeg" ||
        ext === "jpg" ||
        ext === "gif" ||
        ext === "webp"
      ) {
        const bitmap = await toBitmap(file);
        send({
          type: "dropped:bitmap",
          content: {
            name: file.name.replace("." + ext, ""),
            source: bitmap,
          },
        });
        // 🖼️⌛ Recorded Painting (or other complex media)
      } else if (ext === "zip") {
        const reader = new FileReader();
        reader.onload = async function (e) {
          const data = e.target.result;
          if (!window.JSZip) await loadJSZip();
          const record = await unzip(data);
          if (record)
            send({ type: "painting:record:dropped", content: record });
        };
        reader.readAsArrayBuffer(file);
      }
    }
  });

  // Instantly decode the audio before playback if it hasn't been already.
  async function decodeSfx(sound) {
    // If sound is already being decoded, wait a bit and return
    if (decodingInProgress.has(sound)) {
      // Wait a moment and check again
      await new Promise(resolve => setTimeout(resolve, 10));
      return sfx[sound];
    }
    
    if (sfx[sound] instanceof ArrayBuffer) {
      // Mark as being decoded to prevent concurrent decode attempts
      decodingInProgress.add(sound);
      
      let audioBuffer;
      try {
        const buf = sfx[sound];
        sfx[sound] = null;
        if (buf) {
          audioBuffer = await audioContext.decodeAudioData(buf);
          if (debug && logs.audio) console.log("🔈 Decoded:", sound);
          sfx[sound] = audioBuffer;
          
          // Process any queued sounds that might be waiting for this file
          processPendingSfx();
          
          return sfx[sound];
        }
      } catch (err) {
        console.error("🔉 [DECODE] Decode error:", err, "➡️", sound);
      } finally {
        // Always remove from decoding set when done
        decodingInProgress.delete(sound);
      }
    } else {
      return sfx[sound];
    }
  }

  // Queue for sounds that need to be played once audio context is available
  let pendingSfxQueue = [];
  
  // Track sounds that are currently being decoded to prevent multiple decode attempts
  const decodingInProgress = new Set();

  // Process any queued sound effects once audio context is ready
  function processPendingSfx() {
    if (audioContext && pendingSfxQueue.length > 0) {
      const remaining = [];
      const currentTime = Date.now();
      
      pendingSfxQueue.forEach(({ id, soundData, options, completed, queuedAt }) => {
        // Add timestamp if not present
        const queueTime = queuedAt || currentTime;
        const timeInQueue = currentTime - queueTime;
        
        // Only play sounds that have been loaded into the sfx cache
        if (sfx[soundData] && !(sfx[soundData] instanceof ArrayBuffer)) {
          playSfx(id, soundData, options, completed);
        } else if (timeInQueue < 5000) { // Only retry for 5 seconds
          remaining.push({ id, soundData, options, completed, queuedAt: queueTime });
        }
      });
      
      pendingSfxQueue = remaining;
      
      // If there are still sounds waiting, check again soon
      if (pendingSfxQueue.length > 0) {
        setTimeout(processPendingSfx, 100);
      }
    }
  }
}

// Utilities

// Convert an img or blob object to an ac formatted bitmap  / "painting".
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

// Unzip a file.
async function unzip(data) {
  try {
    const zip = await window.JSZip.loadAsync(data);

    console.log("🤐 Zip opened...");
    // Detect type of media based on presence of "steps" file...
    const steps = JSON.parse(await zip.file("painting.json")?.async("text"));
    const record = [];

    if (steps) {
      console.log("🖼️⌛ Painting record detected.");

      // TODO: Parse the JSON from steps.
      const lines = steps; // Remove timestamp.

      // Load `painting:recording` step text format.
      for (let i = 0; i < lines.length; i += 1) {
        const components = lines[i].step.split(" - ");
        const step = { timestamp: components[0], label: components[1] };
        if (lines[i].gesture?.length > 0) step.gesture = lines[i].gesture;
        const picture = zip.file(`${lines[i].step}.png`);

        if (picture) {
          const blob = await picture.async("blob");
          step.painting = await toBitmap(blob);
        }
        record.push(step);
      }
      console.log("🖼️⌛ Loaded record:", record);

      return record;
    } else {
      console.warn("🤐 Could not detect ZIP media type.");
      return record;
    }
  } catch (err) {
    console.error("🤐 Error reading ZIP:", err);
    return record;
  }
}

// Convert a blob oobject to an ArrayBuffer.
function blobToArrayBuffer(blob) {
  return new Promise((resolve, reject) => {
    const reader = new FileReader();
    reader.onloadend = () => resolve(reader.result);
    reader.onerror = reject;
    reader.readAsArrayBuffer(blob);
  });
}

window.iMessageExtensionResize = (mode) => {
  console.log("📱 iMessage Extension Resized:", mode);
  window.acSEND({ type: "imessage-extension:resized", content: { mode } });
};

window.iOSAppSwitchPiece = (piece) => {
  console.log("📱 iOS Switch Piece:", piece);
  window.acSEND({
    type: "jump",
    content: { piece, ahistorical: false, alias: false },
  });
};

function iOSAppSend(message) {
  const packedMessage = JSON.stringify(message);
  console.log("📱 Sending to iOS App:", packedMessage);
  window.webkit?.messageHandlers?.iOSApp.postMessage(packedMessage);
}

async function checkMicrophonePermission() {
  try {
    const permissionStatus = await navigator.permissions.query({
      name: "microphone",
    });
    // console.log('Microphone permission status:', permissionStatus.state);
    if (permissionStatus.state === "granted") {
      // console.log('Microphone access is granted.');
    } else if (permissionStatus.state === "denied") {
      // console.log('Microphone access is denied.');
    } else {
      // console.log(`Microphone access is in prompt state (user hasn't decided yet).`);
    }
    return permissionStatus.state; // 'granted', 'denied', or 'prompt'
  } catch (error) {
    console.error("Permission query error:", error);
    return null;
  }
}

export { boot };
