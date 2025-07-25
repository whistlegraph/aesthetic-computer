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
import { isKidlispSource, encodeKidlispForUrl, getSyntaxHighlightingColors } from "./lib/kidlisp.mjs";
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

// 🎬 GIF Encoder Configuration
// Set to true to use gifenc by default, false for gif.js
const DEFAULT_USE_GIFENC = false;

// 🚫 Cache Control Flags
// Set these to true to disable caching for dynamic content updates
window.acDISABLE_HUD_LABEL_CACHE = true; // Disable HUD label caching for dynamic coloring
window.acDISABLE_QR_OVERLAY_CACHE = true; // Disable QR overlay caching for dynamic updates

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
  
  // Only preserve density/zoom if they were actually in the URL (not from localStorage)
  const currentParams = new URLSearchParams(location.search);
  if (currentParams.has("density")) preservedParams.density = currentParams.get("density");
  if (currentParams.has("zoom")) preservedParams.zoom = currentParams.get("zoom");

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
    mediaRecorderStartTime,
    mediaRecorderFrameCount = 0; // Frame counter for performance optimization
  let needs$creenshot = false; // Flag when a capture is requested.

  // Duration Progress Bar (for timed pieces from query parameter)
  let durationProgressStartTime = undefined;
  let durationProgressDuration = resolution.duration; // Duration in seconds from query parameter
  let durationProgressCompleted = false;

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

  // Reusable canvas for dirtyBox updates
  let dirtyBoxCanvas = document.createElement("canvas");
  let dirtyBoxCtx = dirtyBoxCanvas.getContext("2d");

  // Track overlay canvas dimensions to avoid unnecessary resizing
  let overlayCanvasWidth = 0;
  let overlayCanvasHeight = 0;

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
  let reframeJustCompleted = false; // Track when we just finished reframing
  let freezeFrame = false,
    freezeFrameFrozen = false,
    freezeFrameGlaze = false;

  const screen = apiObject("pixels", "width", "height");
  let subdivisions = 1; // Gets set in frame.

  // Track UI scaling state to avoid redundant operations
  let currentUiScale = 0;
  let uiContextScaled = false;

  const REFRAME_DELAY = 80; //250;
  let curReframeDelay = REFRAME_DELAY;
  let lastGap = undefined;
  let density = resolution.density !== undefined ? resolution.density : 2; // Use URL parameter or default to 2

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
      imageData &&
      imageData.data &&
      imageData.data.buffer &&
      imageData.data.buffer.byteLength > 0 &&
      !document.body.contains(freezeFrameCan) &&
      !underlayFrame // Don't show freeze frame during tape playback
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

      if (!wrapper.contains(freezeFrameCan)) {
        wrapper.append(freezeFrameCan);
      } else {
        freezeFrameCan.style.removeProperty("opacity");
      }

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

    // Copy existing canvas contents from clean imageData (not from canvas which may have overlays)
    tempCanvas.width = canvas.width;
    tempCanvas.height = canvas.height;
    if (
      imageData &&
      imageData.width === canvas.width &&
      imageData.height === canvas.height &&
      imageData.data &&
      imageData.data.buffer &&
      imageData.data.buffer.byteLength > 0
    ) {
      // Use clean imageData to avoid copying overlays (only if data is not detached)
      tempCtx.putImageData(imageData, 0, 0);
    } else {
      // Fallback to copying from canvas if imageData doesn't match or data is detached
      tempCtx.drawImage(canvas, 0, 0);
    }

    // Resize the original canvas
    canvas.width = width;
    canvas.height = height;

    // Restore the clean pixels onto the resized canvas
    ctx.drawImage(tempCanvas, 0, 0);

    tempCanvas.width = glazeComposite.width;
    tempCanvas.height = glazeComposite.height;

    tempCtx.drawImage(glazeComposite, 0, 0);

    glazeComposite.width = canvas.width;
    glazeComposite.height = canvas.height;
    glazeCompositeCtx.drawImage(tempCanvas, 0, 0);

    uiCanvas.width = projectedWidth * window.devicePixelRatio;
    uiCanvas.height = projectedHeight * window.devicePixelRatio;

    // Reset UI scaling state when canvas size changes
    uiContextScaled = false;
    currentUiScale = 0;

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

    if (
      imageData &&
      imageData.data &&
      imageData.data.buffer &&
      imageData.data.buffer.byteLength > 0
    ) {
      ctx.putImageData(imageData, 0, 0);
    } else {
      imageData = ctx.getImageData(0, 0, canvas.width, canvas.height);
      // This will have zero alpha.
    }

    // Store clean pixel data for worker communication (before any overlays are drawn)
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
        (async () => {
          try {
            // Wait for stylesheet to load first
            await new Promise((resolve) => {
              const checkFonts = () => {
                // Check if at least one font face is available
                const fontFaces = Array.from(document.fonts);
                if (fontFaces.length > 0) {
                  resolve();
                } else {
                  setTimeout(checkFonts, 50);
                }
              };
              checkFonts();
            });

            // Now load the fonts explicitly
            await Promise.all([
              document.fonts.load("1em YWFTProcessing-Light"),
              document.fonts.load("1em YWFTProcessing-Regular"),
              document.fonts.load("bold 1em YWFTProcessing-Regular"),
              document.fonts.load("normal 1em YWFTProcessing-Light"),
            ]);
          } catch (error) {
            console.warn("⚠️ Font loading during boot failed:", error);
            // Fallback to fonts.ready
            await document.fonts.ready;
          }
        })();
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
    reframeJustCompleted = true; // Mark that we just completed a reframe
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
  const sfxCompletionCallbacks = {}; // Completion callbacks for sound effects.

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
          delete sfxCompletionCallbacks[id]; // Clean up completion callback
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
            // Call the completion callback if it exists
            const completionCallback = sfxCompletionCallbacks[msg.content.id];
            if (completionCallback) {
              completionCallback();
              delete sfxCompletionCallbacks[msg.content.id];
            }

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
        pendingSfxQueue.push({
          id,
          soundData,
          options,
          completed,
          queuedAt: Date.now(),
        });
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

      // Store the completion callback if provided
      if (completed) {
        sfxCompletionCallbacks[id] = completed;
      }

      if (triggerSound) sfxLoaded[soundData] = true;
    } else {
      // Queue the sound effect to be played once audioContext is available
      pendingSfxQueue.push({
        id,
        soundData,
        options,
        completed,
        queuedAt: Date.now(),
      });
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

  // Extract embedded source if available
  let embeddedSource = null;
  try {
    const embeddedScript = document.getElementById("embedded-source");
    if (embeddedScript) {
      embeddedSource = JSON.parse(embeddedScript.textContent);
    }
  } catch (err) {
    console.warn("⚠️ Failed to parse embedded source:", err);
  }

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
      embeddedSource,
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
      pen?.retransformPosition();
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
    let transferrableObjects = [];
    let pixelsBuffer = null;
    
    // Only transfer pixels buffer if it's not detached
    try {
      if (screen.pixels.buffer.byteLength > 0) {
        transferrableObjects = [screen.pixels.buffer];
        pixelsBuffer = screen.pixels.buffer;
      } else {
        // Buffer is detached, create a fresh copy from imageData
        console.warn("🎬 Screen buffer detached during transfer, creating fresh copy from imageData");
        if (imageData && imageData.data && imageData.data.buffer.byteLength > 0) {
          pixelsBuffer = imageData.data.slice().buffer;
        } else {
          // Last resort: create empty buffer with correct size
          const emptyPixels = new Uint8ClampedArray(canvas.width * canvas.height * 4);
          pixelsBuffer = emptyPixels.buffer;
        }
        transferrableObjects = [pixelsBuffer];
      }
    } catch (e) {
      // If we can't access the buffer at all, create a fresh copy from imageData or empty buffer
      console.warn("🎬 Screen buffer detached during transfer, creating copy from imageData");
      try {
        if (imageData && imageData.data && imageData.data.buffer.byteLength > 0) {
          pixelsBuffer = imageData.data.slice().buffer;
        } else {
          // Last resort: create empty buffer with correct size
          const emptyPixels = new Uint8ClampedArray(canvas.width * canvas.height * 4);
          pixelsBuffer = emptyPixels.buffer;
        }
        transferrableObjects = [pixelsBuffer];
      } catch (fallbackError) {
        console.error("🎬 Failed to create fallback buffer:", fallbackError);
        // Create minimal empty buffer as absolute last resort
        const emptyPixels = new Uint8ClampedArray(64 * 64 * 4); // 64x64 fallback
        pixelsBuffer = emptyPixels.buffer;
        transferrableObjects = [pixelsBuffer];
      }
    }

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
          pixels: pixelsBuffer,
          audioTime: audioContext?.currentTime || 0,
          audioBpm: sound.bpm, // TODO: Turn this into a messaging thing.
          audioMusicAmplitude: amplitude,
          audioMusicSampleData: amplitude > 0 ? frequencyData : [],
          width: canvas.width,
          height: canvas.height,
          // TODO: Do all fields of `pointer` need to be sent? 22.09.19.23.30
          pen: { events: pen?.events || [], pointers: pen?.pointers || {} },
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

    pen?.updatePastPositions();

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
    if (pen) pen.events.length = 0;
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
    // Helper function to generate appropriate filenames for tape recordings
    function generateTapeFilename(extension, suffix = "") {
      const options = window.currentRecordingOptions || {
        pieceName: "tape",
        pieceParams: "",
        originalCommand: ""
      };
      
      let baseName = options.pieceName || "tape";
      
      // Handle special cases for kidlisp codes
      if (baseName === "$code") {
        // For kidlisp source code, use the cached code if available
        if (options.cachedCode) {
          console.log(`🎬 Using cached code for filename: $${options.cachedCode}`);
          baseName = `$${options.cachedCode}`;
        } else {
          // Try to extract a $code from the original command as fallback
          const codeMatch = options.originalCommand.match(/\$([a-zA-Z0-9]+)/);
          if (codeMatch) {
            console.log(`🎬 Using extracted code for filename: $${codeMatch[1]}`);
            baseName = `$${codeMatch[1]}`;
          } else {
            // No $code available, use "kidlisp" instead of literal "$code"
            console.log(`🎬 No code available, using "kidlisp" for filename`);
            baseName = "kidlisp";
          }
        }
      }
      // Note: If pieceName already has $ prefix (from cached code), use it as-is
      
      // Add parameters if they exist
      const params = options.pieceParams || "";
      const paramsStr = params ? params.replace(/~/g, "-") : "";
      
      // Build filename: pieceName[params]-timestamp[suffix].extension
      // Use recording start timestamp if available (for tape recordings),
      // otherwise fall back to current timestamp
      let fileTimestamp;
      if (window.recordingStartTimestamp && extension === "gif") {
        // For GIF recordings, use the same timestamp as shown in the overlay
        const d = new Date(window.recordingStartTimestamp);
        const pad = (n, digits = 2) => n.toString().padStart(digits, "0");
        fileTimestamp = `
          ${d.getFullYear()}.
          ${pad(d.getMonth() + 1)}.
          ${pad(d.getDate())}.
          ${pad(d.getHours())}.
          ${pad(d.getMinutes())}.
          ${pad(d.getSeconds())}.
          ${pad(d.getMilliseconds(), 3)}`.replace(/\s/g, "");
      } else {
        // For other files or when no recording timestamp is available
        fileTimestamp = timestamp();
      }
      
      const parts = [
        baseName + paramsStr,
        fileTimestamp + suffix
      ].filter(Boolean);
      
      return parts.join("-") + "." + extension;
    }

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
    } // Disconect from the UDP server.
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

    if (type === "kidlisp:cached-code") {
      // Update recording options with the newly cached code
      if (window.currentRecordingOptions) {
        window.currentRecordingOptions.cachedCode = content;
        // If pieceName was initially set to "$code", update it to the actual cached code
        if (window.currentRecordingOptions.pieceName === "$code") {
          window.currentRecordingOptions.pieceName = `$${content}`;
          console.log(`🎬 Updated pieceName to: $${content}`);
        }
      }
      return;
    }

    // Sync labelBack state between worker and main thread
    if (type === "labelBack:set") {
      mainThreadLabelBack = true;
      sessionStorage.setItem("aesthetic-labelBack", "true");
      console.log("🔗 Main thread: Set labelBack in sessionStorage");
      return;
    }

    if (type === "labelBack:clear") {
      mainThreadLabelBack = false;
      sessionStorage.removeItem("aesthetic-labelBack");
      console.log("🔗 Main thread: Cleared labelBack from sessionStorage");
      return;
    }

    // Helper function to ensure fonts are loaded before use
    let fontsLoaded = false;
    async function ensureFontsLoaded() {
      if (fontsLoaded) {
        return; // Fonts already loaded
      }

      if ("fonts" in document) {
        try {
          // Force font loading with multiple strategies
          const fontPromises = [
            document.fonts.load("1em YWFTProcessing-Regular"),
            document.fonts.load("1em YWFTProcessing-Light"),
            document.fonts.load("bold 1em YWFTProcessing-Regular"),
            document.fonts.load("normal 1em YWFTProcessing-Light"),
          ];

          await Promise.all(fontPromises);
          console.log("✅ Fonts loaded for tape overlay");

          // Additional verification: check if fonts are actually available
          const fontFaces = Array.from(document.fonts);
          const hasRegular = fontFaces.some((font) =>
            font.family.includes("YWFTProcessing-Regular"),
          );
          const hasLight = fontFaces.some((font) =>
            font.family.includes("YWFTProcessing-Light"),
          );

          if (!hasRegular || !hasLight) {
            console.warn(
              "⚠️ Font verification failed - fonts may not be fully loaded",
            );
            // Wait a bit more and try again
            await new Promise((resolve) => setTimeout(resolve, 500));
            await Promise.all(fontPromises);
          }

          fontsLoaded = true; // Mark fonts as loaded
        } catch (error) {
          console.warn("⚠️ Failed to load fonts for tape overlay:", error);
          // Fallback: wait for document.fonts.ready
          await document.fonts.ready;
          fontsLoaded = true; // Mark as loaded even if fallback was used
        }
      }
    }

    // Helper function to add the sideways AC stamp (same as video recording)
    async function addAestheticComputerStamp(ctx, canvasWidth, canvasHeight, progress = 0, frameData = null) {
      // Ensure fonts are loaded before drawing
      await ensureFontsLoaded();

      // 2. Set up the font.
      const typeSize = Math.min(
        16,
        Math.max(12, Math.floor(canvasHeight / 40)),
      );
      const gap = typeSize * 0.75;

      ctx.save(); // Save the current state of the canvas

      // Add film camera style timestamp at bottom-left corner
      addFilmTimestamp(ctx, canvasWidth, canvasHeight, typeSize, progress, frameData);

      drawTextAtPosition(0, 90); // Left
      drawTextAtPosition(canvasWidth, -90); // Right

      ctx.restore();
      ctx.globalAlpha = 1;

      function drawTextAtPosition(positionX, deg) {
        ctx.save();
        ctx.translate(positionX, 0);
        ctx.rotate((deg * Math.PI) / 180); // Convert degrees to radians
        
        // Separate positioning for left and right sides for better spacing
        const leftYDist = 0.45; // Move left side closer to bottom (increased from 0.25)
        const rightYDist = 0.15; // Move right side closer to top (decreased from 0.25)

        // Use same size as timestamp for consistency and sharpness
        const stampSize = Math.min(
          32,
          Math.max(18, Math.floor(canvasHeight / 25)),
        );
        ctx.font = `${stampSize}px YWFTProcessing-Regular`;
        const text = "aesthetic.computer";
        const measured = ctx.measureText(text);
        const textWidth = measured.width;

        ["red", "lime", "blue", "white"].forEach((color) => {
          let offsetX, offsetY;
          if (color !== "white") {
            ctx.globalAlpha = 0.45;
            offsetX = choose([-1, -2, 0, 1, 2]); // Smaller offsets for sharper look
            offsetY = choose([-1, -2, 0, 1, 2]); // Smaller offsets for sharper look
          } else {
            ctx.globalAlpha = choose([0.5, 0.4, 0.6]);
            offsetX = choose([-1, 0, 1]);
            offsetY = choose([-1, 0, 1]);
            color = choose(["white", "white", "white", "magenta", "yellow"]);
          }

          ctx.fillStyle = color;

          if (deg === 90) {
            // Left side - move lower (closer to bottom)
            ctx.fillText(
              text,
              floor(canvasHeight * (1 - leftYDist) - textWidth + offsetY),
              -floor(offsetX + gap),
            );
          } else if (deg === -90) {
            // Right side - move higher (closer to top)
            ctx.fillText(
              text,
              -Math.floor(canvasHeight * rightYDist + textWidth + offsetY),
              Math.floor(offsetX - gap),
            );
          }
        });

        if (HANDLE) {
          ctx.font = `${typeSize * 1.6}px YWFTProcessing-Light`; // Even larger handle text
          const handleWidth = textWidth / 2 + ctx.measureText(HANDLE).width / 2;
          const handleSpace = typeSize * 2.2; // Increased spacing by another 0.2
          
          // Restore original 4-layer system with warmer color palette
          ["red", "lime", "blue", "white"].forEach((color) => {
            let offsetX, offsetY;
            if (color !== "white") {
              ctx.globalAlpha = 0.6;
              offsetX = choose([-2, -4, 0, 2, 4]);
              offsetY = choose([-2, -4, 0, 2, 4]);
            } else {
              ctx.globalAlpha = choose([0.7, 0.8, 0.9]);
              offsetX = choose([-1, 0, 1]);
              offsetY = choose([-1, 0, 1]);
              color = choose(["white", "white", "white", "magenta", "yellow"]); // Original warmer palette
            }

            ctx.fillStyle = color;

            if (deg === 90) {
              // Handle - left side positioning (lower)
              ctx.fillText(
                HANDLE,
                floor(canvasHeight * (1 - leftYDist) - handleWidth + offsetY),
                -floor(offsetX + handleSpace + gap),
              );
            } else if (deg === -90) {
              // Handle - right side positioning (higher)
              ctx.fillText(
                HANDLE,
                -Math.floor(canvasHeight * rightYDist + handleWidth + offsetY),
                Math.floor(offsetX - handleSpace - gap),
              );
            }
          });
        }

        // Remove the old timestamp code from here since it's now in addFilmTimestamp

        ctx.restore();
      }

      // Helper function to get syntax highlighting colors from kidlisp module
      function getSyntaxHighlightColors() {
        // Get cached kidlisp source code
        const cachedCode = window.currentRecordingOptions?.cachedCode;
        if (!cachedCode) {
          // Fallback to default red if no code available
          return [{ type: "default", r: 220, g: 60, b: 60, weight: 1.0 }];
        }
        
        // Use the centralized syntax highlighting from kidlisp module
        const colors = getSyntaxHighlightingColors(cachedCode);
        
        // Debug logging to show we're using kidlisp syntax highlighting
        console.log("🎨 Using kidlisp syntax highlighting:", {
          sourceLength: cachedCode.length,
          tokenCount: colors.length,
          colors: colors.slice(0, 5) // Show first 5 colors
        });
        
        return colors;
      }
      
      // Helper function to get syntax color at a specific position in the progress bar
      function getSyntaxColorAtPosition(syntaxColors, position) {
        if (syntaxColors.length === 0) {
          return { r: 220, g: 60, b: 60 }; // Red fallback
        }
        
        if (syntaxColors.length === 1) {
          return syntaxColors[0];
        }
        
        // Calculate which color segment we're in based on cumulative weights
        let totalWeight = syntaxColors.reduce((sum, color) => sum + color.weight, 0);
        let currentWeight = 0;
        
        for (let i = 0; i < syntaxColors.length; i++) {
          const color = syntaxColors[i];
          const segmentStart = currentWeight / totalWeight;
          const segmentEnd = (currentWeight + color.weight) / totalWeight;
          
          if (position >= segmentStart && position <= segmentEnd) {
            // Optionally blend with adjacent colors for smoother transitions
            if (i < syntaxColors.length - 1 && position > segmentEnd - 0.1) {
              const nextColor = syntaxColors[i + 1];
              const blendFactor = (position - (segmentEnd - 0.1)) / 0.1;
              return {
                r: Math.floor(color.r * (1 - blendFactor) + nextColor.r * blendFactor),
                g: Math.floor(color.g * (1 - blendFactor) + nextColor.g * blendFactor),
                b: Math.floor(color.b * (1 - blendFactor) + nextColor.b * blendFactor)
              };
            }
            return color;
          }
          
          currentWeight += color.weight;
        }
        
        // Fallback to last color if position is beyond the end
        return syntaxColors[syntaxColors.length - 1];
      }

      // Film camera style timestamp at bottom-left corner
      function addFilmTimestamp(ctx, canvasWidth, canvasHeight, typeSize, progress = 0, frameData = null) {
        // Define progress bar height consistently across all contexts
        const progressBarHeight = 1;
        
        // Define timestamp margin for consistent spacing
        const timestampMargin = Math.max(8, Math.floor(canvasHeight / 50)); // Responsive margin
        
        // Calculate timestamp based on recording progress for real-time GIF playback
        let timestampDate;
        if (mediaRecorderStartTime !== undefined) {
          // During recording: show time progressing from when recording started
          const elapsedMs = performance.now() - mediaRecorderStartTime;
          // Use a fixed start time and add elapsed recording time for consistent progression
          if (!window.recordingStartTimestamp) {
            window.recordingStartTimestamp = Date.now() - elapsedMs;
          }
          timestampDate = new Date(window.recordingStartTimestamp + elapsedMs);
        } else {
          // Fallback: use current time (for non-recording contexts)
          timestampDate = new Date();
        }
        
        const year = timestampDate.getUTCFullYear();
        const month = timestampDate.getUTCMonth() + 1; // getUTCMonth() returns 0-11
        const day = timestampDate.getUTCDate();
        const hour = timestampDate.getUTCHours();
        const minute = timestampDate.getUTCMinutes();
        const second = timestampDate.getUTCSeconds();
        const millisecond = timestampDate.getUTCMilliseconds();

        // Include milliseconds for frame-by-frame uniqueness
        const timestamp = `${year}.${month}.${day}.${hour}.${minute}.${second}.${millisecond.toString().padStart(3, "0")}`;

        ctx.save();
        
        // Remove shake/jitter effect - keep timestamp steady
        const shakeX = 0; // No more shaking
        const shakeY = 0; // No more shaking
        
        // Position timestamp above the progress bar with proper margin, more flush left and larger
        const timestampY = canvasHeight - progressBarHeight - timestampMargin; // Above progress bar
        const timestampX = timestampMargin * 0.5; // More flush left (was timestampMargin * 2)
        
        // Make timestamp much larger and more watermark-like, similar to aesthetic.computer stamps
        const watermarkSize = Math.min(
          32,
          Math.max(18, Math.floor(canvasHeight / 25)), // Much larger base size
        );
        
        // Draw multiple layered versions for that vibey aesthetic.computer stamp effect
        ["red", "lime", "blue", "white"].forEach((color, index) => {
          // Calculate fade alpha for bouncy looping effect
          // Fade out in last 10% and fade in during first 10%
          let timestampAlpha = 1.0;
          if (progress <= 0.1) {
            // Fade in during first 10%
            timestampAlpha = progress / 0.1;
          } else if (progress >= 0.9) {
            // Fade out during last 10%
            timestampAlpha = (1.0 - progress) / 0.1;
          }

          let offsetX, offsetY;
          if (color !== "white") {
            ctx.globalAlpha = 0.45 * timestampAlpha; // Semi-transparent colored layers with fade
            offsetX = choose([-3, -5, 0, 3, 5]); // Larger offsets for bigger text
            offsetY = choose([-3, -5, 0, 3, 5]);
          } else {
            ctx.globalAlpha = choose([0.6, 0.5, 0.7]) * timestampAlpha; // Main white layer with fade
            offsetX = choose([-1, 0, 1]);
            offsetY = choose([-1, 0, 1]);
            color = choose(["white", "white", "white", "magenta", "yellow"]); // Occasional color pops
          }

          ctx.fillStyle = color;
          ctx.font = `bold ${watermarkSize}px YWFTProcessing-Regular`;
          
          ctx.fillText(
            timestamp,
            timestampX + offsetX + shakeX,
            timestampY + offsetY + shakeY,
          );
        });

        ctx.restore();
      }

      // Helper function to sample representative colors from frame data
      function sampleFrameColors(frameData, canvasWidth, canvasHeight, numColors = 5) {
        if (!frameData || frameData.length < 4) {
          // Fallback to original colors if no frame data
          return ["#FF4444", "#FF6666", "#FFAA44", "#FF8888", "#FFCCCC"];
        }

        const sampledColors = [];
        const totalPixels = canvasWidth * canvasHeight;
        
        // Sample colors from different regions of the frame
        let samplePositions;
        if (numColors <= 5) {
          samplePositions = [0.1, 0.3, 0.5, 0.7, 0.9]; // Original 5 positions
        } else {
          // Generate more evenly distributed positions for higher numColors
          samplePositions = [];
          for (let i = 0; i < numColors; i++) {
            samplePositions.push((i + 1) / (numColors + 1)); // Evenly distributed from ~0.1 to ~0.9
          }
        }

        for (let i = 0; i < numColors; i++) {
          const samplePos = Math.floor(samplePositions[i % samplePositions.length] * totalPixels);
          const pixelIndex = Math.min(samplePos * 4, frameData.length - 4);
          
          const r = frameData[pixelIndex];
          const g = frameData[pixelIndex + 1];
          const b = frameData[pixelIndex + 2];
          
          // Enhance the colors to make them more vibrant for the progress bar
          const enhancedR = Math.min(255, Math.floor(r * 1.2));
          const enhancedG = Math.min(255, Math.floor(g * 1.2));
          const enhancedB = Math.min(255, Math.floor(b * 1.2));
          
          const hexColor = `#${enhancedR.toString(16).padStart(2, '0')}${enhancedG.toString(16).padStart(2, '0')}${enhancedB.toString(16).padStart(2, '0')}`;
          sampledColors.push(hexColor);
        }

        // If all sampled colors are too dark, add some brightness
        const avgBrightness = sampledColors.reduce((sum, color) => {
          const r = parseInt(color.slice(1, 3), 16);
          const g = parseInt(color.slice(3, 5), 16);
          const b = parseInt(color.slice(5, 7), 16);
          return sum + (r + g + b) / 3;
        }, 0) / sampledColors.length;

        if (avgBrightness < 60) {
          // Mix in some brighter colors if the frame is too dark
          return sampledColors.map((color, i) => {
            if (i % 2 === 0) {
              return choose(["#FF6666", "#FFAA44", "#FF8888", "#FFCCCC", "#66FF66", "#6666FF"]);
            }
            return color;
          });
        }

        return sampledColors;
      }

      // Helper function for choosing random values (simplified version)
      function choose(options) {
        return options[Math.floor(Math.random() * options.length)];
      }
    }

    // Helper function to upscale pixels with nearest neighbor
    function upscalePixels(imageData, originalWidth, originalHeight, scale) {
      if (scale === 1) {
        return imageData;
      }

      const scaledWidth = originalWidth * scale;
      const scaledHeight = originalHeight * scale;
      const scaledImageData = new Uint8ClampedArray(
        scaledWidth * scaledHeight * 4,
      );

      for (let y = 0; y < scaledHeight; y++) {
        for (let x = 0; x < scaledWidth; x++) {
          const sourceX = Math.floor(x / scale);
          const sourceY = Math.floor(y / scale);
          const sourceIndex = (sourceY * originalWidth + sourceX) * 4;
          const targetIndex = (y * scaledWidth + x) * 4;

          scaledImageData[targetIndex] = imageData[sourceIndex]; // R
          scaledImageData[targetIndex + 1] = imageData[sourceIndex + 1]; // G
          scaledImageData[targetIndex + 2] = imageData[sourceIndex + 2]; // B
          scaledImageData[targetIndex + 3] = imageData[sourceIndex + 3]; // A
        }
      }

      return scaledImageData;
    }

    // Helper function to add stamp to pixel data by rendering to a canvas first
    async function addStampToPixelData(pixelData, width, height, scale = 1, progress = 0) {
      // Create a temporary canvas to render the stamp
      const tempCanvas = document.createElement("canvas");
      const tempCtx = tempCanvas.getContext("2d");

      tempCanvas.width = width * scale;
      tempCanvas.height = height * scale;

      // Put the original image data
      const scaledData = upscalePixels(pixelData, width, height, scale);
      const imageData = new ImageData(
        scaledData,
        width * scale,
        height * scale,
      );
      tempCtx.putImageData(imageData, 0, 0);

      // Add the stamp (await to ensure fonts are loaded)
      await addAestheticComputerStamp(tempCtx, width * scale, height * scale, progress, pixelData);

      // Get the stamped image data back
      const stampedImageData = tempCtx.getImageData(
        0,
        0,
        width * scale,
        height * scale,
      );
      return stampedImageData.data;
    }

    // 🎬 Create animated WebP from frame data
    if (type === "create-animated-webp") {
      console.log(
        "📼 Creating animated WebP from",
        content.frames.length,
        "frames",
      );

      // Send initial progress to show the progress bar immediately
      send({
        type: "recorder:transcode-progress",
        content: 0.01, // 1% to start
      });

      try {
        // Create a canvas for frame processing
        const canvas = document.createElement("canvas");
        const ctx = canvas.getContext("2d");

        if (content.frames.length === 0) {
          console.warn("No frames provided for WebP creation");
          return;
        }

        // Set canvas size based on first frame
        const firstFrame = content.frames[0];
        canvas.width = firstFrame.width;
        canvas.height = firstFrame.height;
        console.log(`🖼️ Canvas size: ${canvas.width}x${canvas.height}`);

        // For now, create a zip of WebP frames with timing info
        // TODO: Implement proper animated WebP encoding when browser support improves
        if (!window.JSZip) await loadJSZip();
        const zip = new window.JSZip();

        // Add timing info
        const timingInfo = content.frames.map((frame, i) => ({
          frame: i,
          duration: frame.duration,
          timestamp: frame.timestamp,
        }));

        zip.file("timing.json", JSON.stringify(timingInfo, null, 2));
        console.log(
          "📋 Added timing.json with",
          timingInfo.length,
          "frame entries",
        );

        // Convert each frame to WebP and add to zip
        for (let i = 0; i < content.frames.length; i++) {
          const frame = content.frames[i];

          if (i % 50 === 0 || i === content.frames.length - 1) {
            console.log(
              `🎞️ Processing frame ${i + 1}/${content.frames.length} (${Math.round(((i + 1) / content.frames.length) * 100)}%)`,
            );
          }

          // Create ImageData from frame data
          const imageData = new ImageData(
            new Uint8ClampedArray(frame.data),
            frame.width,
            frame.height,
          );

          // Draw to canvas and convert to WebP
          ctx.putImageData(imageData, 0, 0);

          // Add sideways AC stamp like in video recordings (await to ensure fonts are loaded)
          const currentProgress = (i + 1) / content.frames.length;
          await addAestheticComputerStamp(ctx, frame.width, frame.height, currentProgress, frame.data);

          // Convert to WebP blob
          const webpBlob = await new Promise((resolve) => {
            canvas.toBlob(resolve, "image/webp", 0.8);
          });

          // Add to zip
          zip.file(`frame_${i.toString().padStart(4, "0")}.webp`, webpBlob);
        }

        console.log("📦 Generating ZIP file...");

        // Generate and download the zip
        const zipBlob = await zip.generateAsync({ type: "blob" });
        const filename = generateTapeFilename("zip", "-webp");

        console.log(
          `💾 ZIP generated: ${filename} (${Math.round((zipBlob.size / 1024 / 1024) * 100) / 100} MB)`,
        );

        // Use the existing download function
        receivedDownload({ filename, data: zipBlob });

        console.log("🎬 WebP frames exported successfully as ZIP");
      } catch (error) {
        console.error("Error creating animated WebP:", error);
      }
      return;
    }

    // 🎬 Create single animated WebP file
    if (type === "create-single-animated-webp") {
      console.log(
        "🎞️ Creating animated image from",
        content.frames.length,
        "frames",
      );

      try {
        if (content.frames.length === 0) {
          console.warn("No frames provided for animated image creation");
          return;
        }

        // Try WebP animation first using webpxmux.js
        console.log("🔄 Creating animated WebP using webpxmux.js library");

        // Load webpxmux.js if not already loaded
        if (!window.WebPXMux) {
          console.log("📦 Loading webpxmux.js library...");
          await new Promise((resolve, reject) => {
            const script = document.createElement("script");
            script.src =
              "https://cdn.jsdelivr.net/npm/webpxmux@1.0.2/dist/webpxmux.min.js";
            script.onload = resolve;
            script.onerror = reject;
            document.head.appendChild(script);
          });
        }

        console.log(
          `🖼️ Canvas size: ${content.frames[0].width}x${content.frames[0].height}`,
        );

        // Initialize WebPXMux
        const xMux = window.WebPXMux(
          "https://cdn.jsdelivr.net/npm/webpxmux@1.0.2/dist/webpxmux.wasm",
        );
        await xMux.waitRuntime();

        // Convert our frame data to WebPXMux format
        const frames = [];

        // Create a canvas for rendering stamps
        const tempCanvas = document.createElement("canvas");
        const tempCtx = tempCanvas.getContext("2d");
        tempCanvas.width = content.frames[0].width;
        tempCanvas.height = content.frames[0].height;

        for (let i = 0; i < content.frames.length; i++) {
          if (i % 50 === 0 || i === content.frames.length - 1) {
            console.log(
              `🎞️ Processing frame ${i + 1}/${content.frames.length} (${Math.round(((i + 1) / content.frames.length) * 100)}%)`,
            );
          }

          const frame = content.frames[i];

          // Create ImageData from frame data and draw to canvas
          const imageData = new ImageData(
            new Uint8ClampedArray(frame.data),
            frame.width,
            frame.height,
          );
          tempCtx.putImageData(imageData, 0, 0);

          // Add sideways AC stamp like in other video recordings
          const currentProgress = (i + 1) / content.frames.length;
          await addAestheticComputerStamp(tempCtx, frame.width, frame.height, currentProgress, frame.data);

          // Get the stamped image data back
          const stampedImageData = tempCtx.getImageData(
            0,
            0,
            frame.width,
            frame.height,
          );
          const stampedData = stampedImageData.data;

          // Convert frame data to RGBA Uint32Array format expected by webpxmux
          const rgba = new Uint32Array(frame.width * frame.height);

          for (let j = 0; j < rgba.length; j++) {
            const pixelIndex = j * 4;
            const r = stampedData[pixelIndex];
            const g = stampedData[pixelIndex + 1];
            const b = stampedData[pixelIndex + 2];
            const a = stampedData[pixelIndex + 3];

            // Pack RGBA into 32-bit integer (0xRRGGBBAA format)
            rgba[j] = (r << 24) | (g << 16) | (b << 8) | a;
          }

          frames.push({
            duration: Math.max(frame.duration || 100, 10),
            isKeyframe: i === 0, // First frame is keyframe
            rgba: rgba,
          });
        }

        console.log("🔄 Encoding animated WebP...");

        const webpFrames = {
          frameCount: frames.length,
          width: content.frames[0].width,
          height: content.frames[0].height,
          loopCount: 0, // Infinite loop
          bgColor: 0xffffffff, // White background
          frames: frames,
        };

        const webpData = await xMux.encodeFrames(webpFrames);

        const webpBlob = new Blob([webpData], { type: "image/webp" });
        const filename = generateTapeFilename("webp", "-animated");

        console.log(
          `💾 Animated WebP generated: ${filename} (${Math.round((webpBlob.size / 1024 / 1024) * 100) / 100} MB)`,
        );

        // Use the existing download function
        receivedDownload({ filename, data: webpBlob });

        console.log("🎬 Animated WebP exported successfully!");
      } catch (error) {
        console.error("Error creating animated WebP:", error);
        console.log("🔄 Falling back to animated PNG (APNG)");

        try {
          // Fallback to APNG using UPNG.js
          console.log("🔄 Creating animated PNG (APNG) - fallback option");

          // Load pako (required for UPNG.js compression) and UPNG.js library
          if (!window.pako) {
            console.log("📦 Loading pako compression library...");
            await new Promise((resolve, reject) => {
              const script = document.createElement("script");
              script.src =
                "https://cdn.jsdelivr.net/npm/pako@2.1.0/dist/pako.min.js";
              script.onload = resolve;
              script.onerror = reject;
              document.head.appendChild(script);
            });
          }

          if (!window.UPNG) {
            console.log("📦 Loading UPNG.js library...");
            await new Promise((resolve, reject) => {
              const script = document.createElement("script");
              script.src = "https://cdn.jsdelivr.net/npm/upng-js@2.1.0/UPNG.js";
              script.onload = resolve;
              script.onerror = reject;
              document.head.appendChild(script);
            });
          }

          console.log(
            `🖼️ Canvas size: ${content.frames[0].width}x${content.frames[0].height}`,
          );

          // Convert frames to the format UPNG expects
          const frameBuffers = [];
          const delays = [];

          for (let i = 0; i < content.frames.length; i++) {
            const frame = content.frames[i];

            if (i % 50 === 0 || i === content.frames.length - 1) {
              console.log(
                `🎞️ Processing frame ${i + 1}/${content.frames.length} (${Math.round(((i + 1) / content.frames.length) * 100)}%)`,
              );
            }

            // UPNG expects RGBA data as ArrayBuffer
            const frameData = new Uint8Array(frame.data);
            frameBuffers.push(frameData.buffer);

            // Convert to milliseconds for APNG delays
            delays.push(Math.max(frame.duration || 100, 10));
          }

          console.log("🔄 Encoding animated PNG...");

          // Create animated PNG using UPNG
          const apngBuffer = window.UPNG.encode(
            frameBuffers,
            content.frames[0].width,
            content.frames[0].height,
            0, // 0 = lossless, or use 256 for lossy
            delays,
          );

          const apngBlob = new Blob([apngBuffer], { type: "image/png" });
          const filename = generateTapeFilename("png", "-animated");

          console.log(
            `💾 Animated PNG generated: ${filename} (${Math.round((apngBlob.size / 1024 / 1024) * 100) / 100} MB)`,
          );

          // Use the existing download function
          receivedDownload({ filename, data: apngBlob });

          console.log("🎬 Animated PNG (APNG) exported successfully!");
        } catch (apngError) {
          console.error("Error creating animated PNG:", apngError);

          // Final fallback: create a static WebP of the first frame
          console.log("🔄 Falling back to static WebP of first frame");
          try {
            const canvas = document.createElement("canvas");
            const ctx = canvas.getContext("2d");
            const firstFrame = content.frames[0];

            canvas.width = firstFrame.width;
            canvas.height = firstFrame.height;

            const imageData = new ImageData(
              new Uint8ClampedArray(firstFrame.data),
              firstFrame.width,
              firstFrame.height,
            );

            ctx.putImageData(imageData, 0, 0);

            const webpBlob = await new Promise((resolve) => {
              canvas.toBlob(resolve, "image/webp", 0.9);
            });

            const filename = generateTapeFilename("webp", "-static");
            receivedDownload({ filename, data: webpBlob });

            console.log("📸 Static WebP fallback exported successfully");
          } catch (fallbackError) {
            console.error("Error in fallback WebP creation:", fallbackError);
          }
        }
      }
      return;
    }

    // 🎬 Create animated WebP only (no fallback to APNG)
    if (type === "create-animated-webp-only") {
      console.log(
        "🎞️ Creating animated WebP from",
        content.frames.length,
        "frames",
      );

      // Helper function to upscale pixels with nearest neighbor - with memory safety
      function upscalePixels(rgba, originalWidth, originalHeight, scale) {
        if (scale === 1) {
          return { rgba: rgba, actualScale: 1 };
        }

        const scaledWidth = originalWidth * scale;
        const scaledHeight = originalHeight * scale;
        const totalPixels = scaledWidth * scaledHeight;

        // Safety check to prevent array allocation errors
        const maxSafeArraySize = 100 * 1024 * 1024; // 100M elements max
        if (totalPixels > maxSafeArraySize) {
          console.warn(
            `⚠️ Scaled array too large (${totalPixels} pixels), falling back to 1x scaling`,
          );
          return { rgba: rgba, actualScale: 1 };
        }

        let scaledRgba;
        try {
          scaledRgba = new Uint32Array(totalPixels);
        } catch (error) {
          console.warn(
            `⚠️ Failed to allocate scaled array: ${error.message}, falling back to 1x scaling`,
          );
          return { rgba: rgba, actualScale: 1 };
        }

        for (let y = 0; y < scaledHeight; y++) {
          for (let x = 0; x < scaledWidth; x++) {
            const sourceX = Math.floor(x / scale);
            const sourceY = Math.floor(y / scale);
            const sourceIndex = sourceY * originalWidth + sourceX;
            const targetIndex = y * scaledWidth + x;
            scaledRgba[targetIndex] = rgba[sourceIndex];
          }
        }

        return { rgba: scaledRgba, actualScale: scale };
      }

      try {
        if (content.frames.length === 0) {
          console.warn("No frames provided for animated WebP creation");
          return;
        }

        console.log("🔄 Creating animated WebP using webpxmux.js library");

        // Load webpxmux.js using local dep files
        if (!window.WebPXMux) {
          console.log("📦 Loading webpxmux.js library...");
          console.log(
            "📍 Script URL: /aesthetic.computer/dep/webpxmux/webpxmux.min.js",
          );

          await new Promise((resolve, reject) => {
            const script = document.createElement("script");
            script.src = "/aesthetic.computer/dep/webpxmux/webpxmux.min.js";
            script.type = "text/javascript";

            script.onload = () => {
              console.log("✅ webpxmux.js script loaded successfully");
              console.log(
                "🔍 Checking window.WebPXMux:",
                typeof window.WebPXMux,
              );
              if (window.WebPXMux) {
                console.log("✅ WebPXMux constructor found on window");
              } else {
                console.error(
                  "❌ WebPXMux not found on window after script load",
                );
              }
              resolve();
            };

            script.onerror = (error) => {
              console.error("❌ Failed to load webpxmux.js script");
              console.error("📍 Error event:", error);
              console.error("📍 Script src:", script.src);
              console.error("📍 Script type:", script.type);
              reject(error);
            };

            console.log("📎 Appending script to document head");
            document.head.appendChild(script);
          });
        } else {
          console.log("✅ webpxmux.js already loaded");
        }

        console.log(
          `🖼️ Canvas size: ${content.frames[0].width}x${content.frames[0].height} -> checking memory requirements...`,
        );

        // Pre-calculate optimal scale to avoid memory issues
        const originalWidth = content.frames[0].width;
        const originalHeight = content.frames[0].height;
        const frameCount = content.frames.length;

        // Ultra-conservative memory management for WebP animations
        // Each frame needs significant memory overhead beyond just pixel data
        const baseMemoryLimit = 5 * 1024 * 1024; // 5M pixels base limit (much smaller)

        // Much more aggressive scaling penalties for large frame counts
        let framePenalty = 1;
        if (frameCount > 500)
          framePenalty = 16; // 500+ frames: divide by 16
        else if (frameCount > 300)
          framePenalty = 12; // 300+ frames: divide by 12
        else if (frameCount > 200)
          framePenalty = 8; // 200+ frames: divide by 8
        else if (frameCount > 100)
          framePenalty = 6; // 100+ frames: divide by 6
        else if (frameCount > 50)
          framePenalty = 4; // 50+ frames: divide by 4
        else if (frameCount > 25) framePenalty = 2; // 25+ frames: divide by 2

        const adjustedMemoryLimit = Math.max(
          baseMemoryLimit / framePenalty,
          256 * 1024, // Minimum 256K pixels (very small for safety)
        );

        // Start with smaller default scaling
        let optimalScale = frameCount > 400 ? 1 : 2; // Default to 2x for more animations, only 1x for very large ones
        let scaledPixels =
          originalWidth * originalHeight * optimalScale * optimalScale;

        console.log(
          `🧮 Conservative memory calculation: ${frameCount} frames, penalty: ${framePenalty}x, ${scaledPixels} pixels per frame (${optimalScale}x), limit: ${Math.round(adjustedMemoryLimit / 1024)}K pixels`,
        );

        // More aggressive downscaling if needed
        if (scaledPixels > adjustedMemoryLimit) {
          optimalScale = 1;
          scaledPixels = originalWidth * originalHeight;
          console.warn(
            `⚠️ Forcing 1x scaling for ${frameCount} frames to prevent memory errors`,
          );

          // If even 1x is too large, we have a problem
          if (scaledPixels > adjustedMemoryLimit) {
            console.error(
              `❌ Video too large even at 1x scaling: ${Math.round(scaledPixels / 1024)}K pixels > ${Math.round(adjustedMemoryLimit / 1024)}K limit`,
            );
            throw new Error(
              `Video dimensions too large for WebP encoding: ${originalWidth}x${originalHeight} with ${frameCount} frames`,
            );
          }
        }

        console.log(
          `📏 Using ${optimalScale}x scaling: ${originalWidth}x${originalHeight} -> ${originalWidth * optimalScale}x${originalHeight * optimalScale}`,
        );

        // Initialize WebPXMux
        console.log("🔧 Initializing WebPXMux...");
        console.log(
          "📍 WASM URL: /aesthetic.computer/dep/webpxmux/webpxmux.wasm",
        );

        const xMux = window.WebPXMux(
          "/aesthetic.computer/dep/webpxmux/webpxmux.wasm",
        );
        console.log("✅ WebPXMux instance created:", xMux);

        console.log("⏳ Waiting for WebAssembly runtime...");
        await xMux.waitRuntime();
        console.log("✅ WebAssembly runtime ready!");

        // Verify xMux instance methods are available
        console.log("🔍 Verifying xMux methods:", {
          hasEncodeFrames: typeof xMux.encodeFrames === "function",
          hasWaitRuntime: typeof xMux.waitRuntime === "function",
          xMuxKeys: Object.keys(xMux),
        });

        // Convert our frame data to WebPXMux format with smart upscaling
        const frames = [];

        for (let i = 0; i < content.frames.length; i++) {
          if (i % 50 === 0 || i === content.frames.length - 1) {
            console.log(
              `🎞️ Processing frame ${i + 1}/${content.frames.length} (${Math.round(((i + 1) / content.frames.length) * 100)}%)`,
            );
          }

          const frame = content.frames[i];

          // Convert frame data to RGBA Uint32Array format expected by webpxmux
          let rgba = new Uint32Array(frame.width * frame.height);
          const data = frame.data;

          for (let j = 0; j < rgba.length; j++) {
            const pixelIndex = j * 4;
            const r = data[pixelIndex];
            const g = data[pixelIndex + 1];
            const b = data[pixelIndex + 2];
            const a = data[pixelIndex + 3];

            // Pack RGBA into 32-bit integer (0xRRGGBBAA format)
            rgba[j] = (r << 24) | (g << 16) | (b << 8) | a;
          }

          // Add AC stamp to the frame data by rendering through canvas
          const currentProgress = (i + 1) / content.frames.length;
          const stampedPixelData = await addStampToPixelData(
            new Uint8ClampedArray(data),
            frame.width,
            frame.height,
            optimalScale,
            currentProgress,
          );

          // Convert stamped pixel data back to RGBA Uint32Array
          rgba = new Uint32Array(
            frame.width * frame.height * optimalScale * optimalScale,
          );
          for (let j = 0; j < rgba.length; j++) {
            const pixelIndex = j * 4;
            const r = stampedPixelData[pixelIndex];
            const g = stampedPixelData[pixelIndex + 1];
            const b = stampedPixelData[pixelIndex + 2];
            const a = stampedPixelData[pixelIndex + 3];

            // Pack RGBA into 32-bit integer (0xRRGGBBAA format)
            rgba[j] = (r << 24) | (g << 16) | (b << 8) | a;
          }

          // Skip the original upscaling since we already did it with the stamp
          const upscaleResult = { rgba: rgba, actualScale: optimalScale };

          // Update actual scale if upscaling failed
          if (upscaleResult.actualScale !== optimalScale && i === 0) {
            console.warn(
              `⚠️ Upscaling failed, using ${upscaleResult.actualScale}x instead of ${optimalScale}x`,
            );
            optimalScale = upscaleResult.actualScale; // Update for remaining frames
          }

          frames.push({
            duration: Math.max(frame.duration || 16.67, 10), // Default 60fps (~16.67ms)
            isKeyframe: i === 0, // First frame is keyframe
            rgba: upscaleResult.rgba,
          });

          // Aggressive memory cleanup
          rgba.fill(0);
          rgba = null; // Help GC
        }

        console.log("🔄 Encoding animated WebP...");

        // Use the actual scale that was achieved (may be different if upscaling failed)
        const actualWidth =
          frames.length > 0
            ? Math.sqrt(
                frames[0].rgba.length / (originalWidth * originalHeight),
              ) * originalWidth
            : originalWidth;
        const actualHeight =
          frames.length > 0
            ? Math.sqrt(
                frames[0].rgba.length / (originalWidth * originalHeight),
              ) * originalHeight
            : originalHeight;

        const webpFrames = {
          frameCount: frames.length,
          width: Math.round(actualWidth),
          height: Math.round(actualHeight),
          loopCount: 0, // Infinite loop
          bgColor: 0xffffffff, // White background
          frames: frames,
        };

        console.log("� WebP frames data structure:", {
          frameCount: webpFrames.frameCount,
          width: webpFrames.width,
          height: webpFrames.height,
          loopCount: webpFrames.loopCount,
          bgColor: webpFrames.bgColor.toString(16),
          firstFrameDuration: webpFrames.frames[0]?.duration,
          firstFrameRgbaLength: webpFrames.frames[0]?.rgba?.length,
          totalFrames: webpFrames.frames.length,
        });

        console.log("�🔧 Calling xMux.encodeFrames...");
        const webpData = await xMux.encodeFrames(webpFrames);
        console.log("✅ WebP encoding complete! Data length:", webpData.length);

        const webpBlob = new Blob([webpData], { type: "image/webp" });
        const filename = generateTapeFilename("webp", "-animated");

        console.log(
          `💾 Animated WebP generated: ${filename} (${Math.round((webpBlob.size / 1024 / 1024) * 100) / 100} MB)`,
        );

        // Use the existing download function
        receivedDownload({ filename, data: webpBlob });

        console.log("🎬 Animated WebP exported successfully!");
      } catch (error) {
        console.error("Error creating animated WebP:", error);
        console.log("🔄 Falling back to static WebP of first frame");

        try {
          const canvas = document.createElement("canvas");
          const ctx = canvas.getContext("2d");
          const firstFrame = content.frames[0];

          canvas.width = firstFrame.width;
          canvas.height = firstFrame.height;

          const imageData = new ImageData(
            new Uint8ClampedArray(firstFrame.data),
            firstFrame.width,
            firstFrame.height,
          );

          ctx.putImageData(imageData, 0, 0);

          // Add sideways AC stamp to fallback WebP as well (await to ensure fonts are loaded)
          await addAestheticComputerStamp(
            ctx,
            firstFrame.width,
            firstFrame.height,
          );

          const webpBlob = await new Promise((resolve) => {
            canvas.toBlob(resolve, "image/webp", 0.9);
          });

          const filename = generateTapeFilename("webp", "-static");
          receivedDownload({ filename, data: webpBlob });

          console.log("📸 Static WebP fallback exported successfully");
        } catch (fallbackError) {
          console.error("Error in fallback WebP creation:", fallbackError);
        }
      }
      return;
    }

    // 🎬 Create APNG only
    if (type === "create-single-animated-apng") {
      console.log("🎞️ Creating APNG from", content.frames.length, "frames");

      // Helper function to upscale pixels with nearest neighbor (same as WebP)
      function upscalePixels(imageData, originalWidth, originalHeight, scale) {
        if (scale === 1) {
          return imageData;
        }

        const scaledWidth = originalWidth * scale;
        const scaledHeight = originalHeight * scale;
        const scaledImageData = new Uint8ClampedArray(
          scaledWidth * scaledHeight * 4,
        );

        for (let y = 0; y < scaledHeight; y++) {
          for (let x = 0; x < scaledWidth; x++) {
            const sourceX = Math.floor(x / scale);
            const sourceY = Math.floor(y / scale);
            const sourceIndex = (sourceY * originalWidth + sourceX) * 4;
            const targetIndex = (y * scaledWidth + x) * 4;

            scaledImageData[targetIndex] = imageData[sourceIndex]; // R
            scaledImageData[targetIndex + 1] = imageData[sourceIndex + 1]; // G
            scaledImageData[targetIndex + 2] = imageData[sourceIndex + 2]; // B
            scaledImageData[targetIndex + 3] = imageData[sourceIndex + 3]; // A
          }
        }

        return scaledImageData;
      }

      try {
        if (content.frames.length === 0) {
          console.warn("No frames provided for APNG creation");
          return;
        }

        console.log("🔄 Creating animated PNG (APNG)");

        // Pre-calculate optimal scale for APNG (same logic as WebP)
        const originalWidth = content.frames[0].width;
        const originalHeight = content.frames[0].height;
        const frameCount = content.frames.length;

        // Be more conservative with memory limits for large frame counts
        const baseMemoryLimit = 25 * 1024 * 1024; // 25M pixels base limit
        const adjustedMemoryLimit = Math.max(
          baseMemoryLimit / Math.max(1, frameCount / 500), // Reduce limit for many frames
          1024 * 1024, // Minimum 1M pixels
        );

        let optimalScale = 4;
        let scaledPixels =
          originalWidth * originalHeight * optimalScale * optimalScale;

        console.log(
          `🧮 APNG Memory calculation: ${frameCount} frames, ${scaledPixels} pixels per frame (4x), limit: ${Math.round(adjustedMemoryLimit / 1024)}K pixels`,
        );

        if (scaledPixels > adjustedMemoryLimit) {
          optimalScale = 2;
          scaledPixels =
            originalWidth * originalHeight * optimalScale * optimalScale;
          console.warn(
            `⚠️ 4x scaling would exceed memory limit for ${frameCount} frames, using 2x scaling`,
          );

          if (scaledPixels > adjustedMemoryLimit) {
            optimalScale = 1;
            console.warn(
              `⚠️ Even 2x scaling too large for ${frameCount} frames, using original size (1x)`,
            );
          }
        }

        console.log(
          `📏 APNG Using ${optimalScale}x scaling: ${originalWidth}x${originalHeight} -> ${originalWidth * optimalScale}x${originalHeight * optimalScale}`,
        );

        // Load pako (required for UPNG.js compression) and UPNG.js library
        if (!window.pako) {
          console.log("📦 Loading pako compression library...");
          await new Promise((resolve, reject) => {
            const script = document.createElement("script");
            script.src =
              "https://cdn.jsdelivr.net/npm/pako@2.1.0/dist/pako.min.js";
            script.onload = resolve;
            script.onerror = reject;
            document.head.appendChild(script);
          });
        }

        if (!window.UPNG) {
          console.log("📦 Loading UPNG.js library...");
          await new Promise((resolve, reject) => {
            const script = document.createElement("script");
            script.src = "https://cdn.jsdelivr.net/npm/upng-js@2.1.0/UPNG.js";
            script.onload = resolve;
            script.onerror = reject;
            document.head.appendChild(script);
          });
        }

        console.log(
          `🖼️ Canvas size: ${originalWidth}x${originalHeight} -> ${originalWidth * optimalScale}x${originalHeight * optimalScale}`,
        );

        // Convert frames to the format UPNG expects with upscaling
        const frameBuffers = [];
        const delays = [];

        for (let i = 0; i < content.frames.length; i++) {
          const frame = content.frames[i];

          if (i % 50 === 0 || i === content.frames.length - 1) {
            console.log(
              `🎞️ Processing APNG frame ${i + 1}/${content.frames.length} (${Math.round(((i + 1) / content.frames.length) * 100)}%)`,
            );
          }

          // Apply upscaling and add AC stamp to frame data
          const originalData = new Uint8ClampedArray(frame.data);
          const currentProgress = (i + 1) / content.frames.length;
          const scaledData = await addStampToPixelData(
            originalData,
            frame.width,
            frame.height,
            optimalScale,
            currentProgress,
          );

          // UPNG expects RGBA data as ArrayBuffer
          const frameData = new Uint8Array(scaledData);
          frameBuffers.push(frameData.buffer);

          // Convert to milliseconds for APNG delays with 60fps default
          delays.push(Math.max(frame.duration || 16.67, 10)); // Default 60fps (~16.67ms)
        }

        console.log("🔄 Encoding animated PNG...");

        // Create animated PNG using UPNG with scaled dimensions
        const apngBuffer = window.UPNG.encode(
          frameBuffers,
          originalWidth * optimalScale, // Use scaled width
          originalHeight * optimalScale, // Use scaled height
          0, // 0 = lossless, or use 256 for lossy
          delays,
        );

        const apngBlob = new Blob([apngBuffer], { type: "image/png" });
        const filename = generateTapeFilename("png", "-animated");

        console.log(
          `💾 Animated PNG generated: ${filename} (${Math.round((apngBlob.size / 1024 / 1024) * 100) / 100} MB)`,
        );

        // Use the existing download function
        receivedDownload({ filename, data: apngBlob });

        console.log("🎬 Animated PNG (APNG) exported successfully!");
      } catch (error) {
        console.error("Error creating APNG:", error);
        console.log("🔄 Falling back to static PNG of first frame");

        try {
          const canvas = document.createElement("canvas");
          const ctx = canvas.getContext("2d");
          const firstFrame = content.frames[0];

          canvas.width = firstFrame.width;
          canvas.height = firstFrame.height;

          const imageData = new ImageData(
            new Uint8ClampedArray(firstFrame.data),
            firstFrame.width,
            firstFrame.height,
          );

          ctx.putImageData(imageData, 0, 0);

          // Add sideways AC stamp to fallback PNG as well (await to ensure fonts are loaded)
          await addAestheticComputerStamp(
            ctx,
            firstFrame.width,
            firstFrame.height,
          );

          const pngBlob = await new Promise((resolve) => {
            canvas.toBlob(resolve, "image/png");
          });

          const filename = generateTapeFilename("png", "-static");
          receivedDownload({ filename, data: pngBlob });

          console.log("📸 Static PNG fallback exported successfully");
        } catch (fallbackError) {
          console.error("Error in fallback PNG creation:", fallbackError);
        }
      }
      return;
    }

    // 🎬 Create animated GIF
    if (type === "create-animated-gif") {
      console.log(
        "🎞️ Creating animated GIF from",
        content.frames.length,
        "frames",
      );

      // GIF encoder selection flag - set to true to use gifenc, false for gif.js
      const useGifenc = content.useGifenc !== false; // Default to gifenc (true), unless explicitly set to false

      try {
        // Load appropriate GIF library if not already loaded
        if (useGifenc) {
          if (!window.gifenc) {
            console.log("📦 Loading gifenc library...");
            const { GIFEncoder, quantize, applyPalette } = await import(
              "/aesthetic.computer/dep/gifenc/gifenc.esm.js"
            );
            window.gifenc = { GIFEncoder, quantize, applyPalette };
            console.log("✅ gifenc library loaded successfully");
          }
        } else {
          if (!window.GIF) {
            console.log("📦 Loading gif.js library...");
            await new Promise((resolve, reject) => {
              const script = document.createElement("script");
              script.src = "/aesthetic.computer/dep/gif/gif.js";
              script.onload = () => {
                console.log("✅ gif.js library loaded successfully");
                resolve();
              };
              script.onerror = () => {
                console.error("❌ Failed to load gif.js library");
                reject(new Error("Failed to load gif.js"));
              };
              document.head.appendChild(script);
            });
          }
        }

        // Force 3x scaling for consistent, fast GIF output
        const originalWidth = content.frames[0].width;
        const originalHeight = content.frames[0].height;
        const frameCount = content.frames.length;

        const optimalScale = 3; // Always use 3x scaling for GIFs

        console.log(
          `📏 Using fixed 3x scaling for GIF (${useGifenc ? 'gifenc' : 'gif.js'}): ${originalWidth}x${originalHeight} -> ${originalWidth * optimalScale}x${originalHeight * optimalScale}`,
        );

        // Helper function to upscale pixels with nearest neighbor
        function upscalePixels(
          imageData,
          originalWidth,
          originalHeight,
          scale,
        ) {
          if (scale === 1) {
            return imageData;
          }

          const scaledWidth = originalWidth * scale;
          const scaledHeight = originalHeight * scale;
          const scaledImageData = new Uint8ClampedArray(
            scaledWidth * scaledHeight * 4,
          );

          for (let y = 0; y < scaledHeight; y++) {
            for (let x = 0; x < scaledWidth; x++) {
              const sourceX = Math.floor(x / scale);
              const sourceY = Math.floor(y / scale);
              const sourceIndex = (sourceY * originalWidth + sourceX) * 4;
              const targetIndex = (y * scaledWidth + x) * 4;

              scaledImageData[targetIndex] = imageData[sourceIndex]; // R
              scaledImageData[targetIndex + 1] = imageData[sourceIndex + 1]; // G
              scaledImageData[targetIndex + 2] = imageData[sourceIndex + 2]; // B
              scaledImageData[targetIndex + 3] = imageData[sourceIndex + 3]; // A
            }
          }

          return scaledImageData;
        }

        if (useGifenc) {
          // Use gifenc encoder
          console.log("🎞️ Processing frames for GIF with gifenc...");
          
          // Send initial status
          send({
            type: "recorder:export-status",
            content: { 
              type: "gif", 
              phase: "analyzing", 
              message: "Analyzing frames" 
            }
          });
          
          const { GIFEncoder, quantize, applyPalette } = window.gifenc;
          
          // Calculate a better initial capacity based on frame count and size
          const estimatedFrameSize = content.frames[0].width * content.frames[0].height * optimalScale * optimalScale;
          const estimatedSize = content.frames.length * estimatedFrameSize * 0.5; // Rough estimate
          const initialCapacity = Math.max(4096, Math.min(estimatedSize, 1024 * 1024)); // Between 4KB and 1MB
          
          const gif = GIFEncoder({
            auto: true, // Auto mode for simpler usage
            initialCapacity: initialCapacity // Pre-allocate buffer for better performance
          });
          
          const finalFrames = [];
          
          // Send frame processing status
          send({
            type: "recorder:export-status",
            content: { 
              type: "gif", 
              phase: "processing", 
              message: "Processing frames" 
            }
          });
          
          // Process each frame
          for (let index = 0; index < content.frames.length; index++) {
            const frame = content.frames[index];
            const canvas = document.createElement("canvas");
            const ctx = canvas.getContext("2d");

            canvas.width = originalWidth * optimalScale;
            canvas.height = originalHeight * optimalScale;

            const originalImageData = new Uint8ClampedArray(frame.data);
            const scaledImageData = upscalePixels(
              originalImageData,
              originalWidth,
              originalHeight,
              optimalScale,
            );

            const imageData = new ImageData(
              scaledImageData,
              originalWidth * optimalScale,
              originalHeight * optimalScale,
            );

            ctx.putImageData(imageData, 0, 0);

            // Add sideways AC stamp like in video recordings
            const progress = (index + 1) / content.frames.length;
            await addAestheticComputerStamp(
              ctx,
              originalWidth * optimalScale,
              originalHeight * optimalScale,
              progress,
            );

            // Get RGBA data for gifenc
            const frameImageData = ctx.getImageData(0, 0, canvas.width, canvas.height);
            
            finalFrames.push({
              data: frameImageData.data,
              width: canvas.width,
              height: canvas.height
            });

            // Send detailed progress for frame processing (first 40% of total progress)
            const frameProgress = (index + 1) / content.frames.length;
            const totalProgress = frameProgress * 0.4; // Frames take up 40% of total progress
            
            if ((index + 1) % 10 === 0 || index === content.frames.length - 1) {
              console.log(
                `🎞️ Processed frame ${index + 1}/${content.frames.length} (${Math.round(frameProgress * 100)}%)`,
              );
              
              send({
                type: "recorder:export-progress",
                content: { 
                  progress: totalProgress, 
                  type: "gif",
                  message: `Processing frame ${index + 1}/${content.frames.length}`
                }
              });
            }
          }
          
          // Send color optimization status
          send({
            type: "recorder:export-status",
            content: { 
              type: "gif", 
              phase: "optimizing", 
              message: "Optimizing colors" 
            }
          });
          
          send({
            type: "recorder:export-progress",
            content: { 
              progress: 0.5, 
              type: "gif",
              message: "Analyzing color palette"
            }
          });
          
          console.log("🔄 Encoding GIF with gifenc (optimized for file size)...");
          
          // Use global palette for all frames for consistency and better compression
          const allPixels = new Uint8ClampedArray(finalFrames.length * finalFrames[0].width * finalFrames[0].height * 4);
          let offset = 0;
          
          for (const frame of finalFrames) {
            allPixels.set(frame.data, offset);
            offset += frame.data.length;
          }
          
          // Quantize to 256 colors with optimized settings for file size
          const palette = quantize(allPixels, 256, { 
            format: "rgb565", // Good balance of quality and speed
            clearAlpha: false, // Disable alpha processing since we don't need transparency
            oneBitAlpha: false // Disable alpha quantization for smaller files
          });
          
          // Send encoding status
          send({
            type: "recorder:export-status",
            content: { 
              type: "gif", 
              phase: "encoding", 
              message: "Encoding GIF" 
            }
          });
          
          // Encode frames with optimized settings
          for (let i = 0; i < finalFrames.length; i++) {
            const frame = finalFrames[i];
            const index = applyPalette(frame.data, palette, "rgb565"); // Use same format as quantization
            
            // Use 50fps timing (20ms delay) - safe minimum for all browsers including Chrome/Safari accessibility limits
            const delayInMilliseconds = Math.max(Math.round(1000 / 50), 20); // 1000ms / 50fps = 20ms minimum for browser compatibility
            
            gif.writeFrame(index, frame.width, frame.height, {
              palette: i === 0 ? palette : undefined, // Only include palette for first frame (global palette)
              delay: delayInMilliseconds, // Fixed 50fps timing for reliable cross-browser playback (gifenc uses milliseconds)
              repeat: i === 0 ? 0 : undefined, // Set infinite loop only on first frame
              transparent: false, // Disable transparency for smaller file size
              dispose: -1 // Use default dispose method for better compression
            });
            
            // Progress updates for encoding (40% to 90% of total progress)
            if ((i + 1) % 5 === 0 || i === finalFrames.length - 1) {
              const encodingProgress = (i + 1) / finalFrames.length;
              const totalProgress = 0.4 + (encodingProgress * 0.5); // 40-90% of total
              console.log(`🔄 GIF encoding progress: ${Math.round(encodingProgress * 100)}%`);
              
              send({
                type: "recorder:export-progress",
                content: { 
                  progress: totalProgress, 
                  type: "gif",
                  message: `Encoding frame ${i + 1}/${finalFrames.length}`
                }
              });
            }
          }
          
          // Send finalization status
          send({
            type: "recorder:export-status",
            content: { 
              type: "gif", 
              phase: "finalizing", 
              message: "Finalizing GIF" 
            }
          });
          
          send({
            type: "recorder:export-progress",
            content: { 
              progress: 0.95, 
              type: "gif",
              message: "Finalizing file"
            }
          });
          
          gif.finish();
          
          const gifBytes = gif.bytes();
          const blob = new Blob([gifBytes], { type: "image/gif" });
          
          console.log(
            `💾 GIF generated with gifenc: ${Math.round((blob.size / 1024 / 1024) * 100) / 100} MB`,
          );

          const filename = generateTapeFilename("gif");
          receivedDownload({ filename, data: blob });

          console.log("🎬 Animated GIF exported successfully with gifenc!");

          // Send completion message to video piece
          send({
            type: "recorder:export-complete",
            content: { type: "gif", filename }
          });
          
        } else {
          // Use gif.js encoder (existing implementation)
          // Add space for progress bar at bottom - but don't extend canvas, progress bar is built into stamp
          const progressBarHeight = 6;

          // Create GIF instance with optimized quality settings for better color fidelity
          const gif = new window.GIF({
            workers: 4, // More workers for faster processing
            quality: 5, // Higher quality for better colors (1-30, lower = better quality)
            dither: 'FloydSteinberg', // Better dithering for color accuracy
            transparent: null, // No transparency to reduce file size
            width: originalWidth * optimalScale,
            height: originalHeight * optimalScale, // Use original height, progress bar is part of stamp
            workerScript: "/aesthetic.computer/dep/gif/gif.worker.js",
          });

          console.log("🎞️ Processing frames for GIF...");

          // Calculate timing statistics from recorded frames
          if (content.frames.length > 1) {
            const timings = [];
            for (let i = 0; i < content.frames.length - 1; i++) {
              const currentTime = content.frames[i][0];
              const nextTime = content.frames[i + 1][0];
              timings.push(nextTime - currentTime);
            }
            const avgTiming = timings.reduce((a, b) => a + b, 0) / timings.length;
            const minTiming = Math.min(...timings);
            const maxTiming = Math.max(...timings);
            const totalDuration = content.frames[content.frames.length - 1][0] - content.frames[0][0];
            
            // Detect GC pauses - outlier frames that are 3x longer than average
            const gcPauseThreshold = avgTiming * 3;
            const gcPauses = timings.filter(t => t > gcPauseThreshold);
            
            console.log(`📊 Frame timing stats: avg=${avgTiming.toFixed(1)}ms, min=${minTiming.toFixed(1)}ms, max=${maxTiming.toFixed(1)}ms, total=${(totalDuration/1000).toFixed(2)}s`);
            if (gcPauses.length > 0) {
              console.log(`🗑️ Detected ${gcPauses.length} potential GC pauses (>${gcPauseThreshold.toFixed(1)}ms): ${gcPauses.map(p => p.toFixed(1)).join(', ')}ms`);
            }
          }

          // Process each frame with real-time timing
          for (let index = 0; index < content.frames.length; index++) {
            const frame = content.frames[index];
            const canvas = document.createElement("canvas");
            const ctx = canvas.getContext("2d");

            canvas.width = originalWidth * optimalScale;
            canvas.height = originalHeight * optimalScale; // Use original height, progress bar is part of stamp

            const originalImageData = new Uint8ClampedArray(frame.data);
            const scaledImageData = upscalePixels(
              originalImageData,
              originalWidth,
              originalHeight,
              optimalScale,
            );

            const imageData = new ImageData(
              scaledImageData,
              originalWidth * optimalScale,
              originalHeight * optimalScale,
            );

            ctx.putImageData(imageData, 0, 0);

            // Add sideways AC stamp like in video recordings (await to ensure fonts are loaded)
            const progress = (index + 1) / content.frames.length;
            await addAestheticComputerStamp(
              ctx,
              originalWidth * optimalScale,
              originalHeight * optimalScale, // Use original height, progress bar is part of stamp
              progress,
            );

            // Calculate real-time delay between this frame and the next with GC pause smoothing
            let frameDelay;
            if (index < content.frames.length - 1) {
              // Calculate actual time difference to next frame in milliseconds
              const currentTimestamp = content.frames[index][0];
              const nextTimestamp = content.frames[index + 1][0];
              const rawDelay = nextTimestamp - currentTimestamp;
              
              // Calculate average timing for GC pause detection
              let avgTiming = 33; // Default ~30fps fallback
              if (content.frames.length > 5) {
                const sampleSize = Math.min(10, content.frames.length - 1);
                const recentTimings = [];
                for (let i = Math.max(0, index - sampleSize); i < Math.min(content.frames.length - 1, index + sampleSize); i++) {
                  if (i !== index) { // Skip current frame
                    recentTimings.push(content.frames[i + 1][0] - content.frames[i][0]);
                  }
                }
                if (recentTimings.length > 0) {
                  avgTiming = recentTimings.reduce((a, b) => a + b, 0) / recentTimings.length;
                }
              }
              
              // Detect and smooth GC pauses (delays >3x average)
              const gcPauseThreshold = avgTiming * 3;
              if (rawDelay > gcPauseThreshold) {
                // This looks like a GC pause - use average timing instead
                frameDelay = Math.round(avgTiming);
                console.log(`🗑️ Smoothed GC pause: ${rawDelay.toFixed(1)}ms → ${frameDelay}ms (frame ${index})`);
              } else {
                frameDelay = Math.round(rawDelay);
              }
              
              // Clamp to much faster GIF timing for real-time feel (10ms to 30ms)
              frameDelay = Math.max(10, Math.min(30, frameDelay));
            } else {
              // Last frame - use average delay or loop back to first frame timing
              if (content.frames.length > 1) {
                const firstTimestamp = content.frames[0][0];
                const lastTimestamp = content.frames[content.frames.length - 1][0];
                const totalDuration = lastTimestamp - firstTimestamp;
                const averageDelay = Math.round(totalDuration / (content.frames.length - 1));
                frameDelay = Math.max(10, Math.min(30, averageDelay)); // Cap to 30ms for faster playback
              } else {
                frameDelay = 20; // Default 20ms for single frame (faster than 100ms)
              }
            }

            gif.addFrame(canvas, { copy: true, delay: frameDelay });

            if ((index + 1) % 50 === 0 || index === content.frames.length - 1) {
              console.log(
                `🎞️ Processed frame ${index + 1}/${content.frames.length} (${Math.round(((index + 1) / content.frames.length) * 100)}%) - delay: ${frameDelay}ms`,
              );
            }
          }

          console.log("🔄 Rendering GIF...");

          // Render the GIF
          await new Promise((resolve, reject) => {
            gif.on("finished", (blob) => {
              console.log(
                `💾 GIF generated: ${Math.round((blob.size / 1024 / 1024) * 100) / 100} MB`,
              );

              const filename = generateTapeFilename("gif");
              receivedDownload({ filename, data: blob });

              console.log("🎬 Animated GIF exported successfully!");

              // Send completion message to video piece
              send({
                type: "recorder:export-complete",
                content: { type: "gif", filename }
              });

              // Add a small delay to ensure UI processes the completion signal
              setTimeout(() => {
                resolve();
              }, 100);
            });

            gif.on("progress", (progress) => {
              console.log(
                `🔄 GIF encoding progress: ${Math.round(progress * 100)}%`,
              );
              
              // Send progress updates to video piece
              send({
                type: "recorder:export-progress", 
                content: { progress, type: "gif" }
              });
            });

            gif.render();
          });
        }
      } catch (error) {
        console.error("Error creating animated GIF:", error);
        console.log("🔄 Falling back to static GIF of first frame");

        try {
          const progressBarHeight = 3; // Define progress bar height for fallback
          const canvas = document.createElement("canvas");
          const ctx = canvas.getContext("2d");
          const firstFrame = content.frames[0];

          canvas.width = firstFrame.width;
          canvas.height = firstFrame.height + progressBarHeight;

          const imageData = new ImageData(
            new Uint8ClampedArray(firstFrame.data),
            firstFrame.width,
            firstFrame.height,
          );

          ctx.putImageData(imageData, 0, 0);

          // Add sideways AC stamp to fallback GIF as well (await to ensure fonts are loaded)
          await addAestheticComputerStamp(
            ctx,
            firstFrame.width,
            firstFrame.height + progressBarHeight,
          );

          const gifBlob = await new Promise((resolve) => {
            canvas.toBlob(resolve, "image/gif");
          });

          const filename = generateTapeFilename("gif", "-static");
          receivedDownload({ filename, data: gifBlob });

          console.log("📸 Static GIF fallback exported successfully");
        } catch (fallbackError) {
          console.error("Error in fallback GIF creation:", fallbackError);
        }
      }
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
              } else {
                input.blur();
              }
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
      }); // 🌒 Detect light or dark mode.
      // See also: https://flaviocopes.com/javascript-detect-dark-mode,
      //           https://developer.mozilla.org/en-US/docs/Web/CSS/@media/prefers-color-scheme

      if (window.matchMedia) {
        if (window.matchMedia("(prefers-color-scheme: dark)").matches) {
          document.documentElement.style.setProperty("color-scheme", "dark");
          send({ type: "dark-mode", content: { enabled: true } });
        } else {
          document.documentElement.style.setProperty("color-scheme", "light");
          send({ type: "dark-mode", content: { enabled: false } });
        }

        window
          .matchMedia("(prefers-color-scheme: dark)")
          .addEventListener("change", (event) => {
            if (event.matches) {
              document.documentElement.style.setProperty(
                "color-scheme",
                "dark",
              );
              send({ type: "dark-mode", content: { enabled: true } });
            } else {
              document.documentElement.style.setProperty(
                "color-scheme",
                "light",
              );
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
    if (type === "disk-loaded") {
      // Clear any active parameters once the disk has been loaded.
      // Special handling for prompt piece with kidlisp content
      if (
        content.path === "aesthetic.computer/disks/prompt" &&
        content.params &&
        content.params.length > 0 &&
        isKidlispSource(content.params[0])
      ) {
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

      if (pen) pen.events.length = 0; // Clear pen events.
      keyboard.events.length = 0; // Clear keyboard events.
      gamepad.events.length = 0; // Clear gamepad events.

      // Clear when events.
      whens = {};

      // Close (defocus) software keyboard if we are NOT entering the prompt.
      if (content.text && content.text.split("~")[0] !== "prompt") {
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
          } else if (
            content.path === "aesthetic.computer/disks/prompt" &&
            content.params &&
            content.params.length > 0 &&
            isKidlispSource(content.params[0])
          ) {
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
                } else if (
                  content.path === "aesthetic.computer/disks/prompt" &&
                  content.params &&
                  content.params.length > 0 &&
                  isKidlispSource(content.params[0])
                ) {
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
            } else if (
              content.path === "aesthetic.computer/disks/prompt" &&
              content.params &&
              content.params.length > 0 &&
              isKidlispSource(content.params[0])
            ) {
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
          window.parent.postMessage(
            {
              type: "openExternal",
              url: content.url,
            },
            "*",
          );
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

    // Store recording metadata for filename generation
    let recordingOptions = {};
    let actualContent = content;
    
    if (typeof content === "object") {
      // New format with piece metadata
      recordingOptions = {
        pieceName: content.pieceName || "tape",
        pieceParams: content.pieceParams || "",
        originalCommand: content.originalCommand || ""
      };
      actualContent = content.type || "video";
    } else {
      // Legacy string format - default to "tape"
      recordingOptions = {
        pieceName: "tape",
        pieceParams: "",
        originalCommand: ""
      };
      actualContent = content;
    }
    
    // Store for use during export
    window.currentRecordingOptions = recordingOptions;

    const colonSplit = actualContent.split(":");
    // tiktokVideo = colonSplit[1] === "tiktok";
    actualContent = colonSplit[0];

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
          console.log("🔴 Recorder: Resumed", actualContent);
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
        // Clear recording timestamp for next recording
        if (window.recordingStartTimestamp) {
          delete window.recordingStartTimestamp;
        }
        // Clear cached tape data when stopping to start a new recording
        Store.del("tape").catch(() => {}); // Clear cache, ignore errors
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

      if (actualContent === "video") {
        // Start recording audio.
        try {
          mediaRecorder = new MediaRecorder(audioStreamDest.stream, options);
        } catch (error) {
          console.error("MediaRecorder creation failed:", error);
          return;
        }
      }

      // 🗺️ mediaRecorder:Start
      if (mediaRecorder) {
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
            console.log("🔴 Recorder: Rolling", actualContent);

          // window.addEventListener("resize", () => (mediaRecorderResized = true), {
          // once: true,
          // });
        };
      }

      // 🗺️ mediaRecorder:Stop (Recorder Printing)
      if (mediaRecorder) {
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
      }

      //if (content === "video") {
      // Start media recorder once svg loads.
      //svgCursor.onload = function (e) {
      // Use small chunk sizes. (`1000` broke TikTok)
      //  mediaRecorder.start(100);
      //};
      //svgCursor.src = "/aesthetic.computer/cursors/precise.svg";
      //} else {
      // console.log("Start audio recording...");

      // Clear any cached tape data and audio before starting new recording
      Store.del("tape").catch(() => {}); // Clear cache, ignore errors
      delete sfx["tape:audio"]; // Clear any cached audio data
      mediaRecorderDuration = 0; // Reset duration for new recording
      mediaRecorderFrameCount = 0; // Reset frame capture counter for optimization

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

      // Store the tape data to IndexedDB for persistence across page refreshes
      const blob = new Blob(mediaRecorderChunks, {
        type: mediaRecorder.mimeType,
      });

      await receivedChange({
        data: {
          type: "store:persist",
          content: {
            key: "tape",
            method: "local:db",
            data: {
              blob,
              duration: mediaRecorderDuration,
              frames: recordedFrames, // Include frame data for WebP/Frame exports
              timestamp: Date.now(),
            },
          },
        },
      });

      if (debug && logs.recorder) console.log("📼 Stored tape to IndexedDB");

      send({ type: "recorder:rolling:ended" });
      return;
    }

    if (type === "recorder:present") {
      // Check for cached video if no active recording AND no recorded frames
      if (
        (!mediaRecorder || mediaRecorder.state !== "paused") &&
        recordedFrames.length === 0
      ) {
        const cachedTape = await Store.get("tape");
        if (cachedTape && cachedTape.blob) {
          sfx["tape:audio"] = await blobToArrayBuffer(cachedTape.blob);

          // Restore frame data if available
          if (cachedTape.frames) {
            recordedFrames.length = 0;
            recordedFrames.push(...cachedTape.frames);
          }

          // Set duration if available
          if (cachedTape.duration) {
            mediaRecorderDuration = cachedTape.duration;
          }
        }
      }

      if (
        (mediaRecorder && mediaRecorder.state === "paused") ||
        recordedFrames.length > 0
      ) {
        // Handle live recording or cached video
        if (mediaRecorder && mediaRecorder.state === "paused") {
          const blob = new Blob(mediaRecorderChunks, {
            type: mediaRecorder.mimeType,
          });
          sfx["tape:audio"] = await blobToArrayBuffer(blob);
          // console.log("Update tape audio with new blob!");
        }

        underlayFrame = document.createElement("div");
        underlayFrame.id = "underlay";

        const frameCan = document.createElement("canvas");
        const fctx = frameCan.getContext("2d");

        if (recordedFrames.length > 0) {
          frameCan.width = recordedFrames[0][1].width;
          frameCan.height = recordedFrames[0][1].height;
        }

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
            // Kill the sound since pause is not available
            sfxPlaying[tapeSoundId]?.kill();
            delete sfxPlaying[tapeSoundId]; // Clean up reference
            send({ type: "recorder:present-paused" });
          };

          resumeTapePlayback = () => {
            if (stopped) {
              send({ type: "recorder:present-playing" });
              return startTapePlayback(true);
            }
            continuePlaying = true;
            window.requestAnimationFrame(update);

            // Calculate audio position when paused
            const pauseDuration = performance.now() - pauseStart;
            playbackStart += pauseDuration;

            // Restart the audio from the correct position if it was killed during pause
            if (!sfxPlaying[tapeSoundId] && mediaRecorderDuration > 0) {
              const audioPosition =
                (performance.now() - playbackStart) /
                (mediaRecorderDuration * 1000);
              const clampedPosition = Math.max(0, Math.min(1, audioPosition));

              tapeSoundId = "tape:audio_" + performance.now();
              playSfx(tapeSoundId, "tape:audio", {
                stream,
                from: clampedPosition, // Start from the paused position
              });
            }

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

        // CRITICAL: Clear freeze frame that would layer above the underlay video (z-index 3 > 0)
        if (freezeFrame || freezeFrameFrozen || wrapper.contains(freezeFrameCan)) {
          freezeFrameCan.remove();
          freezeFrame = false;
          freezeFrameGlaze = false;
          freezeFrameFrozen = false;
        }

        underlayFrame.appendChild(frameCan);
        wrapper.appendChild(underlayFrame);
        send({ type: "recorder:presented" });
        send({ type: "recorder:present-playing" });
      } else {
        if (debug && logs.recorder)
          console.error(
            "📼 No media recorder or cached video to present from!",
          );
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
      
      // Send initial status
      send({
        type: "recorder:export-status",
        content: { 
          type: "video", 
          phase: "preparing", 
          message: "Preparing video export" 
        }
      });
      
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
      
      // Add progress bar height to the final export canvas
      const progressBarHeight = 3;
      
      sctx.canvas.width = newWidth;
      sctx.canvas.height = newHeight + progressBarHeight;
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
        
        // Initialize frame counter for detailed progress reporting
        let framesRendered = 0;
        
        // Send encoding start status
        send({
          type: "recorder:export-status",
          content: { 
            type: "video", 
            phase: "encoding", 
            message: "Starting video encoding" 
          }
        });
        
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
            framesRendered++; // Increment frame counter
            
            const progressBarHeight = 20;
            const frameWidth = sctx.canvas.width;
            const totalFrameHeight = sctx.canvas.height;
            const contentFrameHeight = totalFrameHeight - progressBarHeight; // Height for content area
            const frameAspectRatio = contentFrameHeight / frameWidth;
            const aspectRatio = can.height / can.width;

            sctx.clearRect(0, 0, frameWidth, totalFrameHeight);

            // if (glaze.on) can = Glaze.getCan();

            let x = 0,
              y = 0,
              width,
              height;

            if (frameAspectRatio > aspectRatio) {
              height = sctx.canvas.width * aspectRatio;
              y = floor(contentFrameHeight / 2 - height / 2);
              width = sctx.canvas.width;
            } else {
              width = contentFrameHeight / aspectRatio;
              x = floor(frameWidth / 2 - width / 2);
              height = contentFrameHeight; // Use content frame height
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

            if (pen?.pointers[1]) {
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
            const typeSize = min(16, max(12, floor(sctx.canvas.height / 40)));
            // const textHeight = typeSize;
            const gap = typeSize * 0.75;

            // Film camera style timestamp at bottom-left corner
            function addFilmTimestamp(
              sctx,
              canvasWidth,
              canvasHeight,
              typeSize,
              progress, // Export progress for the progress bar
            ) {
              // Progress bar height (should match what's set above)
              const progressBarHeight = 20;
              
              // Calculate the content area height (canvas height minus progress bar)
              const contentHeight = sctx.canvas.height - progressBarHeight;
              
              const now = new Date();
              const year = now.getUTCFullYear();
              const month = now.getUTCMonth() + 1; // getUTCMonth() returns 0-11
              const day = now.getUTCDate();
              const hour = now.getUTCHours();
              const minute = now.getUTCMinutes();
              const second = now.getUTCSeconds();
              const millisecond = now.getUTCMilliseconds();

              // Include milliseconds for frame-by-frame uniqueness
              const timestamp = `${year}.${month}.${day}.${hour}.${minute}.${second}.${millisecond.toString().padStart(3, "0")}`;

              sctx.save();
              // Make timestamp more opaque and clearer
              sctx.font = `bold ${typeSize * 1.8}px YWFTProcessing-Regular`;
              sctx.globalAlpha = 0.8; // More opaque (was 0.45)
              sctx.fillStyle = choose("white", "yellow", "red"); // White/yellow/red blinking

              const timestampMargin = typeSize * 0.5; // Small margin from edges

              // Smoother, gentler shake effect
              const frameBasedSeed = Math.floor(Date.now() / 300); // Slower change (was 200ms)
              const shakeX = (frameBasedSeed % 3) - 1; // Range -1 to 1 (was -3 to 3)
              const shakeY = ((frameBasedSeed + 2) % 3) - 1; // Range -1 to 1, offset for variation

              // Position timestamp in the content area (not in the progress bar area)
              const timestampY = contentHeight - timestampMargin - typeSize * 0.5;

              sctx.fillText(
                timestamp,
                timestampMargin + shakeX,
                timestampY + shakeY,
              );

              // Draw progress bar in the extended area at the bottom
              const progressBarY = contentHeight; // Start right after content
              const progressBarWidth = Math.floor(progress * canvasWidth);
              
              // Fill the entire progress bar area with a dark background
              sctx.fillStyle = "#000000";
              sctx.fillRect(0, progressBarY, canvasWidth, progressBarHeight);
              
              // Draw the actual progress bar
              sctx.fillStyle = "#FF0000"; // Red progress bar
              sctx.fillRect(0, progressBarY, progressBarWidth, progressBarHeight);
              
              // Add a subtle border
              sctx.strokeStyle = "#333333";
              sctx.lineWidth = 1;
              sctx.strokeRect(0, progressBarY, canvasWidth, progressBarHeight);

              sctx.restore();
            }

            sctx.save(); // Save the current state of the canvas

            // Add film camera style timestamp at bottom-left corner
            addFilmTimestamp(
              sctx,
              sctx.canvas.width,
              sctx.canvas.height,
              typeSize,
              progress, // Pass progress to the timestamp function
            );

            drawTextAtPosition(0, 90); // Left side stamp (rotated 90 degrees)
            drawTextAtPosition(sctx.canvas.width, -90); // Right side stamp (rotated -90 degrees)

            sctx.restore();
            sctx.globalAlpha = 1;

            function drawTextAtPosition(positionX, deg) {
              sctx.save();
              sctx.translate(positionX, 0);
              sctx.rotate(radians(deg));
              const yDist = 0.25; // Move towards center (was 0.05)

              // Make the stamps larger
              sctx.font = `${typeSize * 1.2}px YWFTProcessing-Regular`;
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
                  // LEFT SIDE: Main text positioning
                  sctx.fillText(
                    text,
                    floor(
                      sctx.canvas.height * (1 - yDist) - textWidth + offsetY,
                    ),
                    -floor(offsetX + gap),
                  );
                } else if (deg === -90) {
                  // RIGHT SIDE: Main text positioning
                  sctx.fillText(
                    text,
                    -floor(sctx.canvas.height * 0.05 + offsetY),
                    floor(offsetX + gap * 3),
                  );
                }
              });

              if (HANDLE) {
                sctx.font = `${typeSize * 1.5}px YWFTProcessing-Light`; // Larger handle
                sctx.fillStyle = choose("yellow", "red", "blue");
                let offsetX, offsetY;
                const handleWidth =
                  textWidth / 2 + sctx.measureText(HANDLE).width / 2;
                const handleSpace = typeSize * 1.35;
                offsetX = choose(-1, 0, 1);
                offsetY = choose(-1, 0, 1);

                if (deg === 90) {
                  // LEFT SIDE: Handle positioning
                  sctx.fillText(
                    HANDLE,
                    floor(
                      sctx.canvas.height * (1 - yDist) - handleWidth + offsetY,
                    ),
                    -floor(offsetX + handleSpace + gap),
                  );
                } else if (deg === -90) {
                  // RIGHT SIDE: Handle positioning
                  sctx.fillText(
                    HANDLE,
                    -floor(sctx.canvas.height * 0.05 + handleWidth + offsetY),
                    floor(offsetX + handleSpace + gap * 3),
                  );
                }
              }

              // Remove the old timestamp code from here since it's now in addFilmTimestamp

              sctx.restore();
            }

            // Film camera style timestamp at bottom-left corner
            function addFilmTimestamp(
              sctx,
              canvasWidth,
              canvasHeight,
              typeSize,
              progress, // Export progress for the progress bar
            ) {
              // Progress bar height (should match what's set in renderTape)
              const progressBarHeight = 20;
              
              // Calculate the original content area height
              const originalCanvasHeight = sctx.canvas.height - progressBarHeight;
              
              const now = new Date();
              const year = now.getUTCFullYear();
              const month = now.getUTCMonth() + 1; // getUTCMonth() returns 0-11
              const day = now.getUTCDate();
              const hour = now.getUTCHours();
              const minute = now.getUTCMinutes();
              const second = now.getUTCSeconds();
              const millisecond = now.getUTCMilliseconds();

              // Include milliseconds for frame-by-frame uniqueness
              const timestamp = `${year}.${month}.${day}.${hour}.${minute}.${second}.${millisecond.toString().padStart(3, "0")}`;

              sctx.save();
              // Make timestamp more opaque and clearer
              sctx.font = `bold ${typeSize * 1.8}px YWFTProcessing-Regular`;
              sctx.globalAlpha = 0.8; // More opaque (was 0.45)
              sctx.fillStyle = choose("white", "yellow", "red"); // White/yellow/red blinking

              const timestampMargin = typeSize * 0.5; // Small margin from edges

              // Smoother, gentler shake effect
              const frameBasedSeed = Math.floor(Date.now() / 300); // Slower change (was 200ms)
              const shakeX = (frameBasedSeed % 3) - 1; // Range -1 to 1 (was -3 to 3)
              const shakeY = ((frameBasedSeed + 2) % 3) - 1; // Range -1 to 1, offset for variation

              // Position timestamp in the main content area (not in the progress bar area)
              const timestampY = originalCanvasHeight - timestampMargin - typeSize * 0.5;

              sctx.fillText(
                timestamp,
                timestampMargin + shakeX,
                timestampY + shakeY,
              );

              // Draw progress bar in the extended area at the bottom of the scaled content
              const progressBarY = originalCanvasHeight; // Start right after original content
              const progressBarWidth = Math.floor(progress * canvasWidth);
              
              // Fill the entire progress bar area with a dark background
              sctx.fillStyle = "#000000";
              sctx.fillRect(0, progressBarY, canvasWidth, progressBarHeight);
              
              // Draw the actual progress bar
              sctx.fillStyle = "#FF0000"; // Red progress bar
              sctx.fillRect(0, progressBarY, progressBarWidth, progressBarHeight);
              
              // Add a subtle border
              sctx.strokeStyle = "#333333";
              sctx.lineWidth = 1;
              sctx.strokeRect(0, progressBarY, canvasWidth, progressBarHeight);

              sctx.restore();
            }

            // Send detailed progress with phase information
            let phase = "encoding";
            let message = "Encoding video";
            
            if (progress < 0.3) {
              phase = "preparing";
              message = `Preparing frame ${framesRendered + 1}`;
            } else if (progress < 0.7) {
              phase = "encoding";
              message = `Encoding frame ${framesRendered + 1}`;
            } else if (progress < 0.95) {
              phase = "transcoding";
              message = "Transcoding MP4";
            } else {
              phase = "finalizing";
              message = "Finalizing video";
            }

            send({
              type: "recorder:transcode-progress",
              content: progress,
            });
            
            // Send detailed status every few frames to avoid spam
            if (framesRendered % 5 === 0 || progress >= 0.95) {
              send({
                type: "recorder:export-status",
                content: { 
                  type: "video", 
                  phase: phase, 
                  message: message 
                }
              });
            }
          },
        );
      }

      const svgCursor = new Image();
      svgCursor.onload = (e) => startRendering();
      svgCursor.src = "/aesthetic.computer/cursors/precise.svg";

      // Video rendered, now download...
      videoRecorder.onstop = async function (e) {
        const blob = new Blob(chunks, { type: videoRecorder.mimeType });
        const filename = generateTapeFilename("mp4");

        // Store video with frame data for complete persistence
        await receivedChange({
          data: {
            type: "store:persist",
            content: {
              key: "tape",
              method: "local:db",
              data: {
                blob,
                duration: mediaRecorderDuration,
                frames: recordedFrames, // Include frame data for WebP/Frame exports
                timestamp: Date.now(),
                filename, // Store the generated filename
              },
            },
          },
        });

        // 📥 Download the video.
        receivedDownload({ filename, data: blob });
        
        // Send completion message
        send({
          type: "recorder:export-complete",
          content: { type: "video", filename }
        });
        
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
      // Always clear cached tape data when slating, regardless of media type
      await Store.del("tape");
      mediaRecorder?.stop();
      return;
    }

    // Request recorded frames for export
    if (type === "recorder:request-frames") {
      if (recordedFrames.length > 0) {
        send({
          type: "recorder:frames-response",
          content: { frames: recordedFrames },
        });
      } else {
        send({
          type: "recorder:frames-response",
          content: { frames: [] },
        });
      }
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
      if (debug && logs.audio)
        console.log("🔈 BIOS received sfx:load request for:", content);

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

      if (debug && logs.audio)
        console.log("🔈 BIOS loading sound from URL:", url);

      // Enhanced instant playback audio loading strategy
      const audioId = content;

      // Strategy 1: Immediate HTML5 Audio for instant playback
      const htmlAudioElement = new Audio();
      htmlAudioElement.crossOrigin = "anonymous";
      htmlAudioElement.preload = "auto";
      htmlAudioElement.src = url;

      // Store the HTML5 audio for instant playback
      sfx[audioId + "_html5"] = htmlAudioElement;

      if (debug && logs.audio)
        console.log(
          "🔈 BIOS stored HTML5 audio element for:",
          audioId + "_html5",
        );

      // Strategy 2: Background fetch and decode for high-quality playback
      fetch(url)
        .then((response) => {
          return response.arrayBuffer();
        })
        .then(async (arrayBuffer) => {
          if (debug && logs.audio)
            console.log("🔈 BIOS fetched audio data for:", audioId);
          try {
            if (!audioContext) {
              sfx[audioId] = arrayBuffer;
              if (debug && logs.audio)
                console.log(
                  "🔈 BIOS stored raw audio buffer (no audioContext):",
                  audioId,
                );
              // Process any queued sounds that might be waiting for this file
              processPendingSfx();
            } else {
              // Background decode the buffer
              const audioBuffer =
                await audioContext.decodeAudioData(arrayBuffer);
              sfx[audioId] = audioBuffer;

              if (debug && logs.audio)
                console.log(
                  "🔈 BIOS decoded and stored audio buffer:",
                  audioId,
                );

              // Clean up HTML5 audio once high-quality buffer is ready
              if (sfx[audioId + "_html5"]) {
                delete sfx[audioId + "_html5"];
              }

              if (debug && logs.audio)
                console.log(
                  "🔈 Background decoded for high-quality playback:",
                  audioId,
                );

              // Process any queued sounds that might be waiting for this file
              processPendingSfx();
            }

            send({
              type: "loaded-sfx-success",
              content: { url, sfx: audioId },
            });
            if (debug && logs.audio)
              console.log(
                "🔈 BIOS sent loaded-sfx-success (background):",
                audioId,
              );
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
      if (debug && logs.audio)
        console.log("🔈 BIOS sent loaded-sfx-success (instant):", audioId);

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
    }
    if (type === "back-to-piece") {
      // Check if we have labelBack state active
      if (mainThreadLabelBack) {
        // Clear the labelBack state since we're handling the navigation
        mainThreadLabelBack = false;
        sessionStorage.removeItem("aesthetic-labelBack");

        // Navigate directly to the target piece from worker instead of using history.back()
        // This avoids the reload cycle for kidlisp pieces
        const targetPiece = content?.targetPiece || "chat"; // Use target from worker or fallback to chat

        // Update the URL and load the target piece directly
        const targetUrl = new URL(window.location);
        targetUrl.pathname = "/" + targetPiece;
        window.history.pushState(null, "", targetUrl);

        const parsed = parse(targetPiece);
        send({
          type: "history-load",
          content: parsed,
        });
      } else {
        history.back();
      }
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

    // 🖥️ Compositing - OPTIMIZED for zero-copy pixel buffer transfer

    // TEMPORARILY DISABLE PIXEL OPTIMIZER to debug black buffer dragging issue
    /*
    if (!window.pixelOptimizer) {
      try {
        const { pixelOptimizer } = await import('./lib/graphics-optimizer.mjs');
        window.pixelOptimizer = pixelOptimizer;
      } catch (err) {
        console.warn('🟡 Graphics optimizer not available, using fallback:', err);
      }
    }
    */

    // This is a bit messy compared to what happens inside of content.reframe -> frame below. 22.10.27.02.05
    if (
      content.pixels?.byteLength > 0 &&
      content.width === screen.width &&
      content.height === screen.height
    ) {
      // 🚀 OPTIMIZATION: Use zero-copy ImageData creation when possible
      if (window.pixelOptimizer) {
        try {
          imageData = window.pixelOptimizer.createImageDataZeroCopy(
            content.pixels, 
            content.width, 
            content.height
          );
          // Update screen reference to use the zero-copy buffer
          assign(screen, {
            pixels: imageData.data,
            width: content.width,
            height: content.height,
          });
          window.pixelOptimizer.stats.framesProcessed++;
        } catch (err) {
          console.warn('🟡 Zero-copy optimization failed, using fallback:', err);
          // Fallback to original method
          screen.pixels = new Uint8ClampedArray(content.pixels);
          let width = screen.width;
          let height = screen.height;
          const expectedLength = width * height * 4;
          
          if (
            !content.reframe &&
            (screen.pixels.length === expectedLength || reframeJustCompleted)
          ) {
            if (screen.pixels.length === expectedLength) {
              imageData = new ImageData(screen.pixels, width, height);
              if (underlayFrame) {
                console.log("🎬 Fallback ImageData created during tape playback");
              }
            }
            if (reframeJustCompleted) {
              reframeJustCompleted = false;
            }
          }
        }
      } else {
        // Original fallback code
        screen.pixels = new Uint8ClampedArray(content.pixels);
        let width = screen.width;
        let height = screen.height;
        const expectedLength = width * height * 4;

        // Only create ImageData if dimensions match the pixel buffer
        // (Don't create with reframe dimensions until after the reframe happens)
        // Be more lenient right after reframe completion to restore animation
        if (
          !content.reframe &&
          (screen.pixels.length === expectedLength || reframeJustCompleted)
        ) {
          if (screen.pixels.length === expectedLength) {
            imageData = new ImageData(screen.pixels, width, height);
          }
          // Reset flag regardless of whether ImageData creation succeeded
          if (reframeJustCompleted) {
            reframeJustCompleted = false;
          }
        }
      }
    } else if (reframeJustCompleted && content.pixels?.byteLength > 0) {
      // Special case: after reframe with new dimensions, create ImageData even if screen dimensions don't match yet
      try {
        if (window.pixelOptimizer) {
          // 🚀 OPTIMIZATION: Zero-copy reframe handling
          imageData = window.pixelOptimizer.createImageDataZeroCopy(
            content.pixels,
            content.width,
            content.height
          );
          // Update screen dimensions to match
          assign(screen, {
            pixels: imageData.data,
            width: content.width,
            height: content.height,
          });
          window.pixelOptimizer.stats.framesProcessed++;
        } else {
          // Original fallback
          const newPixels = new Uint8ClampedArray(content.pixels);
          imageData = new ImageData(newPixels, content.width, content.height);
          // Update screen dimensions to match
          assign(screen, {
            pixels: imageData.data,
            width: content.width,
            height: content.height,
          });
        }
        reframeJustCompleted = false;
      } catch (err) {
        console.warn("⚠️ Failed to create post-reframe ImageData:", err);
        reframeJustCompleted = false;
      }
    }

    // old threed garbage collection (remove)

    // Check for a change in resolution.
    if (content.reframe) {
      // Reframe the captured pixels.
      frame(content.reframe.width, content.reframe.height, content.reframe.gap);
      pen?.retransformPosition();
    }

    if (content.cursorCode) pen?.setCursorCode(content.cursorCode);

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
      // 🅰️ Cropped update - OPTIMIZED for zero-copy and async rendering
      if (window.pixelOptimizer) {
        try {
          // Use optimized dirty box rendering
          const imageData = window.pixelOptimizer.createImageDataZeroCopy(
            content.pixels,
            content.dirtyBox.w,
            content.dirtyBox.h
          );

          // Reuse the dirtyBox canvas instead of creating new ones
          if (
            dirtyBoxCanvas.width !== imageData.width ||
            dirtyBoxCanvas.height !== imageData.height
          ) {
            dirtyBoxCanvas.width = imageData.width;
            dirtyBoxCanvas.height = imageData.height;
          }

          // Use async rendering for better performance
          if (window.pixelOptimizer.asyncRenderingSupported && imageData.width * imageData.height > 1024) {
            window.pixelOptimizer.renderImageDataAsync(imageData, dirtyBoxCtx, 0, 0);
          } else {
            dirtyBoxCtx.putImageData(imageData, 0, 0);
          }
          
          dirtyBoxBitmapCan = dirtyBoxCanvas; // Reference the reused canvas
          
        } catch (err) {
          console.warn('🟡 Optimized dirty box rendering failed, using fallback:', err);
          // Fallback to original method
          const imageData = new ImageData(
            new Uint8ClampedArray(content.pixels),
            content.dirtyBox.w,
            content.dirtyBox.h,
          );

          if (
            dirtyBoxCanvas.width !== imageData.width ||
            dirtyBoxCanvas.height !== imageData.height
          ) {
            dirtyBoxCanvas.width = imageData.width;
            dirtyBoxCanvas.height = imageData.height;
          }

          dirtyBoxCtx.putImageData(imageData, 0, 0);
          dirtyBoxBitmapCan = dirtyBoxCanvas;
        }
      } else {
        // Original fallback code
        const imageData = new ImageData(
          new Uint8ClampedArray(content.pixels), // Is this the only necessary part?
          content.dirtyBox.w,
          content.dirtyBox.h,
        );

        // Reuse the dirtyBox canvas instead of creating new ones
        if (
          dirtyBoxCanvas.width !== imageData.width ||
          dirtyBoxCanvas.height !== imageData.height
        ) {
          dirtyBoxCanvas.width = imageData.width;
          dirtyBoxCanvas.height = imageData.height;
        }

        dirtyBoxCtx.putImageData(imageData, 0, 0);
        dirtyBoxBitmapCan = dirtyBoxCanvas; // Reference the reused canvas

        // Use this alternative once it's faster. 2022.01.29.02.46
        // const dbCtx = dirtyBoxBitmapCan.getContext("bitmaprenderer");
        // dbCtx.transferFromImageBitmap(dirtyBoxBitmap);
      }
    } else if (content.paintChanged && content.pixels && !content.reframe) {
      // 🅱️ Normal full-screen update (skip during reframe to avoid dimension mismatch) - OPTIMIZED
      if (window.pixelOptimizer) {
        try {
          // Use zero-copy optimization for full screen updates
          imageData = window.pixelOptimizer.createImageDataZeroCopy(
            content.pixels,
            content.width,
            content.height
          );
        } catch (err) {
          console.warn('🟡 Zero-copy full screen update failed, using fallback:', err);
          // Fallback to original method
          const pixelArray = new Uint8ClampedArray(content.pixels);
          const expectedLength = content.width * content.height * 4;

          if (pixelArray.length === expectedLength || reframeJustCompleted) {
            if (pixelArray.length === expectedLength) {
              imageData = new ImageData(pixelArray, content.width, content.height);
            }
            if (reframeJustCompleted) {
              reframeJustCompleted = false;
            }
          }
        }
      } else {
        // Original fallback code
        const pixelArray = new Uint8ClampedArray(content.pixels);
        const expectedLength = content.width * content.height * 4;

        // Only create ImageData if pixel buffer length matches expected dimensions
        // Be more lenient right after reframe completion to restore animation
        if (pixelArray.length === expectedLength || reframeJustCompleted) {
          if (pixelArray.length === expectedLength) {
            imageData = new ImageData(pixelArray, content.width, content.height);
          }
          // Reset flag regardless of whether ImageData creation succeeded
          if (reframeJustCompleted) {
            reframeJustCompleted = false;
          }
        }
      }
    }

    pixelsDidChange = content.paintChanged || false;

    // ✨ UI Overlay (Composite) Layer
    // This currently paints corner labels and tape progress bars only.
    // (So they can be skipped for recordings?)
    let paintOverlays = {};

    // Frame-persistent overlay cache that survives reframes
    if (!window.framePersistentOverlayCache) {
      window.framePersistentOverlayCache = {};
    }

    // Per-overlay canvas cache to prevent canvas thrashing
    if (!window.overlayCanvasCache) {
      window.overlayCanvasCache = {};
    }

    function buildOverlay(name, o) {
      // Only log reframe operations to debug flicker
      const isHudOverlay = name === "label" || name === "qrOverlay";

      if (!o || !o.img) {
        // During reframes, if overlay data is missing but we have a cached version, use it
        // EXCEPT for tapeProgressBar, durationProgressBar and qrOverlay which should never use cached versions
        if (content.reframe && window.framePersistentOverlayCache[name] && name !== "tapeProgressBar" && name !== "durationProgressBar" && name !== "qrOverlay") {
          paintOverlays[name] = window.framePersistentOverlayCache[name];
          return;
        }
        return;
      }

      // Create or reuse dedicated canvas for this overlay
      if (!window.overlayCanvasCache[name]) {
        window.overlayCanvasCache[name] = {
          canvas: document.createElement("canvas"),
          lastKey: null,
        };
        window.overlayCanvasCache[name].ctx =
          window.overlayCanvasCache[name].canvas.getContext("2d");
      }

      const overlayCache = window.overlayCanvasCache[name];
      
      // Create a content-aware cache key that includes pixel data hash for HUD overlays
      let currentKey = `${o.img.width}x${o.img.height}_${o.x}_${o.y}`;
      
      // For HUD overlays (like labels), disable caching to ensure real-time updates for KidLisp syntax highlighting
      if (isHudOverlay && name === "label") {
        overlayCache.lastKey = null; // Force regeneration every frame like QR overlay
        delete window.framePersistentOverlayCache[name]; // Clear persistent cache
        currentKey += `_${performance.now()}`; // Force unique key every time
      }
      
      // For tape progress bar, completely disable ALL caching to ensure every frame is painted
      if (name === "tapeProgressBar") {
        overlayCache.lastKey = null; // Force regeneration every frame
        delete window.framePersistentOverlayCache[name]; // Clear persistent cache
        currentKey += `_${performance.now()}`; // Force unique key every time
      }
      
      // For QR overlay, completely disable ALL caching to allow text label font loading
      if (name === "qrOverlay") {
        overlayCache.lastKey = null; // Force regeneration every frame
        delete window.framePersistentOverlayCache[name]; // Clear persistent cache
        currentKey += `_${performance.now()}`; // Force unique key every time
      }

      // Only rebuild if overlay actually changed
      // Force rebuild every frame for tape progress bar, duration progress bar and QR overlay (no caching)
      if (
        name !== "tapeProgressBar" &&
        name !== "durationProgressBar" &&
        name !== "qrOverlay" &&
        overlayCache.lastKey === currentKey &&
        window.framePersistentOverlayCache[name]
      ) {
        paintOverlays[name] = window.framePersistentOverlayCache[name];
        return;
      }
      
      // DEBUG: Log cache invalidation for HUD overlays
      if (isHudOverlay && overlayCache.lastKey && overlayCache.lastKey !== currentKey) {
        // Cache invalidation tracking removed for cleaner console output
      }

      overlayCache.lastKey = currentKey;

      paintOverlays[name] = () => {
        const canvas = overlayCache.canvas;
        const overlayCtx = overlayCache.ctx;

        overlayCtx.imageSmoothingEnabled = false;

        // Resize this overlay's dedicated canvas if needed
        if (canvas.width !== o.img.width || canvas.height !== o.img.height) {
          canvas.width = o.img.width;
          canvas.height = o.img.height;
        }

        try {
          let imageData;
          // Use graphics optimizer if available, fallback to traditional method
          if (window.pixelOptimizer) {
            imageData = window.pixelOptimizer.createImageDataZeroCopy(
              o.img.pixels.buffer || o.img.pixels,
              o.img.width,
              o.img.height,
            );
          } else {
            imageData = new ImageData(
              o.img.pixels,
              o.img.width,
              o.img.height,
            );
          }
          overlayCtx.putImageData(imageData, 0, 0);
        } catch (error) {
          console.error(`❌ Error creating ImageData for ${name}:`, error);
          return;
        }

        // Paint overlay to main canvas (ctx is the main canvas context)
        if (name === "tapeProgressBar") {
          // console.log(`📼 Drawing tape progress bar to main canvas at (${o.x}, ${o.y}) with size ${canvas.width}x${canvas.height}`);
          ctx.drawImage(canvas, o.x, o.y);
          // console.log(`📼 Finished drawing tape progress bar`);
        } else {
          ctx.drawImage(canvas, o.x, o.y);
        }
      };

      // Debug logging for tape progress bar painter creation
      if (name === "tapeProgressBar" && false) { // Disabled verbose logging
        console.log(`🏗️ Created painter for "${name}":`, {
          painterExists: !!paintOverlays[name],
          canvasSize: `${overlayCache.canvas.width}x${overlayCache.canvas.height}`
        });
      }

      // Don't cache QR overlay painters to allow animation
      // Don't cache tapeProgressBar or durationProgressBar painters either - force regeneration every frame
      if (isHudOverlay && name !== "qrOverlay" && name !== "tapeProgressBar" && name !== "durationProgressBar") {
        window.framePersistentOverlayCache[name] = paintOverlays[name];
      }
    }

    buildOverlay("label", content.label);
    buildOverlay("qrOverlay", content.qrOverlay);
    buildOverlay("tapeProgressBar", content.tapeProgressBar);
    buildOverlay("durationProgressBar", content.durationProgressBar);
    // console.log("🖼️ Received overlay data:", {
    //   hasLabel: !!content.label,
    //   hasQR: !!content.qrOverlay,
    //   hasTapeProgress: !!content.tapeProgressBar,
    //   labelDetails: content.label ? {
    //     x: content.label.x,
    //     y: content.label.y,
    //     width: content.label.img?.width,
    //     height: content.label.img?.height,
    //     pixelLength: content.label.img?.pixels?.length,
    //     hasPixels: !!content.label.img?.pixels
    //   } : null
    // });

    function draw() {
      // During tape playback, render main canvas but make it semi-transparent so video shows through
      if (underlayFrame) {
        // Set canvas to be semi-transparent so the video shows underneath but UI is still visible
        canvas.style.opacity = 0.95; // Allow video to show through slightly
        canvas.style.mixBlendMode = "normal"; // Ensure proper blending
      } else {
        // Restore canvas visibility when not playing tape
        canvas.style.removeProperty("opacity");
        canvas.style.removeProperty("mix-blend-mode");
      }

      // �🅰️ Draw updated content from the piece.

      const db = content.dirtyBox;
      if (db) {
        ctx.drawImage(dirtyBoxBitmapCan, db.x, db.y);
        if (glaze.on) Glaze.update(dirtyBoxBitmapCan, db.x, db.y);
      } else if (
        pixelsDidChange ||
        needs$creenshot ||
        mediaRecorder?.state === "recording"
      ) {
        // 🚀 OPTIMIZATION: Async bitmap rendering with safety checks
        let skipImmediateOverlays = false; // Flag to skip immediate overlay painting for async rendering
        
        if (
          imageData &&
          imageData.data &&
          imageData.data.buffer &&
          imageData.data.buffer.byteLength > 0 &&
          imageData.width === ctx.canvas.width &&
          imageData.height === ctx.canvas.height
        ) {
          // Use async rendering for better performance (except during tape playback for immediate UI)
          if (underlayFrame) {
            // Force sync rendering during tape playback for immediate UI updates
            ctx.putImageData(imageData, 0, 0);
          } else if (window.pixelOptimizer && window.pixelOptimizer.asyncRenderingSupported) {
            try {
              // Non-blocking async rendering
              window.pixelOptimizer.renderImageDataAsync(imageData, ctx, 0, 0).then(() => {
                // Paint overlays after async rendering completes
                if (paintOverlays["label"]) paintOverlays["label"]();
                if (paintOverlays["qrOverlay"]) paintOverlays["qrOverlay"]();
                if (paintOverlays["tapeProgressBar"]) paintOverlays["tapeProgressBar"]();
                if (paintOverlays["durationProgressBar"]) paintOverlays["durationProgressBar"]();
              }).catch(err => {
                console.warn('🟡 Async rendering failed, falling back to sync:', err);
              });
              skipImmediateOverlays = true; // Skip immediate overlay painting for async rendering
            } catch (err) {
              console.warn('🟡 Async rendering not available, using sync:', err);
              ctx.putImageData(imageData, 0, 0);
            }
          } else {
            ctx.putImageData(imageData, 0, 0);
          }
        } else {
          // If imageData buffer is detached after reframe or dimensions don't match, get fresh data from canvas
          if (
            imageData &&
            imageData.data &&
            imageData.data.buffer &&
            (imageData.data.buffer.byteLength === 0 ||
              imageData.width !== ctx.canvas.width ||
              imageData.height !== ctx.canvas.height)
          ) {
            imageData = ctx.getImageData(
              0,
              0,
              ctx.canvas.width,
              ctx.canvas.height,
            );
            if (imageData.data.buffer.byteLength > 0) {
              // Use async rendering for better performance (except during tape playback for immediate UI)
              if (underlayFrame) {
                // Force sync rendering during tape playback for immediate UI updates
                ctx.putImageData(imageData, 0, 0);
              } else if (window.pixelOptimizer && window.pixelOptimizer.asyncRenderingSupported) {
                try {
                  // Non-blocking async rendering
                  window.pixelOptimizer.renderImageDataAsync(imageData, ctx, 0, 0).then(() => {
                    // Paint overlays after async fallback rendering completes
                    if (paintOverlays["label"]) paintOverlays["label"]();
                    if (paintOverlays["qrOverlay"]) paintOverlays["qrOverlay"]();
                    if (paintOverlays["tapeProgressBar"]) paintOverlays["tapeProgressBar"]();
                    if (paintOverlays["durationProgressBar"]) paintOverlays["durationProgressBar"]();
                  }).catch(err => {
                    console.warn('🟡 Fallback async rendering failed:', err);
                  });
                  skipImmediateOverlays = !underlayFrame; // Don't skip overlays during tape playback
                } catch (err) {
                  ctx.putImageData(imageData, 0, 0);
                }
              } else {
                ctx.putImageData(imageData, 0, 0);
              }
            }
          }
        }

        // Store clean pixel data for worker communication (before overlays)
        if (
          imageData &&
          imageData.data &&
          imageData.data.buffer &&
          imageData.data.buffer.byteLength > 0
        ) {
          assign(screen, {
            pixels: new Uint8ClampedArray(imageData.data),
            width: ctx.canvas.width,
            height: ctx.canvas.height,
          });
        }

        // Check if we need to record frames (before painting overlays)
        const isRecording =
          // typeof paintToStreamCanvas === "function" &&
          mediaRecorder?.state === "recording" &&
          mediaRecorderStartTime !== undefined;

        // 📸 Capture clean screenshot data BEFORE overlays are painted (only when needed)
        let cleanScreenshotData = null;
        if (needs$creenshot) {
          cleanScreenshotData = ctx.getImageData(
            0,
            0,
            ctx.canvas.width,
            ctx.canvas.height,
          );
        }

        // Paint overlays (but exclude tape progress from recordings)
        
        // console.log("🎨 Available overlay painters:", Object.keys(paintOverlays));
        if (!skipImmediateOverlays && paintOverlays["label"]) {
          paintOverlays["label"]();
        } else if (skipImmediateOverlays) {
          // console.log("🏷️ Skipping immediate label overlay painting (async mode)");
        } else {
          // Label overlay painter not found (no logging)
        }

        if (!skipImmediateOverlays && paintOverlays["qrOverlay"]) {
          paintOverlays["qrOverlay"]();
        } else if (skipImmediateOverlays) {
          // console.log("🔲 Skipping immediate QR overlay painting (async mode)");
        } else {
          // QR overlay painter not found (no logging)
        }

        // Paint tape progress bar immediately (not affected by async skip)
        if (paintOverlays["tapeProgressBar"]) {
          console.log("📼 Painting tape progress bar overlay (immediate)");
          paintOverlays["tapeProgressBar"]();
        } else if (content.tapeProgressBar) {
          console.log("📼 Rebuilding tape progress bar overlay due to timing issue");
          buildOverlay("tapeProgressBar", content.tapeProgressBar);
          if (paintOverlays["tapeProgressBar"]) {
            paintOverlays["tapeProgressBar"]();
          }
        }

        // Paint duration progress bar immediately (not affected by async skip)
        if (paintOverlays["durationProgressBar"]) {
          paintOverlays["durationProgressBar"]();
        } else if (content.durationProgressBar) {
          buildOverlay("durationProgressBar", content.durationProgressBar);
          if (paintOverlays["durationProgressBar"]) {
            paintOverlays["durationProgressBar"]();
          }
        }

        // 📼 Capture frame data AFTER HUD overlays but BEFORE tape progress bar (including HUD in recording)
        if (isRecording) {
          // ⚡ Capture every frame for full 60fps recording (no optimization to maintain quality)
          mediaRecorderFrameCount = (mediaRecorderFrameCount || 0) + 1;
          const frameTimestamp = performance.now() - mediaRecorderStartTime;
          const frameDataWithHUD = ctx.getImageData(
            0,
            0,
            ctx.canvas.width,
            ctx.canvas.height,
          );
          recordedFrames.push([frameTimestamp, frameDataWithHUD]);
          
          // Log timing info every 120 frames (2 seconds at 60fps)
          if (recordedFrames.length % 120 === 0) {
            const timeSinceStart = frameTimestamp / 1000;
            console.log(`📼 Frame ${recordedFrames.length} captured at ${timeSinceStart.toFixed(2)}s (60fps)`);
          }
        }

        //  Return clean screenshot data (without overlays)
        if (needs$creenshot) {
          needs$creenshot(cleanScreenshotData);
          needs$creenshot = null;
        }

        if (glaze.on) {
          ThreeD?.pasteTo(glazeCompositeCtx);
          glazeCompositeCtx.drawImage(canvas, 0, 0);
          Glaze.update(glazeComposite);
        }

        // TODO: Is this actually updating with a blank image at first? How to prevent the glaze.clear flicker? 2022.6.8
      }

      if (glaze.on) {
        Glaze.render(now, pen?.normalizedPosition(canvasRect));
      } else {
        Glaze.off();
        canvas.style.removeProperty("opacity");
      }

      // 🅱️ Draw anything from the system UI layer on top.

      const dpi = window.devicePixelRatio;

      // Only scale UI context when DPI changes
      if (!uiContextScaled || currentUiScale !== dpi) {
        if (uiContextScaled) {
          uiCtx.resetTransform();
        }
        uiCtx.scale(dpi, dpi);
        currentUiScale = dpi;
        uiContextScaled = true;
      }

      // Combine clear operations for better performance
      // Modified to avoid clearing overlay areas - label overlay is typically at (6,6) with small dimensions
      if (content.label) {
        // If there's a label overlay, clear around it instead of over it
        const labelX = content.label.x;
        const labelY = content.label.y;
        const labelW = content.label.img?.width || 0;
        const labelH = content.label.img?.height || 0;
        
        // Clear top-left area but avoid the label overlay region
        if (labelX > 0) {
          uiCtx.clearRect(0, 0, labelX, 64); // Left of label
        }
        if (labelY > 0) {
          uiCtx.clearRect(0, 0, 64, labelY); // Above label
        }
        if (labelX + labelW < 64) {
          uiCtx.clearRect(labelX + labelW, 0, 64 - (labelX + labelW), 64); // Right of label
        }
        if (labelY + labelH < 64) {
          uiCtx.clearRect(0, labelY + labelH, 64, 64 - (labelY + labelH)); // Below label
        }
      } else {
        // No label overlay, clear normally
        uiCtx.clearRect(0, 0, 64, 64); // Top left
      }
      
      uiCtx.clearRect(0, uiCtx.canvas.height / dpi - 64, 64, 64); // Bottom left
      uiCtx.clearRect(uiCtx.canvas.width / dpi - 64, 0, 64, 64); // Top right

      pen?.render(uiCtx, canvasRect); // ️ 🐭 Draw the cursor.

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

      // Note: We keep the transform scaled for the next frame to avoid redundant operations
    }

    if (
      pixelsDidChange ||
      needs$creenshot ||
      mediaRecorder?.state === "recording" ||
      (pen && pen.changedInPiece) ||
      content.tapeProgressBar // Force draw when tape progress bar is active
    ) {
      frameCached = false;
      if (pen) pen.changedInPiece = false;
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
  } // End of receivedChange function

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
    } else if (ext === "png" || ext === "webp" || ext === "gif") {
      // 🖼️ Images
      MIME = "image/png"; // PNG

      if (extension(filename) === "webp") {
        MIME = "image/webp";
      } else if (extension(filename) === "gif") {
        MIME = "image/gif";
      }

      if (data) {
        // Download locally if data is provided.
        if (data instanceof Blob) {
          // If data is already a blob, use it directly
          blob = data;
          object = URL.createObjectURL(blob);
        } else {
          // Otherwise, convert pixel data to blob
          blob = await bufferToBlob(data, MIME, modifiers);
          object = URL.createObjectURL(blob, { type: MIME });
        }
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
      const tapeData = data || (await Store.get("tape"));
      const tape = tapeData?.blob;

      // Restore frame data if available for WebP/Frame exports
      if (tapeData?.frames && recordedFrames.length === 0) {
        recordedFrames.length = 0; // Clear existing
        recordedFrames.push(...tapeData.frames);
        console.log(
          "📼 Restored",
          recordedFrames.length,
          "frames from cached video",
        );
      }

      // Use stored filename if available, otherwise fall back to provided filename
      if (tapeData?.filename) {
        filename = tapeData.filename;
      } else {
        // 🫲 Make sure the container matches the extension.
        const tapeMIME = tape?.type; // Check the tape's blob's type.
        if (tapeMIME?.indexOf("webm") > -1) {
          filename = filename.replace(".mp4", ".webm"); // Replaces ".mp4" set from `video`.
        } else {
          filename = filename.replace(".webm", ".mp4");
        }
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
    } else if (ext === "zip") {
      MIME = "application/zip";
      object = URL.createObjectURL(data, { type: MIME });
    }

    // Fetch download url from `/presigned-download-url?for=${filename}` if we
    // don't already have a blob string.

    if (object && !object.startsWith("blob:")) {
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

      // Add the link to DOM for better browser compatibility with large files
      document.body.appendChild(a);

      console.log(
        `💾 Triggering download: ${filename.split("/").pop()} (${blob ? `${Math.round((blob.size / 1024 / 1024) * 100) / 100} MB` : "unknown size"})`,
      );

      a.click();

      // Clean up after a delay to ensure download starts
      setTimeout(() => {
        document.body.removeChild(a);
        if (typeof a.href !== "string") URL.revokeObjectURL(a.href);
      }, 1000);
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

  // Track labelBack state in main thread (persists across worker reloads)
  let mainThreadLabelBack =
    sessionStorage.getItem("aesthetic-labelBack") === "true";

  window.onpopstate = function (e) {
    if (
      document.location.hash === "#debug" ||
      document.location.hash === "#nodebug"
    ) {
      document.location.reload();
    }

    const sluggy = slug(document.location.href);
    if (sluggy === "prompt") keyboard?.input.focus();

    const parsed = parse(sluggy || window.acSTARTING_PIECE);

    // Restore labelBack state for history navigation
    if (mainThreadLabelBack) {
      console.log("🔗 Main thread restoring labelBack for history navigation");
      parsed.labelBack = true;
      // Clear the state after using it for navigation
      mainThreadLabelBack = false;
      sessionStorage.removeItem("aesthetic-labelBack");
      console.log(
        "🔗 Main thread: Cleared labelBack after using it for history navigation",
      );
    }

    send({
      type: "history-load",
      content: parsed,
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
    
    // Process multiple files if dropped together
    if (files.length > 0) {
      console.log(`💧 Dropped ${files.length} file(s)`);
      
      // Check for ALS + WAV combination
      let alsFile = null;
      let wavFile = null;
      
      // Scan all files to identify types
      for (let i = 0; i < files.length; i++) {
        const file = files[i];
        const ext = extension(file.name);
        console.log(`💧 File ${i + 1}: ${file.name} (${ext})`);
        
        if (ext === "als" && !alsFile) {
          alsFile = file;
        } else if (ext === "wav" && !wavFile) {
          wavFile = file;
        }
      }
      
      // If we have both ALS and WAV, process them as a pair
      if (alsFile && wavFile) {
        console.log("🎵🔊 Processing ALS + WAV combination:", alsFile.name, "+", wavFile.name);
        
        // Process ALS file first
        await processDroppedFile(alsFile);
        
        // Then process WAV file
        await processDroppedFile(wavFile);
        
        return; // Exit early - we've handled the multi-file drop
      }
      
      // Otherwise, process files individually (existing behavior)
      for (let i = 0; i < files.length; i++) {
        await processDroppedFile(files[i]);
      }
    }
  });

  // Extract file processing logic into reusable function
  async function processDroppedFile(file) {
    const ext = extension(file.name);
    console.log("💧 Processing:", file.name, ext);
    
    // 🗒️ Source code file.
    if (ext === "mjs" || ext === "lisp") {
        const reader = new FileReader();
        reader.onload = function (e) {
          send({
            type: "dropped:piece",
            content: {
              name: file.name.replace(/\.(mjs|lisp)$/, ""),
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
      // 🔊 Audio file (.wav)
      } else if (ext === "wav") {
        const reader = new FileReader();
        reader.onload = async function (e) {
          try {
            console.log("🔊 Dropped WAV file:", file.name, `(${e.target.result.byteLength} bytes)`);
            
            // Create a unique ID for this WAV file
            const wavId = "dropped-wav-" + file.name.replace(/\.wav$/, "") + "-" + performance.now();
            const arrayBuffer = e.target.result;
            const fileSize = arrayBuffer.byteLength; // Capture size before storing
            
            // Store the WAV data in the sfx cache for decoding and playback
            sfx[wavId] = arrayBuffer;
            
            // Trigger immediate decoding
            await decodeSfx(wavId);
            
            send({
              type: "dropped:wav",
              content: {
                name: file.name.replace(/\.wav$/, ""),
                originalName: file.name,
                size: fileSize,
                id: wavId // Include the audio ID for playback
              },
            });
          } catch (error) {
            console.error("❌ Failed to process WAV file:", error);
          }
        };
        reader.readAsArrayBuffer(file);
      // 🎵 Ableton Live Set file (ZIP archive containing XML)
      } else if (ext === "als") {
        
        // Parse XML and extract key project structure using robust regex patterns
        function parseAbletonProject(xmlString) {
          try {
            console.log("🎵 BIOS parsing ALS XML data:", xmlString.length, "characters");
            
            const structure = {
              version: "Unknown",
              creator: "Unknown", 
              tracks: [],
              tempo: "Unknown",
              sceneCount: 0,
              sampleRate: "Unknown",
              lastModDate: "Unknown"
            };
            
            // Extract Ableton version info
            const abletonMatch = xmlString.match(/<Ableton[^>]*MajorVersion="([^"]*)"[^>]*MinorVersion="([^"]*)"[^>]*Creator="([^"]*)"/);
            if (abletonMatch) {
              structure.version = `${abletonMatch[1]}.${abletonMatch[2]}`;
              structure.creator = abletonMatch[3];
            }
            
            // Extract sample rate for accurate timing calculations
            const sampleRateMatch = xmlString.match(/<SampleRate[^>]*Value="([^"]*)"/);
            if (sampleRateMatch) {
              structure.sampleRate = parseFloat(sampleRateMatch[1]) || 44100;
            }
            
            // Extract last modified date
            const lastModMatch = xmlString.match(/<LastModDate[^>]*Value="([^"]*)"/);
            if (lastModMatch) {
              structure.lastModDate = lastModMatch[1];
            }
            
            // Extract tempo - Enhanced with multiple strategies
            let tempo = "Unknown";
            
            // Strategy 1: Look for global tempo in LiveSet
            const globalTempoMatch = xmlString.match(/<LiveSet[^>]*>[\s\S]*?<MasterTrack[^>]*>[\s\S]*?<DeviceChain[^>]*>[\s\S]*?<Tempo[^>]*>[\s\S]*?<Manual[^>]*Value="([^"]*)"[\s\S]*?<\/Tempo>/);
            if (globalTempoMatch) {
              tempo = parseFloat(globalTempoMatch[1]) || "Unknown";
              console.log(`🎵 BIOS found global tempo: ${tempo} BPM`);
            }
            
            // Strategy 2: Manual tempo value (enhanced pattern)
            if (tempo === "Unknown") {
              const tempoMatch = xmlString.match(/<Manual[^>]*Value="([^"]*)"/);
              if (tempoMatch) {
                const tempoValue = parseFloat(tempoMatch[1]);
                // Only accept reasonable tempo values (between 60-300 BPM)
                if (tempoValue && tempoValue >= 60 && tempoValue <= 300) {
                  tempo = tempoValue;
                  console.log(`🎵 BIOS found manual tempo: ${tempo} BPM`);
                }
              }
            }
            
            // Strategy 3: MasterTrack tempo (enhanced)
            if (tempo === "Unknown") {
              const masterTempoMatch = xmlString.match(/<MasterTrack[^>]*>[\s\S]*?<Tempo[^>]*>[\s\S]*?<Manual[^>]*Value="([^"]*)"[\s\S]*?<\/Tempo>/);
              if (masterTempoMatch) {
                tempo = parseFloat(masterTempoMatch[1]) || "Unknown";
                console.log(`🎵 BIOS found MasterTrack tempo: ${tempo} BPM`);
              }
            }
            
            structure.tempo = tempo;
            
            // Count scenes
            const sceneMatches = xmlString.match(/<Scene[^>]*>/g);
            structure.sceneCount = sceneMatches ? sceneMatches.length : 0;
            
            // Extract tracks - simplified for now to avoid complexity
            const trackTypes = ['GroupTrack', 'MidiTrack', 'AudioTrack', 'ReturnTrack'];
            console.log("🎵 BIOS starting track detection...");
            
            trackTypes.forEach(trackType => {
              const trackRegex = new RegExp(`<${trackType}[^>]*>([\\s\\S]*?)</${trackType}>`, 'g');
              let match;
              
              while ((match = trackRegex.exec(xmlString)) !== null) {
                const trackContent = match[1];
                
                // Extract track name
                const nameMatch = trackContent.match(/<(?:EffectiveName|UserName)[^>]*Value="([^"]*)"/);
                const trackName = nameMatch ? nameMatch[1] : `${trackType} ${structure.tracks.length + 1}`;
                
                console.log(`🎵 BIOS found track: "${trackName}" (${trackType})`);
                
                // Extract MIDI notes for MIDI tracks
                let midiNotes = [];
                let clips = [];
                
                if (trackType === "MidiTrack") {
                  // Look for MIDI notes in clips
                  const clipMatches = trackContent.match(/<MidiClip[^>]*>[\s\S]*?<\/MidiClip>/g);
                  if (clipMatches) {
                    clipMatches.forEach((clipContent, clipIndex) => {
                      // Extract notes from this clip
                      const noteMatches = clipContent.match(/<KeyTrack[^>]*>[\s\S]*?<Notes>[\s\S]*?<\/Notes>/g);
                      if (noteMatches) {
                        noteMatches.forEach(noteSection => {
                          const individualNotes = noteSection.match(/<MidiNoteEvent[^>]*Time="([^"]*)"[^>]*Duration="([^"]*)"[^>]*Velocity="([^"]*)"[^>]*>/g);
                          if (individualNotes) {
                            individualNotes.forEach(noteEvent => {
                              const timeMatch = noteEvent.match(/Time="([^"]*)"/);
                              const durationMatch = noteEvent.match(/Duration="([^"]*)"/);
                              const velocityMatch = noteEvent.match(/Velocity="([^"]*)"/);
                              
                              if (timeMatch && durationMatch && velocityMatch) {
                                midiNotes.push({
                                  time: parseFloat(timeMatch[1]) || 0,
                                  duration: parseFloat(durationMatch[1]) || 0,
                                  velocity: parseFloat(velocityMatch[1]) || 0,
                                  clip: clipIndex
                                });
                              }
                            });
                          }
                        });
                      }
                      
                      // Extract clip info
                      const clipNameMatch = clipContent.match(/<Name[^>]*Value="([^"]*)"/);
                      const clipTimeMatch = clipContent.match(/<CurrentStart[^>]*Value="([^"]*)"/);
                      const clipLengthMatch = clipContent.match(/<Length[^>]*Value="([^"]*)"/);
                      
                      clips.push({
                        name: clipNameMatch ? clipNameMatch[1] : `Clip ${clipIndex + 1}`,
                        start: clipTimeMatch ? parseFloat(clipTimeMatch[1]) : 0,
                        length: clipLengthMatch ? parseFloat(clipLengthMatch[1]) : 0
                      });
                    });
                  }
                } else if (trackType === "AudioTrack") {
                  // Look for audio clips
                  const audioClipMatches = trackContent.match(/<AudioClip[^>]*>[\s\S]*?<\/AudioClip>/g);
                  if (audioClipMatches) {
                    audioClipMatches.forEach((clipContent, clipIndex) => {
                      const clipNameMatch = clipContent.match(/<Name[^>]*Value="([^"]*)"/);
                      const clipTimeMatch = clipContent.match(/<CurrentStart[^>]*Value="([^"]*)"/);
                      const clipLengthMatch = clipContent.match(/<Length[^>]*Value="([^"]*)"/);
                      
                      clips.push({
                        name: clipNameMatch ? clipNameMatch[1] : `Audio Clip ${clipIndex + 1}`,
                        start: clipTimeMatch ? parseFloat(clipTimeMatch[1]) : 0,
                        length: clipLengthMatch ? parseFloat(clipLengthMatch[1]) : 0
                      });
                    });
                  }
                }
                
                structure.tracks.push({
                  name: trackName.replace(/^[#\s]*/, ''), // Remove leading # and spaces
                  type: trackType,
                  clipCount: clips.length,
                  clips: clips,
                  midiNotes: midiNotes,
                  noteCount: midiNotes.length
                });
              }
            });
            
            console.log(`🎵 BIOS ✅ Parsing complete: ${structure.tracks.length} tracks found`);
            return structure;
          } catch (e) {
            console.warn("🎵 BIOS Failed to parse XML:", e);
            return null;
          }
        }
        
        const reader = new FileReader();
        reader.onload = async function (e) {
          try {
            const data = e.target.result;
            const uint8Data = new Uint8Array(data);
            
            console.log("🔍 ALS file signature:", Array.from(uint8Data.slice(0, 4)).map(b => b.toString(16).padStart(2, '0')).join(' '));
            
            // Check if it's a ZIP file (signature: 50 4b 03 04)
            if (uint8Data[0] === 0x50 && uint8Data[1] === 0x4b && uint8Data[2] === 0x03 && uint8Data[3] === 0x04) {
              console.log("📦 ALS file is a ZIP archive, loading JSZip...");
              
              // Load JSZip if not already loaded
              if (!window.JSZip) await loadJSZip();
              
              const zip = new window.JSZip();
              const zipData = await zip.loadAsync(data);
              
              console.log("📂 ZIP contents:", Object.keys(zipData.files));
              
              // Look for the main ALS XML file (usually named after the project)
              let xmlContent = null;
              let xmlFileName = null;
              
              // Try to find XML files in the ZIP
              for (const fileName of Object.keys(zipData.files)) {
                const file = zipData.files[fileName];
                if (!file.dir && (fileName.endsWith('.xml') || fileName.endsWith('.als') || fileName === 'Project.xml')) {
                  console.log(`📄 Found XML file: ${fileName}`);
                  try {
                    // Try different extraction methods
                    console.log("🔍 Trying string extraction...");
                    xmlContent = await file.async("string");
                    console.log(`🔍 String extraction result (first 200 chars): ${xmlContent.substring(0, 200)}`);
                    
                    // If the content looks binary/garbled, try extracting as uint8array and decompress
                    if (xmlContent.charCodeAt(0) > 127 || !xmlContent.includes('<')) {
                      console.log("🔍 Content appears binary, trying binary extraction and decompression...");
                      const binaryData = await file.async("uint8array");
                      console.log(`🔍 Binary data length: ${binaryData.length}`);
                      
                      try {
                        // Try gzip decompression with pako
                        console.log("🔍 Trying gzip decompression...");
                        const decompressed = pako.inflate(binaryData, { to: 'string' });
                        console.log(`🔍 Gzip decompression result (first 200 chars): ${decompressed.substring(0, 200)}`);
                        if (decompressed.includes('<')) {
                          xmlContent = decompressed;
                        }
                      } catch (e) {
                        console.log("🔍 Gzip failed, trying deflate...");
                        try {
                          const decompressed = pako.inflateRaw(binaryData, { to: 'string' });
                          console.log(`🔍 Deflate decompression result (first 200 chars): ${decompressed.substring(0, 200)}`);
                          if (decompressed.includes('<')) {
                            xmlContent = decompressed;
                          }
                        } catch (e2) {
                          console.warn("❌ Both gzip and deflate decompression failed:", e2);
                        }
                      }
                    }
                    
                    xmlFileName = fileName;
                    break;
                  } catch (e) {
                    console.warn(`❌ Failed to extract ${fileName}:`, e);
                  }
                }
              }
              
              // If no XML found, try the first non-directory file
              if (!xmlContent) {
                for (const fileName of Object.keys(zipData.files)) {
                  const file = zipData.files[fileName];
                  if (!file.dir) {
                    console.log(`📄 Trying file: ${fileName}`);
                    const content = await file.async("string");
                    if (content.includes('<?xml') || content.includes('<Ableton')) {
                      xmlContent = content;
                      xmlFileName = fileName;
                      break;
                    }
                  }
                }
              }
              
              if (xmlContent) {
                console.log(`✅ Successfully extracted XML from ${xmlFileName} (${xmlContent.length} chars)`);
                console.log("🎵 🚀 BIOS sending dropped:als event with content:", {
                  name: file.name.replace(/\.als$/, ""),
                  xmlData: xmlContent.length + " chars"
                });
                send({
                  type: "dropped:als",
                  content: {
                    name: file.name.replace(/\.als$/, ""),
                    xmlData: xmlContent,
                  },
                });
              } else {
                console.error("❌ No XML content found in ALS ZIP file");
              }
            } else {
              // Fall back to compression methods for non-ZIP ALS files
              // Load pako if not already loaded
              if (!window.pako) {
                console.log("📦 Loading pako compression library for ALS file...");
                await new Promise((resolve, reject) => {
                  const script = document.createElement("script");
                  script.src =
                    "https://cdn.jsdelivr.net/npm/pako@2.1.0/dist/pako.min.js";
                  script.onload = resolve;
                  script.onerror = reject;
                  document.head.appendChild(script);
                });
              }
              
              let decompressedData = null;
              
              // Try different decompression methods
              const methods = [
                { name: 'zlib (inflate)', fn: () => pako.inflate(uint8Data, { to: 'string' }) },
                { name: 'gzip (ungzip)', fn: () => pako.ungzip(uint8Data, { to: 'string' }) },
                { name: 'raw deflate (inflateRaw)', fn: () => pako.inflateRaw(uint8Data, { to: 'string' }) }
              ];
              
              for (const method of methods) {
                try {
                  console.log(`🧪 Trying ${method.name}...`);
                  decompressedData = method.fn();
                  console.log(`✅ Successfully decompressed using ${method.name}`);
                  break;
                } catch (err) {
                  console.log(`❌ ${method.name} failed:`, err.message);
                }
              }
              
              if (decompressedData) {
                // Parse the XML data into a structured object
                const parsedProject = parseAbletonProject(decompressedData);
                
                console.log("🎵 🚀 BIOS sending dropped:als event with parsed content:", {
                  name: file.name.replace(/\.als$/, ""),
                  project: parsedProject ? `${parsedProject.tracks.length} tracks, ${parsedProject.tempo} BPM` : "parsing failed"
                });
                
                send({
                  type: "dropped:als",
                  content: {
                    name: file.name.replace(/\.als$/, ""),
                    xmlData: decompressedData,
                    project: parsedProject, // Send parsed structure
                  },
                });
              } else {
                console.error("❌ All decompression methods failed for ALS file");
                // Try to read as plain text in case it's not compressed
                try {
                  const textData = new TextDecoder().decode(uint8Data);
                  if (textData.includes('<?xml') || textData.includes('<Ableton')) {
                    console.log("📄 File appears to be uncompressed XML");
                    
                    // Parse the XML data into a structured object
                    const parsedProject = parseAbletonProject(textData);
                    
                    console.log("🎵 🚀 BIOS sending dropped:als event with uncompressed parsed content:", {
                      name: file.name.replace(/\.als$/, ""),
                      project: parsedProject ? `${parsedProject.tracks.length} tracks, ${parsedProject.tempo} BPM` : "parsing failed"
                    });
                    
                    send({
                      type: "dropped:als",
                      content: {
                        name: file.name.replace(/\.als$/, ""),
                        xmlData: textData,
                        project: parsedProject, // Send parsed structure
                      },
                    });
                  } else {
                    console.error("File doesn't appear to be XML either");
                  }
                } catch (textErr) {
                  console.error("Failed to read as text:", textErr);
                }
              }
            }
          } catch (error) {
            console.error("Failed to process ALS file:", error);
          }
        };
        reader.readAsArrayBuffer(file);
    }
  }

  // Instantly decode the audio before playback if it hasn't been already.
  async function decodeSfx(sound) {
    // If sound is already being decoded, wait a bit and return
    if (decodingInProgress.has(sound)) {
      // Wait a moment and check again
      await new Promise((resolve) => setTimeout(resolve, 10));
      return sfx[sound];
    }

    if (sfx[sound] instanceof ArrayBuffer) {
      // Mark as being decoded to prevent concurrent decode attempts
      decodingInProgress.add(sound);

      // Ensure audioContext is initialized before trying to decode
      if (!audioContext) {
        console.log("🔊 Initializing audio context for WAV decoding...");
        if (activateSound) {
          activateSound();
          // Wait a moment for audio context to initialize
          await new Promise((resolve) => setTimeout(resolve, 100));
        }
      }

      let audioBuffer;
      try {
        const buf = sfx[sound];
        sfx[sound] = null;
        if (buf && audioContext) {
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

      pendingSfxQueue.forEach(
        ({ id, soundData, options, completed, queuedAt }) => {
          // Add timestamp if not present
          const queueTime = queuedAt || currentTime;
          const timeInQueue = currentTime - queueTime;

          // Only play sounds that have been loaded into the sfx cache
          if (sfx[soundData] && !(sfx[soundData] instanceof ArrayBuffer)) {
            playSfx(id, soundData, options, completed);
          } else if (timeInQueue < 5000) {
            // Only retry for 5 seconds
            remaining.push({
              id,
              soundData,
              options,
              completed,
              queuedAt: queueTime,
            });
          }
        },
      );

      pendingSfxQueue = remaining;

      // If there are still sounds waiting, check again soon
      if (pendingSfxQueue.length > 0) {
        setTimeout(processPendingSfx, 100);
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
} // End of boot function

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
