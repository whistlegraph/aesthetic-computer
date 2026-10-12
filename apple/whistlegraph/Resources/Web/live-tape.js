// Injected into the AirPlay runtime's piece frame with the shared AC canvas-tape
// encoder. Records the piece live, 1920×1080 with its sound, and hands the MP4
// fragments to native, which serves them to the Apple TV as an HLS stream.
(() => {
  if (window === window.top || location.origin !== 'https://aesthetic.computer') return;
  // This runtime is only a camera: nothing it plays reaches the phone's speaker.
  // Every connection to a speaker passes through a silent gain, while AC's own
  // stream destination (what its tapes record) still carries the sound.
  let pieceAudio = null;
  const Context = window.AudioContext;
  const createDestination = Context.prototype.createMediaStreamDestination;
  Context.prototype.createMediaStreamDestination = function () {
    const destination = createDestination.call(this);
    if (!pieceAudio) { pieceAudio = destination.stream; mix?.(); }
    return destination;
  };
  const silenced = new WeakMap(), connect = AudioNode.prototype.connect;
  AudioNode.prototype.connect = function (target, ...rest) {
    if (target instanceof AudioDestinationNode) {
      let silent = silenced.get(target);
      if (!silent) { silent = target.context.createGain(); silent.gain.value = 0; connect.call(silent, target); silenced.set(target, silent); }
      target = silent;
    }
    return connect.call(this, target, ...rest);
  };

  // The recorder needs its audio track from the first frame, before the piece
  // has started any sound, so it records a mix the piece joins once it does.
  let mixer = null, mix = null;
  function audioTrack() {
    if (!mixer) {
      const context = new Context(), output = createDestination.call(context);
      // A silent path to the speaker keeps WebKit rendering this context.
      const pulse = context.createGain(); pulse.gain.value = 0; connect.call(pulse, context.destination);
      mixer = {context, output};
      mix = () => { if (!pieceAudio) return; const source = context.createMediaStreamSource(pieceAudio); connect.call(source, output); connect.call(source, pulse); mix = null; };
      if (pieceAudio) mix();
    }
    mixer.context.resume?.();
    return mixer.output.stream.getAudioTracks();
  }

  const W = 1920, H = 1080, FPS = 30;
  const canvas = document.createElement('canvas'); canvas.width = W; canvas.height = H;
  const ctx = canvas.getContext('2d');
  function draw() {
    ctx.fillStyle = '#000'; ctx.fillRect(0, 0, W, H); ctx.imageSmoothingEnabled = false;
    const layers = [document.querySelector('canvas[data-ac-visit-canvas]'), ...document.querySelectorAll('canvas[data-type="webgl-composite"],canvas[data-type="webgpu"],canvas[data-type="hd"]')];
    for (const layer of layers) {
      if (!layer?.width || getComputedStyle(layer).display === 'none') continue;
      const scale = Math.min(W / layer.width, H / layer.height), w = layer.width * scale, h = layer.height * scale;
      ctx.drawImage(layer, (W - w) / 2, (H - h) / 2, w, h);
    }
  }

  const post = body => window.webkit.messageHandlers.whistlegraph.postMessage({action: 'liveTape', ...body});
  let capture = null;
  window.whistlegraphLiveTape = {
    start(generation, slice = 250) {
      this.stop();
      const current = capture = {generation, timer: null, recorder: null, delivery: Promise.resolve()};
      draw();
      const recorder = current.recorder = createCanvasTapeRecorder(canvas, {fps: FPS, audioTracks: audioTrack(), videoBitsPerSecond: 6e6, mp4Only: true});
      recorder.ondataavailable = event => {
        if (!event.data.size) return;
        current.delivery = current.delivery.then(async () => {
          if (capture !== current) return;
          const bytes = new Uint8Array(await event.data.arrayBuffer());
          for (let i = 0; i < bytes.length; i += 192000) {
            let text = ''; for (const byte of bytes.subarray(i, i + 192000)) text += String.fromCharCode(byte);
            post({generation, kind: 'chunk', data: btoa(text)});
          }
        });
      };
      recorder.onerror = event => post({generation, kind: 'error', error: String(event.error?.message || 'encoding failed').slice(0, 120)});
      current.timer = setInterval(draw, 1000 / FPS);
      recorder.start(slice);
      post({generation, kind: 'started', type: recorder.mimeType});
    },
    stop() {
      const current = capture; capture = null;
      if (!current) return;
      clearInterval(current.timer);
      const recorder = current.recorder;
      if (recorder) { recorder.ondataavailable = null; if (recorder.state !== 'inactive') recorder.stop(); }
    },
  };
})();
