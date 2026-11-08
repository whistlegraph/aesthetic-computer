// Host the original Oskiewar engine in an opaque frame. Its implementation
// stays in the private product; this adapter only supplies the browser host.
const engineFiles = ["oskiewar.js", "scene3d.mjs", "scene3d-webgl.mjs", "oskiewar-sfx.mjs", "ComicRelief-Regular.woff2"];
let sources;
export async function loadEngineSources() {
  return sources ||= Promise.all(engineFiles.map(async name => {
    const response = await fetch("/oskie/engine/" + name, {credentials: "omit"});
    if (!response.ok) throw Error("Could not load Oskiewar engine: " + name);
    return [name, await (name.endsWith(".woff2") ? response.arrayBuffer() : response.text())];
  })).then(Object.fromEntries).catch(error => { sources = undefined; throw error; });
}

export function engineBootstrap() {
  let port, running = false;
  const report = error => {
    running = false;
    port?.postMessage({type: "error", error: String(error?.message || error).slice(0, 2000)});
  };
  addEventListener("unhandledrejection", event => { event.preventDefault(); report(event.reason); });
  addEventListener("error", event => report(event.error || event.message));
  addEventListener("message", async event => {
    if (event.source !== parent || event.data?.type !== "start" || port || !event.ports[0]) return;
    port = event.ports[0];
    try {
      const {bundle: game, engineSources} = event.data;
      const blob = text => URL.createObjectURL(new Blob([text], {type: "text/javascript"}));
      const sceneURL = blob(engineSources["scene3d.mjs"]);
      const rendererURL = blob(engineSources["scene3d-webgl.mjs"].replace('"./scene3d.mjs"', JSON.stringify(sceneURL)));
      const {WebGLOskiewarScene3D} = await import(rendererURL);
      // Match the face whose advances api.textWidth uses. Load before boot so
      // even the first frame has the same metrics as the original browser host.
      const font = new FontFace("Comic Relief", engineSources["ComicRelief-Regular.woff2"]);
      document.fonts.add(await font.load());
      const canvas = document.createElement("canvas");
      canvas.style.cssText = "display:block;width:100%;height:100%;touch-action:none";
      document.body.style.cssText = "margin:0;overflow:hidden;background:#080c16";
      document.body.append(canvas);
      const output = canvas.getContext("2d"), surface = new OffscreenCanvas(1280, 720);
      const backdrop = new OffscreenCanvas(1280, 720), ctx = backdrop.getContext("2d");
      const textSurface = new OffscreenCanvas(1280, 720), textContext = textSurface.getContext("2d");
      const renderer = new WebGLOskiewarScene3D(surface, {maxTriangles: 65536});
      let view, time = 0, ticks = 0, paints = 0, audio;
      const {createOskiewarSfx} = await import(blob(engineSources["oskiewar-sfx.mjs"]));
      const sfx = createOskiewarSfx();
      function resize() {
        view = {width: 1080 * innerWidth / Math.max(1, innerHeight), height: 1080};
        canvas.width = Math.min(1920, Math.max(1, innerWidth));
        canvas.height = Math.max(1, Math.round(canvas.width * innerHeight / Math.max(1, innerWidth)));
        surface.width = canvas.width; surface.height = canvas.height;
        backdrop.width = canvas.width; backdrop.height = canvas.height;
        textSurface.width = canvas.width; textSurface.height = canvas.height;
        ctx.setTransform(canvas.width / view.width, 0, 0, canvas.height / 1080, 0, 0);
        textContext.setTransform(canvas.width / view.width, 0, 0, canvas.height / 1080, 0, 0);
        renderer.setLogicalSize(view.width, 1080);
      }
      resize(); addEventListener("resize", resize);
      globalThis.__oskiewarKeys = new Set();
      globalThis.__oskiewarTouch = {pointer: {x: 0, y: 0, active: false, down: false}};
      addEventListener("keydown", event => {
        globalThis.__oskiewarKeys.add(event.code);
        if (["Space", "Tab", "ArrowUp", "ArrowDown", "ArrowLeft", "ArrowRight"].includes(event.code)) event.preventDefault();
      });
      addEventListener("keyup", event => globalThis.__oskiewarKeys.delete(event.code));
      addEventListener("blur", () => { globalThis.__oskiewarKeys.clear(); globalThis.__oskiewarTouch.pointer.down = false; });
      for (const name of ["pointerdown", "pointerup", "pointermove", "pointercancel"]) canvas.addEventListener(name, event => {
        const p = globalThis.__oskiewarTouch.pointer;
        Object.assign(p, {x: event.clientX / innerHeight * 1080, y: event.clientY / innerHeight * 1080, active: true});
        if (name !== "pointermove") p.down = name === "pointerdown";
      });
      const unlockAudio = () => { audio ||= new AudioContext(); void audio.resume(); void sfx.unlock(); };
      addEventListener("pointerdown", unlockAudio); addEventListener("keydown", unlockAudio);
      const tone = (frequency, seconds = .12, volume = .05, pan = 0) => {
        if (!audio || !Number.isFinite(frequency)) return;
        const osc = audio.createOscillator(), gain = audio.createGain(), stereo = audio.createStereoPanner();
        osc.frequency.value = Math.max(20, Math.min(20000, frequency));
        const duration = Math.max(.01, Math.min(2, Number(seconds) || .12));
        gain.gain.setValueAtTime(Math.max(.001, Math.min(.2, volume)), audio.currentTime);
        gain.gain.exponentialRampToValueAtTime(.001, audio.currentTime + duration);
        stereo.pan.value = Math.max(-1, Math.min(1, pan));
        osc.connect(gain).connect(stereo).connect(audio.destination); osc.start(); osc.stop(audio.currentTime + duration);
      };
      const urls = new Map(), pending = new Set();
      const fileURL = path => {
        if (urls.has(path)) return urls.get(path);
        if (pending.has(path)) throw Error("Cyclic import: " + path);
        const file = game.files[path];
        if (!file) throw Error("Missing bundled file: " + path);
        pending.add(path);
        let body = file.text ?? Uint8Array.from(atob(file.base64), c => c.charCodeAt(0));
        if (/\.m?js$/.test(path)) body = body.replace(/(\bfrom\s*|\bimport\s*\(?\s*)(["'])(\.\.?\/[^"']+)\2/g,
          (match, lead, quote, spec) => lead + quote + fileURL(new URL(spec, "https://bundle.invalid/" + path).pathname.slice(1)) + quote);
        const url = URL.createObjectURL(new Blob([body], {type: /\.m?js$/.test(path) ? "text/javascript" : file.type}));
        urls.set(path, url); pending.delete(path); return url;
      };
      const mod = await import(fileURL(game.manifest.entry));
      const noOp = () => {};
      const buttons = ["A", "B", "X", "Y", "LeftShoulder", "RightShoulder", "LeftTrigger", "RightTrigger", "View", "Menu", "LeftStick", "RightStick", "ArrowUp", "ArrowDown", "ArrowLeft", "ArrowRight"];
      const pad = (index = 0) => {
        let raw;
        try { raw = navigator.getGamepads()[index]; } catch {}
        if (raw) return {connected: true, down: raw.buttons.flatMap((b, i) => b.pressed ? [buttons[i]] : []), leftX: raw.axes[0] || 0, leftY: -(raw.axes[1] || 0), rightX: raw.axes[2] || 0, rightY: -(raw.axes[3] || 0)};
        return {connected: index === 0, down: index ? [] : [...globalThis.__oskiewarKeys].map(k => ({KeyW:"ArrowUp",KeyA:"ArrowLeft",KeyS:"ArrowDown",KeyD:"ArrowRight",KeyJ:"X",KeyK:"A",KeyI:"Y",Space:"B",Enter:"A",Escape:"Menu",Tab:"View"})[k] || k), leftX:0,leftY:0,rightX:0,rightY:0};
      };
      const rgb = (r = 255, g = r, b = r) => `rgb(${r},${g},${b})`;
      const box = (x,y,w,h,r,g,b) => {ctx.fillStyle=rgb(r,g,b);ctx.fillRect(x,y,w,h);};
      const write = (text,x,y,size=32,r,g,b) => {
        textContext.fillStyle=rgb(r,g,b);
        textContext.font=`${size}px "Comic Relief"`;
        textContext.textBaseline="alphabetic";
        const sx=canvas.width/view.width, sy=canvas.height/1080;
        // Original host baseline and multiline spacing (font ascent: 2160/2048).
        String(text).split("\n").forEach((line,i) => textContext.fillText(line,
          Math.round(x*sx)/sx, Math.round((y+size*2160/2048+i*size*1.15)*sy)/sy));
      };
      const host = {
        runtime: () => ({monotonicUs: time * 1e6, unixMs: Date.now(), simCount: ticks, paintCount: paints, renderAlpha: 1}),
        gamepad: pad, controllers: () => [], capabilities: () => ({platform:"web",inputFamily:"keyboard"}), telemetry:noOp,
        gameSignal:sfx.signal, saveReplay:async()=>false, publishLive:noOp, analytics:noOp,
        ac:()=>({fighters:[],player:{handle:game.manifest.author || "",colors:[],anonymous:true},moodHandle:"",mood:"",source:"oskie"}),
        drum:sfx.drum,
        synth:tone, oscillator:noOp, oscillatorStop:noOp, skateAudio:noOp,
        wipe:(r,g,b)=>box(0,0,view.width,1080,r,g,b), box,
        line:(x,y,x2,y2,width,r,g,b)=>{ctx.strokeStyle=rgb(r,g,b);ctx.lineWidth=width;ctx.beginPath();ctx.moveTo(x,y);ctx.lineTo(x2,y2);ctx.stroke();},
        triangle:(x,y,x2,y2,x3,y3,r,g,b)=>{ctx.fillStyle=rgb(r,g,b);ctx.beginPath();ctx.moveTo(x,y);ctx.lineTo(x2,y2);ctx.lineTo(x3,y3);ctx.fill();},
        triangle3d:(...values)=>renderer.triangle3d(...values),
        postEffects:undefined,frame:undefined,write,systemWrite:write,comicWrite:write,gameView:()=>view,
        downstreamGame:{manifest:game.manifest,files:game.files,mod,asset:fileURL},
      };
      // Compile trusted engine source as a module; no eval / unsafe-eval.
      const engineURL = blob(`export default function(host) { let {${Object.keys(host).join(",")}} = host;\n${engineSources["oskiewar.js"]}\nreturn {boot, sim, paint, fault:()=>downstreamFault || (clientError ? "Engine stopped" : "")};\n}`);
      const piece = (await import(engineURL)).default(host);
      piece.boot();
      if (piece.fault()) throw Error(piece.fault());
      running = true;
      let previous = performance.now(), accumulator = 0;
      function frame(stamp) {
        if (!running) return;
        try {
          accumulator += Math.min(.1, (stamp - previous) / 1000); previous = stamp;
          while (accumulator >= 1/60) { time += 1/60; ticks++; piece.sim(); accumulator -= 1/60; }
          textContext.clearRect(0, 0, view.width, 1080);
          renderer.beginFrame(); piece.paint(); paints++;
          if (piece.fault()) throw Error(piece.fault());
          renderer.present();
          // Text is the final stratum in the original host, above depth-tested
          // world and HUD geometry. Rebuild the captured canvas so old text
          // cannot stick to the backdrop when a game doesn't wipe every frame.
          output.clearRect(0, 0, canvas.width, canvas.height);
          output.drawImage(backdrop, 0, 0);
          output.drawImage(surface, 0, 0);
          output.drawImage(textSurface, 0, 0);
          if (["default","none","crosshair","pointer"].includes(globalThis.__oskiewarCursor)) canvas.style.cursor=globalThis.__oskiewarCursor;
          requestAnimationFrame(frame);
        } catch (error) { report(error); }
      }
      port.onmessage = async ({data}) => {
        if (data.type !== "capture" || !running) return;
        canvas.toBlob(async captured => {
          if (captured && captured.size < 650000) port.postMessage({type:"frame",bytes:await captured.arrayBuffer()});
        }, "image/png");
      };
      requestAnimationFrame(frame);
      port.postMessage({type:"ready"});
    } catch (error) { report(error); }
  });
}

export function engineSandboxDocument(nonce) {
  const source = `(${engineBootstrap.toString()})()`.replace(/<\//g, "<\\/");
  return `<!doctype html><html><head><meta charset="utf-8"><meta http-equiv="Content-Security-Policy" content="default-src 'none'; script-src 'nonce-${nonce}' blob:; worker-src blob:; img-src blob: data:; media-src blob:; font-src blob:; style-src 'unsafe-inline'; connect-src blob:; base-uri 'none'; form-action 'none'"></head><body><script nonce="${nonce}">${source}</script></body></html>`;
}
