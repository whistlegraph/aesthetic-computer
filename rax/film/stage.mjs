// 🎬 The stage: the page that films. Loaded by client.mjs only when the
// address says ?film. It watches the room without a body, puts the camera
// where the director says (shoot.mjs, over a control socket), composes the
// frame with titles and captions, and records it.
import { pose } from './camera.mjs';
import { glyphRows } from '../handle-font.mjs';

// AC's font_1 on a 2D canvas: 6 columns, 10 rows, one square per pixel.
function write(ctx, string, cx, top, unit, color) {
  ctx.fillStyle = color;
  let left = Math.round(cx - ((string.length * 6 - 1) * unit) / 2);
  for (const raw of string) {
    const rows = glyphRows[raw.toLowerCase()] || glyphRows[raw] || glyphRows['?'] || [];
    rows.forEach((bits, row) => {
      for (let col = 0; col < 6; col++)
        if (bits & (1 << col)) ctx.fillRect(left + col * unit, top + row * unit, unit, unit);
    });
    left += 6 * unit;
  }
}
const shadowed = (ctx, s, cx, top, unit, color) => {
  const d = Math.max(1, Math.round(unit / 3));
  write(ctx, s, cx + d, top + d, unit, '#0b0d12');
  write(ctx, s, cx, top, unit, color);
};

// `world()` answers { room, players, members } as the client has them now;
// `travel(room)` moves the watcher to another room.
export function stage({ canvas, world, travel }) {
  let shot = null, shotAt = 0, last = null, lastFrame = performance.now();
  let fade = { from: 0, to: 0, at: 0, seconds: 0 }; // black over the picture, 0 … 1
  let card = null, caption = null, counter = null;
  let out = null, ctx = null, recorder = null, chunks = [], take = null, frames = 0;
  const find = (handle) => {
    const w = world(), who = String(handle).replace(/^@/, '').toLowerCase();
    const m = (w.members || []).find((x) => x.handle?.toLowerCase() === who && x.seat >= 0);
    return (m && w.players?.[m.seat]) || null;
  };
  const url = new URL('./control', import.meta.url);
  url.protocol = url.protocol === 'https:' ? 'wss:' : 'ws:';
  const socket = new WebSocket(url);
  const tell = (message) => socket.readyState === WebSocket.OPEN && socket.send(JSON.stringify(message));
  socket.addEventListener('open', () => tell({ op: 'ready', width: canvas.width, height: canvas.height }));
  // The director asks who is in view by name: say who is here, four times a second.
  setInterval(() => {
    const w = world();
    tell({ op: 'state', room: w.room, connected: !!w.connected, frames, hidden: document.hidden, riding: !!w.ride?.moving,
      here: (w.members || []).filter((m) => m.handle && m.seat >= 0 && w.players?.[m.seat]).map((m) => m.handle.toLowerCase()) });
  }, 250);
  const fadeNow = (now) => {
    const k = fade.seconds ? Math.min(1, (now - fade.at) / (fade.seconds * 1000)) : 1;
    return fade.from + (fade.to - fade.from) * k;
  };
  socket.addEventListener('message', async (event) => {
    const d = JSON.parse(event.data), now = performance.now();
    if (d.op === 'shot') {
      shot = d.shot;
      shotAt = now;
      if (!d.shot.smooth || d.cut) last = null; // a cut lands at once
    }
    if (d.op === 'room') travel(d.room, d.ride || null);
    if (d.op === 'fade') fade = { from: fadeNow(now), to: d.to, at: now, seconds: d.seconds || 0 };
    if (d.op === 'card') card = d.title ? { ...d, at: now } : null;
    if (d.op === 'caption') caption = d.text ? { ...d, at: now } : null;
    if (d.op === 'counter') counter = { ...d, at: now };
    if (d.op === 'roll') {
      out = document.createElement('canvas');
      [out.width, out.height] = d.size;
      ctx = out.getContext('2d');
      ctx.imageSmoothingEnabled = false;
      const type = ['video/webm;codecs=vp9', 'video/webm;codecs=vp8', 'video/webm'].find((t) => MediaRecorder.isTypeSupported(t));
      recorder = new MediaRecorder(out.captureStream(d.fps || 60), { mimeType: type, videoBitsPerSecond: d.bitrate || 16e6 });
      chunks = [];
      take = d.take;
      recorder.ondataavailable = (e) => e.data.size && chunks.push(e.data);
      recorder.onstop = async () => {
        const blob = new Blob(chunks, { type: 'video/webm' });
        try {
          const r = await fetch(new URL(`./take?name=${encodeURIComponent(take)}`, import.meta.url), { method: 'POST', body: blob });
          tell({ op: 'uploaded', ok: r.ok, bytes: blob.size });
        } catch (error) {
          tell({ op: 'uploaded', ok: false, error: String(error) });
        }
      };
      recorder.start(500);
      tell({ op: 'rolling' });
    }
    if (d.op === 'wrap' && recorder?.state === 'recording') recorder.stop();
  });

  return {
    // Called every frame before the world is drawn: the camera's pose, or
    // null while the director has not called a shot.
    camera(now) {
      const dt = Math.min(0.1, (now - lastFrame) / 1000);
      lastFrame = now;
      if (!shot) return null;
      last = pose(shot, (now - shotAt) / 1000, find, last, dt) || last;
      return last;
    },
    // Called after the world is drawn: compose and record this frame.
    frame(now) {
      frames++;
      if (!ctx) return;
      const W = out.width, H = out.height, u = Math.max(1, Math.round(H / 360));
      // The world, cropped to the film's shape.
      const scale = Math.max(W / canvas.width, H / canvas.height);
      const w = canvas.width * scale, h = canvas.height * scale;
      ctx.fillStyle = '#000';
      ctx.fillRect(0, 0, W, H);
      try { ctx.drawImage(canvas, (W - w) / 2, (H - h) / 2, w, h); } catch {}
      const black = fadeNow(now);
      if (black > 0) { ctx.fillStyle = `rgba(0,0,0,${black})`; ctx.fillRect(0, 0, W, H); }
      // The elevator's own readout while the camera rides: the floor passing now.
      const riding = world().ride;
      if (riding) {
        shadowed(ctx, riding.floor ? `${riding.floor}` : 'G', W / 2, 14 * u, u * 6, riding.moving ? '#f4f1ea' : '#ff3296');
        shadowed(ctx, riding.moving ? (riding.to > riding.from ? 'going up' : 'going down') : riding.handle ? `@${riding.handle}` : 'ground', W / 2, 80 * u, u * 2, '#c9cedb');
      }
      // A floor counter called by the director, top centre.
      if (counter && !riding) {
        const t = (now - counter.at) / 1000, run = Math.min(1, t / counter.seconds), e = 1 - (1 - run) ** 3;
        const n = Math.round(counter.from + (counter.to - counter.from) * e), digits = n ? `${n}` : 'G';
        shadowed(ctx, digits, W / 2, 14 * u, u * 6, run < 1 ? '#f4f1ea' : '#ff3296');
        if (counter.label) shadowed(ctx, run < 1 ? (counter.to > counter.from ? 'going up' : 'going down') : counter.label, W / 2, 80 * u, u * 2, '#c9cedb');
        if (t > counter.seconds + (counter.hold ?? 1.5)) counter = null;
      }
      // A title card: large words in the middle, a line under them.
      if (card) {
        const t = (now - card.at) / 1000, a = Math.min(1, t / 0.25, (card.seconds - t) / 0.25);
        if (t >= card.seconds) card = null;
        else {
          ctx.globalAlpha = Math.max(0, a);
          const size = Math.min(u * 8, Math.floor((W * 0.8) / (card.title.length * 6)));
          shadowed(ctx, card.title, W / 2, H / 2 - size * 8, size, '#ff3296');
          if (card.sub) shadowed(ctx, card.sub, W / 2, H / 2 + size * 5, u * 2, '#f4f1ea');
          ctx.globalAlpha = 1;
        }
      }
      // A caption: one line along the bottom.
      if (caption) {
        const t = (now - caption.at) / 1000;
        if (caption.seconds && t >= caption.seconds) caption = null;
        else {
          const size = u * 2, width = (caption.text.length * 6 + 4) * size;
          ctx.fillStyle = 'rgba(11,13,18,0.72)';
          ctx.fillRect((W - width) / 2, H - 34 * u, width, 14 * size / 2 + 6 * u);
          write(ctx, caption.text, W / 2, H - 30 * u, size, '#f4f1ea');
        }
      }
    },
  };
}
