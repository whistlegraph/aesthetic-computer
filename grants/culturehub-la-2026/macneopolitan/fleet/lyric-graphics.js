// lyric-graphics.js — the room's lyric language on a dark canvas (Xbox
// oskiewar, ac7, the Windows sub page). Self-contained; no dependencies.
//
//   drawTrioRoom(ctx, w, h, music, elapsed, now)
//
// `music` is the venue feed's /api/performance for dance 'trio-round-v1':
// {title, bpm, duration, elapsed, lyric:{text,member,rgb,t,dur,role,answer,
// syllables:[{t,dur,text}],syllable}, next:{text,member,rgb,in},
// faces:{neo,blueberry,frisbee:{text,rgb,t,dur,role}|null}}. `elapsed` is the
// caller's locally extrapolated score time; `now` is performance.now() ms.
// Rules (LYRICS-SCREENS.md): headline = the lead line being sung, syllables
// lit as sung, singer's name above, next lead faint beneath; an answer is
// bigger, framed, flashes white on its first syllable and lingers 2.5 s; hums
// are small, low and breathe with the beat; the six seats stand as dim
// figures around the words.
const MEMBER_RGB = { neo: [143, 209, 63], blueberry: [90, 87, 211], frisbee: [242, 167, 185] };
const SEAT_RGB = [[143, 209, 63], [90, 87, 211], [242, 167, 185], [143, 209, 63], [90, 87, 211], [242, 167, 185]];
const LINGER_ANSWER = 2.5, FLASH = 0.25;
let lastHead = null, lastHeadEnd = -1;
const rgba = (c, a = 1) => `rgba(${c[0]},${c[1]},${c[2]},${a})`;
function wrap(ctx, text, maxW) {
  const words = text.split(' '), rows = []; let row = '';
  for (const wd of words) { const cand = row ? row + ' ' + wd : wd; if (ctx.measureText(cand).width > maxW && row) { rows.push(row); row = wd; } else row = cand; }
  if (row) rows.push(row); return rows;
}
function fitFont(ctx, text, maxW, maxPx, minPx, maxRows) {
  for (let px = maxPx; px >= minPx; px -= 4) { ctx.font = `600 ${px}px "Comic Sans MS", "Chalkboard SE", system-ui, sans-serif`; const rows = wrap(ctx, text, maxW); if (rows.length <= maxRows) return [px, rows]; }
  ctx.font = `600 ${minPx}px "Comic Sans MS", "Chalkboard SE", system-ui, sans-serif`; return [minPx, wrap(ctx, text, maxW)];
}
function karaoke(ctx, line, elapsed, cx, y, px, rows, lit, dim) {
  const syls = line.syllables || []; let sung = -1; for (let i = 0; i < syls.length; i++) { if (syls[i].t <= elapsed) sung = i; else break; }
  let sylIdx = 0; const lineH = px * 1.18;
  rows.forEach((rw, r) => {
    const words = rw.split(' '), rowW = ctx.measureText(rw).width; let x = cx - rowW / 2; const yy = y + r * lineH;
    for (const wd of words) {
      let take = 0, acc = ''; while (sylIdx + take < syls.length && acc.length < wd.length) { acc += syls[sylIdx + take].text; take++; }
      if (take === 0) take = 1; let px2 = x;
      for (let k = 0; k < take; k++) { const piece = syls[sylIdx + k]?.text ?? wd; ctx.fillStyle = (sylIdx + k) <= sung ? lit : dim; ctx.fillText(piece, px2, yy); px2 += ctx.measureText(piece).width; }
      sylIdx += take; x += ctx.measureText(wd + ' ').width;
    }
  });
  return rows.length * lineH;
}
export function drawTrioRoom(ctx, w, h, music, elapsed, now = 0) {
  ctx.save(); ctx.textBaseline = 'top'; ctx.textAlign = 'left';
  const beat = 60 / (music?.bpm || 100), pulse = Math.max(0, 1 - ((elapsed / beat) % 1) * 1.4);
  // six dim seats, breathing
  const cx = w / 2, cy = h / 2, R = Math.min(w, h) * 0.42;
  for (let i = 0; i < 6; i++) { const a = -Math.PI / 2 + i * Math.PI / 3, x = cx + Math.cos(a) * R, y = cy + Math.sin(a) * R * 0.78; const c = SEAT_RGB[i]; ctx.fillStyle = rgba(c, 0.22 + 0.12 * pulse); ctx.beginPath(); ctx.arc(x, y - 18, 7, 0, Math.PI * 2); ctx.fill(); ctx.strokeStyle = rgba(c, 0.28 + 0.12 * pulse); ctx.lineWidth = 3; ctx.beginPath(); ctx.moveTo(x, y - 10); ctx.lineTo(x, y + 14); ctx.moveTo(x - 10, y); ctx.lineTo(x + 10, y); ctx.moveTo(x, y + 14); ctx.lineTo(x - 8, y + 30); ctx.moveTo(x, y + 14); ctx.lineTo(x + 8, y + 30); ctx.stroke(); }
  const lyric = music?.lyric && music.lyric.role !== 'hum' ? music.lyric : null;
  // the headline lingers after it ends: remember the last lead line
  if (lyric) { lastHead = lyric; lastHeadEnd = lyric.t + lyric.dur; }
  const head = lyric || (lastHead && elapsed < lastHeadEnd + (lastHead.answer ? LINGER_ANSWER : 0.8) && elapsed >= lastHead.t ? lastHead : null);
  if (head) {
    const rgb = head.rgb || MEMBER_RGB[head.member] || [240, 240, 240], isAnswer = !!head.answer, flash = isAnswer && elapsed >= head.t && elapsed < head.t + FLASH;
    const [px, rows] = fitFont(ctx, head.text, w * 0.86, isAnswer ? Math.round(h * 0.2) : Math.round(h * 0.15), 28, 3);
    const lineH = px * 1.18, blockH = rows.length * lineH, y0 = cy - blockH / 2 - (isAnswer ? 0 : 10);
    if (isAnswer) { const pad = px * 0.5, bw = Math.max(...rows.map(r => ctx.measureText(r).width)) + pad * 2; ctx.fillStyle = flash ? 'rgba(255,255,255,0.95)' : rgba(rgb, 0.16); ctx.fillRect(cx - bw / 2, y0 - pad * 0.6, bw, blockH + pad * 1.2); ctx.fillStyle = flash ? '#fff' : rgba(rgb, 0.9); ctx.fillRect(cx - bw / 2, y0 + blockH + pad * 0.6, bw, Math.max(4, px * 0.08)); }
    ctx.font = `600 ${Math.round(px * 0.32)}px "Comic Sans MS", "Chalkboard SE", system-ui, sans-serif`; ctx.fillStyle = rgba(rgb, 0.75); const name = head.member || ''; ctx.fillText(name, cx - ctx.measureText(name).width / 2, y0 - px * 0.5);
    ctx.font = `600 ${px}px "Comic Sans MS", "Chalkboard SE", system-ui, sans-serif`;
    karaoke(ctx, head, elapsed, cx, y0, px, rows, flash ? '#111' : rgba(rgb, 1), flash ? 'rgba(20,20,30,0.5)' : rgba(rgb, 0.38));
    const next = music?.next; if (next && next.text !== head.text) { const nrgb = next.rgb || MEMBER_RGB[next.member] || rgb; ctx.font = `600 ${Math.round(px * 0.4)}px "Comic Sans MS", "Chalkboard SE", system-ui, sans-serif`; ctx.fillStyle = rgba(nrgb, 0.42); ctx.fillText(next.text, cx - ctx.measureText(next.text).width / 2, y0 + blockH + px * 0.7); }
  } else if (music?.next) { const n = music.next, nrgb = n.rgb || MEMBER_RGB[n.member] || [200, 200, 200]; const [px, rows] = fitFont(ctx, n.text, w * 0.6, Math.round(h * 0.08), 24, 2); ctx.fillStyle = rgba(nrgb, 0.45); rows.forEach((r, i) => ctx.fillText(r, cx - ctx.measureText(r).width / 2, cy - px * 0.6 + i * px * 1.18)); }
  // hums: each face's current hum, small and low, breathing
  const faces = music?.faces || {}; let k = 0;
  for (const m of ['neo', 'blueberry', 'frisbee']) { const f = faces[m]; if (!f || f.role !== 'hum') continue; const rgb = f.rgb || MEMBER_RGB[m]; const px = Math.round(h * 0.035); ctx.font = `600 ${px}px "Comic Sans MS", "Chalkboard SE", system-ui, sans-serif`; ctx.fillStyle = rgba(rgb, 0.35 + 0.25 * pulse); const x = w * (0.2 + k * 0.3) - ctx.measureText(f.text).width / 2; ctx.fillText(f.text, x, h * 0.86 - pulse * 6); k++; }
  ctx.restore();
}
export const trioRoomVersion = 'trio-room-v2';
