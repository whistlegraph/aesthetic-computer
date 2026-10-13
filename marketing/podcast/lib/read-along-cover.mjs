// The same Unifont text/colors as the reader, rendered without a browser on
// the release appliance. The interactive HTML remains the primary artifact.
import { spawn, execFileSync } from 'node:child_process';
import { unifontGlyphs } from './read-along-pack.mjs';

export async function renderReadAlongCover(episode, gifPath, thumbnailPath) {
  const glyphs = unifontGlyphs(episode), lines = [];
  const glyph = ch => glyphs[ch.codePointAt(0).toString(16).toUpperCase().padStart(4, '0')];
  let x = 10, y = 10, offset = 0, line = [];
  const flush = () => { if (line.length) lines.push({ y, chars: line }); line = []; x = 10; y += 23; };
  for (const paragraph of episode.body.split('\n\n')) {
    for (const token of paragraph.match(/\s+|\S+/gu) || []) {
      const width = [...token].reduce((n, ch) => n + (glyph(ch)?.advance || 8), 0);
      if (x > 10 && x + width > 246 && !/^\s+$/.test(token)) flush();
      for (const ch of token) {
        const w = glyph(ch)?.advance || 8;
        if (x + w > 246) flush();
        if (x !== 10 || !/\s/u.test(ch)) { line.push({ ch, x, offset }); x += w; }
        offset += ch.length;
      }
    }
    flush(); y += 8; offset += 2;
  }
  const width = 512, height = 512, frames = 12;
  const ff = spawn('ffmpeg', ['-loglevel', 'error', '-y', '-f', 'rawvideo', '-pix_fmt', 'rgb24', '-s', '512x512', '-r', '3', '-i', '-',
    '-filter_complex', '[0]split[a][b];[a]palettegen=max_colors=64:stats_mode=full[p];[b][p]paletteuse=dither=none', '-loop', '0', gifPath], { stdio: ['pipe', 'ignore', 'pipe'] });
  let error = ''; ff.stderr.on('data', chunk => error += chunk);
  const done = new Promise((ok, fail) => { ff.on('error', fail); ff.on('close', status => status === 0 ? ok() : fail(new Error(`Readalong cover: ${error}`))); });
  for (let frame = 0; frame < frames; frame++) {
    const word = episode.words[Math.min(frame, episode.words.length - 1)];
    const colors = episode.themes.find(t => word.start >= t.start && word.start < t.end) || episode.themes[0];
    const pixels = Buffer.alloc(width * height * 3);
    for (let i = 0; i < pixels.length; i += 3) { pixels[i] = colors.background[0]; pixels[i + 1] = colors.background[1]; pixels[i + 2] = colors.background[2]; }
    const activeLine = lines.find(l => l.chars.some(c => c.offset >= word.start && c.offset < word.end));
    const scroll = Math.max(0, (activeLine?.y || 0) - 99);
    const dot = (px, py, color) => {
      if (px < 0 || py < 0 || px >= 256 || py >= 249) return;
      for (let dy = 0; dy < 2; dy++) for (let dx = 0; dx < 2; dx++) {
        const index = ((py * 2 + dy) * width + px * 2 + dx) * 3;
        pixels[index] = color[0]; pixels[index + 1] = color[1]; pixels[index + 2] = color[2];
      }
    };
    for (const row of lines) for (const c of row.chars) {
      const shape = glyph(c.ch); if (!shape) continue;
      const active = c.offset >= word.start && c.offset < word.end;
      const color = active ? colors.highlight : colors.foreground;
      const shake = active ? Math.round(Math.sin(frame + c.offset) * 0.8) : 0;
      for (let gy = 0; gy < 16; gy++) for (let gx = 0; gx < shape.resolution[0]; gx++) {
        if (shape.pixels[gy][gx]) dot(c.x + gx, row.y - scroll + gy + shake, color);
      }
    }
    if (!ff.stdin.write(pixels)) await new Promise(resolve => ff.stdin.once('drain', resolve));
  }
  ff.stdin.end(); await done;
  execFileSync('ffmpeg', ['-loglevel', 'error', '-y', '-i', gifPath, '-frames:v', '1', '-vf', 'scale=256:256:flags=neighbor', thumbnailPath]);
  return frames;
}
