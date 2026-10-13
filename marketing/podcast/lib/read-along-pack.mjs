import { readFileSync } from 'node:fs';
import { resolve } from 'node:path';
import { fileURLToPath } from 'node:url';
import { gunzipSync } from 'node:zlib';
import { createHash } from 'node:crypto';
import { validateReadAlong } from '../../../system/public/aesthetic.computer/lib/rich-text.mjs';

const repo = fileURLToPath(new URL('../../../', import.meta.url));
export const READALONG_FONT = resolve(repo, 'marketing/podcast/assets/unifont-16.0.03.hex.gz');
export function readAlongSource(episode, url = 'episode.json') {
  const background = episode.themes?.[0]?.background || [246, 235, 196];
  return `(wipe ${background.join(' ')})\n(def episode (fetch ${JSON.stringify(url)} "episode.json"))\n(flow (listen episode))\n`;
}
export function readAlongAssets(episode, audio) {
  validateReadAlong(episode);
  if (episode.audio.url !== 'episode.mp3' || createHash('sha256').update(audio).digest('hex') !== episode.audio.sha256) throw new Error('Audio differs from the aligned master');
  return [
    { name: 'episode.json', mime: 'application/json', content: Buffer.from(JSON.stringify(episode, null, 2) + '\n') },
    { name: 'episode.mp3', mime: 'audio/mpeg', content: audio },
    { name: 'UNIFONT-OFL-1.1.txt', mime: 'text/plain', content: readFileSync(resolve(repo, 'system/public/aesel/UNIFONT-OFL-1.1.txt')) },
    { name: 'UNIFONT-COPYRIGHT.txt', mime: 'text/plain', content: Buffer.from(
      'GNU Unifont 16.0.03. https://unifoundry.com/unifont/\n' +
      'Copyright (C) 1998-2024 Roman Czyborra, Paul Hardy, Qianqian Fang, Andrew Miller, Johnnie Weaver, David Corbett, Rebecca Bettencourt, et al.\n' +
      'Distributed under the SIL Open Font License 1.1. See UNIFONT-OFL-1.1.txt.\n' +
      'Bitmap subset for this reading; original glyph designs preserved.\n') },
  ];
}
export function unifontGlyphs(episode, fontFile = READALONG_FONT) {
  validateReadAlong(episode);
  const chars = new Set(episode.title + episode.body + '?'), glyphs = {};
  for (const line of gunzipSync(readFileSync(fontFile)).toString('utf8').trim().split('\n')) {
    const [hex, bitmap] = line.trim().split(':');
    const char = String.fromCodePoint(parseInt(hex, 16));
    if (!chars.has(char)) continue;
    if (!/^[0-9A-Fa-f]+$/.test(bitmap) || ![32, 64].includes(bitmap.length)) throw new Error(`Expected an 8x16 or 16x16 Unifont glyph: ${hex}`);
    const width = bitmap.length / 4, stride = width / 4;
    const pixels = Array.from({ length: 16 }, (_, y) => {
      const bits = parseInt(bitmap.slice(y * stride, (y + 1) * stride), 16);
      return Array.from({ length: width }, (_, x) => (bits >> (width - x - 1)) & 1);
    });
    glyphs[hex.toUpperCase().padStart(4, '0')] = { resolution: [width, 16], advance: width, pixels };
    chars.delete(char);
  }
  for (const char of chars) if (!/\s/u.test(char)) throw new Error(`Unifont lacks ${JSON.stringify(char)}`);
  return glyphs;
}
export async function readAlongBundle(episode, { code = 'readalong-preview', source = readAlongSource(episode), fontFile = READALONG_FONT } = {}) {
  const glyphs = unifontGlyphs(episode, fontFile);
  process.env.AC_SOURCE_DIR ||= resolve(repo, 'system/public/aesthetic.computer');
  const { createBundleFromSource } = await import('../../../oven/bundler.mjs');
  const bg = episode.themes?.[0]?.background || [246, 235, 196];
  const { html } = await createBundleFromSource(code, source, {
    authorHandle: 'jeffrey', density: 2, glyphs: { unifont: glyphs },
    style: `#aesthetic-computer { position: relative; overflow: hidden; } html,body,canvas { background: rgb(${bg.join(',')}); }`,
  });
  return { html, source, glyphs: Object.keys(glyphs).length };
}
