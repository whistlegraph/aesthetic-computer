#!/usr/bin/env node
// Make a looping email poster from two reviewed product images. No generated media.
// node icp-email-poster.mjs comparison.json output.gif
// comparison.json: {panels:[{file,crop?:[x,y,width,height]}, {file,crop?:[x,y,width,height]}]}
// Paths are relative to the manifest. Crops must preserve the entire product.
import { createHash } from 'node:crypto';
import { spawnSync } from 'node:child_process';
import { readFileSync, writeFileSync, mkdtempSync, renameSync, rmSync, mkdirSync } from 'node:fs';
import { dirname, resolve, join } from 'node:path';

const [manifestArg, outputArg] = process.argv.slice(2);
if (!manifestArg || !outputArg) {
  console.error('Usage: node icp-email-poster.mjs comparison.json output.gif');
  process.exit(1);
}
const manifest = resolve(manifestArg), output = resolve(outputArg);
const spec = JSON.parse(readFileSync(manifest, 'utf8'));
if (!output.endsWith('.gif') || spec.panels?.length !== 2) throw Error('Expected two panels and a .gif output.');
const hash = b => createHash('sha256').update(b).digest('hex');
const panels = spec.panels.map(p => {
  if (typeof p.file !== 'string') throw Error('Each panel needs an image file.');
  if (p.crop && (p.crop.length !== 4 || p.crop.some(n => !Number.isInteger(n) || n < 0) || p.crop[2] === 0 || p.crop[3] === 0)) {
    throw Error('Crop must be [x, y, positive width, positive height].');
  }
  const file = resolve(dirname(manifest), p.file);
  return { ...p, file, sha256: hash(readFileSync(file)) };
});
mkdirSync(dirname(output), { recursive: true });
const work = mkdtempSync(join(dirname(output), '.poster-'));
const run = (command, args) => {
  const r = spawnSync(command, args, { encoding: 'utf8', maxBuffer: 8 * 1024 * 1024 });
  if (r.error) throw r.error;
  if (r.status !== 0) throw Error(`${command} failed: ${r.stderr}`);
  return r.stdout;
};
// Use PATH so the fleet's utility-priority ffmpeg shim remains in effect.
const ff = ['-hide_banner', '-loglevel', 'error', '-y', '-threads', '2', '-filter_complex_threads', '2'];
const fit = (p, i) => {
  const crop = p.crop ? `crop=${p.crop[2]}:${p.crop[3]}:${p.crop[0]}:${p.crop[1]},` : '';
  return `[${i}:v]${crop}scale=384:468:force_original_aspect_ratio=decrease:flags=lanczos,setsar=1,pad=384:468:(ow-iw)/2:(oh-ih)/2:color=white,format=rgba[p${i}]`;
};
try {
  const still = join(work, 'comparison.png'), shine = join(work, 'shine.png'), gif = join(work, 'poster.gif');
  run('ffmpeg', [...ff, '-i', panels[0].file, '-i', panels[1].file,
    '-filter_complex', `${panels.map(fit).join(';')};[p0]pad=400:468:0:0:color=white[left];[left][p1]hstack=inputs=2,pad=800:500:8:16:color=white[out]`,
    '-map', '[out]', '-frames:v', '1', still]);
  // A low-opacity highlight passes once; the first and last frames are the same
  // complete comparison. No crossfade, camera move, caption, or sign-off frame.
  run('ffmpeg', [...ff, '-f', 'lavfi', '-i',
    "nullsrc=s=160x1000,format=rgba,geq=r=255:g=255:b=255:a='22*exp(-pow((X-80)/24,2))'",
    '-vf', 'rotate=0.28:ow=rotw(0.28):oh=roth(0.28):fillcolor=black@0,format=rgba', '-frames:v', '1', shine]);
  run('ffmpeg', [...ff, '-loop', '1', '-framerate', '12', '-t', '4', '-i', still, '-loop', '1', '-framerate', '12', '-t', '4', '-i', shine,
    '-filter_complex', "[0][1]overlay=x='-600+1600*(t-1)/1.5':y=-250:enable='between(t,1,2.5)':format=auto,fps=12,split[a][b];[a]palettegen=max_colors=192[p];[b][p]paletteuse=dither=bayer:bayer_scale=4[out]",
    '-map', '[out]', '-frames:v', '48', '-loop', '0', gif]);
  const probe = JSON.parse(run('ffprobe', ['-v', 'error', '-show_entries', 'format=duration:stream=width,height,nb_frames', '-of', 'json', gif]));
  if (probe.streams[0]?.width !== 800 || probe.streams[0]?.height !== 500 || Math.abs(Number(probe.format.duration) - 4) > .02) {
    throw Error('Poster dimensions or duration failed verification.');
  }
  const bytes = readFileSync(gif), receipt = { createdAt: new Date().toISOString(), panels, width: 800, height: 500,
    duration: Number(probe.format.duration), frames: Number(probe.streams[0].nb_frames), bytes: bytes.length, sha256: hash(bytes) };
  renameSync(still, output.replace(/\.gif$/, '.png'));
  renameSync(gif, output);
  writeFileSync(output.replace(/\.gif$/, '.json'), JSON.stringify(receipt, null, 2) + '\n');
  console.log(`${output}: 800×500, ${receipt.duration}s, ${receipt.frames} frames, ${receipt.bytes} bytes`);
} finally {
  rmSync(work, { recursive: true, force: true });
}
