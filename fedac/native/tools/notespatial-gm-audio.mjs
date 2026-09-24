import { openSync, closeSync, readSync, writeFileSync, unlinkSync, statSync } from 'node:fs';
import { spawn, spawnSync } from 'node:child_process';
import { fileURLToPath } from 'node:url';
import { join } from 'node:path';

export function buildGmRenderer(work) {
  const binary = join(work, 'gm-render');
  const src = fileURLToPath(new URL('../src/', import.meta.url));
  const result = spawnSync('cc', ['-O2', '-I', src, fileURLToPath(new URL('./notespatial-gm-render.c', import.meta.url)), join(src, 'gm_synth.c'), '-lm', '-o', binary], { encoding: 'utf8' });
  if (result.status !== 0) throw Error(`Cannot build native GM renderer: ${result.error || result.stderr}`);
  return binary;
}

export async function renderGmBank(events, { work, sampleRate }) {
  if (!events.length) return { read: () => null, close() {} };
  const binary = buildGmRenderer(work), input = join(work, 'gm-notes.txt'), output = join(work, 'gm-notes.f32');
  let bytes = 0;
  const index = new Map();
  writeFileSync(input, events.map((e, i) => {
    const samples = Math.round(e.dur * sampleRate);
    index.set(e, { offset: bytes, samples }); bytes += samples * 4;
    // Stable per-note stochasticism also makes excerpt/full render timbres agree.
    const seed = (Math.round(e.t * 10000) ^ Math.round(e.hz * 100) ^ ((e.gm + 1) * 2654435761)) >>> 0;
    return `${e.gm} ${e.hz} ${samples} ${(e.attack ?? .01) * sampleRate} ${(e.decay ?? .06) * sampleRate} ${seed}`;
  }).join('\n') + '\n');
  const infd = openSync(input, 'r'), outfd = openSync(output, 'w');
  console.log(`Rendering ${events.length} notes with native gm_synth.c…`);
  try {
    await new Promise((resolve, reject) => {
      const proc = spawn(binary, [String(sampleRate)], { stdio: [infd, outfd, 'inherit'] });
      proc.once('error', reject);
      proc.once('exit', code => code === 0 ? resolve() : reject(Error(`GM rendering failed (${code})`)));
    });
  } finally { closeSync(infd); closeSync(outfd); }
  if (statSync(output).size !== bytes) throw Error('Incomplete GM audio bank');
  const fd = openSync(output, 'r');
  return {
    read(e) {
      const item = index.get(e);
      if (!item) return null;
      const pcm = new Float32Array(item.samples), buffer = Buffer.from(pcm.buffer);
      let done = 0;
      while (done < buffer.length) {
        const n = readSync(fd, buffer, done, buffer.length - done, item.offset + done);
        if (!n) throw Error('Truncated GM note');
        done += n;
      }
      return pcm;
    },
    close() { closeSync(fd); unlinkSync(output); unlinkSync(input); },
  };
}
