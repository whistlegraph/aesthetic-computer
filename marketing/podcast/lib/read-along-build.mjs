import { readFileSync, writeFileSync, existsSync, mkdirSync, rmSync } from 'node:fs';
import { resolve } from 'node:path';
import { homedir } from 'node:os';
import { fileURLToPath } from 'node:url';
import { createHash } from 'node:crypto';
import { execFileSync } from 'node:child_process';

const root = fileURLToPath(new URL('../', import.meta.url));
export function prepareReadAlong({ markdown, audioFile, directory }) {
  const timeline = audioFile.replace(/\.[^.]+$/, '.spoken.json');
  if (!existsSync(timeline)) throw new Error('This recording has no complete spoken timeline; retain its original artifact or regenerate it intentionally');
  mkdirSync(directory, { recursive: true });
  const hash = createHash('sha256').update(readFileSync(audioFile)).digest('hex');
  const prefix = resolve(directory, `words-${hash}`), wordsFile = `${prefix}.json`;
  if (!existsSync(wordsFile)) {
    const model = process.env.WHISPER_MODEL || resolve(homedir(), '.whisper-models/ggml-small.bin');
    if (!existsSync(model)) throw new Error('Readalong releases require the local Whisper small model');
    const wav = `${prefix}.wav`;
    try {
      execFileSync('ffmpeg', ['-hide_banner', '-loglevel', 'error', '-y', '-i', audioFile, '-ar', '16000', '-ac', '1', '-c:a', 'pcm_s16le', wav], { stdio: 'inherit', timeout: 120000 });
      const log = execFileSync('whisper-cli', ['-m', model, '-l', 'en', '-t', '4', '-dtw', 'small', '-nfa', '-ml', '1', '-sow', '-ojf', '-of', prefix, wav], { timeout: 300000, maxBuffer: 16 * 1024 * 1024 });
      writeFileSync(`${prefix}.log`, log);
    } catch (error) {
      rmSync(wordsFile, { force: true });
      throw error;
    } finally { rmSync(wav, { force: true }); }
  }
  const output = resolve(directory, 'episode.json');
  execFileSync(process.execPath, [resolve(root, 'bin/read-along.mjs'), markdown, wordsFile, '0', audioFile, output, timeline], { stdio: 'inherit' });
  const episode = JSON.parse(readFileSync(output));
  if (episode.alignment.missingWords > Math.max(2, episode.alignment.canonicalWords * 0.02)) throw new Error('Too many words lack timing; inspect alignment before minting');
  return episode;
}
