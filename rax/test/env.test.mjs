import test from 'node:test';
import assert from 'node:assert/strict';
import { mkdtemp, mkdir, writeFile, readFile, lstat, rm } from 'node:fs/promises';
import { tmpdir } from 'node:os';
import { join } from 'node:path';

test('a host set up as dool keeps its settings and its state under rax', async () => {
  process.env.DOOL_KEEPER = 'someone';
  process.env.DOOL_GUESTS = '1';
  process.env.RAX_GUESTS = '0'; // The new name wins where both are set.
  delete process.env.RAX_STATE_DIR;
  const { stateHome } = await import('../env.mjs');
  assert.equal(process.env.RAX_KEEPER, 'someone');
  assert.equal(process.env.RAX_GUESTS, '0');
  const home = await mkdtemp(join(tmpdir(), 'rax-home-'));
  await mkdir(join(home, '.local/share/dool'), { recursive: true });
  await writeFile(join(home, '.local/share/dool/objects.json'), '{"rooms":{}}');
  const dir = stateHome('rax', home);
  assert.equal(dir, join(home, '.local/share/rax'));
  assert.equal(await readFile(join(dir, 'objects.json'), 'utf8'), '{"rooms":{}}');
  assert.ok((await lstat(join(home, '.local/share/dool'))).isSymbolicLink(), 'the old path still leads there');
  assert.equal(await readFile(join(home, '.local/share/dool/objects.json'), 'utf8'), '{"rooms":{}}');
  assert.equal(stateHome('rax', home), dir, 'a second start moves nothing');
  process.env.RAX_STATE_DIR = '/somewhere/else';
  assert.equal(stateHome('rax', home), '/somewhere/else');
  delete process.env.RAX_STATE_DIR;
  await rm(home, { recursive: true });
});
