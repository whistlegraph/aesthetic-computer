// Parse the new TUI and its source modules without executing them. Called in a
// separate Node process so broken edits cannot take down the running session.
import {readdir, readFile} from 'node:fs/promises';
import {join} from 'node:path';
import {SourceTextModule} from 'node:vm';

async function check(directory) {
  for (const entry of await readdir(directory, {withFileTypes: true})) {
    const file = join(directory, entry.name);
    if (entry.isDirectory()) await check(file);
    else if (entry.name.endsWith('.mjs') && entry.name!=='.tui-built.mjs') {
      try { new SourceTextModule(await readFile(file, 'utf8'), {identifier: file}); }
      catch (error) { throw new Error(`${file}: ${error.message}`); }
    }
  }
}
try { await check(process.argv[2]); }
catch (error) { console.error(error.message); process.exitCode = 1; }
