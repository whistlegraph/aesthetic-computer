// 🏷️ rax was dool, and duel before that (renamed 2026-11-18). Hosts set up
// under the old name keep working: a DOOL_* variable stands in for its RAX_*
// one, and a state directory left at the old path is moved to the new one,
// with a link behind it so a rolled-back build still finds its files.
import { existsSync, lstatSync, renameSync, symlinkSync } from 'node:fs';
import { homedir } from 'node:os';
import { join } from 'node:path';

for (const [key, value] of Object.entries(process.env))
  if (key.startsWith('DOOL_')) process.env['RAX_' + key.slice(5)] ??= value;

// Where a server keeps its state: RAX_STATE_DIR, else ~/.local/share/<name>.
export function stateHome(name = 'rax', home = homedir()) {
  if (process.env.RAX_STATE_DIR) return process.env.RAX_STATE_DIR;
  const now = join(home, '.local/share', name);
  const was = join(home, '.local/share', name.replace(/^rax/, 'dool'));
  try {
    if (!existsSync(now) && existsSync(was) && !lstatSync(was).isSymbolicLink()) {
      renameSync(was, now);
      symlinkSync(now, was);
      console.log(`rax: state moved ${was} → ${now}`);
    }
  } catch (e) {
    console.error(`rax: state not moved from ${was}: ${e.message}`);
    if (!existsSync(now) && existsSync(was)) return was; // Never start empty beside a full room.
  }
  return now;
}
