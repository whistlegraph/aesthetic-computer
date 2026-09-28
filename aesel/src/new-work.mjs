import { randomUUID } from 'node:crypto';
import { join } from 'node:path';
import { writeDesktopSession } from './desktop-session.mjs';

export async function archiveThread(snapshot) {
  const file = join(snapshot.cwd, '.easel', 'threads', `${Date.now()}-${randomUUID()}.json`);
  await writeDesktopSession(file, snapshot);
  return file;
}

// Roll back the selected work and connection if opening the replacement fails.
export async function replaceWork({archive, prepare, connect, restore, discard, accept}) {
  const archived = await archive();
  let replacement;
  try {
    await prepare();
    replacement = await connect();
  } catch (error) {
    await discard(replacement);
    await restore();
    throw error;
  }
  await accept(replacement, archived);
  return archived;
}
