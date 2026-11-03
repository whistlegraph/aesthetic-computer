import { randomBytes } from 'node:crypto';
import { homedir } from 'node:os';
import { join } from 'node:path';
import { createDuelServer } from './server.mjs';
import { connectBuilder } from './aesel.mjs';
import { kidlispPublisher } from './kidlisp-code.mjs';
const token = randomBytes(32).toString('hex');
const app = createDuelServer({
  builderToken: token,
  stateDir: process.env.DOOL_STATE_DIR || join(homedir(), '.local/share/dool-studio'),
  // The studio publishes each finished version to AC's KidLisp store.
  publishKidlisp: process.env.DOOL_KIDLISP === '0' ? null : kidlispPublisher(),
});
await app.ready;
void app.builders.backfill();
const port = Number(process.env.PORT || 8787);
app.server.listen(port, '127.0.0.1', () => {
  console.log(`dool studio → http://127.0.0.1:${port}/?room=studio`);
});
const worker = connectBuilder({
  url: `ws://127.0.0.1:${port}/builder`,
  token,
  onState: (state) => console.log(`Aesel relay bridge ${state}`),
});
for (const signal of ['SIGINT', 'SIGTERM'])
  process.once(signal, async () => {
    worker.close();
    await app.close();
    process.exit(0);
  });
