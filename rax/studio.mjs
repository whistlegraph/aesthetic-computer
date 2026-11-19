import { randomBytes } from 'node:crypto';
import { mkdirSync, readFileSync, writeFileSync } from 'node:fs';
import { homedir } from 'node:os';
import { join } from 'node:path';
import { createRaxServer } from './server.mjs';
import { stateHome } from './env.mjs';
import { connectBuilder } from './aesel.mjs';
import { kidlispPublisher } from './kidlisp-code.mjs';
// A stable builder token in the state folder, so builder processes that
// outlive a server restart can reconnect with it.
const stateDir = stateHome('rax-studio');
mkdirSync(stateDir, { recursive: true, mode: 0o700 });
const tokenFile = join(stateDir, 'builder-token');
let token;
try {
  token = readFileSync(tokenFile, 'utf8').trim();
} catch {}
if (!token || token.length < 32) {
  token = randomBytes(32).toString('hex');
  writeFileSync(tokenFile, token, { mode: 0o600 });
}
const app = createRaxServer({
  builderToken: token,
  stateDir,
  // A local studio has no AC session: anyone is seated unless RAX_GUESTS=0.
  guests: process.env.RAX_GUESTS !== '0',
  // The studio publishes each finished version to AC's KidLisp store.
  publishKidlisp: process.env.RAX_KIDLISP === '0' ? null : kidlispPublisher(),
});
await app.ready;
void app.builders.backfill();
const port = Number(process.env.PORT || 8787);
app.server.listen(port, '127.0.0.1', () => {
  console.log(`rax studio → http://127.0.0.1:${port}/?room=studio`);
});
// In-process builders unless they run on their own (RAX_EXTERNAL_BUILDERS=1,
// `node rax/aesel.mjs`), which survive server restarts.
const workers =
  process.env.RAX_EXTERNAL_BUILDERS === '1'
    ? []
    : Array.from({ length: Number(process.env.RAX_BUILDERS || 2) }, (_, i) =>
        connectBuilder({
          url: `ws://127.0.0.1:${port}/builder`,
          token,
          onState: (state) => console.log(`Aesel builder ${i + 1} ${state}`),
        }),
      );
for (const signal of ['SIGINT', 'SIGTERM'])
  process.once(signal, async () => {
    for (const worker of workers) worker.close();
    await app.close();
    process.exit(0);
  });
