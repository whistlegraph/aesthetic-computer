// 🛰️ A rax satellite: the same server, on a box nearer some players, keeping
// only the rooms world.mjs ROOM_HOSTS sends there. Pages are still served by
// lith; they open their socket here for those rooms. Nothing is built here
// (lith keeps the store), so the room is what the house puts in it.
//   RAX_ONLY_ROOMS=east RAX_ORIGINS=aesthetic.computer,prompt.ac node rax/satellite.mjs
// Bundled into one file and set up on a box by rax/satellite/deploy.sh.
import { createRaxServer } from './server.mjs';

const port = Number(process.env.RAX_PORT || 8786);
const rooms = (process.env.RAX_ONLY_ROOMS || 'east').split(',').filter(Boolean);
const origins = (process.env.RAX_ORIGINS || 'aesthetic.computer,prompt.ac,www.aesthetic.computer').split(',').filter(Boolean);
const app = createRaxServer({
  prefix: '/rax',
  stateDir: process.env.RAX_STATE || './rax-state',
  onlyRooms: rooms,
  origins,
});
await app.ready;
app.server.listen(port, '127.0.0.1', () => console.log(`[rax satellite] ${rooms.join(', ')} on 127.0.0.1:${port}, for pages from ${origins.join(', ')}`));
for (const signal of ['SIGTERM', 'SIGINT'])
  process.on(signal, () => {
    app.close().catch(() => {}).finally(() => process.exit(0));
    setTimeout(() => process.exit(0), 8000).unref();
  });
