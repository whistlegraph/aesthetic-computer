import { createRequire } from 'node:module';
import { fileURLToPath } from 'node:url';
import { spawn } from 'node:child_process';

const electron = createRequire(new URL('../ac-electron/package.json', import.meta.url))('electron');
const env = { ...process.env };
delete env.ELECTRON_RUN_AS_NODE;
const child = spawn(
  electron,
  [fileURLToPath(new URL('../ac-electron/', import.meta.url)), '--dool-rig'],
  { stdio: 'inherit', env },
);
child.on('error', (error) => {
  console.error(error.message);
  process.exitCode = 1;
});
child.on('exit', (code) => {
  process.exitCode = code ?? 1;
});
for (const signal of ['SIGINT', 'SIGTERM']) process.once(signal, () => child.kill(signal));
