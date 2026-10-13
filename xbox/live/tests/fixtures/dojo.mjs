import { readFileSync } from 'node:fs';
import vm from 'node:vm';
import { nativeSource } from '../../../tools/oskiewar-native-source.mjs';
import { qrcode } from '../../../../system/public/aesthetic.computer/dep/@akamfoad/qr/qr.mjs';

const source = readFileSync(new URL('../../oskiewar.js', import.meta.url), 'utf8');
export function fixture() {
  let time = 1000000, state = { status: 'signed-out' };
  const actions = [], sounds = [], signals = [], noop = () => {};
  const context = vm.createContext({ structuredClone, performance, qrcode,
    Date: class extends Date { static now() { return time / 1000; } },
    runtime: () => ({ monotonicUs: time, renderAlpha: 1 }),
    capabilities: () => ({ platform: 'xbox-uwp', inputFamily: 'xbox' }),
    gamepad: () => ({ connected: true, down: [], leftX: 0, leftY: 0 }),
    accountState: () => state, accountAction: action => actions.push(action),
    wipe: noop, box: noop, line: noop, triangle: noop, triangle3d: noop,
    write: noop, systemWrite: noop, telemetry: noop,
    synth: (...args) => sounds.push(['synth', ...args]), drum: (...args) => sounds.push(['drum', ...args]),
    gameSignal: (...args) => signals.push(args),
  });
  const run = code => vm.runInContext(code, context);
  run(nativeSource(source));
  run('syncNativeAccount(); shellMode="MENU"; players[0].npc=false;');
  return { run, actions, sounds, signals, time(value) { time = value; }, state(value) { state = value; time += 600000; run('syncNativeAccount();'); } };
}

