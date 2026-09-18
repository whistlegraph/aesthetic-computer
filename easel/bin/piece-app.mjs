#!/usr/bin/env node
// Local app-mode experiment. APFS clones keep the original Aesel untouched.
import fs from 'node:fs';
import path from 'node:path';
import os from 'node:os';
import { execFileSync } from 'node:child_process';
import { createRequire } from 'node:module';
import { fileURLToPath } from 'node:url';
import identity from '../desktop/piece-app.cjs';

const source = path.resolve(path.dirname(fileURLToPath(import.meta.url)), '..');
const option = (name, fallback) => { const i = process.argv.indexOf(name); if (i < 0) return fallback; const value = process.argv[i + 1]; if (!value || value.startsWith('--')) throw Error(`${name} needs a value`); return value; };
if (process.argv.includes('--help')) {
  console.log('Usage: node easel/bin/piece-app.mjs --cwd PROJECT --name NAME --piece FILE --icon PNG [--instance window-UUID] [--base APP] [--open]\nCreates a local Mac app under ~/Applications/Aesel Pieces, preserving the project session. Re-running updates that same app identity.');
  process.exit(0);
}
if (process.platform !== 'darwin') throw Error('Piece apps currently require macOS.');
const cwd = fs.realpathSync(option('--cwd', process.cwd()));
const name = option('--name', path.basename(cwd)).trim();
const base = fs.realpathSync(option('--base', path.join(os.homedir(), 'Applications/aesel.app')));
const stateRoot = path.resolve(option('--state-root', path.join(os.homedir(), 'Library/Application Support/Easel')));
const previousFile = path.join(cwd, '.easel', 'app.json');
const previous = identity.readPieceApp(previousFile);
const piece = fs.realpathSync(path.resolve(cwd, option('--piece', previous?.piece || `${name}.mjs`)));
const instance = option('--instance', previous?.instance || identity.findSessionInstance({workspace:cwd,piece,stateRoot,
  fallback:/^window-[a-f0-9-]{36}$/.test(path.basename(cwd)) ? path.basename(cwd) : 'default'}));
if (path.relative(cwd, piece).startsWith('..' + path.sep) || path.relative(cwd, piece) === '..') throw Error('The piece must be inside its project.');
const icon = fs.realpathSync(option('--icon', path.join(cwd, '.easel', 'icon.png')));
const readPlist = file => JSON.parse(execFileSync('/usr/bin/plutil', ['-convert', 'json', '-o', '-', file], { encoding: 'utf8' }));
const basePlist = readPlist(path.join(base, 'Contents/Info.plist'));
const baseExecutable = path.join(base, 'Contents/MacOS', basePlist.CFBundleExecutable);
const apps = path.join(os.homedir(), 'Applications/Aesel Pieces');
const project = identity.projectIdentity({ workspace: cwd, name, instance, stateRoot, baseApp: base,
  baseExecutable, appPath: path.join(apps, `${name}.app`), piece, previous });
const target = project.appPath;
if (target === base) throw Error('A piece app cannot replace its Aesel runtime.');
if (fs.existsSync(target) && !previous) throw Error('That app name is already installed; choose another name.');
const isRunning = () => execFileSync('/bin/ps', ['-axo', 'command='], { encoding: 'utf8' }).split('\n').some(line => line.trimStart().startsWith(path.join(target, 'Contents/MacOS') + '/'));
if (isRunning()) throw Error('Quit this piece app before updating its bundle.');
fs.mkdirSync(path.dirname(target), { recursive: true });
const stage = target + `.stage-${process.pid}.app`;
if (fs.existsSync(stage)) throw Error('Staging path already exists.');
const run = (cmd, args, options = {}) => execFileSync(cmd, args, { stdio: 'pipe', timeout: 120000, ...options });
try {
  run('/bin/cp', ['-cR', base, stage]);
  const resources = path.join(stage, 'Contents/Resources');
  const appCode = path.join(resources, 'app');
  fs.rmSync(appCode, { recursive: true, force: true });
  const asar = createRequire(import.meta.url)(path.join(source, 'desktop/node_modules/@electron/asar'));
  asar.extractAll(path.join(base, 'Contents/Resources/app.asar'), appCode);
  const spawnHelper = path.join(appCode, 'node_modules/node-pty/build/Release/spawn-helper');
  if (fs.existsSync(spawnHelper)) fs.chmodSync(spawnHelper, 0o755);
  fs.rmSync(path.join(resources, 'app.asar'));
  // Freeze just the tested host changes into this prototype; retain the installed
  // runtime/dependencies, and keep its native helpers in app.asar.unpacked.
  for (const name of ['main.cjs', 'piece-app.cjs', 'net-clock.js', 'preview-waveform.js', 'donkey.css']) {
    fs.copyFileSync(path.join(source, 'desktop', name), path.join(appCode, name));
  }
  const ui = path.join(resources, 'easel/desktop-ui');
  if (fs.existsSync(ui)) for (const name of ['preview-waveform.js', 'donkey.css']) fs.copyFileSync(path.join(source, 'desktop', name), path.join(ui, name));
  const slab = path.join(resources, 'easel/src/slab-session.mjs');
  let slabText = fs.readFileSync(slab, 'utf8');
  if (!slabText.includes('host_bundle_id:')) slabText = slabText.replace("host_app:'computer.aesthetic.easel',", "host_app:'computer.aesthetic.easel',\n        host_bundle_id:process.env.EASEL_HOST_BUNDLE_ID || 'computer.aesthetic.easel',");
  fs.writeFileSync(slab, slabText);
  identity.atomicJSON(path.join(resources, 'piece-app.json'), project);

  // Use the actual piece pixels as its app icon; no generated artwork or model call.
  const iconset = path.join(stage, 'piece.iconset'); fs.mkdirSync(iconset);
  for (const size of [16, 32, 128, 256, 512]) for (const scale of [1, 2]) {
    const px = String(size * scale);
    run('/usr/bin/sips', ['-s', 'format', 'png', '-z', px, px, icon, '--out', path.join(iconset, `icon_${size}x${size}${scale === 2 ? '@2x' : ''}.png`)]);
  }
  fs.copyFileSync(path.join(iconset, 'icon_512x512@2x.png'), path.join(resources, 'piece-app.png'));
  run('/usr/bin/iconutil', ['-c', 'icns', iconset, '-o', path.join(resources, 'piece-app.icns')]);
  fs.rmSync(iconset, { recursive: true });
  // Electron resolves helpers from CFBundleName before running any app code.
  const frameworks = path.join(stage, 'Contents/Frameworks');
  for (const suffix of [' Helper', ' Helper (GPU)', ' Helper (Renderer)', ' Helper (Plugin)']) {
    const oldName = basePlist.CFBundleName + suffix, newName = name + suffix;
    const helper = path.join(frameworks, oldName + '.app');
    if (!fs.existsSync(helper)) continue;
    const helperInfo = path.join(helper, 'Contents/Info.plist'), helperPlist = readPlist(helperInfo);
    fs.renameSync(path.join(helper, 'Contents/MacOS', helperPlist.CFBundleExecutable), path.join(helper, 'Contents/MacOS', newName));
    Object.assign(helperPlist, { CFBundleExecutable:newName, CFBundleName:newName, CFBundleDisplayName:newName,
      CFBundleIdentifier:project.bundleId + '.helper' + (suffix.match(/\((\w+)\)/)?.[1] ? '.' + suffix.match(/\((\w+)\)/)[1].toLowerCase() : '') });
    fs.writeFileSync(helperInfo, JSON.stringify(helperPlist)); run('/usr/bin/plutil', ['-convert', 'xml1', helperInfo]);
    fs.renameSync(helper, path.join(frameworks, newName + '.app'));
  }
  fs.renameSync(path.join(stage, 'Contents/MacOS', basePlist.CFBundleExecutable), path.join(stage, 'Contents/MacOS', name));
  const plist = { ...basePlist, CFBundleIdentifier: project.bundleId, CFBundleExecutable:name, CFBundleName: name, CFBundleDisplayName: name, CFBundleIconFile: 'piece-app.icns' };
  // The project must not take over Aesel's URL or document handlers, updater,
  // or the integrity hash of the original app.asar.
  for (const key of ['ElectronAsarIntegrity', 'CFBundleURLTypes', 'CFBundleDocumentTypes']) delete plist[key];
  const info = path.join(stage, 'Contents/Info.plist');
  fs.writeFileSync(info, JSON.stringify(plist)); run('/usr/bin/plutil', ['-convert', 'xml1', info]);
  fs.rmSync(path.join(resources, 'app-update.yml'), { force: true });
  run('/usr/bin/codesign', ['--force', '--deep', '--sign', '-', '--preserve-metadata=entitlements,flags', stage]);
  run('/usr/bin/codesign', ['--verify', '--deep', '--strict', stage]);
  // Exercise the renamed helpers and the extracted PTY launcher before install.
  // This starts only a bounded local version probe, never an agent or prompt.
  const probe = `const pty=require(${JSON.stringify(path.join(appCode, 'node_modules/node-pty'))});const t=pty.spawn(process.execPath,['-p','process.versions.electron'],{cwd:${JSON.stringify(cwd)},env:{...process.env,ELECTRON_RUN_AS_NODE:'1',NODE_OPTIONS:''}});let out='';t.onData(s=>out+=s);t.onExit(({exitCode})=>process.exit(exitCode===0&&out.includes(process.versions.electron)?0:1));setTimeout(()=>process.exit(1),8000).unref();`;
  run(path.join(stage, 'Contents/MacOS', name), ['-e', probe], { env:{...process.env,ELECTRON_RUN_AS_NODE:'1',NODE_OPTIONS:''},timeout:15000 });
  if (isRunning()) throw Error('The piece app opened during installation; close it before retrying.');
  if (fs.existsSync(target)) {
    const backup = path.join(stateRoot, 'piece-apps', project.id, `previous-${Date.now()}.app`);
    fs.mkdirSync(path.dirname(backup), { recursive: true }); fs.renameSync(target, backup);
  }
  fs.renameSync(stage, target);
  identity.registerProject(project);
  identity.atomicJSON(previousFile, project);
  console.log(JSON.stringify({ name, app: target, bundleId: project.bundleId, projectId: project.id, session: identity.sessionPath(project) }));
  if (process.argv.includes('--open')) run('/usr/bin/open', ['-a', target]);
} catch (error) {
  fs.rmSync(stage, { recursive: true, force: true });
  throw error;
}
