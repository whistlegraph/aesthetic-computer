// A persistent project identity shared by app-mode and the Aesel studio.
const fs = require('node:fs');
const path = require('node:path');
const { createHash, randomUUID } = require('node:crypto');

function validatePieceApp(value) {
  if (value?.format !== 1 || !/^[a-f0-9-]{36}$/.test(value.id || '') ||
      typeof value.name !== 'string' || !value.name.trim() || value.name.length > 64 || /[\x00-\x1f/\\:]/.test(value.name) ||
      !['workspace', 'stateRoot', 'appPath', 'baseApp', 'baseExecutable', 'piece'].every(key => typeof value[key] === 'string' && path.isAbsolute(value[key])) ||
      !/^(default|window-[a-f0-9-]{36})$/.test(value.instance || '') ||
      value.bundleId !== `computer.aesthetic.aesel.piece.${value.id.replaceAll('-', '')}`) {
    throw new Error('Invalid Aesel piece app identity');
  }
  return value;
}

function readPieceApp(file) {
  try { return validatePieceApp(JSON.parse(fs.readFileSync(file, 'utf8'))); }
  catch (error) { if (error.code === 'ENOENT') return null; throw error; }
}

function projectIdentity({ workspace, name, instance, stateRoot, appPath, baseApp, baseExecutable, piece, previous }) {
  const id = previous ? validatePieceApp(previous).id : randomUUID();
  return validatePieceApp({ format: 1, id, name, workspace, instance, stateRoot,
    appPath: previous?.appPath || appPath, baseApp, baseExecutable, piece,
    bundleId: `computer.aesthetic.aesel.piece.${id.replaceAll('-', '')}` });
}

const sessionPath = project => path.join(project.stateRoot, 'sessions',
  createHash('sha256').update(`${project.workspace}\0${project.instance}`).digest('hex') + '.json');
function findSessionInstance({workspace, piece, stateRoot, fallback = 'default'}) {
  const candidates = new Set([fallback, 'default']);
  if (/^window-[a-f0-9-]{36}$/.test(path.basename(workspace))) candidates.add(path.basename(workspace));
  return [...candidates].flatMap(instance => {
    try {
      const saved = JSON.parse(fs.readFileSync(sessionPath({workspace, stateRoot, instance}), 'utf8'));
      if (saved.cwd !== workspace || saved.live?.file !== piece || !Number.isFinite(Date.parse(saved.savedAt))) return [];
      return [{instance, at:Date.parse(saved.savedAt)}];
    } catch { return []; }
  }).sort((a,b)=>b.at-a.at)[0]?.instance || fallback;
}
const registryDirectory = stateRoot => path.join(stateRoot, 'piece-apps');
function atomicJSON(file, value) {
  fs.mkdirSync(path.dirname(file), { recursive: true, mode: 0o700 });
  const temporary = file + '.' + randomUUID() + '.tmp';
  try { fs.writeFileSync(temporary, JSON.stringify(value, null, 2) + '\n', { mode: 0o600, flag: 'wx' }); fs.renameSync(temporary, file); }
  finally { fs.rmSync(temporary, { force: true }); }
}
function registerProject(project) {
  validatePieceApp(project);
  atomicJSON(path.join(registryDirectory(project.stateRoot), project.id, 'project.json'), project);
}
function registerRunning(project, { pid = process.pid, alive = n => process.kill(n, 0) } = {}) {
  const file = path.join(registryDirectory(project.stateRoot), project.id, 'running.json');
  try {
    const old = JSON.parse(fs.readFileSync(file, 'utf8'));
    if (old.pid !== pid) { try { alive(old.pid); throw new Error('This piece app is already running'); } catch (error) { if (error.code !== 'ESRCH') throw error; } }
  } catch (error) { if (error.code !== 'ENOENT') throw error; }
  atomicJSON(file, { pid, bundleId: project.bundleId, startedAt: new Date().toISOString() });
  return () => { try { if (JSON.parse(fs.readFileSync(file, 'utf8')).pid === pid) fs.rmSync(file); } catch {} };
}
function listProjects(stateRoot, { alive = pid => process.kill(pid, 0) } = {}) {
  const root = registryDirectory(stateRoot);
  let names; try { names = fs.readdirSync(root); } catch { return []; }
  return names.flatMap(id => {
    try {
      if (!/^[a-f0-9-]{36}$/.test(id)) return [];
      const project = readPieceApp(path.join(root, id, 'project.json'));
      if (!project || project.id !== id) return [];
      let pid = null;
      try { const running = JSON.parse(fs.readFileSync(path.join(root, id, 'running.json'), 'utf8')); if (Number.isInteger(running.pid) && running.pid > 0) { alive(running.pid); pid = running.pid; } } catch {}
      return [{ ...project, pid, installed: fs.existsSync(project.appPath) }];
    } catch { return []; }
  }).sort((a, b) => a.name.localeCompare(b.name));
}

module.exports = { validatePieceApp, readPieceApp, projectIdentity, sessionPath, findSessionInstance, atomicJSON, registerProject, registerRunning, listProjects };
