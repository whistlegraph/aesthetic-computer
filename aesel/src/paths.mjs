// Where Aesel keeps things, in one place.
//
// It was Easel first, so every folder here has an older twin. The ones in the
// home folder move: the first run after the rename carries ~/.config/easel
// across to ~/.config/aesel and leaves a symlink at the old path, because an
// older Aesel on this machine may still read it. When both exist the new one
// is used and the old one is left exactly as it is. Nothing here deletes.
//
// ~/.local/share moves one folder at a time rather than whole, for two
// reasons. ~/.local/share/easel is where easel.sh installed the program, so
// moving it would move the program out from under the links in ~/.local/bin;
// and ~/.local/share/aesel already exists on most machines (transcript.mjs has
// written there for a while), which under the whole-folder rule would leave
// history behind in the old one, unread.
//
// Workspace folders do not move. `.easel/` beside someone's project may be
// checked in (this repository's own is), so a workspace that has one keeps
// using it; only a workspace with neither gets `.aesel/`.
//
// `node:fs` is taken as a namespace so the phone's shimmed fs, which has no
// symlinkSync, can still load this.
import "./env.mjs";
import * as fs from "node:fs";
import { homedir } from "node:os";
import { basename, dirname, join } from "node:path";

function linked(path) {
  try { return fs.lstatSync(path).isSymbolicLink(); } catch { return false; }
}

// The new folder, moving the old one into it the first time.
export function migrated(next, old) {
  if (fs.existsSync(next) || !fs.existsSync(old) || linked(old)) return next;
  try {
    fs.mkdirSync(dirname(next), { recursive: true });
    fs.renameSync(old, next);
  } catch {
    return old; // could not move it (another volume, permissions): keep using it
  }
  try { fs.symlinkSync(next, old); } catch {}
  return next;
}

const envOf = (options) => options.env || process.env;
const homeOf = (options) => options.home || envOf(options).HOME || homedir();

export function configDir(options = {}) {
  const env = envOf(options);
  if (env.AESEL_CONFIG_DIR) return env.AESEL_CONFIG_DIR;
  const root = join(homeOf(options), ".config");
  return migrated(join(root, "aesel"), join(root, "easel"));
}

export const dataDir = (options = {}) => join(homeOf(options), ".local", "share", "aesel");
const legacyData = (options) => join(homeOf(options), ".local", "share", "easel");
const data = (name, old = name) => (options = {}) =>
  migrated(join(dataDir(options), name), join(legacyData(options), old));

// Saved piece revisions (revisions.mjs).
export function historyDir(options = {}) {
  return envOf(options).AESEL_HISTORY_DIR || data("history")(options);
}

// The local session log history-cli reads back (transcript.mjs). It has always
// been under the aesel name.
export function transcriptsDir(options = {}) {
  return envOf(options).AESEL_TRANSCRIPTS || join(dataDir(options), "transcripts");
}

// Shareable .easel transcripts (transcript-journal.mjs). They were kept in
// easel/transcripts, which would collide with the session log above, so the
// new home has its own name.
export const journalDir = data("journal", "transcripts");

// Toolchains fetched on demand, e.g. GBDK for Game Boy builds.
export const toolchainsDir = data("toolchains");

export function cacheDir(options = {}) {
  const root = envOf(options).XDG_CACHE_HOME || join(homeOf(options), ".cache");
  return migrated(join(root, "aesel"), join(root, "easel"));
}

const twin = (next, old) => (fs.existsSync(next) || !fs.existsSync(old) ? next : old);

// <cwd>/.aesel, or the .easel this workspace already has.
export const workspaceDir = (cwd) => twin(join(cwd, ".aesel"), join(cwd, ".easel"));
// <cwd>/.aesel-media, or the .easel-media this workspace already has.
export const mediaDir = (cwd) => twin(join(cwd, ".aesel-media"), join(cwd, ".easel-media"));
// The same two as names relative to cwd, for code that joins them itself.
export const workspaceName = (cwd) => basename(workspaceDir(cwd));
export const mediaName = (cwd) => basename(mediaDir(cwd));
