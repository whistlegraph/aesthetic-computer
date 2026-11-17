// players.mjs — the apps a kind of file can play in when no core runs it here.
//
// A core runs a file inside Aesel. Some kinds are better left to a player the
// machine already has: a video in QuickTime or VLC. This is the table of
// those, a kind a row, and the choice someone made among them. The choice is
// kept in the config folder, so it holds across sessions and workspaces.
import { execFile, spawn } from "node:child_process";
import { readFile, writeFile, mkdir } from "node:fs/promises";
import { join } from "node:path";
import { configDir } from "./paths.mjs";

// The first one installed is used until someone picks another. `app` is a
// macOS application name; `bin` is a command on the PATH elsewhere. `here`
// is no app at all: the film loops in the terminal, drawn by core-stream.mjs.
// `loops` marks an app that can be told to play on a loop when it opens.
export const PLAYERS = {
  video: [
    { id: "here", name: "the terminal", here: true },
    { id: "quicktime", name: "QuickTime Player", app: "QuickTime Player", loops: true },
    { id: "vlc", name: "VLC", app: "VLC", bin: "vlc" },
    { id: "iina", name: "IINA", app: "IINA" },
    { id: "mpv", name: "mpv", bin: "mpv" },
  ],
};

/** The players for a kind that can run on this platform at all. */
export function playersFor(kind, platform = process.platform) {
  return (PLAYERS[kind] || []).filter((player) => player.here || (platform === "darwin" ? player.app || player.bin : player.bin));
}
/** The player an id names, in any kind's row; undefined when no row has it. */
export const playerNamed = (id) => Object.values(PLAYERS).flat().find((player) => player.id === String(id || "").toLowerCase());

/** The command that opens `file` in `player`. `quiet` keeps the player behind the terminal, for a reload. */
export function playerCommand(player, file, { platform = process.platform, quiet = false } = {}) {
  if (player.here) throw new Error("The terminal is not an app to open.");
  // Opened by script so it starts playing and goes round again at the end.
  if (platform === "darwin" && player.loops) {
    return ["osascript", ["-e", "on run argv", "-e", `tell application "${player.app}"`, "-e", "set film to open (POSIX file (item 1 of argv))",
      "-e", "set looping of film to true", "-e", "play film", ...(quiet ? [] : ["-e", "activate"]), "-e", "end tell", "-e", "end run", file]];
  }
  if (platform === "darwin" && player.app) return ["open", [...(quiet ? ["-g"] : []), "-a", player.app, file]];
  if (!player.bin) throw new Error(`${player.name} does not run on this computer.`);
  return [player.bin, [file]];
}

const ran = (command, args) => new Promise((resolve) => execFile(command, args, { timeout: 4000 }, (error) => resolve(!error)));
/** Whether the player is on this machine. */
export function installed(player, platform = process.platform) {
  if (player.here) return import("./core-stream.mjs").then(({ ffmpegPath }) => ffmpegPath()).then(Boolean);
  if (platform === "darwin" && player.app) return ran("open", ["-Ra", player.app]);
  if (!player.bin) return Promise.resolve(false);
  return ran(platform === "win32" ? "where" : "which", [player.bin]);
}

const file = (options) => join(configDir(options), "players.json");
export async function readChoices(options = {}) {
  try { const value = JSON.parse(await readFile(file(options), "utf8")); return value && typeof value === "object" ? value : {}; }
  catch { return {}; }
}
export async function choose(kind, id, options = {}) {
  const choices = { ...(await readChoices(options)), [kind]: id };
  await mkdir(configDir(options), { recursive: true });
  await writeFile(file(options), JSON.stringify(choices, null, 2) + "\n");
}

// The player to use for a kind: the one asked for, else the one chosen
// before, else the first installed. Throws with a sentence that names what
// to do when the one asked for is not for this kind or not on this machine.
export async function pickPlayer(kind, wanted = "", { platform = process.platform, isInstalled = installed, ...options } = {}) {
  const row = playersFor(kind, platform);
  if (!row.length) throw new Error(`No player is listed for ${kind} on this computer.`);
  const names = row.map((player) => player.id).join(", ");
  if (wanted) {
    const player = row.find((candidate) => candidate.id === wanted.toLowerCase());
    if (!player) throw new Error(`"${wanted}" does not play ${kind}. Players: ${names}.`);
    if (!(await isInstalled(player, platform))) throw new Error(`${player.name} is not installed on this computer. Players: ${names}.`);
    return player;
  }
  const choices = await readChoices(options);
  const chosen = row.find((candidate) => candidate.id === choices[kind]);
  if (chosen && (await isInstalled(chosen, platform))) return chosen;
  for (const player of row) if (await isInstalled(player, platform)) return player;
  throw new Error(`None of ${names} is installed on this computer.`);
}

/** Open the file in the player, detached, so it outlives nothing and blocks nothing. */
export function launch(player, target, options = {}) {
  const [command, args] = playerCommand(player, target, options);
  const child = spawn(command, args, { stdio: "ignore", detached: true });
  child.on("error", () => {});
  child.unref();
}
