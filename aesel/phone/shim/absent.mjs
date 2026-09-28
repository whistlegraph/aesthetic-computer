// Stubs for the Node modules that `publish.mjs` and its import chain pull in at
// module scope but only call from code paths a phone never takes — `spawn` for
// the desktop syntax check, `createServer` for the loopback OAuth listener,
// `homedir` for the default ~/.ac-token location.
//
// They throw when called rather than returning something plausible. A stub that
// quietly answers is how you get a session that looks signed in and is not.

const absent = (name) => () => {
  throw new Error(`${name}() is not available in the phone client`);
};

export const spawn = absent("spawn");
export const execFile = absent("execFile");
export const execFileSync = absent("execFileSync");
export const request = absent("request");
export const createServer = absent("createServer");
export const createInterface = absent("createInterface");
export const homedir = () => "/home";
export const tmpdir = () => "/tmp";
export const networkInterfaces = () => ({});
export const platform = () => "webview";

export default { spawn, execFile, execFileSync, createServer, homedir, tmpdir, platform };

// Workspace tools and the MCP client (pro sessions only; the phone never runs
// them) import these at module scope.
export const readFile = async () => absent("readFile")();
export const writeFile = async () => absent("writeFile")();
export const mkdir = async () => absent("mkdir")();
export const rename = async () => absent("rename")();
export const rm = async () => absent("rm")();
export const stat = async () => absent("stat")();
export const readdir = async () => absent("readdir")();
export const copyFile = async () => absent("copyFile")();
export const access = async () => absent("access")();
export const unlink = async () => absent("unlink")();
export const open = async () => absent("open")();
export const appendFile = async () => absent("appendFile")();
