// Aesel was called Easel, and its environment still says so in places: a shell
// profile, a launchd plist, a host app that has not updated. Importing this
// copies every EASEL_X that has no AESEL_X across, so the code reads AESEL_*
// alone and the older spelling keeps working. The newer name wins when both
// are set. Entry points import it first, before anything reads process.env.
//
// No node: imports on purpose — the phone runs this under a shimmed `process`.

export function adoptLegacyEnv(env = process.env) {
  for (const key of Object.keys(env)) {
    if (!key.startsWith("EASEL_")) continue;
    const next = `AESEL_${key.slice(6)}`;
    if (env[next] === undefined) env[next] = env[key];
  }
  return env;
}

// What Aesel hands a child goes out under both names: Slab's hooks on a machine
// that has not updated still look for EASEL_SESSION_ID, and an older Aesel
// reading its own harness socket looks for EASEL_HARNESS_SOCKET.
export function bothNames(vars) {
  const out = {};
  for (const [key, value] of Object.entries(vars)) {
    if (value === undefined) continue;
    out[key] = value;
    if (key.startsWith("AESEL_")) out[`EASEL_${key.slice(6)}`] = value;
  }
  return out;
}

adoptLegacyEnv();
