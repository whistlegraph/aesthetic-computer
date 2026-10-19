// site-folders.mjs — the sites lith hosts each live in a folder named after
// their domain under system/public (nopaint.art/, kidlisp.com/, …). The apex
// serves that whole tree, so aesthetic.computer/nopaint.art/ai/ was a second,
// unbranded front door to every site. Those paths redirect to the real host.
//
// Only folders that are also hosts in the Caddyfile redirect: a folder with no
// host of its own (false.work's build downloads, say) is genuinely served from
// the apex and stays. And a few apex paths are load-bearing by design — No
// Paint's sign-in page is the Auth0 callback and Stripe return registered for
// the app, and must keep the aesthetic.computer origin.

export const APEX_HOSTS = new Set(["aesthetic.computer", "www.aesthetic.computer", "prompt.ac", "www.prompt.ac"]);

// Paths under a site folder that stay on the apex, host-relative.
export const KEEP_ON_APEX = new Set([
  "/nopaint.art/ai/signin.html", // Auth0 callback + Stripe return for No Paint
]);

// Folders under public/ that are named like a domain, minus the apex's own.
export function siteFolders(entries) {
  return new Set(entries.filter((name) => /^[a-z0-9-]+(\.[a-z0-9-]+)+$/i.test(name) && name !== "aesthetic.computer"));
}

// The hosts a Caddyfile serves: every name on its `host` matcher lines.
export function caddyHosts(caddyfile) {
  const hosts = new Set();
  for (const line of caddyfile.split("\n")) {
    const m = /^\s*(?:@\w+\s+)?host\s+(.+)$/.exec(line);
    if (m) for (const name of m[1].trim().split(/\s+/)) hosts.add(name.toLowerCase());
  }
  return hosts;
}

// A path under a folder that belongs to a different host than the folder's
// name: the Note Paint studio lives in the nopaint.art folder's ai/ but its
// home is notepaint.art. Used only once that host is one the Caddyfile serves.
export const FOLDER_HOST_OVERRIDES = [
  { prefix: "/nopaint.art/ai/", host: "notepaint.art", strip: "/nopaint.art" },
];

// Where an apex request for a site folder belongs, or null to serve it here.
export function siteFolderRedirect({ host, path, search = "" }, { folders, hosts }) {
  if (!APEX_HOSTS.has((host || "").toLowerCase())) return null;
  const [, folder, ...rest] = path.split("/");
  if (!folder || !folders.has(folder) || !hosts.has(folder.toLowerCase())) return null;
  if (KEEP_ON_APEX.has(path)) return null;
  const override = FOLDER_HOST_OVERRIDES.find((o) => path.startsWith(o.prefix) && hosts.has(o.host));
  if (override) return `https://${override.host}${path.slice(override.strip.length)}${search}`;
  return `https://${folder}/${rest.join("/")}${search}`;
}
