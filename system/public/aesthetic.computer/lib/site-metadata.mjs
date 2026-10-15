// Branded entries share the runtime, while retaining their public identity.
export function entryMetadata(host, slug) {
  const hostname = host.toLowerCase().split(":")[0].replace(/^www\./, "");
  if (hostname === "laklok.com" && ["prompt", "laklok"].includes(slug)) {
    return { title: "Laklok", desc: "Read along or say hi in the Aesthetic Computer community conversation.", standaloneTitle: true };
  }
  if (["p5.aesthetic.computer", "p5.prompt.ac"].includes(hostname) && slug === "prompt") {
    return { title: "p5", desc: "A creative coding entry point into Aesthetic Computer's pieces and playgrounds." };
  }
  return null;
}

const canonicalHosts = new Map([
  ["aesthetic.computer", "aesthetic.computer"],
  ["www.aesthetic.computer", "aesthetic.computer"],
  ["prompt.ac", "prompt.ac"], ["www.prompt.ac", "prompt.ac"],
  ["notepat.com", "notepat.com"], ["www.notepat.com", "notepat.com"],
  ["nopaint.art", "nopaint.art"], ["www.nopaint.art", "nopaint.art"],
  ["laklok.com", "laklok.com"], ["www.laklok.com", "laklok.com"],
  ["p5.aesthetic.computer", "p5.aesthetic.computer"], ["p5.prompt.ac", "p5.aesthetic.computer"],
]);

// Keep the public path, but never publish login/preview query strings or an
// unrecognized Host header as the canonical URL.
export function canonicalPageUrl(host, pathname = "/") {
  const canonicalHost = canonicalHosts.get(host.toLowerCase().split(":")[0]);
  if (!canonicalHost) return null;
  const url = new URL(`https://${canonicalHost}/`);
  url.pathname = pathname.startsWith("/") ? pathname.split(/[?#]/)[0] : "/";
  return url.href;
}
