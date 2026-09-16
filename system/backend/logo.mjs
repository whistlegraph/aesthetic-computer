// Logo, 23.09.05.22.31 · pals refreshed 2026-07-13
// Retrieve / proxy a random logo url from the aesthetic.computer logo store.
// The pool is the generated PALS logo set (materials / avatars / colorfields /
// insta / natural trays), authored via marketing/podcast/bin/gen-pals.mjs and
// hosted in the pals-aesthetic-computer DO Space.

export const logoSlugs = [
  "pals-av-amethyst.png",
  "pals-av-balloon.png",
  "pals-av-chrome-cool.png",
  "pals-av-clay-terra.png",
  "pals-av-felt-cozy.png",
  "pals-av-glass-pastel.png",
  "pals-av-gold-lux.png",
  "pals-av-holo-foil.png",
  "pals-av-jelly-candy.png",
  "pals-av-neon-punch.png",
  "pals-ceramic.png",
  "pals-cf-aqua.png",
  "pals-cf-citrus.png",
  "pals-cf-coral.png",
  "pals-cf-electric.png",
  "pals-cf-ember.png",
  "pals-cf-peach.png",
  "pals-cf-spectrum.png",
  "pals-cf-sunset.png",
  "pals-cf-teal.png",
  "pals-cf-violet.png",
  "pals-chrome.png",
  "pals-crystal.png",
  "pals-felt.png",
  "pals-glass.png",
  "pals-ig-black.png",
  "pals-ig-bubblegum.png",
  "pals-ig-cherry.png",
  "pals-ig-cobalt.png",
  "pals-ig-grape.png",
  "pals-ig-lime.png",
  "pals-ig-mint.png",
  "pals-ig-sky.png",
  "pals-ig-tangerine.png",
  "pals-nat-amber.png",
  "pals-nat-amethyst.png",
  "pals-nat-bone.png",
  "pals-nat-coral.png",
  "pals-nat-jade.png",
  "pals-nat-marble.png",
  "pals-nat-moss.png",
  "pals-nat-rosequartz.png",
  "pals-nat-sandstone.png",
  "pals-nat-terracotta.png",
  "pals-nat-walnut.png",
  "pals-nat-wool.png",
  "pals-neon.png",
  "pals-risograph.png",
  "pals-wood.png",
];

// Pals with a published looping turnaround (mp4 master + animated webp +
// apng) at art.aesthetic.computer/pals/turnarounds/v1/. Maintained by
// marketing/podcast/bin/publish-pals-turnaround.mjs — don't hand-edit.
export const turnaroundSlugs = [
  "av-balloon",
  "cf-electric",
  "cf-sunset",
  "chrome",
  "crystal",
  "felt",
  "glass",
  "ig-bubblegum",
  "nat-amethyst",
  "nat-jade",
  "nat-terracotta",
  "neon",
  "wood",
];

export const turnaroundFormats = ["mp4", "webp", "apng"];
export const PALS_CDN = "https://pals-aesthetic-computer.sfo3.cdn.digitaloceanspaces.com";
export const ART_CDN = "https://art.aesthetic.computer";

const pick = (list) => list[Math.floor(Math.random() * list.length)];
const stillSlug = (file) => file.replace(/^pals-/, "").replace(/\.png$/, "");
export const stillSlugs = logoSlugs.map(stillSlug);

export function logoUrl(slug = null) {
  const file = slug ? `pals-${slug}.png` : pick(logoSlugs);
  if (slug && !logoSlugs.includes(file)) return null;
  return `${PALS_CDN}/${file}`;
}

export function turnaroundUrl(slug = null, format = "webp") {
  const selected = slug || pick(turnaroundSlugs);
  if (!turnaroundSlugs.includes(selected) || !turnaroundFormats.includes(format)) return null;
  return `${ART_CDN}/pals/turnarounds/v1/${selected}.${format}`;
}

// A random pal, 50/50 still vs animated, never repeating `previous`.
// `animated` forces the coin: true → turnaround, false → still, null → 50/50.
export function randomPal({ previous = null, animated = null, format = "webp" } = {}) {
  let pal;
  for (let i = 0; i < 8; i++) {
    const coin = animated ?? Math.random() < 0.5;
    pal = coin && turnaroundSlugs.length
      ? { slug: pick(turnaroundSlugs), animated: true, format }
      : { slug: pick(stillSlugs), animated: false, format: "png" };
    pal.url = pal.animated ? turnaroundUrl(pal.slug, format) : logoUrl(pal.slug);
    if (pal.url !== previous) break;
  }
  return pal;
}
