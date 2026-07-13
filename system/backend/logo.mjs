// Logo, 23.09.05.22.31 · pals refreshed 2026-07-13
// Retrieve / proxy a random logo url from the aesthetic.computer logo store.
// The pool is the generated PALS logo set (materials / avatars / colorfields /
// insta / natural trays), authored via marketing/podcast/bin/gen-pals.mjs and
// hosted in the pals-aesthetic-computer DO Space.

const slugs = [
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

export function logoUrl() {
  const i = Math.floor(Math.random() * slugs.length);
  return `https://pals-aesthetic-computer.sfo3.cdn.digitaloceanspaces.com/${slugs[i]}`;
}
