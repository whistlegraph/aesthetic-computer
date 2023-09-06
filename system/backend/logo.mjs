// Logo, 23.09.05.22.31
// Retrieve / proxy a logo url from aesthetic.computer.

const slugs = [
  "painting-2023.8.01.17.07.png",
  "painting-2023.8.21.10.25.png",
  "painting-2023.8.21.10.45.png",
  "painting-2023.8.01.14.52.png",
  "painting-2023.8.21.10.19.png",
  "painting-2023.8.01.14.04.png",
];

export function logoUrl() {
  const i = Math.floor(Math.random() * slugs.length);
  return `https://logo.aesthetic.computer.nyc3.cdn.digitaloceanspaces.com/${slugs[i]}`;
}
