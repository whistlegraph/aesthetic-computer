// Logo, 23.09.05.22.31
// Retrieve / proxy a random logo url from the aesthetic.computer logo store.

const slugs = [
  "painting-2023.8.01.17.07.png",
  "painting-2023.8.21.10.25.png",
  "painting-2023.8.21.10.45.png",
  "painting-2023.8.01.14.52.png",
  "painting-2023.8.21.10.19.png",
  "painting-2023.8.01.14.04.png",
  "painting-2023.8.21.10.50.png",
  "painting-2023.8.21.10.31.png",
  "painting-2023.8.01.17.35.png",
  "painting-2023.8.01.14.33.png",
  "painting-2023.7.31.15.33.png",
  "painting-2023.7.29.20.39.png",
];

export function logoUrl() {
  const i = Math.floor(Math.random() * slugs.length);
  return `https://pals-aesthetic-computer.sfo3.cdn.digitaloceanspaces.com/${slugs[i]}`;
}
