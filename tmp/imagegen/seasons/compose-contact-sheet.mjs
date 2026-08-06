import sharp from "sharp";
import { join } from "node:path";

const root = "/Users/jas/aesthetic-computer";
const sourceDir = join(root, "output/imagegen/seasons-source");
const output = join(root, "seasons.png");
const items = [
  ["sterling-polished.png", "01 · POLISHED STERLING"],
  ["silver-oxidized.png", "02 · OXIDIZED SILVER"],
  ["gold-satin.png", "03 · SATIN GOLD"],
  ["rose-hammered.png", "04 · HAMMERED ROSE GOLD"],
  ["bronze-verdigris.png", "05 · AGED BRONZE"],
  ["enamel-four.png", "06 · FOUR-COLOR ENAMEL"],
  ["jade-gold.png", "07 · JADE + GOLD"],
  ["amber-gold.png", "08 · AMBER + GOLD"],
  ["mother-pearl.png", "09 · MOTHER OF PEARL"],
  ["dichroic-glass.png", "10 · DICHROIC GLASS"],
  ["porcelain.png", "11 · IVORY PORCELAIN"],
  ["walnut-gold.png", "12 · WALNUT + GOLD"],
];

const width = 2400;
const margin = 72;
const gutter = 24;
const header = 220;
const cellWidth = Math.floor((width - margin * 2 - gutter * 3) / 4);
const imageHeight = cellWidth;
const labelHeight = 66;
const cellHeight = imageHeight + labelHeight;
const height = header + margin + cellHeight * 3 + gutter * 2;
const composites = [];

for (const [index, [file, label]] of items.entries()) {
  const col = index % 4;
  const row = Math.floor(index / 4);
  const left = margin + col * (cellWidth + gutter);
  const top = header + row * (cellHeight + gutter);
  const image = await sharp(join(sourceDir, file))
    .resize(cellWidth, imageHeight, { fit: "cover" })
    .png()
    .toBuffer();
  composites.push({ input: image, left, top });
  const text = Buffer.from(`<svg width="${cellWidth}" height="${labelHeight}" xmlns="http://www.w3.org/2000/svg">
    <rect width="100%" height="100%" fill="#171714"/>
    <text x="22" y="41" font-family="Helvetica,Arial,sans-serif" font-size="21" font-weight="600" letter-spacing="1.5" fill="#f7f3e8">${label}</text>
  </svg>`);
  composites.push({ input: text, left, top: top + imageHeight });
}

const heading = Buffer.from(`<svg width="${width}" height="${header}" xmlns="http://www.w3.org/2000/svg">
  <rect width="100%" height="100%" fill="#f2efe6"/>
  <text x="72" y="91" font-family="Georgia,Times New Roman,serif" font-size="72" fill="#171714">SEASONS</text>
  <text x="75" y="143" font-family="Helvetica,Arial,sans-serif" font-size="22" letter-spacing="4" fill="#b44887">PENDANT · MATERIAL EXPLORATIONS</text>
  <text x="75" y="182" font-family="Helvetica,Arial,sans-serif" font-size="17" fill="#66645e">12 studies generated directly from the original Whistlegraph chalk drawing</text>
</svg>`);
composites.unshift({ input: heading, left: 0, top: 0 });

await sharp({ create: { width, height, channels: 3, background: "#f2efe6" } })
  .composite(composites)
  .png({ compressionLevel: 9 })
  .toFile(output);

console.log(output);
