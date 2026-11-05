import { isObjectLisp } from "../public/aesthetic.computer/lib/object/viewer.mjs";

export function objectMetadata(code, source) {
  if (!/^[A-Za-z0-9]{3,64}$/.test(code) || !isObjectLisp(source)) return null;
  const title = source.match(/^\s*;\s*(?![ \t]*#3d\b)([^\n]+)/)?.[1]?.trim() || "KidLisp object";
  const base = "https://oven.aesthetic.computer/grab/";
  const piece = encodeURIComponent("$" + code);
  const query = "view=object&cacheKey=object-og-v1";
  return {
    title,
    desc: `A 3D KidLisp object on Aesthetic Computer. Open $${code} to orbit and play.`,
    image_url: `${base}gif/800/420/${piece}?${query}&duration=8000&fps=8&playbackFps=8`,
    twitter_image_url: `${base}png/600/315/${piece}?${query}&density=2`,
    image_type: "image/gif", image_width: 800, image_height: 420,
    image_alt: `Turntable view of ${title}`,
  };
}
