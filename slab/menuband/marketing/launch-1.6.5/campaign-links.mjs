#!/usr/bin/env node

const provider = process.env.MENUBAND_APPLE_PROVIDER_TOKEN;
if (!provider || !/^\d+$/.test(provider)) {
  console.error("Set MENUBAND_APPLE_PROVIDER_TOKEN to the numeric App Store provider token.");
  process.exit(1);
}

const base = "https://apps.apple.com/us/app/menu-band/id6767311903";
const campaigns = [
  "menuband-165-launch",
  "menuband-165-bluesky",
  "menuband-165-mastodon",
  "menuband-165-email",
  "menuband-165-press",
];

console.log("campaign,url");
for (const campaign of campaigns) {
  const query = new URLSearchParams({ pt: provider, ct: campaign, mt: "8" });
  console.log(`${campaign},${base}?${query}`);
}
