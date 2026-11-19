// 🌬️ The air
// What the air is doing at a place and a moment, in metres a second: the
// wind over Los Angeles (weather.mjs) as it would be felt here. Nothing is
// sent: it is worked out from where you are in the world and the clock on
// the wall, so every page feels the same gust at the same place.
//
// Five things, added up:
//   the wind      weaker near the ground, stronger with height (a log law
//                 over rough ground: at ten metres it is the wind reported)
//   gusts         slow waves a few tens of metres long that travel downwind
//                 and swell it toward the gust speed reported
//   eddies        small swirls a few metres across, carried along by the
//                 wind, rougher near the ground and in the tower's lee
//   thermals      by day, under a clear sky, columns of rising air some
//                 forty metres across with sinking air between them
//   the tower     the wind climbs its windward face and tumbles behind it
// x is east, z is north, y is up, as everywhere in rax.
const Z0 = 0.4; // how rough the ground is (a park with trees and low buildings), metres
const TOWER = { x: 0, z: 4, r: 6 }; // the tower over the plaza, as the wind meets it
const wave = Math.sin;

// The wind as a heading: where it blows to, from the compass bearing it comes from.
export function heading(weather) {
  const to = (((weather?.from ?? 270) + 180) * Math.PI) / 180;
  return { x: Math.sin(to), z: Math.cos(to) };
}
// How strong the sun is on the ground: 0 by night, 1 at noon (phase 0 is midnight).
export const sun = (phase) => Math.max(0, Math.sin((phase - 0.25) * 2 * Math.PI));

export function airAt(weather, x, y, z, seconds, { day = 0 } = {}) {
  const speed = weather?.wind || 0, gust = Math.max(speed, weather?.gust || speed), cloud = weather?.cloud ?? 0;
  const u = heading(weather), h = Math.max(0.3, y);
  const rise = Math.min(1.7, Math.log((h + Z0) / Z0) / Math.log((10 + Z0) / Z0));
  // Downwind and crosswind of here, the downwind one sliding with the wind.
  const s = x * u.x + z * u.z - speed * seconds * 0.7, c = x * u.z - z * u.x;
  const swell = 0.5 * wave(s / 23 + c / 41) + 0.3 * wave(s / 9.7 - c / 17 + 1.3) + 0.2 * wave(seconds * 0.21 + c / 29);
  let along = (speed + (gust - speed) * Math.max(-0.4, swell)) * rise;
  // The tower: ahead of it the air is slowed and climbs; behind it, slowed and torn.
  const tx = x - TOWER.x, tz = z - TOWER.z, front = -(tx * u.x + tz * u.z), offside = Math.abs(tx * u.z - tz * u.x);
  const reach = Math.max(0, 1 - offside / (TOWER.r * 1.5));
  const windward = reach * Math.max(0, 1 - Math.abs(front - TOWER.r) / (TOWER.r * 1.5)) * (front > 0 ? 1 : 0);
  const lee = reach * (front < 0 ? Math.max(0, 1 + front / (TOWER.r * 5)) : 0);
  let up = along * 0.6 * windward;
  along *= 1 - 0.5 * windward - 0.6 * lee;
  // Eddies: three swirls of different size on each axis, carried downwind.
  const rough = (0.25 + 0.1 * gust) * (1 + 1.5 / (1 + h)) * (1 + 2.5 * lee), px = x - u.x * along * seconds, pz = z - u.z * along * seconds;
  const ex = wave(px / 3.1 + pz / 5.3 + seconds * 0.9) + 0.6 * wave(pz / 2.3 - y / 3.7 + seconds * 1.7) + 0.4 * wave(px / 7.9 + y / 2.1 - seconds * 0.6);
  const ez = wave(pz / 3.7 - px / 4.9 + seconds * 0.8 + 2) + 0.6 * wave(px / 2.7 + y / 4.3 - seconds * 1.5) + 0.4 * wave(pz / 8.3 - y / 2.9 + seconds * 0.5);
  const ey = wave(px / 4.3 + pz / 3.9 + seconds * 1.1 + 4) + 0.5 * wave(px / 2.1 - pz / 2.9 - seconds * 1.9);
  // Thermals: cells some forty metres across that drift with the wind, risen
  // only by the sun on open ground, and not until a few metres up.
  const heat = day * (1 - 0.75 * cloud) * 1.4 * Math.min(1, h / 10);
  const cell = wave((x - u.x * speed * seconds * 0.3) / 13 + 1.1) * wave((z - u.z * speed * seconds * 0.3) / 13 + 0.4) + 0.4 * wave(seconds / 47 + x / 31);
  up += heat * (cell > 0.2 ? cell : cell * 0.5 - 0.05);
  return { x: u.x * along + ex * rough * 0.5, y: up + ey * rough * 0.35, z: u.z * along + ez * rough * 0.5 };
}
