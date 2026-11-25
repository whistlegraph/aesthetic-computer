// 🔫 The rack beside the ball dispenser (ball.mjs DISPENSER): house guns
// anyone can pick up to try. They belong to the house, so nobody edits or
// clears them; the store lists them with the lobby's own things
// (object-store.mjs objects). One left lying anywhere goes back to its slot
// after a while (match.mjs), so the rack fills itself again.
import { BALL, DISPENSER } from './ball.mjs';
import { sourceGeometry, fitItemSource } from './object-language.mjs';

export const HOUSE = 'house';
export const RACK_ROOM = BALL.room;
export const RACK_RETURN_SECONDS = 15;

// @jeffrey's $qrr pistol, without its beep (three side by side would chatter).
const PISTOL = `; Pistol
(move 0 0 (* -.04 (abs (sin (* time 3))))
  (ink 48 44 42)
  (slab -.04 0 -.07 .04 .28 .07)
  (ink 72 74 80)
  (slab -.04 .28 -.09 .04 .40 .32)
  (slab -.035 .40 -.08 .035 .43 -.04)
  (slab -.01 .40 .27 .01 .43 .30)
  (ink 90 92 98)
  (capsule 0 .345 .28 0 .345 .40 .034 6)
  (ink black)
  (orb 0 .345 .405 .018 6)
  (ink 60 62 66)
  (capsule 0 .17 .06 0 .17 .17 .022 6)
  (capsule 0 .17 .17 0 .27 .175 .022 6)
  (ink 30 30 32)
  (slab -.012 .19 .07 .012 .26 .10)
  (ink 80 82 88)
  (slab -.014 .40 -.10 .014 .47 -.06))`;

let geometry = null;
const pistol = () => (geometry ||= sourceGeometry({ name: 'Pistol', source: fitItemSource(PISTOL) }));

// 🎯 A straw man on a post, to shoot at (match.mjs strike).
const SCARECROW = `; Scarecrow
(ink 120 84 48)
(slab -.06 0 -.06 .06 1.5 .06)
(slab -.7 1.25 -.05 .7 1.35 .05)
(ink 196 64 52)
(slab -.32 .85 -.16 .32 1.45 .16)
(ink 222 196 120)
(orb 0 1.68 0 .2 8)
(ink 70 48 30)
(slab -.26 1.82 -.26 .26 1.86 .26)
(slab -.14 1.86 -.14 .14 2.02 .14)`;
let straw = null;
const scarecrow = () => (straw ||= sourceGeometry({ name: 'Scarecrow', source: SCARECROW }));

// Three slots in a row on the dispenser's east side (its chute opens west).
const LOBBY = [-0.5, 0, 0.5].map((dz, i) => ({ id: `house-gun-${i + 1}`, x: DISPENSER.x + DISPENSER.width / 2 + 0.9, z: DISPENSER.z + dz }));
// 🛰️ The east room (world.mjs ROOM_HOSTS): a rack in the middle and two straw men down the far end.
const EAST = [-0.6, 0, 0.6].map((dx, i) => ({ id: `house-gun-e${i + 1}`, x: dx, z: -2 }));
const EAST_TARGETS = [-3, 3].map((x, i) => ({ id: `house-target-e${i + 1}`, x, z: 7 }));

// A room's house things, shaped as the store lists a room's things.
export function rackFor(room) {
  const gun = (s) => ({ id: s.id, owner: HOUSE, x: s.x, z: s.z, item: 'shoot', effect: null, house: true,
    version: 0, code: 'qrr', geometry: pistol(), first: '$qrr' });
  if (room === RACK_ROOM) return LOBBY.map(gun);
  if (room === 'east') return [...EAST.map(gun), ...EAST_TARGETS.map((s) => ({ id: s.id, owner: HOUSE, x: s.x, z: s.z, item: null,
    target: true, effect: null, house: true, version: 0, code: null, geometry: scarecrow(), first: 'a scarecrow' }))];
  return [];
}
export const isHouse = (id) => typeof id === 'string' && id.startsWith('house-');
