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

// Three slots in a row on the dispenser's east side (its chute opens west).
const SLOTS = [-0.5, 0, 0.5].map((dz, i) => ({ id: `house-gun-${i + 1}`, x: DISPENSER.x + DISPENSER.width / 2 + 0.9, z: DISPENSER.z + dz }));

// The rack's guns, shaped as the store lists a room's things.
export function rackFor(room) {
  if (room !== RACK_ROOM) return [];
  return SLOTS.map((s) => ({ id: s.id, owner: HOUSE, x: s.x, z: s.z, item: 'shoot', effect: null, house: true,
    version: 0, code: 'qrr', geometry: pistol(), first: '$qrr' }));
}
export const isHouse = (id) => typeof id === 'string' && id.startsWith('house-gun-');
