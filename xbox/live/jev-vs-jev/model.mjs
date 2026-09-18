// The model chooses every direction and button. No pursuit/attack heuristic.
export const motion = { left: 'Move left', right: 'Move right', still: 'Do not walk' };
export const actions = { kick: 'Kick (A), effective within about 165 units',
  punch: 'Punch (B), effective within about 130 units', block: 'Shield (X)',
  jump: 'Jump (up)', duck: 'Crouch (down)', item: 'Use held item (Y)', wait: 'No action button' };
export function decisionRequest(scene) {
  const instructions = 'You control one fighter in Oskiewar. Win by landing attacks while avoiding the opponent. ' +
    'Choose actual controller input for the next 250 milliseconds (450 milliseconds for a full jump). No other bot helps you. ' +
    'Coordinates: x increases right, y increases down; opponent.dx is opponent.x minus self.x. ' +
    'Walk toward the opponent to enter striking range. Shield can block attacks but cannot attack simultaneously. ' +
    'Use visible platforms/options to reach an opponent above you. Do not walk off your footing. ' +
    'Both fighters run the same model and receive the same rules.';
  return { state: cleanScene(scene), questions: {
    motion: { type: 'choice', criteria: motion, instructions },
    action: { type: 'choice', criteria: actions, instructions },
  } };
}
const number = value => Number.isFinite(value) ? Math.max(-100000, Math.min(100000, Math.round(value))) : 0;
const point = value => Object.fromEntries(['x','y','vx','vy','dx','dy','distance','facing','left','right','level','aim','takeoffLeft','takeoffRight','landLeft','landRight']
  .filter(key => Number.isFinite(value?.[key])).map(key => [key, number(value[key])]));
export function cleanScene(scene) {
  if (!scene?.self || !Number.isFinite(scene.self.x) || !Number.isFinite(scene.self.y)) throw new Error('Invalid fighter observation');
  const dx = Number.isFinite(scene.opponent?.dx) ? scene.opponent.dx : 0;
  const dy = Number.isFinite(scene.opponent?.dy) ? scene.opponent.dy : 0;
  return { relative: scene.opponent ? { opponentSide: dx < 0 ? 'left' : 'right',
      opponentLevel: dy > 120 ? 'above' : dy < -120 ? 'below' : 'same',
      punchInRange: Math.abs(dx) < 130 && Math.abs(dy) < 90,
      kickInRange: Math.abs(dx) < 165 && Math.abs(dy) < 90 } : null,
    self: { ...point(scene.self), grounded: scene.self.grounded === true,
    footing: scene.self.footing ? point(scene.self.footing) : null },
    opponent: scene.opponent ? { ...point(scene.opponent), attacking: scene.opponent.attacking === true } : null,
    floor: scene.floor ? point(scene.floor) : null,
    rungs: (Array.isArray(scene.rungs) ? scene.rungs : []).slice(0, 8).map(point),
    options: (Array.isArray(scene.options) ? scene.options : []).slice(0, 8).map(o => ({ ...point(o), kind: ['walk','jump','sink'].includes(o.kind) ? o.kind : 'walk' })) };
}
export function buttons(move, action) {
  if (!Object.hasOwn(motion, move) || !Object.hasOwn(actions, action)) throw new Error('Invalid controller choice');
  return [move === 'left' ? 'ArrowLeft' : move === 'right' ? 'ArrowRight' : null,
    { kick:'A', punch:'B', block:'X', jump:'ArrowUp', duck:'ArrowDown', item:'Y' }[action]].filter(Boolean);
}
