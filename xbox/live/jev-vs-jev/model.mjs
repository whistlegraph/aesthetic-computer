// The model chooses every direction and button. No pursuit/attack heuristic.
export const motion = {
  left: 'Walk left for 180 ms (about 158 units). Close a large gap.',
  right: 'Walk right for 180 ms (about 158 units). Close a large gap.',
  step_left: 'Step left for 55 ms (about 48 units). Close a small gap.',
  step_right: 'Step right for 55 ms (about 48 units). Close a small gap.',
  face_left: 'Tap left for 20 ms (about 18 units). Face left to strike without running past.',
  face_right: 'Tap right for 20 ms (about 18 units). Face right to strike without running past.',
  still: 'Stay put and keep facing the same way. Attack from here when already close.',
};
export const actions = { kick: 'Attack with A: kick, reach about 165 units. Can kick while moving toward the opponent.',
  punch: 'Attack with B: punch, reach about 130 units. Can punch while moving toward the opponent.', block: 'Shield (X); cannot move or attack while shielding',
  jump: 'Jump (up)', duck: 'Crouch (down)',
  drop: 'Double tap down: drop through the platform you stand on to reach an opponent below, or ground pound while airborne',
  item: 'Use held item (Y)', wait: 'No action button' };
export function decisionRequest(scene) {
  const state = cleanScene(scene);
  const criteria = { ...actions };
  const combat = state.self.combat;
  if (combat) {
    if (combat.headOnly) {
      criteria.kick = 'Spit a fast projectile with A toward the opponent.';
      criteria.punch = 'Spit a heavy projectile with B toward the opponent.';
      delete criteria.block;
    } else {
      if (!combat.punch) delete criteria.punch;
      const side = state.relative?.opponentSide || (state.self.facing < 0 ? 'left' : 'right');
      if (!(side === 'left' ? combat.kickLeft : combat.kickRight)) delete criteria.kick;
      if (combat.pogo) {
        delete criteria.punch; delete criteria.kick;
        criteria.duck = 'Pogo dive: while airborne press down to attack the opponent below.';
      }
    }
    if (!combat.item) delete criteria.item;
  }
  const instructions = 'You control one fighter in Oskiewar. Win by landing attacks while avoiding the opponent. ' +
    'Choose a direction pulse and an action together. The next decision arrives roughly 400 ms later. No other bot helps you. ' +
    'Coordinates: x increases right, y increases down; opponent.dx = opponent.x - self.x, opponent.dy = self.y - opponent.y (positive means opponent above). ' +
    'Fight actively: close the gap, face the opponent, and punch or kick repeatedly when near. ' +
    'At distance under 100, stay still if facing the opponent, or choose a face tap if facing away; attack instead of walking through them. ' +
    'At distance 100 to 210, use a short step and attack together. At larger gaps, walk toward them. ' +
    'Attacks last 220 ms, so a strike while closing can connect even if just outside its range now. ' +
    'Use self.combat to avoid attacks with missing limbs. Prefer an available punch or kick over waiting. ' +
    'Do not jump just because an airborne opponent is passing overhead: wait on the ground for them to land and strike. ' +
    'Jump to reach a grounded opponent on a higher platform, not as a default dodge. ' +
    'Shield briefly only against an imminent strike. Shielding and retreating do not deal damage. ' +
    'Use visible platforms/options to reach an opponent above you. Choose drop to descend through a platform when the opponent is below; one crouch does not drop through. ' +
    'Both fighters run the same model and receive the same rules.';
  return { state, questions: {
    motion: { type: 'choice', criteria: motion, instructions },
    action: { type: 'choice', criteria, instructions },
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
    footing: scene.self.footing ? point(scene.self.footing) : null,
    combat: scene.self.combat ? Object.fromEntries(['punch','kickLeft','kickRight','item','stunned','headOnly','pogo'].map(key => [key, scene.self.combat[key] === true])) : null },
    opponent: scene.opponent ? { ...point(scene.opponent), grounded: scene.opponent.grounded === true, attacking: scene.opponent.attacking === true } : null,
    floor: scene.floor ? point(scene.floor) : null,
    rungs: (Array.isArray(scene.rungs) ? scene.rungs : []).slice(0, 8).map(point),
    options: (Array.isArray(scene.options) ? scene.options : []).slice(0, 8).map(o => ({ ...point(o), kind: ['walk','jump','sink'].includes(o.kind) ? o.kind : 'walk' })) };
}
export function buttons(move, action) {
  if (!Object.hasOwn(motion, move) || !Object.hasOwn(actions, action)) throw new Error('Invalid controller choice');
  return [move.endsWith('left') ? 'ArrowLeft' : move.endsWith('right') ? 'ArrowRight' : null,
    { kick:'A', punch:'B', block:'X', jump:'ArrowUp', duck:'ArrowDown', drop:'ArrowDown', item:'Y' }[action]].filter(Boolean);
}
export function controlPlan(move, action) {
  const down = buttons(move, action);
  const motionMs = move === 'still' ? 0 : move.startsWith('face_') ? 20 : move.startsWith('step_') ? 55 : 180;
  const actionMs = action === 'jump' ? 450 : action === 'drop' ? 220 : action === 'wait' ? 0 : 150;
  return { down, motionMs, actionMs, actionWindows: action === 'drop' ? [[0,60],[130,220]] : [[0,actionMs]],
    releaseMs: Math.max(motionMs, actionMs, 150) + 60 };
}
export function controlsAt(plan, elapsed) {
  if (!plan || elapsed < 0) return [];
  return plan.down.filter(key => key === 'ArrowLeft' || key === 'ArrowRight'
    ? elapsed < plan.motionMs : plan.actionWindows.some(([from,to])=>elapsed >= from && elapsed < to));
}
