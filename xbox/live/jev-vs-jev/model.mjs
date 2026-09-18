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
  low_kick: 'Crouch, then kick while staying low. Sweeps feet or a detached head near the floor.',
  crouch_punch: 'Crouch, then punch while staying low. Lower strike than a standing punch.',
  crouch_block: 'Crouch and shield together against a low attack.',
  jump_kick: 'Jump, then kick in the air. With left/right motion this travels across the opponent.',
  jump_punch: 'Jump, then punch in the air. Choose the direction for an aerial approach or crossover.',
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
      for (const id of ['low_kick','crouch_punch','crouch_block','jump_kick','jump_punch']) delete criteria[id];
    } else {
      if (!combat.punch) for (const id of ['punch','crouch_punch','jump_punch']) delete criteria[id];
      const side = state.relative?.opponentSide || (state.self.facing < 0 ? 'left' : 'right');
      if (!(side === 'left' ? combat.kickLeft : combat.kickRight))
        for (const id of ['kick','low_kick','jump_kick']) delete criteria[id];
      if (combat.pogo) {
        delete criteria.punch; delete criteria.kick;
        for (const id of ['low_kick','crouch_punch','crouch_block','jump_kick','jump_punch']) delete criteria[id];
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
    'Punch takes 5 startup frames, 3 active frames and 9 recovery frames; kick takes 8/4/14 at 60 Hz. Only active hitboxes deal damage. ' +
    'strikes tests real attack boxes against current hurtboxes, assuming you face the opponent at your current position. canHit is geometric reach, not a guaranteed future hit. ' +
    'Choose an available strike with canHit=true when near. If standing strikes miss low, use low_kick or crouch_punch. ' +
    'Against a head on the ground, prefer a low kick whose box reaches it. Crouch_block handles low threats. ' +
    'Plan a short sequence: jump_kick or jump_punch combines a leap, travel and delayed aerial attack. Cross over a defensive opponent, then turn to attack on the next decision. ' +
    'Read opponent velocity, predictedDx200ms and recoveryMs: intercept where they are going or approach during recovery rather than chase an old position. Predictions assume constant velocity. ' +
    'Use self.combat to avoid attacks with missing limbs. If every strikes entry has canHit=false, change position or posture instead of repeating a missed attack. ' +
    'A missing left or right leg can remove the low kick toward that side: jump across the opponent, turn around, and use the surviving leg. ' +
    'When you are headOnly, A/B spit horizontally: jump or change platforms to align your shots with the opponent. ' +
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
  .concat(['reach','cost','attackFrame','recoveryMs'])
  .filter(key => Number.isFinite(value?.[key])).map(key => [key, number(value[key])]));
export function cleanScene(scene) {
  if (!scene?.self || !Number.isFinite(scene.self.x) || !Number.isFinite(scene.self.y)) throw new Error('Invalid fighter observation');
  const dx = Number.isFinite(scene.opponent?.dx) ? scene.opponent.dx : 0;
  const dy = Number.isFinite(scene.opponent?.dy) ? scene.opponent.dy : 0;
  return { relative: scene.opponent ? { opponentSide: dx < 0 ? 'left' : 'right',
      predictedDx200ms: number(dx + ((scene.opponent.vx || 0) - (scene.self.vx || 0)) * .2),
      opponentLevel: dy > 120 ? 'above' : dy < -120 ? 'below' : 'same',
      punchInRange: Math.abs(dx) < 130 && Math.abs(dy) < 90,
      kickInRange: Math.abs(dx) < 165 && Math.abs(dy) < 90 } : null,
    self: { ...point(scene.self), grounded: scene.self.grounded === true, ducking: scene.self.ducking === true,
    footing: scene.self.footing ? point(scene.self.footing) : null,
    combat: scene.self.combat ? Object.fromEntries(['punch','kickLeft','kickRight','item','stunned','headOnly','pogo'].map(key => [key, scene.self.combat[key] === true])) : null },
    opponent: scene.opponent ? { ...point(scene.opponent), ...Object.fromEntries(
      ['grounded','attacking','ducking','headOnly','pogo','blocking'].map(k=>[k,scene.opponent[k]===true])) } : null,
    strikes: Object.fromEntries(['punch','kick','low_kick','crouch_punch'].filter(k=>scene.strikes?.[k]).map(k=>[k,{
      canHit:scene.strikes[k].canHit===true, headshot:scene.strikes[k].headshot===true,
      clearance:Number.isFinite(scene.strikes[k].clearance)?number(scene.strikes[k].clearance):null,
      facing:number(scene.strikes[k].facing), startupMs:number(scene.strikes[k].startupMs), totalMs:number(scene.strikes[k].totalMs),
    }])),
    floor: scene.floor ? point(scene.floor) : null,
    rungs: (Array.isArray(scene.rungs) ? scene.rungs : []).slice(0, 8).map(point),
    options: (Array.isArray(scene.options) ? scene.options : []).slice(0, 8).map(o => ({ ...point(o), kind: ['walk','jump','sink'].includes(o.kind) ? o.kind : 'walk' })) };
}
const actionButtons = {
  kick:['A'], punch:['B'], block:['X'], jump:['ArrowUp'], duck:['ArrowDown'],
  drop:['ArrowDown'], item:['Y'], wait:[], low_kick:['ArrowDown','A'],
  crouch_punch:['ArrowDown','B'], crouch_block:['ArrowDown','X'],
  jump_kick:['ArrowUp','A'], jump_punch:['ArrowUp','B'],
};
export function buttons(move, action) {
  if (!Object.hasOwn(motion, move) || !Object.hasOwn(actions, action)) throw new Error('Invalid controller choice');
  return [move.endsWith('left') ? 'ArrowLeft' : move.endsWith('right') ? 'ArrowRight' : null,
    ...actionButtons[action]].filter(Boolean);
}
export function controlPlan(move, action) {
  const down = buttons(move, action);
  const aerial = action.startsWith('jump_');
  const low = action === 'low_kick' || action === 'crouch_punch';
  const motionMs = move === 'still' ? 0 : move.startsWith('face_') ? 20
    : move.startsWith('step_') ? 55 : aerial ? 450 : 180;
  const actionMs = action === 'jump' || aerial ? 450 : action === 'drop' ? 220
    : action === 'wait' ? 0 : low ? 550 : action === 'duck' || action === 'crouch_block' ? 300 : 150;
  const presses = down.map(key=>{
    const direction = key === 'ArrowLeft' || key === 'ArrowRight';
    const strike = key === 'A' || key === 'B';
    const windows = direction ? [[0,motionMs]] : action === 'drop' ? [[0,60],[130,220]]
      : low && strike ? [[100,250]] : aerial && strike ? [[240,390]] : [[0,actionMs]];
    return {key,windows};
  });
  const recoveryMs = action.includes('kick') ? 434 : action.includes('punch') ? 284 : 0;
  return { down, motionMs, actionMs, presses,
    releaseMs: Math.max(motionMs, actionMs, recoveryMs+(low?100:aerial?240:0), 150)+60 };
}
export function controlsAt(plan, elapsed) {
  if (!plan || elapsed < 0) return [];
  return plan.presses.filter(p=>p.windows.some(([from,to])=>elapsed>=from && elapsed<to)).map(p=>p.key);
}
