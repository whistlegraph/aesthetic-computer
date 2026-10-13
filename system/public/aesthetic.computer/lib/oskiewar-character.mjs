// Hand-edited Oskiewar colors and facial marks. No account data or generated likeness.
export const CHARACTER_PARTS = Object.freeze([
  'head', 'torso', 'leftArm', 'rightArm', 'leftLeg', 'rightLeg',
]);
export const CHARACTER_EYES = Object.freeze(['none', 'dots', 'wide', 'sleepy']);
export const CHARACTER_MOUTHS = Object.freeze(['none', 'smile', 'neutral', 'frown']);

export function characterHandle(value) {
  const handle = String(value || '').replace(/^@/, '').toLowerCase();
  return /^[a-z0-9][a-z0-9._-]{0,63}$/.test(handle) ? handle : '';
}

export function characterEditorUrl(handle) {
  const name = characterHandle(handle);
  return name ? `https://aesthetic.computer/oskiewarchar:@${name}` : '';
}

export function defaultCharacter(color = [190, 190, 198]) {
  return { version: 1, colors: Object.fromEntries(CHARACTER_PARTS.map(part =>
    [part, color.slice()])), face: { eyes: 'dots', mouth: 'smile', color: [36, 30, 47] } };
}

function rgb(value) {
  if (!Array.isArray(value) || value.length !== 3 ||
      !value.every(channel => Number.isInteger(channel) && channel >= 0 && channel <= 255))
    throw new Error('Colors need three whole numbers from 0 to 255.');
  return value.slice();
}

function keys(value, allowed) {
  if (!value || typeof value !== 'object' || Array.isArray(value) ||
      Object.keys(value).length !== allowed.length ||
      Object.keys(value).some(key => !allowed.includes(key)))
    throw new Error('Invalid character fields.');
}

export function validateCharacter(value) {
  keys(value, ['version', 'colors', 'face']);
  if (value.version !== 1) throw new Error('Unsupported character version.');
  keys(value.colors, CHARACTER_PARTS);
  keys(value.face, ['eyes', 'mouth', 'color']);
  if (!CHARACTER_EYES.includes(value.face.eyes) || !CHARACTER_MOUTHS.includes(value.face.mouth))
    throw new Error('Choose a supported face.');
  return { version: 1,
    colors: Object.fromEntries(CHARACTER_PARTS.map(part => [part, rgb(value.colors[part])])),
    face: { eyes: value.face.eyes, mouth: value.face.mouth, color: rgb(value.face.color) } };
}

// Player part names are authoritative; animation roles vary by held object.
export function characterPartColor(appearance, part, fallback) {
  const key = { head: 'head', torso: 'torso', 'left-arm': 'leftArm',
    'right-arm': 'rightArm', 'left-leg': 'leftLeg', 'right-leg': 'rightLeg' }[part];
  return appearance?.colors?.[key] || fallback;
}
