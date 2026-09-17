// Presentation only: generated colors/accessories never enter the simulation.
export const rgb = value => [1, 3, 5].map(i => parseInt(value.slice(i, i + 2), 16));
export function validateFighter(fighter) {
  const a = fighter?.appearance;
  if (fighter?.version !== 1 || fighter.recipe !== 'oskiewar-capsule-fighter-v1' ||
      !/^[a-f0-9]{64}$/.test(fighter.hash) || !a ||
      ['skin', 'hair', 'shirt', 'pants', 'shoes'].some(k => !/^#[a-f0-9]{6}$/i.test(a[k])) ||
      !['none', 'short', 'long', 'curly'].includes(a.hairStyle) ||
      !['short', 'long'].includes(a.sleeves) || typeof a.beard !== 'boolean' || typeof a.glasses !== 'boolean')
    throw Error('Invalid fighter preview.');
  return { ...a, ...Object.fromEntries(['skin', 'hair', 'shirt', 'pants', 'shoes'].map(k => [k, rgb(a[k])])) };
}

// A compact, actual triangle mesh preview of the same capsule body used in
// play. Body lengths are fixed; generation can only select its appearance.
export function fighterMesh(appearance) {
  const faces = [];
  function ellipsoid(center, scale, color) {
    const point = (row, col) => {
      const t = row / 8 * Math.PI, p = col / 12 * Math.PI * 2;
      return [center[0] + Math.sin(t) * Math.cos(p) * scale[0],
        center[1] + Math.cos(t) * scale[1], center[2] + Math.sin(t) * Math.sin(p) * scale[2]];
    };
    for (let row = 0; row < 8; row++) for (let col = 0; col < 12; col++) {
      const a = point(row, col), b = point(row + 1, col), c = point(row + 1, col + 1), d = point(row, col + 1);
      faces.push({ points: [a, b, c], color }, { points: [a, c, d], color });
    }
  }
  const a = appearance;
  ellipsoid([0, 0.45, 0], [.34, .54, .23], a.shirt);
  ellipsoid([0, .97, 0], [.13, .18, .13], a.skin);
  ellipsoid([0, -.12, 0], [.34, .18, .22], a.pants);
  ellipsoid([0, 1.27, 0], [.31, .36, .29], a.skin);
  for (const side of [-1, 1]) {
    ellipsoid([side * .35, .7, 0], [.22, .16, .17], a.shirt);
    ellipsoid([side * .5, .48, 0], [.14, .35, .15], a.shirt);
    ellipsoid([side * .6, -.02, .03], [.12, .26, .13], a.sleeves === 'long' ? a.shirt : a.skin);
    ellipsoid([side * .61, -.3, .04], [.13, .14, .14], a.skin);
    ellipsoid([side * .2, -.53, 0], [.18, .5, .18], a.pants);
    ellipsoid([side * .21, -1.15, 0], [.14, .31, .15], a.pants);
    ellipsoid([side * .21, -1.46, .1], [.16, .11, .27], a.shoes);
    ellipsoid([side * .105, 1.3, .268], [.034, .035, .018], [20, 20, 25]);
  }
  if (a.hairStyle !== 'none') {
    ellipsoid([0, 1.51, -.025], [.325, a.hairStyle === 'curly' ? .21 : .14, .3], a.hair);
    if (a.hairStyle === 'long') ellipsoid([0, 1.23, -.15], [.34, .44, .18], a.hair);
  }
  if (a.beard) ellipsoid([0, 1.04, .165], [.22, .16, .14], a.hair);
  if (a.glasses) for (const side of [-1, 1]) ellipsoid([side * .115, 1.31, .29], [.09, .065, .02], [18, 20, 28]);
  return faces;
}
export function mountFighterPreview(host, fighter) {
  const appearance = validateFighter(fighter), mesh = fighterMesh(appearance);
  const canvas = document.createElement('canvas');
  canvas.width = 640; canvas.height = 760;
  canvas.style.cssText = 'width:100%;max-height:45vh;object-fit:contain;background:#deded5';
  canvas.setAttribute('aria-label', 'Generated fighter preview. Use the rotation slider to inspect all sides.');
  const slider = document.createElement('input');
  slider.type = 'range'; slider.min = '-180'; slider.max = '180'; slider.value = '-20';
  slider.setAttribute('aria-label', 'Rotate fighter'); slider.style.width = '100%';
  host.replaceChildren(canvas, slider);
  const ctx = canvas.getContext('2d');
  function paint() {
    const angle = Number(slider.value) * Math.PI / 180, c = Math.cos(angle), s = Math.sin(angle);
    ctx.fillStyle = '#deded5'; ctx.fillRect(0, 0, 640, 760);
    ctx.fillStyle = '#0002'; ctx.beginPath(); ctx.ellipse(320, 699, 130, 22, 0, 0, Math.PI * 2); ctx.fill();
    const rotate = ([x, y, z]) => [x * c + z * s, y, z * c - x * s];
    const projected = mesh.map(face => ({ color: face.color, points: face.points.map(rotate) }));
    projected.sort((a, b) => a.points.reduce((n, p) => n + p[2], 0) - b.points.reduce((n, p) => n + p[2], 0));
    for (const face of projected) {
      const [a, b, d] = face.points;
      const u = b.map((v, i) => v - a[i]), v = d.map((n, i) => n - a[i]);
      const normal = [u[1]*v[2]-u[2]*v[1], u[2]*v[0]-u[0]*v[2], u[0]*v[1]-u[1]*v[0]];
      const length = Math.hypot(...normal) || 1;
      const light = .65 + .35 * Math.abs((normal[0] * -.35 + normal[1] * .65 + normal[2] * .7) / length);
      ctx.fillStyle = `rgb(${face.color.map(v => Math.round(v * light)).join(',')})`;
      ctx.beginPath(); face.points.forEach(([x, y, z], i) => { const scale = 205 * 5 / (5 - z); ctx[i ? 'lineTo' : 'moveTo'](320 + x * scale, 390 - y * scale); });
      ctx.closePath(); ctx.fill();
    }
  }
  slider.addEventListener('input', paint); paint();
  return appearance;
}
