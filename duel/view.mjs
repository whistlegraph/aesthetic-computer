import * as T from '/vendor/three.module.js';
import { WALLS, COLORS, CFG, direction, wallDistance, along } from './world.mjs';

export function createView(canvas) {
  const renderer = new T.WebGLRenderer({ canvas, antialias: true });
  renderer.setPixelRatio(Math.min(devicePixelRatio, 2));
  const scene = new T.Scene();
  scene.background = new T.Color(0x141720);
  scene.fog = new T.Fog(0x141720, 32, 85);
  const camera = new T.PerspectiveCamera(90, 1, 0.04, 130);
  scene.add(new T.HemisphereLight(0xffecd6, 0x39384e, 2.4));
  const sun = new T.DirectionalLight(0xffffff, 2);
  sun.position.set(-8, 24, -5); scene.add(sun);
  const materials = new Map();
  function material(color) {
    if (!materials.has(color)) materials.set(color, new T.MeshLambertMaterial({ color }));
    return materials.get(color);
  }
  function box(w, h, d, color, x, y, z, parent = scene) {
    const mesh = new T.Mesh(new T.BoxGeometry(w, h, d), material(color));
    mesh.position.set(x, y, z); parent.add(mesh); return mesh;
  }
  box(38, 0.2, 38, 0xb2a89e, 0, -0.1, 0);
  const grid = new T.GridHelper(36, 18, 0x8b817d, 0x9c928b);
  grid.position.y = 0.005; scene.add(grid);
  for (const [i, w] of WALLS.entries()) {
    const x = (w.xMin + w.xMax) / 2, y = (w.yMin + w.yMax) / 2, z = (w.zMin + w.zMax) / 2;
    box(w.xMax - w.xMin, w.yMax - w.yMin, w.zMax - w.zMin,
      i < 4 ? 0x3e4352 : 0x777780, x, y, z);
    if (i >= 4) box(w.xMax - w.xMin + 0.02, 0.18, w.zMax - w.zMin + 0.02,
      i === 4 ? COLORS[0] : COLORS[1], x, 5.3, z);
  }
  for (const seat of [0, 1]) {
    box(6, 0.02, 0.12, COLORS[seat], 0, 0.02, seat ? 12 : -12);
    box(0.12, 0.02, 2, COLORS[seat], 0, 0.02, seat ? 12 : -12);
    box(35.8, 0.18, 0.04, COLORS[seat], 0, 2.8, seat ? 17.98 : -17.98);
  }
  const ring = new T.Mesh(new T.RingGeometry(2.8, 2.85, 64), material(0xeee6d7));
  ring.rotation.x = -Math.PI / 2; ring.position.y = 0.015; scene.add(ring);

  const bodies = COLORS.map(color => {
    const body = new T.Group(); scene.add(body);
    box(0.6, 0.75, 0.4, color, 0, 0.9, 0, body);
    box(0.42, 0.42, 0.42, color, 0, 1.6, 0, body);
    box(0.32, 0.12, 0.03, 0x171a23, 0, 1.65, 0.22, body);
    box(0.16, 0.55, 0.55, color, -0.4, 0.95, 0.14, body);
    box(0.16, 0.55, 0.55, color, 0.4, 0.95, 0.14, body);
    const legs = [-0.18, 0.18].map(x => box(0.2, 0.55, 0.22, color, x, 0.28, 0, body));
    return { body, legs };
  });
  const rocketGeometry = new T.SphereGeometry(0.12, 8, 6);
  const rocketMeshes = new Map(), effects = [];
  let width = 0, height = 0;

  function effect(e) {
    let object;
    if (e.type === 'beam') {
      const geometry = new T.BufferGeometry().setFromPoints([
        new T.Vector3(e.from.x, e.from.y - 0.08, e.from.z), new T.Vector3(e.to.x, e.to.y, e.to.z),
      ]);
      object = new T.Line(geometry, new T.LineBasicMaterial({ color: COLORS[e.seat], transparent: true }));
    } else if (e.type === 'blast') {
      object = new T.Mesh(new T.SphereGeometry(0.3, 12, 8),
        new T.MeshBasicMaterial({ color: 0xffb059, transparent: true, wireframe: true }));
      object.position.set(e.at.x, e.at.y, e.at.z);
    } else return;
    scene.add(object); effects.push({ object, born: performance.now(), type: e.type });
  }

  function render({ players = [], me, seat = 0, third = false, rockets = [] } = {}) {
    if (width !== innerWidth || height !== innerHeight) {
      width = innerWidth; height = innerHeight;
      renderer.setSize(width, height, false); camera.aspect = width / height; camera.updateProjectionMatrix();
    }
    for (let i = 0; i < 2; i++) {
      const p = i === seat && me ? me : players[i], { body, legs } = bodies[i];
      body.visible = !!p?.alive && (i !== seat || third || !me);
      if (!p) continue;
      body.position.set(p.x, p.y - CFG.eyeHeight, p.z);
      body.rotation.y = p.yaw * Math.PI / 180;
      const stride = Math.min(1, Math.hypot(p.vx, p.vz) / 10);
      legs.forEach((leg, j) => { leg.rotation.x = Math.sin(performance.now() / 85 + j * Math.PI) * stride * 0.65; });
    }
    if (me) {
      const dir = direction(me.yaw, me.pitch);
      const eye = { x: me.x, y: me.y, z: me.z };
      let position = eye;
      if (third) {
        const back = { x: -dir.x, y: -dir.y + 0.22, z: -dir.z };
        const len = Math.hypot(back.x, back.y, back.z);
        for (const k of ['x', 'y', 'z']) back[k] /= len;
        position = along(eye, back, Math.max(0, wallDistance(eye, back, 4) - 0.2));
      }
      // The camera aims at the eye ray's impact; third person cannot shoot around cover.
      const aim = along(eye, dir, Math.max(1, wallDistance(eye, dir, 100)));
      camera.position.set(position.x, position.y, position.z);
      camera.lookAt(aim.x, aim.y, aim.z);
    } else {
      camera.position.set(15, 19, -22); camera.lookAt(0, 0, 0);
    }
    const ids = new Set(rockets.map(r => r.id));
    for (const [id, mesh] of rocketMeshes) if (!ids.has(id)) { scene.remove(mesh); rocketMeshes.delete(id); }
    for (const r of rockets) {
      if (!rocketMeshes.has(r.id)) {
        const mesh = new T.Mesh(rocketGeometry, material(0xffbd60));
        scene.add(mesh); rocketMeshes.set(r.id, mesh);
      }
      rocketMeshes.get(r.id).position.set(r.x, r.y, r.z);
    }
    for (let i = effects.length - 1; i >= 0; i--) {
      const e = effects[i], age = (performance.now() - e.born) / 250;
      if (age > 1) {
        scene.remove(e.object); e.object.geometry.dispose(); e.object.material.dispose(); effects.splice(i, 1);
      } else {
        e.object.material.opacity = 1 - age;
        if (e.type === 'blast') e.object.scale.setScalar(1 + age * 10);
      }
    }
    renderer.render(scene, camera);
  }
  return { render, effect };
}
