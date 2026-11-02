import * as T from '/vendor/three.module.js';
import { createPaint } from './paint.mjs';
import { WALLS, COLORS, CFG, direction, wallDistance, along, rayBox, bodyBox } from './world.mjs';

export function createView(canvas) {
  const renderer = new T.WebGLRenderer({ canvas, antialias: true });
  renderer.setPixelRatio(Math.min(devicePixelRatio, 2));
  const scene = new T.Scene();
  scene.background = new T.Color(0xe6bda0);
  scene.fog = new T.Fog(0xe6bda0, 38, 110);
  const camera = new T.PerspectiveCamera(90, 1, 0.04, 130);
  scene.add(new T.HemisphereLight(0xffecd6, 0x57415c, 1.5));
  const sun = new T.DirectionalLight(0xffe8c6, 2);
  sun.position.set(-8, 24, -5); scene.add(sun);
  const paint = createPaint(), { material, shape } = paint;
  function box(w, h, d, color, x, y, z, parent = scene) {
    const mesh = new T.Mesh(new T.BoxGeometry(w, h, d), material(color));
    const edges = new T.LineSegments(new T.EdgesGeometry(mesh.geometry), new T.LineBasicMaterial({ color: 0x423144 }));
    mesh.add(edges);
    mesh.position.set(x, y, z); parent.add(mesh); return mesh;
  }
  const floor = new T.Mesh(new T.PlaneGeometry(38, 38), new T.MeshToonMaterial({ map: paint.floorTexture }));
  floor.rotation.x = -Math.PI / 2; scene.add(floor);
  const sky = new T.Mesh(new T.SphereGeometry(100, 24, 16), new T.ShaderMaterial({
    side: T.BackSide, depthWrite: false,
    vertexShader: 'varying float height; void main(){height=normalize(position).y; gl_Position=projectionMatrix*modelViewMatrix*vec4(position,1.);}',
    fragmentShader: 'varying float height; void main(){vec3 a=vec3(.90,.74,.63),b=vec3(.51,.47,.65);gl_FragColor=vec4(mix(a,b,smoothstep(0.,.8,height)),1.);}',
  }));
  scene.add(sky);
  const sunDisk = new T.Mesh(new T.SphereGeometry(6, 24, 16), new T.MeshBasicMaterial({ color: 0xffe3a3 }));
  sunDisk.position.set(-30, 30, 65); scene.add(sunDisk);
  for (let i = 0; i < 14; i++) {
    const angle = i / 14 * Math.PI * 2, h = 8 + Math.sin(i * 2.3) * 5;
    shape(new T.ConeGeometry(10, h, 5), i % 2 ? 0xa1889c : 0xc49a8f,
      scene, Math.sin(angle) * 55, h / 2, Math.cos(angle) * 55, false);
  }
  for (const [i, w] of WALLS.entries()) {
    const x = (w.xMin + w.xMax) / 2, y = (w.yMin + w.yMax) / 2, z = (w.zMin + w.zMax) / 2;
    box(w.xMax - w.xMin, w.yMax - w.yMin, w.zMax - w.zMin,
      i < 4 ? 0x866c7e : i === 4 ? 0xc88168 : 0x8e8ca8, x, y, z);
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
  const field = new T.Mesh(new T.CircleGeometry(2.7, 64), new T.MeshBasicMaterial({ map: paint.fieldTexture }));
  field.rotation.x = -Math.PI / 2; field.position.y = 0.01; scene.add(field);
  for (const x of [-1, 1]) {
    const hoop = shape(new T.TorusGeometry(6, 0.32, 6, 32), 0xe7be8d, scene, x * 24, 12, 0);
    hoop.rotation.y = Math.PI / 2;
  }

  // Detached canvases supply textures only. Every visible mark has a world surface.
  function surface(w, h, pixelsW = 1024, pixelsH = 256) {
    const image = document.createElement('canvas'); image.width = pixelsW; image.height = pixelsH;
    const context = image.getContext('2d'), texture = new T.CanvasTexture(image);
    texture.colorSpace = T.SRGBColorSpace;
    const mesh = new T.Mesh(new T.PlaneGeometry(w, h), new T.MeshBasicMaterial({ map: texture }));
    return { mesh, context, texture, image };
  }
  const board = surface(24, 6, 2048, 512);
  for (const end of [-1, 1]) {
    const panel = new T.Mesh(board.mesh.geometry, board.mesh.material);
    panel.position.set(0, 7, end * 17.95); panel.rotation.y = end === 1 ? Math.PI : 0;
    scene.add(panel);
  }
  let boardText = '';
  function updateBoard(scores, status) {
    const text = JSON.stringify([scores, status]);
    if (text === boardText) return;
    boardText = text;
    const c = board.context;
    c.fillStyle = '#252936'; c.fillRect(0, 0, 2048, 512);
    c.textAlign = 'center'; c.textBaseline = 'middle';
    c.font = 'bold 220px monospace';
    c.fillStyle = '#ff8066'; c.fillText(String(scores[0]), 780, 132);
    c.fillStyle = '#fff8e8'; c.fillText(':', 1024, 132);
    c.fillStyle = '#85caff'; c.fillText(String(scores[1]), 1268, 132);
    c.fillStyle = '#fff8e8'; c.font = 'bold 96px monospace';
    c.fillText(status || 'FIRST TO 10', 1024, 302, 1920);
    c.font = '56px monospace';
    c.fillText('WASD MOVE · MOUSE AIM / FIRE · SPACE JUMP', 1024, 410);
    c.fillText('V CAMERA · ESC RELEASE', 1024, 472);
    board.texture.needsUpdate = true;
  }

  const bodies = COLORS.map(color => {
    const body = new T.Group(); scene.add(body);
    shape(new T.CapsuleGeometry(0.16, 0.5, 4, 8), color, body, 0, 1, 0);
    const head = shape(new T.SphereGeometry(0.28, 12, 8), color, body, 0, 1.62, 0);
    for (const x of [-0.095, 0.095]) {
      shape(new T.SphereGeometry(0.075, 8, 6), 0xfff3dc, head, x, 0.045, 0.24, false);
      shape(new T.SphereGeometry(0.032, 8, 6), 0x302638, head, x, 0.035, 0.303, false);
    }
    const mouth = shape(new T.TorusGeometry(0.08, 0.012, 4, 12, Math.PI), 0x302638, head, 0, -0.055, 0.263, false);
    mouth.rotation.z = Math.PI;
    const arms = [-0.3, 0.3].map(x => {
      const arm = shape(new T.CapsuleGeometry(0.085, 0.45, 4, 6), color, body, x, 1.05, 0.1);
      arm.rotation.x = -0.8; arm.rotation.z = x > 0 ? 0.25 : -0.25; return arm;
    });
    const legs = [-0.15, 0.15].map(x => {
      const leg = new T.Group(); leg.position.set(x, 0.68, 0); body.add(leg);
      shape(new T.CapsuleGeometry(0.09, 0.43, 4, 6), color, leg, 0, -0.26, 0);
      shape(new T.CapsuleGeometry(0.1, 0.15, 4, 6), color, leg, 0, -0.57, 0.07).rotation.x = Math.PI / 2;
      return leg;
    });
    const shadow = new T.Mesh(new T.CircleGeometry(0.55, 20), new T.MeshBasicMaterial({ color: 0x45394e, transparent: true, opacity: 0.28, depthWrite: false }));
    shadow.rotation.x = -Math.PI / 2; scene.add(shadow);
    const health = box(0.8, 0.08, 0.08, color, 0, 2.15, 0, body);
    const gun = new T.Group(); scene.add(gun);
    shape(new T.CylinderGeometry(0.17, 0.19, 0.8, 8), 0x65556f, gun).rotation.x = Math.PI / 2;
    shape(new T.TorusGeometry(0.17, 0.045, 4, 8), color, gun, 0, 0, 0.38);
    shape(new T.CircleGeometry(0.135, 8), 0x302638, gun, 0, 0, 0.41, false);
    box(0.12, 0.3, 0.18, color, 0, -0.2, -0.15, gun);
    const display = surface(0.23, 0.14, 256, 128);
    display.mesh.position.set(0, 0, -0.401); display.mesh.rotation.y = Math.PI;
    gun.add(display.mesh);
    return { body, legs, arms, head, shadow, health, gun, display, hp: -1, recoil: 0 };
  });
  const rocketGeometry = new T.SphereGeometry(0.12, 8, 6);
  const rocketMeshes = new Map(), effects = [];
  let width = 0, height = 0;

  function effect(e) {
    if (e.type === 'launch') { bodies[e.seat].recoil = 1; return; }
    if (e.type !== 'blast') return;
    const object = new T.Mesh(new T.IcosahedronGeometry(0.3, 0),
      new T.MeshBasicMaterial({ color: 0xffd78c, transparent: true }));
    object.position.set(e.at.x, e.at.y, e.at.z);
    scene.add(object); effects.push({ object, born: performance.now(), type: e.type });
    for (let i = 0; i < 10; i++) {
      const spark = new T.Mesh(new T.TetrahedronGeometry(0.12), new T.MeshBasicMaterial({ color: i % 2 ? 0xcd705b : 0xffe1a2, transparent: true }));
      spark.position.copy(object.position); scene.add(spark);
      effects.push({ object: spark, born: performance.now(), type: 'spark',
        origin: object.position.clone(), velocity: new T.Vector3(Math.sin(i * 2.4) * 3, 1 + i % 3, Math.cos(i * 2.4) * 3) });
    }
  }

  const aimPoint = new T.Mesh(new T.SphereGeometry(0.08, 8, 6), new T.MeshBasicMaterial({ color: 0xffe7aa }));
  scene.add(aimPoint);
  function render({ players = [], me, seat = 0, third = false, rockets = [], scores = [0, 0], status = '' } = {}) {
    paint.tick(performance.now() / 1000);
    updateBoard(scores, status);
    if (width !== innerWidth || height !== innerHeight) {
      width = innerWidth; height = innerHeight;
      renderer.setSize(width, height, false); camera.aspect = width / height; camera.updateProjectionMatrix();
    }
    for (let i = 0; i < 2; i++) {
      const p = i === seat && me ? me : players[i], { body, legs, arms, head, shadow, health, gun, display } = bodies[i];
      body.visible = !!p?.alive && (i !== seat || third || !me);
      gun.visible = !!p?.alive;
      shadow.visible = !!p?.alive;
      if (!p) continue;
      health.scale.x = Math.max(0.01, p.health / 100);
      if (bodies[i].hp !== p.health) {
        bodies[i].hp = p.health;
        const c = display.context;
        c.fillStyle = '#11151d'; c.fillRect(0, 0, 256, 128);
        c.fillStyle = p.health < 40 ? '#ff8066' : '#e8ffb0';
        c.font = 'bold 100px monospace'; c.textAlign = 'center'; c.textBaseline = 'middle';
        c.fillText(String(p.health), 128, 70); display.texture.needsUpdate = true;
      }
      const forward = direction(p.yaw, p.pitch), right = direction(p.yaw - 90);
      const recoil = bodies[i].recoil; bodies[i].recoil *= 0.78;
      gun.position.set(p.x + forward.x * (1.05 - recoil * 0.1) + right.x * 0.34,
        p.y + forward.y * 1.05 - 0.3, p.z + forward.z * (1.05 - recoil * 0.1) + right.z * 0.34);
      gun.rotation.set(-p.pitch * Math.PI / 180, p.yaw * Math.PI / 180, 0, 'YXZ');
      body.position.set(p.x, p.y - CFG.eyeHeight, p.z);
      body.rotation.y = p.yaw * Math.PI / 180;
      const stride = Math.min(1, Math.hypot(p.vx, p.vz) / 10);
      legs.forEach((leg, j) => { leg.rotation.x = Math.sin(performance.now() / 85 + j * Math.PI) * stride * 0.65; });
      arms.forEach((arm, j) => { arm.rotation.x = -0.8 + Math.sin(performance.now() / 85 + j * Math.PI) * stride * 0.15; });
      head.rotation.z = Math.sin(performance.now() / 180) * stride * 0.06;
      shadow.position.set(p.x, 0.025, p.z);
      shadow.scale.setScalar(Math.max(0.4, 1 - (p.y - CFG.eyeHeight) * 0.1));
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
      let distance = wallDistance(eye, dir, 100);
      for (const [i, p] of players.entries()) if (i !== seat && p?.alive) {
        distance = Math.min(distance, rayBox(eye, dir, bodyBox(p), 100));
      }
      const aim = along(eye, dir, Math.max(0.1, distance - 0.1));
      aimPoint.visible = me.alive;
      aimPoint.position.set(aim.x, aim.y, aim.z);
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
        if (e.type === 'spark') {
          e.object.position.copy(e.origin).addScaledVector(e.velocity, age);
          e.object.rotation.set(age * 3, age * 5, age);
        }
      }
    }
    renderer.render(scene, camera);
  }
  return { render, effect, setSurfaceFrame: paint.setSurfaceFrame };
}
