import { model, tint } from './paint.mjs';
import { POOL, deck, seaStairs, scaffold, windowFrame, berths } from './basement.mjs';
import { PLANET } from './planet.mjs';

export function poolFloor({ box }) {
  for (const b of [deck, ...seaStairs])
    box((b.xMin+b.xMax)/2,(b.yMin+b.yMax)/2,(b.zMin+b.zMax)/2,b.xMax-b.xMin,b.yMax-b.yMin,b.zMax-b.zMin,b.ink);
  for (const b of scaffold) {
    box((b.xMin+b.xMax)/2,b.yMax-0.06,(b.zMin+b.zMax)/2,b.xMax-b.xMin,0.12,b.zMax-b.zMin,0xbd9164,0xa97b53,0xe1ba86);
    for (const x of [b.xMin+0.1,b.xMax-0.1])
      box(x,b.yMax/2,b.zMin+0.1,0.11,b.yMax,0.11,0x48646a);
  }
  for (const b of windowFrame)
    box((b.xMin+b.xMax)/2,(b.yMin+b.yMax)/2,(b.zMin+b.zMax)/2,b.xMax-b.xMin,b.yMax-b.yMin,b.zMax-b.zMin,b.ink);
}

export function poolView() {
  // An altitude on the planet. The tessellation stays on a world-aligned
  // grid as the eye moves, and extends past the antipode in every direction.
  const tile = 4, repeat = tile * 4;
  const extent = Math.ceil(Math.PI * PLANET / repeat) * repeat + repeat;
  const water = model(({quad}) => {
    for (let x = -extent; x < extent; x += tile) for (let z = -extent; z < extent; z += tile) {
      const phase = 2 * Math.PI / repeat;
      const wave = Math.sin(x * phase + Math.sin(z * phase));
      const color = tint(0x248b99, 0.91 + 0.06 * wave);
      quad([x,0,z],[x,0,z+tile],[x+tile,0,z+tile],[x+tile,0,z],color);
    }
  });
  const glints = model(({quad}) => {
    for (let i = -8; i <= 8; i++) for (let j = -8; j <= 8; j++) {
      // Repeat on the same 16 m grid as the water; crossing a tile preserves the pattern.
      const x = i * 4 + Math.cos(j * Math.PI / 2);
      const z = j * 4 + Math.sin(i * Math.PI / 2);
      quad([x,0,z],[x+0.2,0,z+0.09],[x+1.1,0,z+0.05],[x+0.8,0,z],0x64b5b9);
    }
  });
  const boat = model(({quad,box}) => {
    const rim=[[-0.38,-0.54],[0,-0.92],[0.38,-0.54],[0.38,0.53],[0,0.92],[-0.38,0.53]];
    for (let i=0;i<rim.length;i++) {
      const [x,z]=rim[i],[xx,zz]=rim[(i+1)%rim.length];
      quad([x,0.25,z],[xx,0.25,zz],[xx*0.62,-0.11,zz*0.8],[x*0.62,-0.11,z*0.8],i%2?0xe0904b:0xb66a3e);
    }
    box(0,0.03,0,0.53,0.08,1.22,0x5b4235);
    for (const z of [-0.34,0.32]) box(0,0.17,z,0.71,0.07,0.16,0xe6c18c);
  });
  return function drawSea(draw, eye, plaza, riders, now) {
    const x = Math.round((eye.x-plaza.dx)/repeat)*repeat + plaza.dx;
    const z = Math.round((eye.z-plaza.dz)/repeat)*repeat + plaza.dz;
    const level = plaza.dy + POOL.level;
    draw(water,x,level,z);
    draw(glints,x+0.15*Math.sin(now/950),level+0.025,z);
    const rowboat = p => draw(boat,plaza.dx+p.x,level+0.035,plaza.dz+p.z,(p.yaw || 0)*Math.PI/180);
    for (const berth of berths)
      if (!riders.some(p=>p?.boat && Math.hypot(p.x-berth.x,p.z-berth.z)<1.3)) rowboat(berth);
    for (const p of riders) if (p?.alive && p.boat) rowboat(p);
  };
}
