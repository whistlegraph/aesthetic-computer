import { model, tint } from './paint.mjs';
import { POOL, scaffold, windowFrame, berths } from './basement.mjs';
import { PLANET } from './planet.mjs';
import { LAP, SPAN } from './park.mjs';

export function poolFloor({ box }) {
  const { xMin, xMax, zMin, zMax, bottom } = POOL;
  const slab = (x0,x1,z0,z1,y) => box((x0+x1)/2,y-0.1,(z0+z1)/2,x1-x0,0.2,z1-z0,0x334050,0x283540,0x536476);
  slab(-8,xMin,-8,8,0); slab(xMax,8,-8,8,0);
  slab(xMin,xMax,-8,zMin,0); slab(xMin,xMax,zMax,8,0);
  slab(xMin,xMax,zMin,zMax,bottom);
  for (const x of [xMin,xMax]) box(x,bottom/2,(zMin+zMax)/2,0.08,-bottom,zMax-zMin,0x207d85);
  for (const z of [zMin,zMax]) box((xMin+xMax)/2,bottom/2,z,xMax-xMin,-bottom,0.08,0x207d85);
  for (const b of scaffold) {
    box((b.xMin+b.xMax)/2,b.yMax-0.06,(b.zMin+b.zMax)/2,b.xMax-b.xMin,0.12,b.zMax-b.zMin,0xbd9164,0xa97b53,0xe1ba86);
    for (const x of [b.xMin+0.1,b.xMax-0.1])
      box(x,b.yMax/2,b.zMin+0.1,0.11,b.yMax,0.11,0x48646a);
  }
  for (const b of windowFrame)
    box((b.xMin+b.xMax)/2,(b.yMin+b.yMax)/2,(b.zMin+b.zMax)/2,b.xMax-b.xMin,b.yMax-b.yMin,b.zMax-b.zMin,b.ink);
}

export function poolView() {
  const {xMin,xMax,zMin,zMax} = POOL;
  const water = model(({quad}) => {
    const cols = 8, rows = 17;
    for (let i=0; i<cols; i++) for (let j=0; j<rows; j++) {
      const x=xMin+(xMax-xMin)*i/cols, z=zMin+(zMax-zMin)*j/rows;
      const xx=xMin+(xMax-xMin)*(i+1)/cols, zz=zMin+(zMax-zMin)*(j+1)/rows;
      const ink=tint(0x237b8d,0.92+0.08*Math.sin(i*1.3+j*0.8));
      quad([x,0,z],[x,0,zz],[xx,0,zz],[xx,0,z],ink);
    }
  });
  const ripple = model(({box}) => box(0,0,0,0.38,0.009,0.028,0x76cada));
  const boat = model(({quad,box}) => {
    const rim=[[-0.38,-0.54],[0,-0.92],[0.38,-0.54],[0.38,0.53],[0,0.92],[-0.38,0.53]];
    for (let i=0;i<rim.length;i++) {
      const [x,z]=rim[i],[xx,zz]=rim[(i+1)%rim.length];
      quad([x,0.25,z],[xx,0.25,zz],[xx*0.62,-0.11,zz*0.8],[x*0.62,-0.11,z*0.8],i%2?0xe0904b:0xb66a3e);
    }
    box(0,0.03,0,0.53,0.08,1.22,0x5b4235);
    for (const z of [-0.34,0.32]) box(0,0.17,z,0.71,0.07,0.16,0xe6c18c);
  });
  let lastAtlas, pins;
  const globeRadius=1.16;
  return function drawPool(draw, globe, atlas, riders, now) {
    draw(water);
    for (let i=0;i<24;i++) {
      const x=xMin+0.3+((i*37)%73)/73*(xMax-xMin-0.6);
      const z=zMin+0.4+((i*23)%71)/71*(zMax-zMin-0.8);
      draw(ripple,x+0.14*Math.sin(now/550+i),0.016,z+0.06*Math.sin(now/900+i));
    }
    for (const berth of berths)
      if (!riders.some(p=>p?.boat && Math.hypot(p.x-berth.x,p.z-berth.z)<1.3))
        draw(boat,berth.x,0.03,berth.z);
    for (const p of riders) if (p?.alive && p.boat)
      draw(boat,p.x,0.035,p.z,p.yaw*Math.PI/180);
    if (atlas !== lastAtlas || !pins) {
      lastAtlas=atlas;
      pins=model(({box})=>{
        const mark=(x,z,color,size) => {
          const lon=2*Math.PI*x/(LAP*SPAN), lat=Math.PI*Math.max(-0.5,Math.min(0.5,z/(LAP*SPAN)));
          const r=globeRadius+0.015;
          box(r*Math.cos(lat)*Math.sin(lon),r*Math.sin(lat),r*Math.cos(lat)*Math.cos(lon),size,size,size,color);
        };
        for (const s of atlas?.structure || []) if (s.role==='block') mark((s.x0+s.x1)/2,(s.z0+s.z1)/2,0xbeeba8,0.055);
        mark(0,0,0xffca63,0.1);
        for (const p of atlas?.people || []) mark(p.x,p.z,0xff62a8,0.055);
      });
    }
    const turn=now/8000;
    draw(globe,-5.15,2.25,-1.5,turn,0,globeRadius/(PLANET-0.14));
    draw(pins,-5.15,2.25,-1.5,turn);
  };
}
