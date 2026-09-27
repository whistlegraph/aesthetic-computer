// Presentation-only prototype. Coordinates come from a host's existing pose.
// Never change authoritative physics or consume its random stream here.
export const manifest = Object.freeze({
  id: 'miniature-night-v1', status: 'prototype',
  switchablePrototype: true, nativeReady: true, native60FpsVerified: false,
  assets: { background: 'assets/underpass.png', props: 'assets/props.png' },
  backgroundSize: [1672, 941], atlasSize: [1774, 887],
  regions: {
    acHead: [80, 104, 285, 285], xboxHead: [524, 104, 285, 285],
    acLimb: [1058, 48, 104, 372], xboxLimb: [1503, 48, 104, 372],
    gun: [51, 543, 365, 253], skateboard: [460, 611, 412, 136],
    platform: [900, 606, 418, 162], rocket: [1368, 542, 372, 236],
  },
  seats: [{ id: 'ac', color: '#9f7ae8' }, { id: 'xbox', color: '#78c848' }],
});

// The same operation contract can target Canvas2D, D3D11 or GLES2. Native
// hosts need a retained texture + UV quad API before they can use this.
export function createThemeRenderer(driver) {
  for (const method of ['clear', 'sprite', 'line', 'disc', 'rect']) {
    if (typeof driver[method] !== 'function') throw new TypeError(`Missing ${method}`);
  }
  let theme = 'photorealistic';
  function sprite(region, x, y, width, height, angle = 0, flip = false) {
    driver.sprite('props', manifest.regions[region], x, y, width, height, angle, flip);
  }
  function limb(seat, a, b, width) {
    const dx = b[0] - a[0], dy = b[1] - a[1];
    const length = Math.hypot(dx, dy);
    if (length < 0.001) return;
    if (theme === 'flat') driver.line(a, b, width, manifest.seats[seat].color);
    else sprite(seat === 0 ? 'acLimb' : 'xboxLimb', (a[0]+b[0])/2,
      (a[1]+b[1])/2, width, length + width*.45, Math.atan2(dy, dx)-Math.PI/2);
  }
  return {
    get theme() { return theme; },
    setTheme(value) {
      if (!['flat', 'photorealistic'].includes(value)) throw new RangeError('Unknown theme');
      theme = value;
    },
    background(width, height) {
      driver.clear('#070b13');
      if (theme === 'photorealistic') driver.sprite('background', [0,0,...manifest.backgroundSize],
        width/2, height/2, width, height, 0, false);
    },
    platform(x,y,width,height) {
      if (theme === 'flat') driver.rect(x-width/2,y-height/2,width,height,'#465365');
      else sprite('platform',x,y,width,height);
    },
    prop(kind,x,y,width,height,angle=0,flip=false) {
      if (!manifest.regions[kind]) throw new RangeError('Unknown prop');
      if (theme === 'flat') driver.rect(x-width/2,y-height/2,width,height,'#88939e');
      else sprite(kind,x,y,width,height,angle,flip);
    },
    fighter(pose) {
      const {seat, head, radius, segments, hand, facing} = pose;
      if (seat !== 0 && seat !== 1) throw new RangeError('Unknown seat');
      for (const segment of segments) limb(seat, segment[0], segment[1], segment[2]);
      if (theme === 'flat') driver.disc(head[0],head[1],radius,manifest.seats[seat].color);
      else sprite(seat === 0 ? 'acHead' : 'xboxHead',head[0],head[1],radius*2,radius*2,
        pose.headAngle || 0, (seat === 0 ? 1 : -1) !== facing);
      this.prop('gun', hand[0]+facing*15,hand[1],48,32,0,facing<0);
    },
  };
}

export function canvasDriver(context, images) {
  return {
    clear(color) { context.fillStyle=color;context.fillRect(0,0,context.canvas.width,context.canvas.height); },
    sprite(asset, region, x,y,w,h,angle,flip) {
      context.save();context.translate(x,y);context.rotate(angle);context.scale(flip?-1:1,1);
      context.drawImage(images[asset],...region,-w/2,-h/2,w,h);context.restore();
    },
    line(a,b,width,color) { context.strokeStyle=color;context.lineWidth=width;context.lineCap='round';
      context.beginPath();context.moveTo(...a);context.lineTo(...b);context.stroke(); },
    disc(x,y,r,color) {context.fillStyle=color;context.beginPath();context.arc(x,y,r,0,Math.PI*2);context.fill();},
    rect(x,y,w,h,color) {context.fillStyle=color;context.fillRect(x,y,w,h);},
  };
}
