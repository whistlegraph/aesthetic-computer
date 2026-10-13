// Oskiewarchar, 26.10.12
// Edit your Oskiewar character's colors and face live.

import { CHARACTER_PARTS, CHARACTER_EYES, CHARACTER_MOUTHS, characterHandle,
  defaultCharacter, validateCharacter } from '../lib/oskiewar-character.mjs';

const API = '/api/oskiewar-character';
const LABELS = { head: 'Head', torso: 'Torso', leftArm: 'L arm', rightArm: 'R arm',
  leftLeg: 'L leg', rightLeg: 'R leg', face: 'Face ink' };
const SWATCHES = [[255,119,171],[255,190,135],[255,232,145],[187,233,159],
  [131,222,212],[142,180,250],[192,161,236],[239,224,238],
  [180,74,102],[178,119,78],[227,176,73],[95,145,109],
  [50,121,132],[68,87,145],[113,78,143],[36,30,47]];
let api, target, owner, appearance, revision, selected, mode, status, editable;
let regions = [], saveTimer, saving = false, dirty = false, disposed = false, generation = 0;
let scroll = 0, maxScroll = 0, dragY = null, activeSlider = null;
let blocked = false;

export function meta() { return { title: 'oskiewarchar', desc: 'Your Oskiewar character, live.' }; }

export async function boot(context) {
  clearTimeout(saveTimer);
  generation++; disposed = false; api = context; saving = false; dirty = false;
  scroll = maxScroll = 0; dragY = null; activeSlider = null;
  blocked = false;
  target = characterHandle(context.colon?.[0] || context.params?.[0]);
  owner = ''; revision = 0; selected = 'head'; mode = 'body';
  appearance = defaultCharacter(); editable = false; status = 'Loading';
  context.hud?.label?.(''); // The editor draws its handle header in this corner.
  await load();
}

async function load() {
  if (saving) return;
  const ticket = generation;
  status = 'Loading'; editable = false; blocked = false; dirty = false;
  try {
    let mine;
    if (api.user) mine = await api.net.userRequest('GET', API);
    if (ticket !== generation || disposed) return;
    owner = characterHandle(mine?.handle);
    target ||= owner;
    const record = target && target !== owner
      ? await fetch(`${API}?handle=${encodeURIComponent(target)}`).then(async response =>
        ({ ...(await response.json()), status: response.status }))
      : mine;
    if (ticket !== generation || disposed) return;
    if (record?.status === 200) {
      appearance = record.appearance ? validateCharacter(record.appearance) : defaultCharacter();
      revision = record.revision;
    } else if (record && record.status !== 401) {
      throw new Error(record.error || 'Could not load your character.');
    }
    editable = !!owner && target === owner && record?.status === 200;
    status = editable ? 'Live' : owner ? `Log in as @${target}` : 'Log in to edit';
  } catch (error) { status = error.message || 'Could not connect. Tap Reload.'; }
  api.help?.repeat?.();
}

function changed() {
  if (!editable || blocked) return;
  dirty = true; status = 'Saving';
  clearTimeout(saveTimer);
  saveTimer = setTimeout(save, 300);
  api.help?.repeat?.();
}

async function save() {
  if (saving || !dirty || !editable || blocked || disposed) return;
  const ticket = generation;
  saving = true; dirty = false;
  const payload = { handle: target, baseRevision: revision, appearance: validateCharacter(appearance) };
  try {
    const response = await api.net.userRequest('POST', API, payload);
    if (disposed || ticket !== generation) return;
    if (response?.status !== 200) {
      blocked = response?.status === 409 || response?.status === 403 || response?.status === 401;
      throw new Error(blocked ? 'Reload to continue' : 'Could not save');
    }
    revision = response.revision;
    status = dirty ? 'Saving' : 'Live';
  } catch (error) {
    if (disposed || ticket !== generation) return;
    dirty = true; status = error.message || 'Could not save. Tap Retry.';
  } finally {
    if (ticket === generation) {
      saving = false;
      if (!disposed && dirty && status === 'Saving') saveTimer = setTimeout(save, 300);
      api.help?.repeat?.();
    }
  }
}

function color() { return selected === 'face' ? appearance.face.color : appearance.colors[selected]; }
function chooseColor(value) {
  if (selected === 'face') appearance.face.color = value.slice();
  else appearance.colors[selected] = value.slice();
  changed();
}
function fitText(value, width) { const n = Math.max(1, Math.floor(width / 6)); return value.length <= n ? value : value.slice(0, n - 1) + '…'; }

export function paint({ wipe, ink, screen }) {
  wipe(28, 26, 39); regions = [];
  const width = screen.width, height = screen.height, margin = 12;
  const label = (text, x, y, rgb = [235,224,241]) => ink(...rgb).write(text, { x: Math.round(x), y: Math.round(y) });
  const button = (text, x, y, w, action, active = false) => {
    ink(...(active ? [83,64,103] : [45,41,58])).box(x,y,w,22);
    label(fitText(text,w-10),x+5,y+6,active ? [255,194,223] : [235,224,241]);
    regions.push({x,y,w,h:22,action});
  };
  label(target ? '@' + target : 'Your character',margin,14);
  const landscape = width > height * 1.3;
  const previewWidth = landscape ? Math.max(90,width*.32) : width;
  const previewHeight = landscape ? height - 80 : Math.min(150,Math.max(82,height*.27));
  drawPreview(ink, previewWidth/2, 43 + previewHeight*.43 - (landscape ? 0 : scroll),
    Math.min(previewHeight/104,previewWidth/100));
  let x = landscape ? previewWidth + 4 : margin;
  let y = (landscape ? 42 : 43 + previewHeight) - scroll;
  const cw = width-x-margin;
  if (!editable) {
    label(fitText(status,width-margin*2),margin,height-58,[255,182,208]);
    button(owner && target !== owner ? 'Switch account' : 'Log in',margin,height-36,105,
      () => { if (owner && target !== owner) api.net.logout(); else api.net.login(); });
    button('Reload',width-78,height-36,66,load);
    return;
  }

  button('Body',x,y,cw/2-3,()=>{mode='body';if(selected==='face')selected='head';},mode==='body');
  button('Face',x+cw/2+3,y,cw/2-3,()=>{mode='face';selected='face';},mode==='face');
  y += 27;
  if (mode === 'body') {
    const cell = (cw-8)/3;
    CHARACTER_PARTS.forEach((part,i)=>button(LABELS[part],x+(i%3)*(cell+4),y+Math.floor(i/3)*26,cell,
      ()=>{selected=part;},selected===part));
    y += 57;
  } else {
    button('Eyes: '+appearance.face.eyes,x,y,cw,()=>{
      appearance.face.eyes=CHARACTER_EYES[(CHARACTER_EYES.indexOf(appearance.face.eyes)+1)%CHARACTER_EYES.length];changed();
    });
    button('Mouth: '+appearance.face.mouth,x,y+26,cw,()=>{
      appearance.face.mouth=CHARACTER_MOUTHS[(CHARACTER_MOUTHS.indexOf(appearance.face.mouth)+1)%CHARACTER_MOUTHS.length];changed();
    });
    y += 57;
  }
  const sw = cw/8;
  SWATCHES.forEach((rgb,i)=>{
    const sx=x+(i%8)*sw, sy=y+Math.floor(i/8)*23;
    ink(...rgb).box(sx+1,sy,sw-3,19);
    regions.push({x:sx,y:sy,w:sw,h:21,action:()=>chooseColor(rgb)});
  });
  y += 49;
  const rgb=color(), trackX=x+16, trackWidth=cw-44;
  for(let i=0;i<3;i++) {
    const sy=y+i*22;
    label(['R','G','B'][i],x,sy+4);
    ink(61,54,76).box(trackX,sy+7,trackWidth,4);
    ink(...rgb).box(trackX,sy+7,trackWidth*rgb[i]/255,4);
    ink(245,234,248).circle(trackX+trackWidth*rgb[i]/255,sy+8,4,true);
    label(String(rgb[i]),trackX+trackWidth+5,sy+4);
    regions.push({x:trackX-4,y:sy-2,w:trackWidth+8,h:22,drag:true,
      action:px=>{const next=color().slice();next[i]=Math.max(0,Math.min(255,Math.round((px-trackX)/trackWidth*255)));chooseColor(next);}});
  }
  const footerY = Math.max(y+70,height-25);
  maxScroll = Math.max(0,y+70+scroll+27-height);
  label(fitText(status,cw-55),x,footerY,[169,219,187]);
  if (dirty && !saving && !blocked) button('Retry',width-65,footerY-6,53,save);
  else button('Reload',width-70,footerY-6,58,()=>{dirty=false;load();});
  if (maxScroll) {
    const rail=height-45, thumb=Math.max(20,rail*height/(height+maxScroll));
    ink(70,60,88).box(width-4,35,2,rail);
    ink(206,158,221).box(width-4,35+(rail-thumb)*scroll/maxScroll,2,thumb);
  }
  ink(28,26,39).box(0,0,width,34);
  label(target ? '@' + target : 'Your character',margin,14);
}

function drawPreview(ink,cx,cy,scale) {
  const at=(x,y)=>[cx+x*scale,cy+y*scale];
  const box=(part,x,y,w,h)=>{ink(...appearance.colors[part]).box(...at(x,y),w*scale,h*scale);};
  ink(18,17,27).box(...at(-28,58),56*scale,4*scale);
  box('leftLeg',-17,19,13,37); box('rightLeg',4,19,13,37);
  box('leftArm',-31,-20,11,47); box('rightArm',20,-20,11,47);
  box('torso',-17,-24,34,49);
  ink(...appearance.colors.head).circle(...at(0,-38),18*scale,true);
  const face=appearance.face;
  ink(...face.color);
  for (const x of [-6,6]) {
    if(face.eyes==='dots') ink(...face.color).circle(...at(x,-40),2*scale,true);
    if(face.eyes==='wide') ink(...face.color).circle(...at(x,-40),3.5*scale,false);
    if(face.eyes==='sleepy') ink(...face.color).box(...at(x-3,-40),6*scale,2*scale);
  }
  if(face.mouth==='neutral') ink(...face.color).box(...at(-5,-30),10*scale,2*scale);
  if(face.mouth==='smile'||face.mouth==='frown') {
    const sign=face.mouth==='smile'?1:-1;
    for(let x=-5;x<=5;x++) ink(...face.color).box(...at(x,-30+sign*(2-Math.abs(x)*.4)),Math.max(1,scale),Math.max(1,scale));
  }
}

export function act({ event }) {
  if (event.is('wheel') || event.is('scroll')) {
    scroll=Math.max(0,Math.min(maxScroll,scroll+(event.dy||event.dir*20||0)));
    api.help?.repeat?.();
  }
  if (event.is('touch')) {
    const hit=event.y>=34&&regions.find(region=>event.x>=region.x&&event.x<=region.x+region.w&&event.y>=region.y&&event.y<=region.y+region.h);
    activeSlider=hit?.drag?hit:null;
    dragY=event.y;
    hit?.action(event.x,event.y);api.help?.repeat?.();
  } else if (event.is('draw')) {
    if(activeSlider)activeSlider.action(event.x,event.y);
    else if(dragY!==null)scroll=Math.max(0,Math.min(maxScroll,scroll+dragY-event.y));
    dragY=event.y;api.help?.repeat?.();
  } else if (event.is('lift')) {
    activeSlider=null;dragY=null;
  }
}

export function leave() { disposed=true;generation++;clearTimeout(saveTimer); }
