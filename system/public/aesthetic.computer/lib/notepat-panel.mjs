import { GM_PROGRAM_NAMES } from './notepat-programs.mjs';
const WAVES=['sine','composite','triangle','square','whistle','sawtooth','harp','stample','drum'];
const sounds=[...GM_PROGRAM_NAMES.map((name,program)=>({id:`gm:${program}`,label:`${program+1} ${name}`})),...WAVES.map(wave=>({id:wave,label:wave==='stample'?'Sample':wave}))];
const item=(id,label,disabled=false)=>({id,label,disabled});
export function notepatPanelItems(view,state={}) {
 const midi=state.midi||{}, instrument=state.instrument||{wave:'gm',program:0,local:true};
 if(view==='sounds')return sounds;
 if(view==='inputs')return [item('all','All inputs'),...(midi.inputs||[]).map(p=>item(p.id,p.name))];
 if(view==='outputs')return [item('','No MIDI output'),...(midi.outputs||[]).map(p=>item(p.id,p.name))];
 if(view==='channels')return Array.from({length:16},(_,n)=>item(String(n),`Channel ${n+1}`));
 const name=instrument.wave==='gm'?(GM_PROGRAM_NAMES[instrument.program]||'Piano'):instrument.wave;
 const rows=[item('sounds',name),item('local',`Local sound: ${instrument.local?'on':'off'}`),
  item('connect',midi.pending?'Connecting MIDI...':midi.connected?'MIDI connected':midi.supported?'Connect MIDI':'MIDI unavailable',midi.pending||!midi.supported)];
 if(midi.connected) rows.push(item('inputs',`In: ${midi.inputId==='all'?'all':midi.inputs?.find(p=>p.id===midi.inputId)?.name||'none'}`),
  item('outputs',`Out: ${midi.outputs?.find(p=>p.id===midi.outputId)?.name||'none'}`),item('channels',`Channel ${Number(midi.channel||0)+1}`,!midi.outputId));
 if(midi.error)rows.push(item('status',midi.error,true));
 if(state.soundStatus)rows.push(item('status',state.soundStatus,true));
 rows.push(item('record',instrument.recording?'Stop + play phrase':'Record a phrase'),item('panic','Stop all notes'));
 return rows;
}
export function notepatPanelLayout(screen,{view='main',page=0,state={}}={}) {
 const x=Math.max(2,Math.floor((screen.width-Math.min(280,screen.width-8))/2)),y=25;
 const w=Math.max(36,screen.width-2*x),h=Math.max(40,Math.min(230,screen.height-y-4));
 const rowHeight=Math.min(17,Math.max(8,h-40)),capacity=Math.max(1,Math.floor((h-40)/rowHeight)),items=notepatPanelItems(view,state);
 const pages=Math.max(1,Math.ceil(items.length/capacity)),current=Math.max(0,Math.min(pages-1,page));
 const controls=[{id:'back',label:view==='main'?'':'<',x:x+3,y:y+2,w:16,h:16,disabled:view==='main'},
  {id:'close',label:'x',x:x+w-19,y:y+2,w:16,h:16}];
 items.slice(current*capacity,(current+1)*capacity).forEach((row,i)=>controls.push({...row,action:'select',x:x+4,y:y+20+i*rowHeight,w:w-8,h:rowHeight-1}));
 if(pages>1)controls.push({id:'previous',label:'<',x:x+4,y:y+h-18,w:24,h:15,disabled:current===0},{id:'next',label:'>',x:x+w-28,y:y+h-18,w:24,h:15,disabled:current===pages-1});
 return {x,y,w,h,controls,page:current,pages,capacity,title:({main:'Sound + MIDI',sounds:'Instrument',inputs:'MIDI input',outputs:'MIDI output',channels:'Channel'})[view]};
}
export function createNotepatPanel({host,close}) {
 let view='main',page=0,state={},layout,buttons=[],signature='';
 const selected=()=>view==='sounds'?(state.instrument?.wave==='gm'?`gm:${state.instrument.program}`:state.instrument?.wave):view==='inputs'?state.midi?.inputId:view==='outputs'?state.midi?.outputId:view==='channels'?String(state.midi?.channel):null;
 function update(api) {
  layout=notepatPanelLayout(api.screen,{view,page,state});page=layout.page;
  const next=JSON.stringify(layout);
  if(next!==signature){signature=next;buttons=layout.controls.map(c=>({...c,button:new api.ui.Button(c.x,c.y,c.w,c.h)}))}
 }
 function activate(c) {
  if(c.disabled)return;
  if(c.action!=='select') {
   if(c.id==='close')close();
   else if(c.id==='back'){view='main';page=0}
   else page+=c.id==='next'?1:-1;
   return;
  }
  if(view==='main') {
   if(['sounds','inputs','outputs','channels'].includes(c.id)) {
    view=c.id;page=0;
    if(view==='sounds') {const i=sounds.findIndex(s=>s.id===selected());page=Math.floor(Math.max(0,i)/(layout?.capacity||1))}
   } else if(c.id==='local')host({op:'local',enabled:!state.instrument?.local});
   else if(c.id==='record'){host({op:'record'});close()}
   else host({op:c.id==='panic'?'stop-notes':c.id});
  } else {
   const op={sounds:'select-sound',inputs:'select-input',outputs:'select-output',channels:'select-channel'}[view];
   host({op,value:c.id});
  }
 }
 return {
  state(next){state={...state,...next}},
  open(){view='main';page=0;signature=''},
  paint(api,painter,glyph,palette){
   update(api);const {x,y,w,h}=layout;
   painter.ink(0,0,0,145).box(0,21,api.screen.width,api.screen.height-21);
   painter.ink(...palette.bar).box(x,y,w,h);
   painter.ink(...palette.rail).box(x,y,w,1).box(x,y+h-1,w,1).box(x,y,1,h).box(x+w-1,y,1,h);
   const text=(value,tx,ty,max,color=palette.text)=>painter.glyph(glyph(String(value).slice(0,Math.max(0,Math.floor(max/6))),color),tx,ty);
   text(layout.title,x+22,y+5,w-44);
   for(const c of buttons)c.button.paint(btn=>{
    const active=c.action==='select'&&c.id===selected();
    if(c.action==='select'||!c.disabled)painter.ink(...(active?palette.rail:palette.control)).box(c.x,c.y,c.w,c.h);
    if(btn.over&&!c.disabled)painter.ink(...palette.text,24).box(c.x,c.y,c.w,c.h);
    text(c.label,c.x+3,c.y+4,c.w-6,c.disabled?palette.rail:active?palette.background:palette.text);
   });
   if(layout.pages>1)text(`${page+1}/${layout.pages}`,x+Math.floor(w/2)-12,y+h-14,45);
  },
  act(e,api){
   update(api);
   if(e.is('keyboard:down:escape')){if(view==='main')close();else{view='main';page=0}return true}
   if(e.is('keyboard:up:escape'))return true;
   if(e.is('keyboard:down:arrowleft')||e.is('keyboard:down:arrowright')){page=Math.max(0,Math.min(layout.pages-1,page+(e.key==='ArrowLeft'?-1:1)));return true}
   if(e.is('keyboard:up:arrowleft')||e.is('keyboard:up:arrowright'))return true;
   const pointer=e.is('touch')||e.is('draw')||e.is('lift')||e.is('move')||e.is('wheel');
   if(!pointer)return false; // Musical keys remain available for auditioning.
   for(const c of buttons)c.button.act(e,{push:()=>activate(c)},api.pens?.());
   if(e.is('touch')&&(e.x<layout.x||e.x>=layout.x+layout.w||e.y<layout.y||e.y>=layout.y+layout.h))close();
   return true;
  },
 };
}
