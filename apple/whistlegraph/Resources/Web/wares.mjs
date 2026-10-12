// A ware selects an artifact and its tools, not a different account or brain.
export const WARES = Object.freeze([
  Object.freeze({id:'piece', name:'Aesthetic.Computer Piece'}),
  Object.freeze({id:'roblox', name:'Roblox Room'}),
  Object.freeze({id:'doom', name:'Doom WAD'}),
]);
export const WARE_KEY = 'whistlegraph-ware';
export const DEFAULT_WARE = 'piece';
export function validWare(id) { return WARES.some(ware=>ware.id===id); }
export function currentWare(storage) {
  const id=storage.getItem(WARE_KEY);
  return validWare(id)?id:DEFAULT_WARE;
}
export function selectWare(storage,id) {
  if(!validWare(id))throw Error('Unknown ware');
  storage.setItem(WARE_KEY,id);
  return id;
}
// Only complete, explicit switch commands take this local shortcut. A request
// to draw a Roblox logo still belongs to the current artwork.
export function requestedWare(text) {
  const match=String(text).trim().match(/^(?:please\s+)?(?:switch(?:\s+(?:the\s+)?ware)?\s+to|use|open)\s+(doom(?:\s+wad)?|roblox(?:\s+room)?|(?:aesthetic[. ]computer\s+)?piece)(?:\s+(?:ware|mode))?[.!]?$/i);
  return match?(/doom/i.test(match[1])?'doom':/roblox/i.test(match[1])?'roblox':'piece'):null;
}
// Media is always filled: every artifact is seen and heard, on every ware and
// every channel it travels through (2026-10-11: the default pieces were silent).
export const MEDIA_INSTRUCTIONS='Media rule, for every ware (a piece, a Roblox room, anything else): everything made here is both graphical and sonic by default. A silent artifact is unfinished. Give it sound that belongs to its world: an ambient bed or environmental sound (wind, water, crowd, machine hum, birds, room tone), a voice, music, or sounds that answer what moves and what the person touches. Start ambient sound on its own without waiting for a touch, keep it gentle and loopable, and tie rhythm to the same clock as the motion. On a piece use the sound API from the guide (sound.synth, sound.howl, sound.breath, sound.chirp and kin, scheduled in sim with the clock) and kill held voices in leave. On a Roblox room use Sound instances (ambient loops in the world, sounds on interactions). Likewise never leave the picture blank: an audio-first request still gets a visual that moves with the sound. The only exceptions: the person asks for silence, muting, no sound, picture only or audio only, or the ware truly cannot make one of the two; then follow the request and say so in one chat line. On revisions keep the existing sound unless asked to change it, and add sound to a piece that has none.';
export const WARE_INSTRUCTIONS=MEDIA_INSTRUCTIONS+' Ware is the type of artifact the person is making. Available wares: piece = Aesthetic.Computer Piece (default), roblox = Roblox Room, doom = Doom WAD. Use whistlegraph_ware to read or switch ware only when the user requests it. Switching preserves each ware and happens after the reply. Do not edit the current artifact during a switch request. Mentioning Roblox in artwork is not a request to switch. Do not claim a queued switch has happened.';
export function wareControls(current) {
  let pending=null;
  return {
    get pending(){return pending;},
    clear(){pending=null;},
    tools:()=>[{
      name:'whistlegraph_ware',description:'Read available wares or queue a requested switch. Saves no artwork, publishes nothing, and preserves both histories.',
      input_schema:{type:'object',properties:{action:{type:'string',enum:['read','switch']},ware:{type:'string',enum:WARES.map(w=>w.id)}},required:['action'],additionalProperties:false},
    }],
    has:name=>name==='whistlegraph_ware',
    run(name,input){
      if(name!=='whistlegraph_ware'||!input||Object.keys(input).some(k=>!['action','ware'].includes(k)))throw Error('Invalid ware control');
      if(input.action==='read'&&input.ware===undefined)return {current,wares:WARES,status:'ready'};
      if(input.action!=='switch'||!validWare(input.ware))throw Error('Choose a supported ware');
      pending=input.ware===current?null:input.ware;
      return {current,requested:input.ware,status:pending?'queued':'already selected'};
    },
  };
}
