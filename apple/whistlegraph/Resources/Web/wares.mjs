// A ware selects an artifact and its tools, not a different account or brain.
export const WARES = Object.freeze([
  Object.freeze({id:'piece', name:'Aesthetic.Computer Piece'}),
  Object.freeze({id:'roblox', name:'Roblox Room'}),
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
  const match=String(text).trim().match(/^(?:please\s+)?(?:switch(?:\s+(?:the\s+)?ware)?\s+to|use|open)\s+(roblox(?:\s+room)?|(?:aesthetic[. ]computer\s+)?piece)(?:\s+(?:ware|mode))?[.!]?$/i);
  return match?(/roblox/i.test(match[1])?'roblox':'piece'):null;
}
export const WARE_INSTRUCTIONS='Ware is the type of artifact the person is making. Available wares: piece = Aesthetic.Computer Piece (default), roblox = Roblox Room. Use whistlegraph_ware to read or switch ware only when the user requests it. Switching preserves each ware and happens after the reply. Do not edit the current artifact during a switch request. Mentioning Roblox in artwork is not a request to switch. Do not claim a queued switch has happened.';
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
