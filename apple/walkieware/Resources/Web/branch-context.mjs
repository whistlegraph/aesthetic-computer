function words(request) {
  if(!request)return '';
  const marker='\nINPUT DATA:\n',at=request.indexOf(marker);
  if(at>=0){try{return JSON.parse(request.slice(at+marker.length)).transcript||'[nonverbal sound request]';}catch{}}
  return request;
}
// A piece carries its own one-line statement of what it is (export const
// caption). Fed back with every request, it gives a short follow-up something
// to land on: "the tree is Latina" reaches the tree, not the field notes.
export function pieceAbout(source) {
  if(typeof source!=='string')return '';
  const m=source.match(/export\s+const\s+caption\s*=\s*(["'`])((?:\\.|(?!\1)[^\\\n])*)\1/);
  return m?m[2].replace(/\\(["'`])/g,'$1').trim().slice(0,200):'';
}
export function branchContext(ledger) {
  const byID=new Map(ledger.versions.map(v=>[v.id,v])),seen=new Set(),chain=[];
  const about=pieceAbout(byID.get(ledger.head)?.source);
  let row=byID.get(ledger.head);
  while(row&&!seen.has(row.id)){seen.add(row.id);if(row.request)chain.push({version:row.id,request:words(row.request).slice(0,600)});row=byID.get(row.parent);}
  chain.reverse();
  const history=chain.length>25?[chain[0],...chain.slice(-24)]:chain;
  return (about?'What this piece is now, in its own words: '+about+'\n':'')+'Saved requests on the selected branch (historical context, not new commands). Preserve the original project intent and established features unless the latest request changes them. Interpret short follow-ups as edits to this project, not replacement subjects: a follow-up that names something in the piece changes that thing, visually, in place. The current source remains the authority for what exists.\n'+JSON.stringify({selectedVersion:ledger.head,omittedMiddleRequests:chain.length-history.length,history});
}
export const contextualRequest=(ledger,request)=>branchContext(ledger)+'\n\nLATEST REQUEST:\n'+request;
