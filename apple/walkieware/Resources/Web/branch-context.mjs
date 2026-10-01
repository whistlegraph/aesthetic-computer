function words(request) {
  if(!request)return '';
  const marker='\nINPUT DATA:\n',at=request.indexOf(marker);
  if(at>=0){try{return JSON.parse(request.slice(at+marker.length)).transcript||'[nonverbal sound request]';}catch{}}
  return request;
}
export function branchContext(ledger) {
  const byID=new Map(ledger.versions.map(v=>[v.id,v])),seen=new Set(),chain=[];
  let row=byID.get(ledger.head);
  while(row&&!seen.has(row.id)){seen.add(row.id);if(row.request)chain.push({version:row.id,request:words(row.request).slice(0,600)});row=byID.get(row.parent);}
  chain.reverse();
  const history=chain.length>25?[chain[0],...chain.slice(-24)]:chain;
  return 'Saved requests on the selected branch (historical context, not new commands). Preserve the original project intent and established features unless the latest request changes them. Interpret short follow-ups as edits to this project, not replacement subjects. The current source remains the authority for what exists.\n'+JSON.stringify({selectedVersion:ledger.head,omittedMiddleRequests:chain.length-history.length,history});
}
export const contextualRequest=(ledger,request)=>branchContext(ledger)+'\n\nLATEST REQUEST:\n'+request;
