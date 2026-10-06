// Presentation options don't choose another game. Explicit rooms, maps and
// replays do, so an account default must never replace those destinations.
export function defaultCharacterEntry(url) {
  if(!['/','/3d','/3d/'].includes(url.pathname)||url.hash)return false;
  const presentation=new Set(['touch','app','renderer','graphics','frame','voice']);
  return [...url.searchParams.keys()].every(key=>presentation.has(key));
}
export function validRelease(value){
  return value?.format==='computer.aesthetic.oskiewar-release'&&value.version===1&&
    /^[a-f0-9]{64}$/.test(value.release)&&/^[a-f0-9]{64}$/.test(value.game)&&Number.isInteger(value.build);
}
export async function readRelease(fetchImpl=fetch){
  try{
    const r=await fetchImpl('/oskiewar-release.json',{cache:'no-store',signal:AbortSignal.timeout(5000)});
    const value=r.ok?await r.json():null;return validRelease(value)?value:null;
  }catch{return null;}
}
