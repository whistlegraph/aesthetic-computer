// Public Dool geography -> wallpaper projection. The wallpaper never assigns
// production plots; only the server's settled pods appear in the park.
export function parkWorld(real, colors = new Map()) {
  const block = real.block || 16, lots = real.lotRects || [];
  const pods = (real.pods || []).map(p => ({ id: p.id, handle: p.handle,
    placement: p.placement, room: p.room, colors: colors.get(p.handle) || null }));
  const blocks = (real.blocks || []).map(b => ({ n: b.n, i: b.i, j: b.j, id: b.id,
    homes: (b.homes || []).map(h => h.lot), names: (b.homes || []).map(h => h.handle) }));
  const world = { revision: real.revision, settled: real.settled, opened: real.opened,
    across: real.span?.metres, block, lots, blocks, pods };
  const podById = new Map(pods.map(p => [p.id,p]));
  const blockById = new Map(blocks.map(b => [b.id,b]));
  world.awake = (real.awake || []).flatMap(a => {
    let b = blockById.get(a.room), x = a.x, z = a.z;
    const pod = podById.get(a.room);
    if (pod) {
      b = blocks.find(b => b.n === pod.placement.block);
      const r = lots[pod.placement.lot];
      if (!r) return [];
      const scale = Math.min(r.xMax-r.xMin,r.zMax-r.zMin)/block;
      x = (r.xMin+r.xMax)/2 + a.x*scale;
      z = (r.zMin+r.zMax)/2 + a.z*scale;
    }
    return b ? [{handle:a.handle,x:b.i*block+x,z:b.j*block+z}] : [];
  });
  return world;
}
export function roomTargets(world) {
  const rooms = world.blocks.map(b => ({...b,scale:1}));
  for (const pod of world.pods || []) {
    const b=world.blocks.find(b=>b.n===pod.placement.block),r=world.lots[pod.placement.lot];
    if (!b || !r) continue;
    rooms.push({id:pod.id,i:b.i+(r.xMin+r.xMax)/2/world.block,j:b.j+(r.zMin+r.zMax)/2/world.block,
      scale:Math.min(r.xMax-r.xMin,r.zMax-r.zMin)/world.block});
  }
  return rooms;
}
export function placeVisitors(fish, world) {
  for (const f of fish) {
    const handle=f.label?.startsWith('@')?f.label.slice(1):'';
    const pod=world.pods.find(p=>p.handle===handle);
    const b=pod&&world.blocks.find(b=>b.n===pod.placement.block),r=pod&&world.lots[pod.placement.lot];
    if (b&&r) {
      f.at={x:b.i*world.block+(r.xMin+r.xMax)/2,z:b.j*world.block+(r.zMin+r.zMax)/2};
      f.home={pod:pod.id,block:b.id,lot:pod.placement.lot};
    } else {
      let h=2166136261;
      for(const c of String(f.id)) h=Math.imul(h^c.charCodeAt(0),16777619)>>>0;
      f.at={x:((h&65535)/65535*2-1)*6,z:((h>>>16)/65535*2-1)*6};
      delete f.home;
    }
  }
}
