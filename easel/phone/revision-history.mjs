// Older notebooks have file-change events but no explicit transcript checkpoints.
// Infer only a complete, unambiguous sequence; never show later text as old history.
export function migrateRevisionCheckpoints(revisions, events) {
  const writes = [];
  for (let i = 0; i < events.length; i++) {
    const event = events[i], item = event.params?.item;
    if (event.type !== 'bridge' || event.method !== 'item/completed' || item?.type !== 'fileChange' || String(item.status || '').startsWith('failed')) continue;
    let end = i + 1;
    for (let j = i + 1; j < events.length; j++) {
      if (events[j].type === 'you') break;
      end = j + 1;
      if (events[j].method === 'turn/completed') break;
    }
    writes.push(end);
  }
  const generated = revisions.filter(r => r.version !== 0);
  const infer = revisions[0]?.version === 0 && generated.every(r => r.reason === 'generated') && generated.length === writes.length;
  return revisions.map((revision, index) => {
    if (Number.isInteger(revision.transcriptEnd) && revision.transcriptEnd >= 0 && revision.transcriptEnd <= events.length) return revision;
    const {transcriptEnd, ...saved} = revision;
    revision = saved;
    if (revision.version === 0 && revision.reason === 'opened') return {...revision, transcriptEnd: 0};
    return infer ? {...revision, transcriptEnd: writes[index - 1]} : revision;
  });
}
