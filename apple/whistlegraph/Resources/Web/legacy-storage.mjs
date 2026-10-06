// Preserve immutable source and thread IDs while adopting the product name.
// Old keys remain as a recovery copy; reruns never overwrite a new key.
export function migrateLegacyStorage(storage){
  if(storage.getItem('whistlegraph-storage-migrated')==='1')return;
  const keys=Array.from({length:storage.length},(_,i)=>storage.key(i)).filter(k=>k?.startsWith('walkieware-'));
  for(const old of keys){
    const next='whistlegraph-'+old.slice('walkieware-'.length);
    if(storage.getItem(next)===null)storage.setItem(next,storage.getItem(old));
  }
  storage.setItem('whistlegraph-storage-migrated','1');
}
