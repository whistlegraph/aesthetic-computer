// Move legacy inline revisions to private Spaces, verifying bytes before
// replacing the Mongo payload. Dry-run unless --apply is passed.
import { pathToFileURL } from 'node:url';
import { gameObjects } from './oskie-objects.mjs';
import { validateGame } from '../public/oskie/format.mjs';

export async function migrateGameObjects(db, objects, {apply=false}={}) {
  const revisions=db.collection('oskie-revisions'), games=db.collection('oskie-games');
  let scanned=0,migrated=0,skipped=0;
  for await (const r of revisions.find({bundle:{$exists:true}, object:{$exists:false}})) {
    scanned++;
    if (!apply) continue;
    const row=await games.findOne({_id:r.gameId,owner:r.owner});
    if (!row || row.deleting) {skipped++;continue;}
    const bundle=validateGame(r.bundle), object=await objects.put(r,bundle);
    const restored=await objects.get({...r,object});
    if (JSON.stringify(restored)!==JSON.stringify(bundle)) throw Error('Migration verification failed.');
    // Recheck deletion after the upload: don't strand an object for a project
    // removed during the copy. The revision CAS protects concurrent reruns.
    const current=await games.findOne({_id:r.gameId,owner:r.owner});
    if (!current || current.deleting) {await objects.remove({...r,object});skipped++;continue;}
    const result=await revisions.updateOne({_id:r._id,owner:r.owner,object:{$exists:false}},
      {$set:{object,manifest:bundle.manifest},$unset:{bundle:''}});
    if (result.matchedCount) migrated++;
    else if (!await revisions.findOne({_id:r._id})) await objects.remove({...r,object});
  }
  return {apply,scanned,migrated,skipped};
}
if (process.argv[1] && import.meta.url===pathToFileURL(process.argv[1]).href) {
  const {connect,closePool}=await import('./database.mjs');
  const database=await connect();
  try { console.log(JSON.stringify(await migrateGameObjects(database.db,gameObjects(),{apply:process.argv.includes('--apply')}))); }
  finally {await closePool();}
}
