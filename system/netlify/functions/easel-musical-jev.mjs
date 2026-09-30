import {connect} from '../../backend/database.mjs';
import {authorize,getHandleOrEmail} from '../../backend/authorization.mjs';
import {createMusicalHandler,musicalBudget} from '../../backend/easel-musical-jev.mjs';
let storage;
async function budget(){return storage??= (async()=>{const {db}=await connect();const c=db.collection('easel-musical-jev-budget');await c.createIndex({expiresAt:1},{expireAfterSeconds:0});return musicalBudget(c);})().catch(e=>{storage=null;throw e;});}
export const authenticateMusical=async headers=>{const user=await authorize(headers);if(!user?.sub)return null;const handle=await getHandleOrEmail(user.sub);return typeof handle==='string'&&handle.startsWith('@')?user.sub:null;};
export const musicalDecision=createMusicalHandler({
 authenticate:authenticateMusical,
 budget:{consume:async(...args)=>(await budget()).consume(...args)}
});
export const handler=event=>musicalDecision(event);
