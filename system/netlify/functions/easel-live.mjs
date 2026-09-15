import {createClient} from 'redis';
import {authorize,getHandleOrEmail} from '../../backend/authorization.mjs';
import {createLiveHandler,redisLiveStore} from '../../backend/easel-live.mjs';
let client,connecting;
async function redis(){
 if(!process.env.REDIS_CONNECTION_STRING)throw new Error("Redis not configured");
 if(client?.isReady)return client;
 if(!connecting)connecting=(async()=>{client=createClient({url:process.env.REDIS_CONNECTION_STRING,socket:{connectTimeout:3000,reconnectStrategy:false}});client.on('error',()=>{});await client.connect();return client;})().finally(()=>{connecting=null;});
 return connecting;
}
const store={resolve:async(...args)=>redisLiveStore(await redis()).resolve(...args),read:async(...args)=>redisLiveStore(await redis()).read(...args),write:async(...args)=>redisLiveStore(await redis()).write(...args)};
export const handler=createLiveHandler({authorize,getHandleOrEmail,store});
