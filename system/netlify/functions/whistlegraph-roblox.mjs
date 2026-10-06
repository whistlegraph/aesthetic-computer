import {connect} from '../../backend/database.mjs';
import {authenticateMusical} from './easel-musical-jev.mjs';
import {createRoomHandler,mongoRoomStore} from '../../backend/whistlegraph-roblox.mjs';
let pending;
export const handler=createRoomHandler({authenticate:authenticateMusical,
  store:()=>pending??=(async()=>{const {db}=await connect();return mongoRoomStore(db.collection('whistlegraph-roblox-rooms'));})().catch(error=>{pending=null;throw error;}),
  config:()=>({owner:process.env.WHISTLEGRAPH_ROBLOX_PREVIEW_OWNER,playerId:process.env.WHISTLEGRAPH_ROBLOX_PREVIEW_USER_ID,launchURL:process.env.WHISTLEGRAPH_ROBLOX_SHARE_LINK,bridgeKey:process.env.WHISTLEGRAPH_ROBLOX_BRIDGE_KEY}),
});
