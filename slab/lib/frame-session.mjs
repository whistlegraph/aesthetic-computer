import { AsyncLocalStorage } from "node:async_hooks";
import { randomUUID, createHash } from "node:crypto";
const sessions=new AsyncLocalStorage();
const stdioSession=`stdio-${process.pid}-${randomUUID()}`;
export function withFrameSession(args, context, fn) {
  const supplied=args?.sessionId || context?.headers?.["x-slab-observation-session"];
  const id=supplied || (context?.headers ? randomUUID() : stdioSession);
  if(typeof id!=="string" || !/^[a-zA-Z0-9_.:-]{1,128}$/.test(id)) throw new Error("Invalid frame sessionId");
  if(context?.headers && !supplied && ["frame_reframe","frame_commit_click","frame_reject_click","frame_action_trail"].includes(context.tool)) {
    throw new Error("sessionId is required; reuse the sessionId returned by frame");
  }
  return sessions.run(id,fn);
}
export const frameSessionId=()=>sessions.getStore() || stdioSession;
export const nativeFrameSession=()=>createHash("sha256").update(frameSessionId()).digest("hex").slice(0,32);
export const frameStateKey=machine=>`${frameSessionId()}:${machine}`;

// Trails can contain JPEGs. Bound retained client state on a shared host.
export class FrameStateMap extends Map {
  set(key,value) {
    if (!this.has(key) && this.size >= 16) this.delete(this.keys().next().value);
    return super.set(key,value);
  }
}
