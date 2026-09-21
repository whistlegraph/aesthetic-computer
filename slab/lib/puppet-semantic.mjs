import { randomUUID } from "node:crypto";

export function semanticLocator(page, selector) {
  if (!selector || typeof selector !== "object") throw new Error("locator is required");
  const kinds=["role","label","text","testId","css"].filter(key=>typeof selector[key]==="string" && selector[key]);
  if(kinds.length!==1) throw new Error("Choose exactly one locator: role, label, text, testId, or css");
  if(selector.role) {
    if(typeof selector.name!=="string" || !selector.name) throw new Error("Role locators require an exact accessible name");
    return page.getByRole(selector.role,{name:selector.name,exact:true});
  }
  if(selector.label) return page.getByLabel(selector.label,{exact:true});
  if(selector.text) return page.getByText(selector.text,{exact:true});
  if(selector.testId) return page.getByTestId(selector.testId);
  return page.locator(selector.css);
}
function timeout(value=5000) {
  if(!Number.isFinite(value) || value<1 || value>10000) throw new Error("timeout must be 1..10000 ms");
  return value;
}

export class SemanticBrowser {
  constructor(endpoint) { this.endpoint=endpoint;this.connection=null;this.inspector=null;this.pages=new Map();this.queues=new Map(); }
  async browser() {
    if(!this.connection) {
      this.connection=(async()=>{
        const {chromium}=await import("playwright-core");
        // Attach without changing the user's media/focus/download preferences.
        const browser=await chromium.connectOverCDP(this.endpoint(),{noDefaults:true,timeout:10000});
        browser.on("disconnected",()=>{this.connection=null;this.inspector=null;this.pages.clear();});
        return browser;
      })();
      this.connection.catch(()=>{this.connection=null;});
    }
    return this.connection;
  }
  async close() {
    if(this.connection) await (await this.connection).close().catch(()=>{});
    this.connection=null;this.inspector=null;this.pages.clear();
  }
  async page(target) {
    if(typeof target!=="string" || !target) throw new Error("An exact browser target ID is required (puppet_list pages)");
    const browser=await this.browser();
    // Keep the inspector warm, but still validate the exact target every time:
    // another client's page-close event can lag behind isClosed().
    if (!this.inspector) {
      const pending = browser.newBrowserCDPSession();
      this.inspector = pending;
      pending.catch(() => { if (this.inspector === pending) this.inspector = null; });
    }
    const inspector=await this.inspector;
    try { await inspector.send("Target.getTargetInfo",{targetId:target}); }
    catch { throw new Error("Browser target is gone or does not match exactly; no action sent"); }
    const cached=this.pages.get(target);
    if(cached && !cached.isClosed()) return cached;
    for(const context of browser.contexts()) for(const page of context.pages()) {
      if(page.isClosed()) continue;
      const cdp=await context.newCDPSession(page);
      try {
        const {targetInfo}=await cdp.send("Target.getTargetInfo");
        this.pages.set(targetInfo.targetId,page);
      } finally {await cdp.detach();}
    }
    const page=this.pages.get(target);
    if(!page || page.isClosed()) throw new Error("Browser target is gone or does not match exactly; no action sent");
    return page;
  }
  async observe(page,target,{image=false,timeout:ms=5000}={}) {
    const tree=await page.locator("body").ariaSnapshot({timeout:timeout(ms)});
    return {observation:{id:randomUUID(),capturedAt:new Date().toISOString(),target,url:page.url(),coordinateSpace:"browser-css-pixels"},
      tree:tree.slice(0,24000),truncated:tree.length>24000,
      ...(image?{image:(await page.screenshot({type:"jpeg",quality:75,timeout:timeout(ms)})).toString("base64")}: {})};
  }
  async run(action,args) {
    const {target}=args;
    // Readers/waits must not block the action that satisfies their condition.
    if (["snapshot", "wait"].includes(action)) return this.perform(action,args);
    // One page's actions are ordered across every client of the daemon.
    const previous=this.queues.get(target)||Promise.resolve();
    const operation=previous.catch(()=>{}).then(()=>this.perform(action,args));
    this.queues.set(target,operation);
    const cleanup=()=>{if(this.queues.get(target)===operation)this.queues.delete(target);};
    operation.then(cleanup,cleanup);
    return operation;
  }
  async perform(action,args) {
    const ms=timeout(args.timeout), deadline=Date.now()+ms;
    const page=await this.page(args.target);
    const remaining=()=>Math.max(1,deadline-Date.now());
    if(Date.now()>=deadline) throw new Error("Browser connection exhausted operation timeout; no action sent");
    if(action==="snapshot") return this.observe(page,args.target,{...args,timeout:remaining()});
    const locator=semanticLocator(page,args.locator);
    if(action==="wait") {
      if(!["visible","hidden","attached","detached"].includes(args.state||"visible")) throw new Error("Invalid wait state");
      await locator.waitFor({state:args.state||"visible",timeout:remaining()});
      return {verified:true,state:args.state||"visible",target:args.target};
    }
    if(!["click","fill"].includes(action)) throw new Error("Unknown semantic action");
    if(action==="fill" && typeof args.value!=="string") throw new Error("fill requires a string value");
    // Validate the postcondition before any input. Locator actions perform
    // Playwright's strict matching and actionability checks without force.
    const after=args.after?semanticLocator(page,args.after.locator):null;
    const state=args.after?.state||"visible";
    if(after && !["visible","hidden","attached","detached"].includes(state)) throw new Error("Invalid postcondition state");
    try {
      if(action==="click") await locator.click({timeout:remaining()});
      else await locator.fill(args.value,{timeout:remaining()});
    } catch (error) {
      // A protocol/navigation failure can occur after dispatch. Do not claim
      // no input occurred or invite an automatic action retry.
      return {action,performed:"unknown",target:args.target,verification:{ok:false,error:error.message}};
    }
    const result={action,performed:true,target:args.target,verification:{ok:null}};
    try {
      if(after) {await after.waitFor({state,timeout:remaining()});result.verification={ok:true,state};}
      Object.assign(result,await this.observe(page,args.target,{...args,timeout:remaining()}));
    } catch(error) {
      // Input was delivered: never turn failed verification into a retryable
      // action error. Caller can observe again without repeating the action.
      result.verification={ok:false,error:error.message};
    }
    return result;
  }
}
