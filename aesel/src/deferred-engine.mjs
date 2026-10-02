import {EventEmitter} from 'node:events';
import {markStartup} from './startup-trace.mjs';

// The editor can accept a draft before a provider's tools and protocol load.
// Keep restored state and event subscriptions intact across that boundary.
export function deferredEngine(load){
  return class extends EventEmitter {
    constructor(options){
      super();this._options=options;this._engine=null;this._starting=null;
      this._closed=false;this._restored=new Map();
      return new Proxy(this,{
        get(target,key,receiver){
          if(key in target)return Reflect.get(target,key,receiver);
          if(target._engine){const value=target._engine[key];return typeof value==='function'?value.bind(target._engine):value;}
          return target._restored.has(key)?target._restored.get(key):target._options[key];
        },
        set(target,key,value,receiver){
          if(key in target)return Reflect.set(target,key,value,receiver);
          if(target._engine)target._engine[key]=value;
          else target._restored.set(key,value);
          return true;
        },
      });
    }
    get closed(){return this._closed||Boolean(this._engine?.closed);}
    connect(){
      return this._starting ||= Promise.resolve().then(()=>{markStartup('engine-load-start');return load();}).then(Engine=>{
        markStartup('engine-load-done');
        if(this._closed)throw Error('Engine closed while opening');
        const engine=this._engine=new Engine(this._options);
        markStartup('engine-constructed');
        for(const [key,value] of this._restored)engine[key]=value;
        this._restored.clear();
        for(const event of ['notification','request','protocolError','log','fatal','exit'])
          engine.on(event,(...args)=>this.emit(event,...args));
        return engine.connect();
      });
    }
    close(){this._closed=true;return this._engine?.close();}
  };
}
