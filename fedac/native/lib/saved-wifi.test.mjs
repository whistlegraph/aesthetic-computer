import test from 'node:test';
import assert from 'node:assert/strict';
import {savedWifi} from './saved-wifi.mjs';
function fixture(){
 const calls=[];
 const api={system:{readFile:()=>JSON.stringify([{ssid:'away',pass:'a'},{ssid:'home',pass:'b'}])},wifi:{state:0,connected:false,networks:[{ssid:'home',signal:-40}],scan:()=>calls.push('scan'),connect:(ssid)=>calls.push(ssid)}};
 return {step:savedWifi(),api,calls};
}
test('joins a visible saved network instead of retrying the absent venue',()=>{const {step,api,calls}=fixture();step(api,1000);api.wifi.state=2;step(api,3000);assert.deepEqual(calls,['scan','home']);});
test('does not cancel or restart an in-progress association or DHCP',()=>{const {step,api,calls}=fixture();step(api,1000);api.wifi.state=3;step(api,90000);assert.deepEqual(calls,['scan']);});
test('never connects to an unknown network',()=>{const {step,api,calls}=fixture();api.wifi.networks=[{ssid:'unknown',signal:-10}];step(api,1000);api.wifi.state=2;step(api,3000);assert.deepEqual(calls,['scan']);});
test('leaves an existing connection alone',()=>{const {step,api,calls}=fixture();api.wifi.connected=true;step(api,1000);assert.deepEqual(calls,[]);});
