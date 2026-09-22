import test from 'node:test';
import assert from 'node:assert/strict';
import vm from 'node:vm';
import {CAPTURE_SCRIPT} from '../src/frame-capture-script.mjs';

test('capture stays pending through startup and captures once the canvas appears',()=>{
  let root=null;
  const context={document:{getElementById:()=>root},location:{href:'https://aesthetic.computer/@jeffrey/test'},getComputedStyle:()=>({display:'block'}),btoa:s=>Buffer.from(s,'binary').toString('base64')};
  assert.equal(vm.runInNewContext(CAPTURE_SCRIPT,context).pending,true);
  root={children:[],querySelector:()=>null};
  assert.equal(vm.runInNewContext(CAPTURE_SCRIPT,context).pending,true);
  root.children.push({tagName:'CANVAS',dataset:{},width:1,height:1,getContext:()=>({getImageData:()=>({data:new Uint8Array([255,0,0,255])})}),toDataURL:()=> 'data:image/png;base64,test'});
  const captured=vm.runInNewContext(CAPTURE_SCRIPT,context);
  assert.equal(captured.width,1);
  assert.equal(captured.source,'software-canvas');
  assert.equal(captured.pending,undefined);
});
test('capture failures return to the requester without throwing into the preview page',()=>{
  const result=vm.runInNewContext(CAPTURE_SCRIPT,{document:{getElementById(){throw new Error('Unreadable canvas')}},location:{href:'https://aesthetic.computer/@jeffrey/test'}});
  assert.equal(result.error,'Unreadable canvas');
});
