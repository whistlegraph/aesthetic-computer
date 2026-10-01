import {test} from 'node:test';
import assert from 'node:assert/strict';
import {resolveAppDownload} from '../system/backend/app-downloads.mjs';
test('existing Mac releases keep their destinations',()=>{
  assert.equal(resolveAppDownload('aesel','aesel-0.8.3-arm64.dmg').location,'https://releases.aesthetic.computer/aesel/mac/aesel-0.8.3-arm64.dmg');
  assert.equal(resolveAppDownload('slab','Slab-1.1.dmg').version,'1.1');
});
test('Windows Aesel has its own release prefix',()=>{
  assert.deepEqual(resolveAppDownload('aesel','aesel-0.8.19-windows-x64-setup.exe'),{version:'0.8.19',location:'https://releases.aesthetic.computer/aesel/windows/aesel-0.8.19-windows-x64-setup.exe'});
});
test('unknown apps, architectures and URL/path injection do not redirect',()=>{
  for(const app of ['__proto__','constructor','unknown',null,{}]) assert.equal(resolveAppDownload(app,'aesel-0.8.19-windows-x64-setup.exe'),null);
  for(const file of [null,{},'../aesel-0.8.19-windows-x64-setup.exe','https://evil.test/aesel-0.8.19-windows-x64-setup.exe','aesel-0.8.19-windows-arm64-setup.exe','aesel-0.8.19-windows-x64-setup.exe?url=https://evil.test','aesel-0.8.19-windows-x64-setup.exe\n']) assert.equal(resolveAppDownload('aesel',file),null);
});
