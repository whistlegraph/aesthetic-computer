#!/usr/bin/env python3
"""Explicit deployment only. Never sends play, prepare, stop or reboot commands."""
from pathlib import Path
import argparse, concurrent.futures, hashlib, json, time, urllib.request
parser=argparse.ArgumentParser()
parser.add_argument('--built',type=Path,default=Path(__file__).resolve().parents[3]/'.tmp/femrag-spatial-stems')
parser.add_argument('--deploy',action='store_true',help='Upload and load idle receivers only after conductor releases the rig')
args=parser.parse_args();here=Path(__file__).resolve().parent
plan=json.loads((args.built/'plan.json').read_text());manifest=json.loads((args.built/'manifest.json').read_text())
assert manifest['masterBakedIn']==1
for n in plan['nodes']:
 m=next(s for s in manifest['stems'] if s['seat']==n['seat'])
 assert hashlib.sha256((args.built/m['file']).read_bytes()).hexdigest()==m['sha256']
if not args.deploy:
 print(json.dumps({'readyToDeploy':True,'arrangementHash':plan['arrangementHash'],'nodes':plan['nodes'],'bytes':sum(s['bytes'] for s in manifest['stems'] if s['seat']!='sub'),'note':'No network operations; pass --deploy only after rig release.'},indent=2))
 raise SystemExit(0)
def request(node,path,data=None):
 base=f"http://{node['host']}:{node['port']}"
 if data is not None:
  if not isinstance(data,bytes):data=json.dumps(data).encode()
  req=urllib.request.Request(base+path,data=data,method='PUT')
 else:req=base+path
 with urllib.request.urlopen(req,timeout=45) as response:return response.read()
# All-six preflight before writing any device. Unknown/running pieces are rejected.
before={}
for n in plan['nodes']:
 status=json.loads(request(n,'/status'))
 assert status['piece']=='trio-fleet',status
 state=json.loads(request(n,'/pieces/trio-fleet-status.json'))
 assert state.get('phase') in ['ready','finished','error'],state
 before[n['id']]=state
code=(here/'native-femrag.mjs').read_bytes()
battery=(here.parents[2]/'fedac/native/lib/battery-watch.mjs').read_bytes()
def deploy(n):
 state=before[n['id']];cfg=json.loads((args.built/f"{n['id']}-config.json").read_text())
 m=next(s for s in manifest['stems'] if s['seat']==n['seat']);wave=(args.built/m['file']).read_bytes()
 # Recheck idle immediately before writes; producer must continue holding rig.
 current=json.loads(request(n,'/pieces/trio-fleet-status.json'));assert current.get('phase') in ['ready','finished','error'],current
 for i,remote in enumerate(cfg['center']['parts']):
  chunk=wave[i*4*1024*1024:(i+1)*4*1024*1024];request(n,remote,chunk)
  assert hashlib.sha256(request(n,remote)).digest()==hashlib.sha256(chunk).digest(),remote
 request(n,'/pieces/femrag-battery-watch.mjs',battery)
 request(n,'/pieces/trio-fleet-config.json',cfg)
 request(n,'/pieces/trio-fleet-command.json',{'id':'femrag-idle-'+str(time.time_ns()),'action':'idle'})
 request(n,'/pieces/trio-fleet.mjs',code)
 request(n,'/jump/trio-fleet',b'')
 deadline=time.monotonic()+90
 while time.monotonic()<deadline:
  s=json.loads(request(n,'/pieces/trio-fleet-status.json'))
  if s.get('instance')!=state.get('instance') and s.get('arrangementHash')==plan['arrangementHash']:
   if s.get('phase')=='error':raise RuntimeError((n['id'],s))
   if s.get('phase')=='ready' and s.get('centerReady'):
    assert s['center']['sha256']==m['sha256']
    assert s['center']['loaded'] and not s['center']['playing'] and not s['microphoneHot']
    print(n['id'],'Femrag stem verified, ready',flush=True)
    return {**n,'url':f"http://{n['host']}:{n['port']}",'status':s,'observedAt':time.time(),'assetReadbackVerified':True}
  time.sleep(.3)
 raise RuntimeError((n['id'],'decoder/readiness timeout'))
# Three at a time bounds Wi-Fi traffic and host memory while decoding.
with concurrent.futures.ThreadPoolExecutor(max_workers=3) as pool:results=list(pool.map(deploy,plan['nodes']))
(args.built/'native-loaded.json').write_text(json.dumps(results,indent=2)+'\n')
print('All six ready; not cued.')
