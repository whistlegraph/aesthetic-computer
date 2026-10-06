#!/usr/bin/env python3
"""Measure the real native app with its debug notebook fixture, without inference.

Process -> automation ready and same-user control round trips are separate from
physical paint latency. The 150 ms mailbox timer is included in control timing.
"""
import argparse,json,os,pathlib,plistlib,shutil,statistics,subprocess,tempfile,time,uuid
p=argparse.ArgumentParser(description=__doc__)
p.add_argument('--app',type=pathlib.Path,required=True)
p.add_argument('--runs',type=int,default=3)
p.add_argument('--out',type=pathlib.Path,required=True)
a=p.parse_args()
if not 1<=a.runs<=10:p.error('runs must be 1–10')
app=a.app.resolve();info=plistlib.loads((app/'Contents/Info.plist').read_bytes());identifier=info['CFBundleIdentifier']
if identifier!='computer.aesthetic.aesel.native':p.error('Use the debug native bundle, with AESEL_NOTEBOOK_PREVIEW support')
samples=[]
for run in range(a.runs):
 namespace='speed-'+uuid.uuid4().hex[:8]
 env={**os.environ,'AESEL_AUTOMATION_NAMESPACE':namespace,'AESEL_NOTEBOOK_PREVIEW':'1','AESEL_PREVIEW_EMPTY':'1'}
 roots=[pathlib.Path.home()/'Library/Application Support'/identifier/('automation-'+namespace),pathlib.Path.home()/'Library/Containers'/identifier/'Data/Library/Application Support'/identifier/('automation-'+namespace)]
 sample={'run':run+1,'status':'failed'}
 with tempfile.TemporaryFile() as log:
  started=time.monotonic();child=subprocess.Popen([str(app/'Contents/MacOS'/info['CFBundleExecutable'])],env=env,stdout=log,stderr=log)
  try:
   root=None
   while time.monotonic()-started<20:
    for candidate in roots:
     try:
      instance=json.loads((candidate/'instance.json').read_text())
      if instance['pid']==child.pid:root=candidate;break
     except (FileNotFoundError,json.JSONDecodeError):pass
    if root:break
    if child.poll() is not None:raise RuntimeError('Native app exited')
    time.sleep(.005)
   if not root:raise RuntimeError('Automation not ready within 20s')
   sample['automationReadyMs']=(time.monotonic()-started)*1000
   def request(method,params={}):
    key=str(uuid.uuid4()).upper();path=root/'requests'/(key+'.json');response=root/'responses'/(key+'.json')
    payload={'id':key,'instance':instance['instance'],'createdAt':time.time(),'method':method,'params':params}
    pending=path.with_suffix('.tmp');pending.write_text(json.dumps(payload));pending.replace(path)
    deadline=time.monotonic()+10
    try:
     while time.monotonic()<deadline:
      try:
       result=json.loads(response.read_text())
       if 'error' in result:raise RuntimeError(result['error'])
       return result['result']
      except FileNotFoundError:time.sleep(.005)
     raise RuntimeError('Native automation timed out')
    finally:
     path.unlink(missing_ok=True);response.unlink(missing_ok=True)
   sent=time.monotonic();state=request('state');sample['firstStateRoundTripMs']=(time.monotonic()-sent)*1000
   if not state['session']['accountReady'] or state['session']['busy']:raise RuntimeError('Debug notebook fixture is not ready')
   if state['composer']['characters']:raise RuntimeError('Refusing to replace an existing draft')
   session=state['session']['id'];sample['composerRoundTripMs']=[]
   for text in ['S','SA','SAB','SABL','SABLE']:
    sent=time.monotonic();result=request('action',{'id':'composer.set','text':text,'expectedSessionID':session})
    sample['composerRoundTripMs'].append((time.monotonic()-sent)*1000)
    if result['state']['composer']['characters']!=len(text):raise RuntimeError('Composer did not accept input')
   request('action',{'id':'composer.clear','expectedSessionID':session})
   sample['appVersion']=state['appVersion'];sample['buildSha256']=state['buildSha256'];sample['status']='ok'
  except Exception as error:sample['error']=str(error)
  finally:
   child.terminate()
   try:child.wait(timeout=5)
   except subprocess.TimeoutExpired:child.kill();child.wait()
   for candidate in roots:
    if candidate.exists():shutil.rmtree(candidate)
  samples.append(sample);print(json.dumps(sample),flush=True)
ok=[s for s in samples if s['status']=='ok']
report={'schema':1,'app':str(app),'measurement':'Debug native notebook fixture. Warm filesystem, fresh process; 5ms readiness polling. Control round trips include 150ms automation mailbox timer; NOT keystroke-to-pixel latency. No provider requests.','samples':samples,'summary':{'passed':len(ok),'attempted':a.runs}}
for metric in ['automationReadyMs','firstStateRoundTripMs','composerRoundTripMs']:
 values=[v for s in ok for v in (s[metric] if isinstance(s[metric],list) else [s[metric]])]
 report['summary'][metric]={'p50':statistics.median(values) if values else None,'max':max(values) if values else None}
a.out.parent.mkdir(parents=True,exist_ok=True);a.out.write_text(json.dumps(report,indent=2)+'\n');print(json.dumps(report['summary'],indent=2))
raise SystemExit(0 if len(ok)==a.runs else 1)
