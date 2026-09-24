#!/usr/bin/env python3
"""One-shot full Trio rehearsal. --check performs only silent readiness checks."""
import concurrent.futures as cf,json,os,select,shlex,subprocess,time,uuid,urllib.request,threading,signal,sys
from pathlib import Path
ROOT=Path(__file__).resolve().parents[1];OUT=Path(os.environ.get('TRIO_OUT','/Users/jas/Shelf/culturehub-one-big-voice'))   # TRIO_OUT=… the song's shelf folder
# Conduct from any of the three Macs: this machine's member name is local, the
# others go over ssh. SUB and DMX are reached at TRIO_SUB / TRIO_DMX; their
# write endpoints are localhost-only on their own hosts, so from another Mac
# point these at ssh port-forwards (ssh -N -L 8791:127.0.0.1:8791 blueberry).
LOCAL=(subprocess.run(['scutil','--get','LocalHostName'],capture_output=True,text=True).stdout.strip() or os.uname().nodename.split('.')[0]).lower()
SUB=os.environ.get('TRIO_SUB','http://127.0.0.1:8788').rstrip('/');DMX=os.environ.get('TRIO_DMX','http://127.0.0.1:8790').rstrip('/')
def islocal(h):return h.lower()==LOCAL
RTT_MAX=float(os.environ.get('TRIO_CLOCK_RTT_MAX','0.04'))   # seconds; the 40 ms contract unless explicitly widened
# The display feed (blueberry:8796 → neo's stage service → Xbox / ac7): a
# transport heartbeat with the lyric being sung. Best effort, never fatal.
VIS=os.environ.get('TRIO_VIS','http://192.168.1.234:8796').rstrip('/')
def lyric_at(t):
 cur=None;nxt=None
 for l in plan.get('lyrics',[]):
  if t>=l['t']-.3 and t<l['t']+l['dur']+.8:cur=l
  elif l['t']>t and nxt is None:nxt=l
 return cur,nxt
def visuals(playing,elapsed):
 cur,nxt=lyric_at(elapsed) if playing else (None,None)
 body={'playing':playing,'elapsed':max(-1,min(plan['duration']+1,elapsed)),'title':plan.get('title'),'dance':'trio-round-v1','bpm':plan['bpm'],'duration':plan['duration'],
  'lyric':cur and {'text':cur['text'],'member':cur['member'],'rgb':cur['rgb'],'t':cur['t'],'dur':cur['dur']},'next':nxt and {'text':nxt['text'],'member':nxt['member'],'rgb':nxt['rgb'],'in':round(nxt['t']-elapsed,2)}}
 try:
  raw=json.dumps(body).encode()
  with urllib.request.urlopen(urllib.request.Request(VIS+'/api/transport',raw,{'Content-Type':'application/json','Origin':VIS},method='POST'),timeout=1) as r:r.read()
  return True
 except Exception:return False
plan=json.loads((OUT/'plan.json').read_text());nodes=json.loads((OUT/'native-loaded.json').read_text())
bundle=json.loads((OUT/'prepared.json').read_text()) if (OUT/'prepared.json').exists() else {'id':None,'singers':[],'stems':{}}   # a piece without singers has no bundle
members=[p['member'] for p in plan.get('payloads',[])];runid='full-trio-'+uuid.uuid4().hex[:10];errors=[];quit=threading.Event();record={'runId':runid,'arrangementHash':plan['arrangementHash'],'timing':'Native simulation-frame dispatch; acoustic alignment not calibrated','checks':{},'samples':[]}
locks={n['id']:threading.Lock() for n in nodes}
def request(url,data=None,method=None):
 raw=None if data is None else json.dumps(data).encode()
 with urllib.request.urlopen(urllib.request.Request(url,data=raw,method=method,headers={'Content-Type':'application/json'}),timeout=2) as r:
  b=r.read()
  try:return json.loads(b)
  except:return b.decode()
def save(): (OUT/(runid+'.json')).write_text(json.dumps(record,indent=2))
def parallel(fn,items):
 with cf.ThreadPoolExecutor(max_workers=12) as p:return list(p.map(fn,items))
def shell(host,script,timeout=12):
 cmd=['bash','-s'] if islocal(host) else ['ssh','-o','BatchMode=yes','-o','ConnectTimeout=5',host,'bash -s']
 return subprocess.check_output(cmd,input='set -e\n'+script+'\n',text=True,timeout=timeout)
def post(host,name,info):
 kv=';'.join(k+'='+str(v) for k,v in info.items())
 return shell(host,'MB_NAME='+shlex.quote('computer.aestheticcomputer.menuband.'+name)+' MB_KV='+shlex.quote(kv)+' /tmp/mbpost')
def skew(host):
 if islocal(host):return {'offset':0,'rtt':0}
 src='import sys,time\nfor line in sys.stdin: print(time.time(),flush=True)'
 p=subprocess.Popen(['ssh','-o','BatchMode=yes',host,'python3 -u -c '+shlex.quote(src)],stdin=subprocess.PIPE,stdout=subprocess.PIPE,text=True)
 samples=[]
 try:
  for _ in range(12):
   a=time.time();p.stdin.write('x\n');p.stdin.flush()
   if not select.select([p.stdout],[],[],6)[0]:raise RuntimeError(host+' clock timeout')
   t=float(p.stdout.readline());b=time.time();samples.append({'offset':t-(a+b)/2,'rtt':b-a})
 finally:p.terminate();p.wait(timeout=3)
 best=min(samples,key=lambda s:s['rtt']);assert best['rtt']<.04,(host,best)
 return best
def singercheck(h):
 post(h,'fleetReady',{'prepareId':bundle['id']});time.sleep(.2)
 s=json.loads(shell(h,'cat /tmp/menuband-trio/'+bundle['id']+'/status.json'))
 e=next(x for x in bundle['singers'] if x['member']==h)
 assert s['phase']=='ready' and s['instance']==e['instance'] and s['fingerprint']==e['fingerprint'],(h,s)
 return h,{'clock':skew(h),'logBase':int(shell(h,'wc -l < /tmp/menuband.err')),'receipt':s}
def status(n):return request(n['url']+'/pieces/trio-fleet-status.json')
def command(n,action,**kw):
 with locks[n['id']]:
  c={'id':uuid.uuid4().hex,'action':action,**kw};request(n['url']+'/pieces/trio-fleet-command.json',c,'PUT');return c['id']
def ack(n,phase):
 end=time.monotonic()+2
 while time.monotonic()<end:
  s=status(n)
  if s['error']:raise RuntimeError((n['id'],s['error']))
  if s['phase']==phase and s['runId']==runid:return s
  time.sleep(.02)
 raise RuntimeError((n['id'],'No '+phase+' acknowledgment'))
def nativecheck(n):
 # a seat mid-stall reads the same audioTime twice: try again a few times before calling it stale
 for attempt in range(6):
  s=status(n);time.sleep(.07);s2=status(n)
  if s2['audioTime']>s['audioTime'] and s2['instance']==s['instance']:break
  time.sleep(.3)
 assert s2['audioTime']>s['audioTime'] and s2['instance']==s['instance'],n['id']+' stale'
 assert s2['phase']=='ready' and not s2['error'] and s2['arrangementHash']==plan['arrangementHash'] and s2['receiverId']==n['id'],s2
 assert s2['mono'] and s2['monoOutput']=='left' and not s2['microphoneHot'] and s2['centerReady'],s2
 want=(bundle.get('stems') or {}).get(n['id']) or (bundle.get('centerMix') if n['seat']==5 else None) or n.get('stem')   # the stem this seat was staged with
 if want:assert s2['center']['rawSha256']==want['sha256'] and s2['center']['loaded'] and n['assetReadbackVerified'],s2
 samples=[]
 for _ in range(24):   # venue Wi-Fi jitters; more probes find a clean round trip
  a=time.monotonic();cid=command(n,'clock')
  for i in range(400):   # up to ~2 s: a seat mid-stall answers late, and that sample simply loses to a faster one
   try:c=request(n['url']+'/pieces/trio-fleet-clock.json')
   except urllib.error.HTTPError as e:
    if e.code!=404:raise
    time.sleep(.005);continue
   if isinstance(c,dict) and c.get('id')==cid:break   # a partial write reads back as text: try again
   time.sleep(.005)
  else:raise RuntimeError(n['id']+' clock probe failed')
  b=time.monotonic();assert c['instance']==s2['instance'];samples.append({'offset':c['audioTime']-(a+b)/2,'rtt':b-a})
 best=min(samples,key=lambda x:x['rtt'])
 # Clock uncertainty is half the round trip. 40 ms is the contract; a wider
 # limit must be asked for (TRIO_CLOCK_RTT_MAX) and is printed, never silent.
 assert best['rtt']<RTT_MAX,(n['id'],'clock uncertainty',best)
 if best['rtt']>=.04:print('WARNING',n['id'],'clock round trip %.1f ms exceeds the 40 ms contract (limit raised to %.0f ms)'%(best['rtt']*1000,RTT_MAX*1000),flush=True)
 return n['id'],{'clock':best,'receipt':s2}
def subcheck():
 rs=request(SUB+'/api/receivers');r=next((x for x in rs if x['ip'].endswith('192.168.1.67') and x['online'] and x['armed'] and x['scoreHash']==plan['arrangementHash']),None)
 assert r and r['audioState']=='running' and r['fullscreen'] and r['route']=='both' and r['level']>0 and abs(r['duration']-plan['duration'])<.01,('SUB not ready',rs)
 return r
def dmxcheck():
 s=request(DMX+'/state');assert s['supportsCancel'] and time.time()-s['bridgeSeen']<4 and s['queueDepth']==0,s
 return {k:s[k] for k in ['bridgeSeen','queueDepth','supportsCancel','lastResult']}
def cancel_dmx():
 request(DMX+'/cancel',{});cid=request(DMX+'/state')['cancelId'];end=time.monotonic()+3
 while time.monotonic()<end:
  s=request(DMX+'/state')
  if s.get('lastResult',{}).get('id')==cid and s['lastResult']['result']=='ok' and s['queueDepth']==0:return s['lastResult']
  time.sleep(.05)
 raise RuntimeError('DMX cancellation not acknowledged')
def keepalive():
 while not quit.is_set():
  try:
   parallel(lambda n:command(n,'keepalive',runId=runid),nodes)
   request(SUB+'/api/trio/keepalive',{'runId':runid})
   t=time.monotonic()-downbeat;record['visuals']=visuals(0<=t<plan['duration'],t)
  except Exception as e:errors.append(str(e));return
  quit.wait(.5)
def lights():
 for e in (x for x in plan['events'] if x['layer']=='dmx'):
  if quit.wait(max(0,downbeat+e['t']-time.monotonic())):return
  try:request(DMX+'/command',dict(e['command'],eventId=e['id']))
  except Exception as x:errors.append('DMX '+str(x));return
try:
 hosts=dict(parallel(singercheck,members)) if members else {};native=dict(parallel(nativecheck,nodes));record['checks']={'singers':hosts,'native':native,'sub':subcheck(),'dmx':dmxcheck()};save()
 print('Ready: %s six ACOS, Windows SUB, four DMX fixtures.'%(('%d singers,'%len(members)) if members else 'no singers,'),flush=True)
 if '--check' in sys.argv:sys.exit(0)
except Exception as e:
 record['error']=str(e);save();raise
cancel='/tmp/'+runid+'.cancel';brightness='/tmp/'+runid+'.brightness.json';epoch=time.time()+15;downbeat=time.monotonic()+(epoch-time.time())
cm=bundle.get('centerMix');duration=max(plan['duration'],(cm['frames']/cm['sampleRate']) if cm else 0)+2
record.update(startEpoch=epoch,duration=duration)
armed=False;threads=[]
def cleanup():
 quit.set()
 for t in threads:t.join(timeout=3)
 results={}
 for n in nodes:
  try:command(n,'stop');results[n['id']]='stop sent'
  except Exception as e:results[n['id']]=str(e)
 try:results['sub']=request(SUB+'/api/trio/stop',{})
 except Exception as e:results['sub']=str(e)
 try:results['dmx']=cancel_dmx()
 except Exception as e:results['dmx']=str(e)
 for h in members:
  try:post(h,'stop',{});shell(h,'touch '+cancel);results[h]='stopped; brightness restoration requested'
  except Exception as e:results[h]=str(e)
 try:visuals(False,0)
 except Exception:pass
 record['cleanup']=results;save()
def interrupted(*_):raise KeyboardInterrupt()
signal.signal(signal.SIGTERM,interrupted);signal.signal(signal.SIGINT,interrupted)
try:
 def prepare(n):
  command(n,'prepare',arrangementHash=plan['arrangementHash'],startAt=downbeat+native[n['id']]['clock']['offset'],runId=runid);return n['id'],ack(n,'prepared')
 record['nativePrepared']=dict(parallel(prepare,nodes))
 record['subPrepared']=request(SUB+'/api/trio/prepare',{'hash':plan['arrangementHash'],'runId':runid,'startEpoch':epoch})
 t=threading.Thread(target=keepalive,daemon=True);threads.append(t);t.start()
 def bright(h):
  helper=str(ROOT/'bin/trio-brightness.py') if islocal(h) else '/tmp/trio-brightness.py';localepoch=epoch+hosts[h]['clock']['offset'];hosts[h]['startEpoch']=localepoch
  if not islocal(h):subprocess.check_call(['scp','-q',str(ROOT/'bin/trio-brightness.py'),h+':/tmp/trio-brightness.py'])   # the helper rides along
  shell(h,f'nohup python3 {shlex.quote(helper)} run --start-epoch {localepoch:.6f} --duration {duration:.6f} --cancel-file {cancel} --status-file {brightness} > /tmp/{runid}.brightness.log 2>&1 < /dev/null &')
  time.sleep(.2);r=json.loads(shell(h,'cat '+brightness));assert r['phase']=='armed',(h,r);return h,r
 record['brightnessArmed']=dict(parallel(bright,members)) if members else {};subcheck();assert epoch-time.time()>4 and not errors
 def playnative(n):command(n,'play',runId=runid);return n['id'],ack(n,'countdown')
 record['nativeCountdown']=dict(parallel(playnative,nodes));record['subCountdown']=request(SUB+'/api/trio/play',{'runId':runid})
 def sing(h):
  payload=dict(next(p['info'] for p in plan['payloads'] if p['member']==h));payload.update(preparedId=bundle['id'],startEpoch=f"{hosts[h]['startEpoch']:.6f}");post(h,'play',payload)
 if members:parallel(sing,members)
 assert epoch-time.time()>2
 t=threading.Thread(target=lights,daemon=True);threads.append(t);t.start();save();print('FULL SYSTEM CUED. Downbeat in',round(epoch-time.time(),1),'seconds.',flush=True)
 last=-1
 while time.monotonic()<downbeat+duration+.2:
  if errors:raise RuntimeError(errors)
  elapsed=time.monotonic()-downbeat
  if elapsed>0 and int(elapsed)//2!=last:
   last=int(elapsed)//2;ss=parallel(status,nodes)
   # One seat stalling on its downbeat silences that seat, not the room:
   # record it and play on; anything else is still a stop.
   for s in ss:
    if s['error'] and 'Missed downbeat' in s['error']:
     if s['receiverId'] not in record.setdefault('seatWarnings',{}):record['seatWarnings'][s['receiverId']]=s['error'];print('WARNING',s['receiverId'],s['error'],flush=True)
    else:assert not s['error'],s
   sub=subcheck();dmx=request(DMX+'/state');assert time.time()-dmx['bridgeSeen']<4,'DMX offline'
   record['samples'].append({'t':elapsed,'native':ss,'sub':sub,'dmxResult':dmx.get('lastResult')});save()
   if last%5==0:print('Playing',round(elapsed),'sec; all receivers online.',flush=True)
  time.sleep(.08)
 def collect(h):
  logs=shell(h,'tail -n +'+str(hosts[h]['logBase']+1)+' /tmp/menuband.err');(OUT/(runid+'-'+h+'.log')).write_text(logs)
  return h,{'scheduled':logs.count('scheduled '),'played':logs.count('sing: playing'),'rejected':'prepared play rejected' in logs,'brightness':json.loads(shell(h,'cat '+brightness))}
 record['singerResults']=dict(parallel(collect,members)) if members else {};record['completed']=True
 print('Performance ended.',json.dumps(record['singerResults']),flush=True)
except BaseException as e:record['error']=str(e);print('Stopping:',repr(e),flush=True);raise
finally:cleanup()
