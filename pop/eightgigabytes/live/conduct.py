#!/usr/bin/env python3
"""Silent staging: prepare / check. Only the explicit play command cues music.
Each Mac synthesizes its instruments and sings its own part in Menu Band.
"""
import concurrent.futures as cf, plistlib, hashlib,json,os,select,shlex,signal,subprocess,sys,time,urllib.request
from pathlib import Path
LANE=Path(__file__).resolve().parents[1]; OUT=LANE/'out/live'
# Surtitles: an AC OS laptop under each face runs fedac/native/pieces/surtitle.mjs and shows
# the member's sung word. live/seats.json maps member → LAN host (see seats.example.json);
# no file = no surtitles. --without-seats ignores the file for one run.
SEATS_FILE=LANE/'live/seats.json';PIECE=LANE.parents[1]/'fedac/native/pieces/surtitle.mjs'
RUN_ID=str(int(time.time()))
def seats():
 if '--without-seats' in sys.argv:return {}
 try:return json.loads(SEATS_FILE.read_text())
 except FileNotFoundError:return {}
def lan(host,path,body=None,timeout=4):
 req=urllib.request.Request('http://%s%s'%(host,path),data=body.encode() if body is not None else None,method='PUT' if body is not None else 'GET')
 with urllib.request.urlopen(req,timeout=timeout) as r:return r.read().decode()
def seat_put(host,name,body):
 for _ in range(3):   # lanserv has dropped bodies; a PUT counts only when it reads back
  lan(host,'/pieces/'+name,body)
  if lan(host,'/pieces/'+name)==body:return
 raise RuntimeError(host+': readback differs for '+name)
def seat_status(host):return json.loads(lan(host,'/pieces/surtitle-status.json'))
def seat_clock(host):
 best=None;lost=0;i=0
 while i<8:
  pid='%s-clock-%d'%(RUN_ID,i);sent=time.time()
  lan(host,'/pieces/surtitle-command.json',json.dumps({'id':pid,'action':'clock'}))
  reply=None;deadline=time.time()+1
  while time.time()<deadline:
   try:
    r=json.loads(lan(host,'/pieces/surtitle-clock.json'))
    if r.get('id')==pid:reply=r;break
   except Exception:pass
   time.sleep(.01)
  received=time.time()
  if not reply:
   lost+=1
   if lost>4:raise RuntimeError(host+' surtitle clock probe timed out')
   continue
  i+=1;rtt=received-sent
  if not best or rtt<best['rtt']:best={'rtt':rtt,'offset':reply['audioTime']-(sent+received)/2}
 return best
def words_for(h):
 """performer.swift's Word table: [token, start, end] in seconds from the downbeat."""
 perf=json.loads((OUT/(h+'.performance.json')).read_text());beat=perf['beat'];out=[]
 for line in perf['lines']:
  onsets=[];cur=line['at']
  for note in line['notation'].split():
   p,b=note.split(':');dur=float(b)*beat
   if p!='r':onsets.append((cur,cur+dur))
   cur+=dur
  k=0
  for tok in line['words']:
   n=len(tok.split('-'))
   if k+n>len(onsets):break
   mine=onsets[k:k+n];k+=n;out.append([tok,round(mine[0][0],4),round(mine[-1][1],4)])
 return out
def member_color(p,h):
 c=p['payloads'][h].get('captionColor')
 if isinstance(c,str) and c.startswith('#') and len(c)==7:return [int(c[i:i+2],16) for i in (1,3,5)]
 if isinstance(c,list) and len(c)>=3:return c[:3]
 return {'neo':[143,209,63],'blueberry':[90,87,211],'frisbee':[242,167,185]}[h]
def stage_seats():
 """Put the piece on each seat (RAM until the OS build carries it) and jump to it."""
 src=PIECE.read_text()
 for m,host in seats().items():
  seat_put(host,'surtitle.mjs',src);lan(host,'/jump/surtitle','')
  deadline=time.time()+8
  while time.time()<deadline:
   try:
    if seat_status(host).get('phase'):break
   except Exception:time.sleep(.25)
  else:raise RuntimeError(host+' (surtitle for '+m+') did not report')
  print(m+': surtitle seat '+host+' ready',flush=True)
def cue_seats(p,epoch):
 for m,host in seats().items():
  c=seat_clock(host)
  lan(host,'/pieces/surtitle-command.json',json.dumps({'id':RUN_ID+'-prepare','action':'prepare','startAt':epoch+c['offset'],
   'member':m,'color':member_color(p,m),'duration':p['duration'],'words':words_for(m)}))
  print('%s: surtitle cued (clock round trip %.1f ms)'%(m,c['rtt']*1000),flush=True)
def stop_seats():
 for m,host in seats().items():
  try:lan(host,'/pieces/surtitle-command.json',json.dumps({'id':RUN_ID+'-stop','action':'stop'}),timeout=2)
  except Exception:pass
REMOTE='/Users/jas/.local/share/eightgigabytes'
MEMBERS=['neo','blueberry','frisbee']; LOCAL=subprocess.check_output(['scutil','--get','LocalHostName'],text=True).strip().lower()
quote=shlex.quote
# --muted: a silent rehearsal for the screens (faces, captions, Esc) while the room is
# busy — every Mac's output is muted and left that way, and the mute preflight is skipped.
MUTED='--muted' in sys.argv

def run(args,**kw):return subprocess.check_output(args,text=True,**kw)
def shell(h,src,timeout=30):
 cmd=['bash','-s'] if h==LOCAL else ['ssh','-o','BatchMode=yes','-o','ConnectTimeout=8',h,'bash -s']
 return run(cmd,input='set -eu\n'+src+'\n',timeout=timeout)
def parallel(fn):
 with cf.ThreadPoolExecutor(max_workers=3) as pool:return dict(zip(MEMBERS,pool.map(fn,MEMBERS)))
def plan():return json.loads((OUT/'plan.json').read_text())
def post(h,name,info):
 path=REMOTE+'/cue.json'
 shell(h,'cat > '+quote(path)+" <<'EIGHTGIGABYTES_JSON'\n"+json.dumps(info)+"\nEIGHTGIGABYTES_JSON\n"+quote(REMOTE+'/post')+' '+quote('computer.aestheticcomputer.menuband.'+name)+' '+quote(path))
def clock(h):
 if h==LOCAL:return {'offset':0,'rtt':0}
 code='import sys,time\nfor line in sys.stdin: print(time.time(),flush=True)'
 proc=subprocess.Popen(['ssh','-o','BatchMode=yes','-o','ConnectTimeout=8',h,'python3 -u -c '+quote(code)],stdin=subprocess.PIPE,stdout=subprocess.PIPE,text=True)
 samples=[]
 try:
  for _ in range(15):
   a=time.time();proc.stdin.write('x\n');proc.stdin.flush()
   if not select.select([proc.stdout],[],[],8)[0]:raise RuntimeError(h+' clock timeout')
   t=float(proc.stdout.readline());b=time.time();samples.append({'offset':t-(a+b)/2,'rtt':b-a})
 finally:
  proc.terminate();proc.wait(timeout=3)
 best=min(samples,key=lambda x:x['rtt'])
 if best['rtt']>.04:raise RuntimeError(h+' clock RTT exceeds 40 ms')
 return best

def inventory(h,p):
 info=p['payloads'][h];volume=int(p['volumes'][h])
 # Every cue starts from the plan's system volume on each Mac (three identical speakers,
 # one level; the members' balance is in the score), unmuted — unless this is a --muted rehearsal.
 level="osascript -e 'set volume with output muted'\n" if MUTED else "osascript -e 'set volume output volume %d'\nosascript -e 'set volume without output muted'\n"%volume
 src="pgrep -x MenuBand\nsay -v '?'\n"+level+"osascript -e 'get volume settings'\nioreg -r -k AppleClamshellState -d 4 | /usr/bin/grep AppleClamshellState || true"
 s=shell(h,src)
 if not any(l.startswith(info['singVoice']+' ') for l in s.splitlines()):raise RuntimeError(h+' missing '+info['singVoice'])
 if not MUTED and ('output muted:true' in s or 'output volume:%d,'%volume not in s):raise RuntimeError(h+' speakers did not settle at %d%%'%volume)
 if '"AppleClamshellState" = Yes' in s:raise RuntimeError(h+' lid is closed')
 return s

def prepare():
 OUT.mkdir(parents=True,exist_ok=True)
 run(['bash',str(LANE/'live/build.sh')])
 run(['node',str(LANE/'live/plan.mjs')])
 p=plan();parallel(lambda h:inventory(h,p))
 def stage(h):
  shell(h,'mkdir -p '+quote(REMOTE))
  files=[OUT/'performer',OUT/f'{h}.tsv',OUT/f'{h}.performance.json']
  dest=REMOTE+'/' if h==LOCAL else h+':'+REMOTE+'/'
  run(['rsync','-a',*[str(f) for f in files],dest])
  run(['rsync','-a',str(LANE/'out/.speech-cache')+'/',dest+'speech-cache/'])
  expected={f.name:hashlib.sha256(f.read_bytes()).hexdigest() for f in files}
  hashes=shell(h,'cd '+quote(REMOTE)+'\nshasum -a 256 '+' '.join(quote(f.name) for f in files))
  for line in hashes.splitlines():
   digest,name=line.split(None,1)
   if digest!=expected[name.strip()]:raise RuntimeError(h+' copy hash mismatch')
  # The persistent launchd path is also tested silently; no cue is sent.
  launch(h,['--check','--out',REMOTE+'/offline','--preview',REMOTE+'/preview.png'])
  deadline=time.monotonic()+180
  while time.monotonic()<deadline:
   state=json.loads(shell(h,'cat '+quote(REMOTE+'/status.json')))
   if state['phase']=='error':raise RuntimeError(h+': '+str(state))
   if state['phase']=='ready':break
   time.sleep(.5)
  else:raise RuntimeError(h+' preparation timed out')
  if state['phrases']!=len(p['payloads'][h]['lyrics'].split(' / ')):raise RuntimeError(h+' phrase mismatch')
  if state['p99BlockMs']>=state['blockBudgetMs']/2:raise RuntimeError(h+' insufficient realtime headroom')
  local=OUT/(h+'-match');local.mkdir(exist_ok=True)
  source=REMOTE+'/offline/' if h==LOCAL else h+':'+REMOTE+'/offline/'
  run(['rsync','-a',source,str(local)+'/'])
  run(['rsync','-a',REMOTE+'/preview.png' if h==LOCAL else h+':'+REMOTE+'/preview.png',str(local/'preview.png')])
  result={'hashes':expected,'proof':state,'clock':clock(h)}
  shell(h,'cat > '+quote(REMOTE+'/prepared.json')+" <<'EIGHTGIGABYTES_JSON'\n"+json.dumps({'hash':p['hash'],'member':h})+"\nEIGHTGIGABYTES_JSON")
  print(h+': ready, '+str(state['phrases'])+' phrases; full live signal path checked silently',flush=True)
  return result
 receipts=parallel(stage)
 stage_seats()
 (OUT/'ready.json').write_text(json.dumps({'hash':p['hash'],'checkedAt':time.time(),'members':receipts,'playbackStarted':False},indent=2))
 print('READY. Music has not started.',flush=True)

def launch(h,extra):
 args=['/usr/bin/env','SINGER_SPEECH_CACHE='+REMOTE+'/speech-cache',REMOTE+'/performer','--config',REMOTE+'/'+h+'.performance.json',*extra]
 cmd=' '.join(quote(a) for a in args)
 # Removing a completed job makes replay possible; the process is never a child of the conductor.
 job=plistlib.dumps({'Label':'computer.aesthetic.eightgigabytes','ProgramArguments':args,'RunAtLoad':True,'KeepAlive':False,'StandardOutPath':REMOTE+'/performance.log','StandardErrorPath':REMOTE+'/performance.log'}).decode()
 shell(h,'launchctl remove computer.aesthetic.eightgigabytes 2>/dev/null || true\ncat > '+quote(REMOTE+'/launch.plist')+" <<'EIGHT_JOB'\n"+job+"\nEIGHT_JOB\nrm -f "+quote(REMOTE+'/status.json')+'\nlaunchctl bootstrap gui/$(id -u) '+quote(REMOTE+'/launch.plist'))
 end=time.monotonic()+5
 while time.monotonic()<end:
  try:return json.loads(shell(h,'cat '+quote(REMOTE+'/status.json')))
  except (subprocess.CalledProcessError,json.JSONDecodeError):time.sleep(.1)
 raise RuntimeError(h+' performer did not launch')

def check():
 p=plan();ready=json.loads((OUT/'ready.json').read_text())
 digest=hashlib.sha256()
 for f in p['sourceFiles']:digest.update((LANE/f).read_bytes())
 if digest.hexdigest()!=p['hash'] or ready['hash']!=p['hash']:raise RuntimeError('Source changed: prepare again')
 def one(h):
  inventory(h,p)
  stamp=json.loads(shell(h,'cat '+quote(REMOTE+'/prepared.json')))
  if stamp['hash']!=p['hash']:raise RuntimeError(h+' stale preparation')
  checks=ready['members'][h]['hashes']
  hashes=shell(h,'cd '+quote(REMOTE)+'\nshasum -a 256 '+' '.join(quote(f) for f in checks))
  for line in hashes.splitlines():
   value,name=line.split(None,1)
   if value!=checks[name.strip()]:raise RuntimeError(h+' staged file changed')
  return clock(h)
 clocks=parallel(one)
 for m,host in seats().items():
  if not seat_status(host).get('phase'):raise RuntimeError(host+' (surtitle for '+m+') not running the piece')
 print('All three ready; no playback.',flush=True);return p,clocks

def stop():
 # Take the job down, then put the house lights back if the performer did not get to
 # (it writes the base brightness to brightness.json when it dims, and removes it on restore).
 lights=('if [ -f %s/brightness.json ]; then ~/.local/bin/acbright "$(python3 -c "import json;print(round(json.load(open(\'%s/brightness.json\'))[\'base\']*100))")" >/dev/null 2>&1; rm -f %s/brightness.json; fi'%(REMOTE,REMOTE,REMOTE))
 parallel(lambda h:shell(h,'launchctl remove computer.aesthetic.eightgigabytes 2>/dev/null || true\nsleep 1\n'+lights))
 stop_seats()

def play():
 p,clocks=check();ready=json.loads((OUT/'ready.json').read_text())
 lead=max(25,max(r['proof']['prepareSeconds'] for r in ready['members'].values())*1.5+8)
 epoch=time.time()+lead
 # A dying conductor stops the piece: SIGTERM/SIGHUP become a normal exit through the
 # stop() below, and the conductor's own member watches this pid (--conductor) in case
 # nothing here gets to run.
 def bail(sig,_):raise SystemExit(128+sig)
 for s in (signal.SIGTERM,signal.SIGHUP):signal.signal(s,bail)
 try:
  parallel(lambda h:launch(h,['--epoch',f"{epoch+clocks[h]['offset']:.6f}"]+(['--conductor',str(os.getpid())] if h==LOCAL else [])))
  pending=set(MEMBERS);armed={}
  while pending and epoch-time.time()>3:
   for h in list(pending):
    s=json.loads(shell(h,'cat '+quote(REMOTE+'/status.json')))
    if s['phase']=='error':raise RuntimeError(h+': '+str(s))
    if s['phase']=='armed':armed[h]=s;pending.remove(h)
   time.sleep(.2)
  if pending:raise RuntimeError('Not all members armed; cancelling before downbeat')
  cue_seats(p,epoch)
  (OUT/'last-run.json').write_text(json.dumps({'epoch':epoch,'clocks':clocks,'armed':armed},indent=2))
  print('Trio cued. Starts in %.1f seconds. Esc or Ctrl-C here, or Esc on any of the three, stops all.'%(epoch-time.time()),flush=True)
  attend(epoch+p['duration']+1)
 except BaseException:
  stop();raise

def attend(until):
 """Stay with the run: Esc here stops everything; a member that cancels or errors stops the rest."""
 import termios,tty
 tty_in=sys.stdin.isatty();old=None
 if tty_in:
  old=termios.tcgetattr(sys.stdin);tty.setcbreak(sys.stdin.fileno())
 try:
  last=0
  while time.time()<until:
   if tty_in and select.select([sys.stdin],[],[],.2)[0]:
    if sys.stdin.read(1)=='\x1b':stop();print('Stopped from the conductor.',flush=True);return
   elif not tty_in:time.sleep(.2)
   if time.time()-last>.5:
    last=time.time()
    for h in MEMBERS:
     try:s=json.loads(shell(h,'cat '+quote(REMOTE+'/status.json')))
     except (subprocess.CalledProcessError,json.JSONDecodeError):continue
     if s['phase'] in ('cancelled','error'):stop();print(h+' '+s['phase']+(' ('+str(s.get('reason',''))+')' if s.get('reason') else '')+'; stopped the others.',flush=True);return
  print('Done.',flush=True)
 except KeyboardInterrupt:
  stop();print('Stopped from the conductor.',flush=True)
 finally:
  if old:termios.tcsetattr(sys.stdin,termios.TCSADRAIN,old)

if __name__=='__main__':
 words=[a for a in sys.argv[1:] if not a.startswith('--')];command=words[0] if len(words)==1 else ''
 if command not in ('prepare','check','play','stop'):sys.exit('Usage: conduct.py prepare|check|play|stop [--muted]')
 {'prepare':prepare,'check':check,'play':play,'stop':stop}[command]()
