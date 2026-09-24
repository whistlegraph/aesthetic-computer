#!/usr/bin/env python3
"""Re-prepare ONLY the three singers for an existing fleet bundle, optionally
overriding visual payload fields (faceAlpha, captionSize) that live inside the
prepared-play fingerprint. Leaves plan.json's arrangementHash string, the
events and the Center mix exactly as they are — for a plan someone else built.

    TRIO_OUT=DIR python3 bin/reprepare-singers.py faceAlpha=1 captionSize=110
"""
import json,os,shlex,subprocess,sys,time,uuid,hashlib
from pathlib import Path
OUT=Path(os.environ['TRIO_OUT']);plan=json.loads((OUT/'plan.json').read_text());bundle=json.loads((OUT/'prepared.json').read_text())
over=dict(a.split('=',1) for a in sys.argv[1:])
LOCAL=(subprocess.run(['scutil','--get','LocalHostName'],capture_output=True,text=True).stdout.strip() or os.uname().nodename.split('.')[0]).lower()
def shell(host,script,timeout=20):
    cmd=['bash','-s'] if host.lower()==LOCAL else ['ssh','-o','BatchMode=yes','-o','ConnectTimeout=8',host,'bash -s']
    return subprocess.run(cmd,input=script+'\n',text=True,capture_output=True,timeout=timeout)
def post(host,name,info):
    kv=';'.join(k+'='+str(v) for k,v in info.items())
    r=shell(host,'MB_NAME='+shlex.quote('computer.aestheticcomputer.menuband.'+name)+' MB_KV='+shlex.quote(kv)+' /tmp/mbpost');assert r.returncode==0,(host,r.stderr)
def canonical(v):
    if isinstance(v,list):return '['+','.join(canonical(x) for x in v)+']'
    if isinstance(v,dict):return '{'+','.join(json.dumps(k)+':'+canonical(v[k]) for k in sorted(v))+'}'
    return json.dumps(v)
newid='trio-'+str(uuid.uuid4())
for p in plan['payloads']:p['info'].update(over)   # the visual overrides, nothing else
for h in [p['member'] for p in plan['payloads']]:post(h,'stop',{'x':'1'})
time.sleep(2)
singers=[]
for p in plan['payloads']:
    member,info=p['member'],p['info'];folder='/tmp/menuband-trio/'+newid
    assert shell(member,'pgrep -x MenuBand >/dev/null').returncode==0,member+': MenuBand not running'
    post(member,'fleetPrepare',{**info,'prepareId':newid})
    deadline=time.time()+120;status=None
    while time.time()<deadline:
        r=shell(member,'cat '+folder+'/status.json')
        if r.returncode==0:
            try:status=json.loads(r.stdout)
            except Exception:status=None
        if status and status.get('id')==newid:
            if status['phase'] in ('error','cancelled','stopped'):raise SystemExit(member+': '+str(status.get('reason',status['phase'])))
            if status['phase']=='ready':break
        time.sleep(.25)
    assert status and status.get('phase')=='ready',member+': preparation timed out; nothing played'
    local=OUT/'assets'/member;local.mkdir(parents=True,exist_ok=True)
    src=folder+'/' if member.lower()==LOCAL else member+':'+folder+'/'
    subprocess.check_call(['rsync','-a',src,str(local)+'/'])
    man=json.loads((local/'manifest.json').read_text())
    assert man['id']==newid and man['instance']==status['instance'] and man['fingerprint']==status['fingerprint'],member+': stale manifest'
    actual={k:v for k,v in man['payload'].items() if k!='prepareId'}
    assert canonical(actual)==canonical(info),member+': payload mismatch'
    for ph in man['phrases']:
        for f,hsh in ((ph['file'],ph['sha256']),(ph['rawFile'],ph['rawSha256'])):
            assert hashlib.sha256((local/f).read_bytes()).hexdigest()==hsh,member+': corrupted asset '+f
    singers.append({'member':member,'prepareId':newid,'instance':status['instance'],'pid':status.get('pid'),'fingerprint':status['fingerprint'],'manifest':str(local/'manifest.json'),'phrases':man['phrases']})
    print(member,len(man['phrases']),'phrases re-prepared silently',flush=True)
bundle['id']=newid;bundle['singers']=singers
(OUT/'plan.json').write_text(json.dumps(plan,indent=2)+'\n');(OUT/'prepared.json').write_text(json.dumps(bundle,indent=2)+'\n')
(OUT/'preparation-state.json').write_text(json.dumps({'id':newid,'phase':'ready','arrangementHash':plan['arrangementHash'],'playbackHeld':True}))
print('prepared',newid,'arrangementHash',plan['arrangementHash'][:16],'center',bundle['centerMix']['sha256'][:12],'overrides',over)
