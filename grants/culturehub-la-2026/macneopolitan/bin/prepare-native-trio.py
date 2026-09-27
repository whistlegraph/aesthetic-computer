#!/usr/bin/env python3
"""Stage the native Trio piece on the six seats, each with ITS OWN vocal stem
(the voice bounce), its notation, its backing events and, for the held center,
its wedge cues. Never sends prepare/play.

    TRIO_OUT=DIR python3 bin/prepare-native-trio.py
    TRIO_KEEP_PIECE=1     leave the seat's staged trio-fleet.mjs alone
    TRIO_ALLOW_PIECES=a,b pieces a seat may be showing before we load
"""
import concurrent.futures,hashlib,json,os,struct,time,urllib.request
from pathlib import Path
ROOT=Path(__file__).resolve().parents[1];OUT=Path(os.environ.get('TRIO_OUT','/Users/jas/Shelf/culturehub-one-big-voice'))   # TRIO_OUT=… picks the song's shelf folder
ALLOW=['culturehub-rehearsal','culturehub-concert','trio-fleet','spatial-rehearsal','notespatial-controls','notespatial-live-vol','notespatial-live-263da9','venue-screen','notepat']+[p for p in os.environ.get('TRIO_ALLOW_PIECES','').split(',') if p]   # pieces a seat may be showing before we load
plan=json.loads((OUT/'plan.json').read_text());assets=json.loads((OUT/'prepared.json').read_text())
assert assets['arrangementHash']==plan['arrangementHash']
stems=assets.get('stems') or {'seat-5':assets['centerMix']}   # v1 bundles carry only the Center mix
code=(ROOT/'fleet/native-trio.mjs').read_bytes()
def wave_of(stem):
    if stem.get('wav'):   # a ready-made PCM WAV stem (Femrag's): sent as-is
        wave=Path(stem['wav']).read_bytes();assert hashlib.sha256(wave).hexdigest()==stem['sha256'],stem['wav']
        return wave,stem['sha256'],stem['sampleRate']
    raw=Path(stem['file']).read_bytes();rate=stem['sampleRate']
    assert hashlib.sha256(raw).hexdigest()==stem['sha256'],stem['file']
    wave=b'RIFF'+struct.pack('<I',36+len(raw))+b'WAVEfmt '+struct.pack('<IHHIIHH',16,3,1,rate,rate*4,4,32)+b'data'+struct.pack('<I',len(raw))+raw
    return wave,hashlib.sha256(wave).hexdigest(),rate
def get(url):
    with urllib.request.urlopen(url,timeout=20) as r:return r.read()
def put(url,data):
    if not isinstance(data,bytes):data=json.dumps(data).encode()
    with urllib.request.urlopen(urllib.request.Request(url,data=data,method='PUT'),timeout=30) as r:return r.read()
def load(node):
    url=f"http://{node['host']}:{node['port']}";previous={'piece':'dry'};old_instance=None
    if not os.environ.get('TRIO_DRY'):
        previous=json.loads(get(url+'/status'))
        try:old_instance=json.loads(get(url+'/pieces/trio-fleet-status.json'))['instance']
        except:old_instance=None
        if previous['piece'] not in ALLOW:raise RuntimeError('Unexpected active piece '+previous['piece']+' (TRIO_ALLOW_PIECES to permit)')
    mine=lambda e:e['receiver']==node['id']
    colors=plan.get('colors',{})
    cfg={'schema':'trio-native-v1','receiverId':node['id'],'arrangementHash':plan['arrangementHash'],'bpm':plan['bpm'],'duration':plan['duration'],
     'color':[[143,209,63],[90,87,211],[242,167,185]][node['seat']%3],'seat':node['seat'],'label':node.get('label'),'heldCenter':node['seat']==5,
     'mix':plan.get('levels',{}).get('mix',.25),'fx':plan.get('fx') or {'room':.15},   # air: a little room on every Trio seat unless the plan says otherwise
     'events':[e for e in plan['events'] if mine(e) and e['layer'] not in ('dmx','voice','light') and not e.get('baked')],   # baked layers ride the stem
     'routes':[{k:e[k] for k in ('t','dur','member','phrase','text','gain','role','delay','rgb')} for e in plan['events'] if mine(e) and e['layer']=='voice'],
     'lyrics':plan.get('lyrics',[]),
     'lightCues':[{'t':e['t'],'dur':e['dur'],'rgb':e['rgb']} for e in plan['events'] if mine(e) and e['layer']=='light'],
     'colors':colors,
     # the incoming-notes roll: every sounding event in the room, this seat's own marked `mine`
     'notes':sorted([{'i':({'taiko':'kick','woodblock':'snare','brush':'hat'}.get(e.get('name'),'perc') if e['layer']=='perc' else 'bass' if e['layer']=='sub' else 'note'),'t':e['t'],'dur':e['dur'],'midi':e.get('note'),'gain':e.get('gain',.05),'label':e.get('name') or e.get('layer'),'mine':mine(e)} for e in plan['events'] if e['layer'] in ('harmony','inst','perc','bed','ornament','sub','drone')]
              +[{'i':'voice','t':e['t'],'dur':e['dur'],'midi':None,'gain':e['gain'],'label':e['role'].upper(),'text':e['text'],'rgb':e['rgb'],'mine':mine(e)} for e in plan['events'] if e['layer']=='voice'],key=lambda n:n['t']),
     'sections':[{'name':s['name'],'startSec':s['beat']*60/plan['bpm']+plan.get('leadIn',0),'endSec':(plan['sections'][i+1]['beat'] if i+1<len(plan['sections']) else (plan.get('arrangement') or {}).get('total') or plan['duration']*plan['bpm']/60)*60/plan['bpm']+plan.get('leadIn',0)} for i,s in enumerate(plan.get('sections',[]))],
     'beatsPerBar':plan.get('layers',{}).get('beatsPerBar',4),'midiLow':36,'midiHigh':96,'title':plan.get('title')}
    feed=OUT/'notes'/(node['id']+'.json')   # a folder may carry per-seat notes feeds (femrag-notes.mjs); they win
    if feed.exists():
        f=json.loads(feed.read_text());cfg.update({k:f[k] for k in ('notes','sections','beatsPerBar','midiLow','midiHigh','title') if k in f})
    if os.environ.get('TRIO_DRY'):   # TRIO_DRY=1: write the configs beside the plan, touch no seat
        d=OUT/'configs';d.mkdir(exist_ok=True);(d/(node['id']+'.json')).write_text(json.dumps(cfg,indent=1));print(node['id'],'dry config:',len(cfg['events']),'events',len(cfg['notes']),'notes',len(cfg['sections']),'sections',flush=True);return {**node,'dry':True}
    stem=stems.get(node['id'])
    if stem:
        wave,wavehash,rate=wave_of(stem);wavename='/pieces/trio-voices-'+wavehash[:16]+'.wav';chunks=[]
        for i,start in enumerate(range(0,len(wave),4*1024*1024)):
            name='/pieces/trio-'+wavehash[:16]+'-'+str(i)+'.part';chunk=wave[start:start+4*1024*1024]
            put(url+name,chunk);assert hashlib.sha256(get(url+name)).digest()==hashlib.sha256(chunk).digest();chunks.append(name)
        cfg['center']={'file':wavename,'parts':chunks,'sha256':wavehash,'rawSha256':stem.get('rawSha256',stem['sha256']),'duration':stem['frames']/rate,'bytes':len(wave),'gainBakedIn':stem.get('bakedGain','per-route')}
    put(url+'/pieces/trio-fleet-config.json',cfg);put(url+'/pieces/trio-fleet-command.json',{'id':'boot-idle-'+str(time.time_ns()),'action':'idle'})
    if os.environ.get('TRIO_KEEP_PIECE'):print(node['id'],'keeping the staged piece code',flush=True)
    else:
        put(url+'/pieces/trio-fleet.mjs',code)
        assert hashlib.sha256(get(url+'/pieces/trio-fleet.mjs')).digest()==hashlib.sha256(code).digest()
    put(url+'/jump/trio-fleet',b'')
    deadline=time.monotonic()+40
    while time.monotonic()<deadline:
        try:
            s=json.loads(get(url+'/pieces/trio-fleet-status.json'))
            if s.get('instance')==old_instance:time.sleep(.2);continue
            if s.get('error'):raise RuntimeError(node['id']+': '+s['error'])
            if s.get('arrangementHash')==plan['arrangementHash'] and s['phase']=='ready' and s['centerReady']:
                assert s['mono'] and s['monoOutput']=='left' and not s['microphoneHot']
                if stem:assert s['center']['rawSha256']==stem.get('rawSha256',stem['sha256']),(node['id'],'stem mismatch')
                print(node['id'],'loaded silently;',('stem %s'%(('%d routes'%stem['routes']) if 'routes' in stem else stem.get('format','wav'))) if stem else 'no stem',';',len(cfg['events']),'events,',len(cfg['routes']),'notation cues',flush=True)
                return {**node,'url':url,'previousPiece':previous['piece'],'status':s,'observedAt':time.time(),'assetReadbackVerified':bool(stem)}
        except urllib.error.HTTPError:pass
        time.sleep(.2)
    raise RuntimeError(node['id']+' readiness timeout')
with concurrent.futures.ThreadPoolExecutor(max_workers=6) as pool:
    results=list(pool.map(load,plan['nodes']))
(OUT/'native-loaded.json').write_text(json.dumps(results,indent=2))
print('Native staging complete; no audio cue.')
