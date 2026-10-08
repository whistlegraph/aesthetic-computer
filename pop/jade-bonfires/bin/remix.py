#!/usr/bin/env python3
"""A 30-second affectionate Menu Band remix, aligned from the take's MIDI."""
from pathlib import Path
import json, subprocess, re
import numpy as np
import soundfile as sf
from scipy.signal import butter, sosfilt
SR=44100; BPM=128; BEAT=60/BPM; N=30*SR
ROOT=Path.home()/'Documents/Shelf/Jade Bonfires 2026-10-07'; SRC=ROOT/'Original/jade-bonfires-oct-7-stems'
OUT=ROOT/'Remixes/jade-bonfires-adoration-full'; OUT.mkdir(exist_ok=True)
# Read the Menu Band type-0 MIDI, including running status and tempo changes.
b=(SRC/'notes.mid').read_bytes(); ppq=int.from_bytes(b[12:14],'big'); p=22; clock=0; status=0; tempo=500000; notes=[]
def var():
 global p
 v=0
 while True:
  x=b[p];p+=1;v=(v<<7)|(x&127)
  if x<128:return v
while p<len(b):
 clock+=var()*tempo/ppq/1e6
 if b[p]&128:status=b[p];p+=1
 if status==255:
  typ=b[p];p+=1;size=var();data=b[p:p+size];p+=size
  if typ==81:tempo=int.from_bytes(data,'big')
 elif status>=240:size=var();p+=size
 else:
  kind=status&240;size=1 if kind in [192,208] else 2;data=b[p:p+size];p+=size
  if kind==144 and data[1]>0:notes.append((clock,data[0]))
tones,rate=sf.read(SRC/'tones.wav',always_2d=True); drums,drate=sf.read(SRC/'percussion.wav',always_2d=True)
assert rate==drate==SR
# Clean subsonics and round the recorded percussion before arranging.
tones=sosfilt(butter(2,65,fs=SR,btype='highpass',output='sos'),tones,axis=0)
drums=sosfilt(butter(2,35,fs=SR,btype='highpass',output='sos'),drums,axis=0)
drums=.32*np.tanh(drums/.32)
music=np.zeros((N,2)); rhythm=np.zeros((N,2)); glints=np.zeros((N,2))
def fade(x,a=.005,r=.045):
 x=x.copy();na=min(len(x),round(a*SR));nr=min(len(x),round(r*SR))
 if na:x[:na]*=np.linspace(0,1,na)[:,None]
 if nr:x[-nr:]*=np.linspace(1,0,nr)[:,None]
 return x
def put(bus,x,at,gain=1,pan=0):
 i=round(at*SR)
 if i<0 or i>=N:return
 if x.ndim==1:x=np.repeat(x[:,None],2,axis=1)
 count=min(len(x),N-i)
 bus[i:i+count]+=x[:count]*gain*np.array([1-max(pan,0)*.5,1+min(pan,0)*.5])
# This is an existing composition: retain its elected melody, snap its attacks
# to eighths, and preserve each recorded note's pitch rather than speeding it up.
start=notes[3][0]; source_beat=float(np.median(np.diff([t for t,n in notes if 4<t<22])))
events=[]
for idx,(at,note) in enumerate(notes):
 if at<start or at>35.2:continue
 grid=round((at-start)/source_beat*2)/2
 if grid>=62:continue
 dest=grid*BEAT
 next_at=notes[idx+1][0] if idx+1<len(notes) else at+.5
 stop=min(next_at+.035,at+.85)
 segment=fade(tones[round(at*SR):round(stop*SR)],.003,.065)
 put(music,segment,dest,1.9)
 # Delicate dotted echoes answer the main phrase without blurring attacks.
 if grid%4 in [0,2]:
  put(glints,segment,dest+BEAT*.75,.24,-.55)
  put(glints,segment,dest+BEAT*1.5,.12,.55)
 events.append({'source':round(at,4),'at':round(dest,4),'midi':note})
# Soft chord-tone bells: an upper third answers the recorded line, panned gently.
scale=[48,50,52,53,55,57,59,60,62,64,65,67,69,71,72,74,76,77,79,81,83,84]
def bell(note,duration=1.25):
 t=np.arange(round(duration*SR))/SR;f=440*2**((note-69)/12)
 y=np.sin(2*np.pi*f*t)*np.exp(-t/.38)+.16*np.sin(2*np.pi*f*2*t)*np.exp(-t/.12)
 return fade(np.repeat(y[:,None],2,axis=1),.008,.13)
for j,e in enumerate(events):
 if j%4!=1:continue
 lead=e['midi']+24
 degree=min(range(len(scale)),key=lambda i:abs(scale[i]-lead))
 put(glints,bell(scale[min(len(scale)-1,degree+2)]),e['at']+BEAT*.5,.033,(-1)**j*.5)
# A tender Cmaj9 landing, then let its tail breathe.
for k,note in enumerate([60,64,67,71,74]):put(glints,bell(note,2.8),27.9+k*.075,.025,(k-2)*.2)
# Warm low support, chosen from the actual phrase's pitch at each bar.
for bar in range(15):
 at=bar*4*BEAT
 candidates=[e for e in events if at<=e['at']<at+4*BEAT]
 if not candidates:continue
 root=candidates[0]['midi'];f=440*2**((root-69)/12)
 for offset in [0,2.5]:
  t=np.arange(round(.48*SR))/SR
  bass=(np.sin(2*np.pi*f*t)+.2*np.sin(4*np.pi*f*t))*np.exp(-t/.20)
  put(music,fade(np.repeat(bass[:,None],2,axis=1),.025,.1),at+offset*BEAT,.035)
# Fill the spaces with voice-led chords and a quiet, regular answering line.
# Each chord supports the take's actual bass note; no new random harmony.
chords={0:[48,55,59,64],2:[50,57,60,65],4:[48,55,59,64],
        5:[53,57,60,64],7:[55,59,62,65],9:[45,52,55,60],11:[55,59,62,65]}
for bar in range(16):
 at=bar*4*BEAT
 candidates=[e for e in events if at<=e['at']<at+4*BEAT]
 root=candidates[0]['midi'] if candidates else 36
 chord=chords.get(root%12,chords[0]) if bar<15 else chords[0]
 duration=4*BEAT+.32;t=np.arange(round(duration*SR))/SR
 pad=np.zeros((len(t),2))
 for j,note in enumerate(chord):
  f=440*2**((note-69)/12)
  core=np.sin(2*np.pi*f*t)+.14*np.sin(4*np.pi*f*t)
  pad[:,0]+=core*.010+np.sin(2*np.pi*f*1.001*t)*.002
  pad[:,1]+=core*.010+np.sin(2*np.pi*f*.999*t)*.002
 put(music,fade(pad,.10,.35),at)
 for j,offset in enumerate([.5,1.5,2.5,3.5]):
  if at+offset*BEAT>29:continue
  note=chord[[1,2,3,2][j]]+12
  put(glints,bell(note,.9),at+offset*BEAT,.026 if bar<8 else .034,(-1)**j*.35)
# Rebuild a straightforward pocket from the recorded kick/backbeat timbres.
# Fixed hat samples on straight eighths replace accidental chopped accents.
kick=fade(sosfilt(butter(2,1800,fs=SR,output='sos'),drums[:round(.18*SR)],axis=0),.003,.045)
snare=fade(sosfilt(butter(2,180,fs=SR,btype='highpass',output='sos'),drums[round(.48*SR):round(.68*SR)],axis=0),.003,.045)
def hit_level(x,peak):return x*peak/max(float(np.max(abs(x))),1e-8)
kick=hit_level(kick,.19);snare=hit_level(snare,.10)
rng=np.random.default_rng(17);t=np.arange(round(.055*SR))/SR
hat=sosfilt(butter(2,[6000,12000],fs=SR,btype='bandpass',output='sos'),rng.normal(size=len(t)))*np.exp(-t/.011)
hat=hit_level(fade(np.repeat(hat[:,None],2,axis=1),.002,.012),.021)
for bar in range(16):
 at=bar*4*BEAT
 for k in range(4):
  if bar==15 and k>1:continue
  if k in [0,2]:put(rhythm,kick,at+k*BEAT,.9)
  if k in [1,3]:put(rhythm,snare,at+k*BEAT,.9)
  put(rhythm,hat,at+k*BEAT,.8,.15)
  put(rhythm,hat,at+(k+.5)*BEAT,1,.15)

# Quiet stereo room on the decorative layer, keeping the low end centered.
for delay,gain in [(.087,.10),(.143,.08),(.231,.06)]:
 shift=round(delay*SR);glints[shift:]+=glints[:-shift,::-1]*gain
phase=(np.arange(N)/SR)%BEAT
music*= (1-.10*np.exp(-phase/.065))[:,None]
mix=fade(music+rhythm+glints,.025,1.25)
assert np.isfinite(mix).all() and np.max(abs(mix))>0
sf.write(OUT/'premaster.wav',mix,SR,subtype='FLOAT')
def ff(*args):
 return subprocess.run(['ffmpeg','-hide_banner','-nostats','-y',*map(str,args)],capture_output=True,text=True,check=True)
def measure(path):
 r=ff('-i',path,'-af','loudnorm=I=-10:TP=-2:LRA=7:print_format=json','-f','null','-')
 return json.loads(re.search(r'\{\s*"input_i"[\s\S]*?\}',r.stderr)[0])
levels=measure(OUT/'premaster.wav')
# Static gain with oversampled peak limiting; never ride the loudness envelope.
gain=min(-10.5-float(levels['input_i']),-2.5-float(levels['input_tp'])+2)
master=OUT/'jade-bonfires-adoration-full.wav';mp3=OUT/'jade-bonfires-adoration-full.mp3'
ff('-i',OUT/'premaster.wav','-af',f'volume={gain}dB,aresample=176400,alimiter=limit=0.749894:attack=5:release=70:level=false,aresample=44100','-c:a','pcm_s24le',master)
ff('-i',master,'-c:a','libmp3lame','-b:a','256k','-id3v2_version','3','-map_metadata','-1',mp3)
report={'mp3':str(mp3),'seconds':30,'bpm':BPM,'source_beat':source_beat,'events':events,'master':measure(mp3),'voice':'Mic stem nearly silent; omitted rather than amplifying noise.'}
(OUT/'remix.json').write_text(json.dumps(report,indent=2)+'\n')
print(json.dumps({k:v for k,v in report.items() if k!='events'}))
# One player, one document, no loop.
script='''on run argv
if application "Music" is running then
 tell application "Music" to pause
end if
tell application "QuickTime Player"
 repeat with d in documents
  try
   pause d
  end try
 end repeat
 set preview to open POSIX file (item 1 of argv)
 if preview is missing value then
  delay 0.5
  set preview to open POSIX file (item 1 of argv)
 end if
 if preview is missing value then error "QuickTime could not open the preview"
 set looping of preview to false
 set current time of preview to 0
 set audio volume of preview to 0.35
 activate
 play preview
end tell
end run'''
subprocess.run(['osascript','-e',script,str(mp3)],check=True)
