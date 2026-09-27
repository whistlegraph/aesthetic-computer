import json,subprocess,re
import numpy as np
from scipy.io import wavfile
from pathlib import Path
p=Path('pop/eightgigabytes/out');sr=48000
x=np.fromfile(p/'live/combined-premaster.f32',dtype='<f4').reshape(-1,2)
x=np.tanh(x*1.1);norm=10**(-1/20)/np.max(np.abs(x));x*=norm
x=np.pad(x,((98400,0),(0,0)));wavfile.write(p/'live/live-mix.wav',sr,x.astype('float32'))
ln='loudnorm=I=-14:TP=-1.5:LRA=9'
r=subprocess.run(['ffmpeg','-hide_banner','-nostats','-i',str(p/'eightgigabytes-mix.wav'),'-af',ln+':print_format=json','-f','null','-'],capture_output=True,text=True)
stats=json.loads(re.findall(r'\{[\s\S]*\}',r.stderr)[0])
ln2=ln+':measured_I='+stats['input_i']+':measured_TP='+stats['input_tp']+':measured_LRA='+stats['input_lra']+':measured_thresh='+stats['input_thresh']+':offset='+stats['target_offset']+':linear=true'
subprocess.run(['ffmpeg','-y','-v','error','-ss','2.050','-i',str(p/'live/live-mix.wav'),'-af','afade=t=in:d=0.3,'+ln2+',alimiter=limit=0.94:attack=3:release=60','-ar','48000','-c:a','pcm_s24le',str(p/'live/live-master-comparison.wav')],check=True)
_,a=wavfile.read(p/'eightgigabytes.wav');_,b=wavfile.read(p/'live/live-master-comparison.wav');n=min(len(a),len(b));a=a[:n].astype('float64')/2147483648;b=b[:n].astype('float64')/2147483648
rms=lambda a:float(np.sqrt(np.mean(a*a)))
j={'studioMasterResidualDb':20*np.log10(rms(a-b)/rms(a)),'peakError':float(np.max(np.abs(a-b))),'liveLinearGain':rms(a)/rms(np.fromfile(p/'live/combined-premaster.f32',dtype='<f4')),'saturationNorm':float(norm),'loudnorm':stats}
print(j);(p/'live/master-comparison.json').write_text(json.dumps(j,indent=2))
