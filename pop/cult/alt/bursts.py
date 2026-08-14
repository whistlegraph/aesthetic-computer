exec(open('alt/analyze.py').read())
import subprocess, tempfile

def bursts(y, th_ratio=0.10, hop=128, min_gap=0.09, min_dur=0.05):
    e,_=rms_env(y,hop)
    th=np.percentile(e,99)*th_ratio
    on=e>=th
    segs=[];i=0
    while i<len(on):
        if on[i]:
            j=i
            while j<len(on) and on[j]: j+=1
            segs.append([i*hop/SR, j*hop/SR]); i=j
        else: i+=1
    m=[]
    for s in segs:
        if m and s[0]-m[-1][1]<min_gap: m[-1][1]=s[1]
        else: m.append(s)
    return [s for s in m if s[1]-s[0]>=min_dur]

def transcribe(y, s, t, pad=0.10):
    a=max(0,int((s-pad)*SR)); b=min(len(y),int((t+pad)*SR))
    seg=y[a:b]
    if len(seg) < SR*0.4:  # pad short clips so whisper has something to chew
        seg=np.concatenate([np.zeros(int(SR*0.2)), seg, np.zeros(int(SR*0.4))])
    with tempfile.NamedTemporaryFile(suffix='.wav', delete=False) as f:
        p=f.name
    sf.write(p, librosa.resample(seg, orig_sr=SR, target_sr=16000), 16000)
    out=subprocess.run(['whisper-cli','-m','/private/tmp/claude-501/-Users-jas-aesthetic-computer/d48a512a-4210-4dd6-88c1-800887ad8fea/scratchpad/ggml-small.en.bin','-f',p,'-nt','--no-prints'],
                       capture_output=True, text=True).stdout
    os.unlink(p)
    return ' '.join(out.split())
