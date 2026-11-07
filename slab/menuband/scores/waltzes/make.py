import json, math, os, subprocess, wave, struct
import numpy as np

OUT = "/Users/jas/aesthetic-computer/slab/menuband/scores/waltzes"
DESK = os.path.expanduser("~/Desktop/qwerty-waltzes")
L2M = {"z":58,"x":59,"c":60,"v":61,"d":62,"s":63,"e":64,"f":65,"w":66,"g":67,"r":68,"a":69,
       "q":70,"b":71,"h":72,"t":73,"i":74,"y":75,"j":76,"k":77,"u":78,"l":79,"o":80,"m":81,
       "p":82,"n":83,";":84,"'":85,"]":86}
NAMES = ["C","C#","D","D#","E","F","F#","G","G#","A","A#","B"]

# Left hand: bass on beat 1, a two-key chord on beats 2 and 3. All keys on the left half.
CHORDS = {
  "C":  ("c", ("e","g")), "F": ("f", ("f","a")), "G": ("g", ("b","d")), "G7": ("g", ("b","f")),
  "Am": ("a", ("c","e")), "Dm": ("d", ("f","a")), "E": ("e", ("r","b")), "D": ("d", ("w","a")),
  "Em": ("e", ("g","b")),
}
CHORD_TONES = {"C":{0,4,7},"F":{5,9,0},"G":{7,11,2},"G7":{7,11,2,5},"Am":{9,0,4},"Dm":{2,5,9},
               "E":{4,8,11},"D":{2,6,9},"Em":{4,7,11}}

WALTZES = [
 {"slug":"home-row-waltz","title":"Home Row Waltz","key":"C major","bpm":132,"program":0,
  "note":"right hand lives on h i j k l m n; left hand rocks c, f, g with e+g, f+a, b+d",
  "bars":[("C","h j l"),("C","l - -"),("F","k m k"),("C","j - -"),
          ("G","i k i"),("G","n - -"),("C","h j l"),("G","i - -"),
          ("C","l l m"),("C","l j h"),("F","k k m"),("F","k - -"),
          ("G","i j k"),("G","l - -"),("G","i j i"),("C","h - -")]},
 {"slug":"minor-waltz","title":"Minor Waltz","key":"A minor","bpm":112,"program":0,
  "note":"one sharp in the left hand: r is G#. the E chord is e with r+b",
  "bars":[("Am","j m j"),("Am","h - -"),("Dm","i k i"),("Am","j - -"),
          ("E","o n o"),("E","j - -"),("Am","h i j"),("E","n - -"),
          ("Am","m - l"),("Am","k - j"),("Dm","i j k"),("Dm","m - -"),
          ("E","o - n"),("E","j - -"),("E","o n o"),("Am","m - -")]},
 {"slug":"sharp-waltz","title":"Sharp Waltz","key":"G major","bpm":126,"program":0,
  "note":"F sharp is w in the left hand and u in the right",
  "bars":[("G","l n l"),("G","i - -"),("C","j l j"),("G","i - -"),
          ("D","u m u"),("D","i j u"),("G","l - n"),("D","m - -"),
          ("Em","j l j"),("Em","n - -"),("C","j l m"),("D","u - -"),
          ("G","l n l"),("C","l m j"),("D","i j u"),("G","l - -")]},
]

def rh_tokens(bars):
    """-> list of (midi or None, beats) in order, merging dashes into the previous note."""
    out=[]
    for chord, pat in bars:
        for tok in pat.split():
            if tok == "-":
                m,b = out[-1]; out[-1]=(m,b+1)
            else:
                out.append((L2M[tok],1))
    return out

def spec(tokens):
    return ",".join(("r" if m is None else str(m))+f":{b:g}" for m,b in tokens)

def lh_lines(bars):
    bass=[]; up=[]; lo=[]
    for chord,_ in bars:
        b,(x,y) = CHORDS[chord]
        bass += [(L2M[b],1),(None,2)]
        up   += [(None,1),(L2M[x],1),(L2M[x],1)]
        lo   += [(None,1),(L2M[y],1),(L2M[y],1)]
    return spec(bass), spec(up), spec(lo)

def check(w):
    """melody on beats 1 and 3 should be a chord tone; beat 2 may pass."""
    bad=[]
    for i,(chord,pat) in enumerate(w["bars"],1):
        toks=pat.split(); prev=None
        for beat,t in enumerate(toks,1):
            if t=="-": t=prev
            prev=t
            if beat in (1,3) and (L2M[t]%12) not in CHORD_TONES[chord]:
                bad.append(f"bar {i} beat {beat}: {t}({NAMES[L2M[t]%12]}) over {chord}")
    return bad

def synth(w, path):
    sr=44100; beat=60/w["bpm"]
    rh=rh_tokens(w["bars"]); b,u,l=[ [(None if a=="r" else int(a),float(c)) for a,c in (t.split(":") for t in s.split(","))] for s in lh_lines(w["bars"])]
    total=sum(x for _,x in rh)*beat+1.5
    buf=np.zeros(int(sr*total))
    def voice(tokens, gain, bright):
        t=0.0
        for m,bt in tokens:
            if m is not None:
                f=440*2**((m-69)/12); n=int(sr*bt*beat*1.05); x=np.arange(n)/sr
                env=np.exp(-x*(2.2 if bright else 3.0))*np.minimum(1,x*400)
                sig=(np.sin(2*np.pi*f*x)+0.35*np.sin(4*np.pi*f*x)+0.12*np.sin(6*np.pi*f*x))*env*gain
                s=int(t*sr); buf[s:s+n]+=sig[:len(buf)-s]
            t+=bt*beat
    voice(rh,0.5,True); voice(b,0.42,False); voice(u,0.26,False); voice(l,0.26,False)
    buf=np.tanh(buf*0.9); pcm=(buf*32767).astype("<i2")
    with wave.open(path,"wb") as f:
        f.setnchannels(1); f.setsampwidth(2); f.setframerate(sr); f.writeframes(pcm.tobytes())

def card_tex(ws):
    def bar_cell(chord,pat):
        b,(x,y)=CHORDS[chord]
        rh=" ".join("–" if t=="-" else t for t in pat.split())
        return (r"\begin{minipage}[t]{0.23\textwidth}\centering"
                r"{\chord "+chord.replace("#",r"$\sharp$")+r"}\\[2pt]"
                r"{\rh "+rh.replace("'",r"\textquotesingle{}").replace(";",r"\,;\,")+r"}\\[2pt]"
                r"{\lh "+b+r"\ \ "+x+"+"+y+r"\ \ "+x+"+"+y+r"}\end{minipage}")
    pages=[]
    for w in ws:
        rows=[]
        for i in range(0,16,4):
            rows.append(r"\noindent"+r"\hfill".join(bar_cell(c,p) for c,p in w["bars"][i:i+4])+r"\par\vspace{14pt}"
                        + (r"\noindent\rule{\textwidth}{0.4pt}\par\vspace{12pt}" if i in (4,) else r"\vspace{2pt}"))
        pages.append(r"""
\begin{center}
{\fontsize{34}{38}\selectfont\bfseries %s}\\[4pt]
{\large %s \quad·\quad $\frac{3}{4}$ \quad·\quad %d bpm \quad·\quad 16 bars}\\[6pt]
{\small\itshape %s}\\[10pt]
\includegraphics[width=0.62\textwidth]{keys-strip.png}\\[14pt]
\end{center}
{\small\sffamily top line = chord · middle = right hand, one letter per beat, – holds the note · bottom = left hand: bass on 1, then the two-key chord on 2 and 3}\par\vspace{16pt}
%s
\clearpage""" % (w["title"], w["key"], w["bpm"], w["note"].replace("#",r"$\sharp$"), "".join(rows)))
    return r"""\documentclass[11pt,letterpaper]{article}
\usepackage[top=0.7in,bottom=0.6in,left=0.7in,right=0.7in]{geometry}
\usepackage{fontspec}\usepackage{graphicx}\usepackage{xcolor}\usepackage{amsmath}
\setmainfont[Extension=.otf,UprightFont=lmroman10-regular,BoldFont=lmroman10-bold,ItalicFont=lmroman10-italic]{lmroman10}
\setsansfont[Extension=.otf,UprightFont=lmsans10-regular,BoldFont=lmsans10-bold]{lmsans10}
\setmonofont[Extension=.otf,UprightFont=lmmono10-regular]{lmmono10}
\definecolor{cream}{HTML}{F6F0E2}\definecolor{ink}{HTML}{1E1A14}\definecolor{red}{HTML}{C03A2B}\definecolor{blue}{HTML}{2457A8}
\pagecolor{cream}\color{ink}\pagestyle{empty}
\newcommand{\chord}[1]{\sffamily\bfseries\color{red}\large #1}
\newcommand{\rh}[1]{\ttfamily\fontsize{30}{34}\selectfont\color{ink}#1}
\newcommand{\lh}[1]{\ttfamily\fontsize{17}{20}\selectfont\color{blue}#1}
\begin{document}
\begin{center}{\fontsize{40}{44}\selectfont\bfseries QWERTY Waltzes}\\[6pt]{\large three waltzes for one notepat keyboard, left hand and right}\\[4pt]{\small by @jeffrey with Claude · aesthetic.computer · November 2026}\\[18pt]
\includegraphics[width=0.9\textwidth]{keys-strip.png}\\[18pt]\end{center}
{\large\noindent\textbf{How to read a card.} Each box is one bar of three beats. The red word is the chord. The big letters are the right hand: one key per beat, and a dash means keep holding the last key. The blue line is the left hand: press the single key on beat one, then press the two keys together on beats two and three. Letters are notepat letters: \texttt{c d e f g a b} are the low octave, \texttt{h i j k l m n} the high one, and the sharps sit one row up (\texttt{r} is G$\sharp$, \texttt{w} is F$\sharp$ low, \texttt{u} is F$\sharp$ high, \texttt{o} is G$\sharp$ high). Count ``ONE two three'' and let the left hand do the waltz; the right hand only ever moves to a neighbour or a chord tone.}\par\vspace{14pt}
{\large\noindent\textbf{Learning order.} Left hand alone until the bass-chord-chord rocks without looking. Then right hand alone, singing the letters. Then together at half speed. Menu Band plays each score when you open its \texttt{.mbscore}; play along with the file, then without it.}
\clearpage
%s
\end{document}""" % "".join(pages)

if __name__=="__main__":
    tracks=[]
    for w in WALTZES:
        bad=check(w)
        print(w["title"], "check:", "ok" if not bad else bad)
        rh=spec(rh_tokens(w["bars"])); b,u,l=lh_lines(w["bars"])
        score={"title":w["title"],"composer":"@jeffrey with Claude — QWERTY waltzes for one notepat keyboard","machines":1,
               "bpm":w["bpm"],"lead":3.0,"key":w["key"],"timeSignature":"3/4",
               "description":w["note"]+". Right hand = voice 1; left hand = voice 2 with the bass in notes and the two-key chord in notes2/notes3.",
               "voices":[{"name":"right hand","program":w["program"],"velocity":100,"notes":rh},
                         {"name":"left hand","program":w["program"],"velocity":84,"notes":b,"notes2":u,"notes3":l}]}
        p=f"{OUT}/{w['slug']}.mbscore"; json.dump(score,open(p,"w"),indent=2); print("wrote",p)
        wav=f"{DESK}/{w['slug']}.wav"; synth(w,wav)
        subprocess.run(["ffmpeg","-y","-loglevel","error","-i",wav,"-codec:a","libmp3lame","-q:a","3",f"{DESK}/{w['slug']}.mp3"]); os.remove(wav)
        beats=sum(bb for _,bb in rh_tokens(w["bars"]))
        tracks.append({"file":f"{w['slug']}.mbscore","title":w["title"],"machines":1,"bpm":w["bpm"],"seconds":round(beats*60/w["bpm"],1),"section":"QWERTY waltzes"})
    json.dump({"title":"QWERTY Waltzes","description":"Three waltzes for one notepat keyboard, left hand and right. Open a track to hear Menu Band play it; the score cards (cards.pdf) show the letters.","tracks":tracks},open(f"{OUT}/waltzes.mbscore","w"),indent=2)
    open(f"{OUT}/cards.tex","w").write(card_tex(WALTZES))
    import shutil; pass
    print("done")
