"""Hand-notation score cards: two keycap rows per system, one per hand."""
import sys, importlib.util, os
spec = importlib.util.spec_from_file_location("w", os.path.join(os.path.dirname(os.path.abspath(__file__)), "make.py"))
w = importlib.util.module_from_spec(spec); spec.loader.exec_module(w)
WALTZES, CHORDS, L2M = w.WALTZES, w.CHORDS, w.L2M
COLOR = {0:"FF3232",2:"FFA000",4:"FFE600",5:"32C832",7:"3278FF",9:"8232C8",11:"B450FF"}
def capcolor(letter):
    pc = L2M[letter] % 12
    return COLOR.get(pc, "333333"), ("white" if pc not in COLOR else "ink")

def cap(letter, held=False, small=False):
    fill, txt = capcolor(letter)
    size = "small" if small else "big"
    if held:
        return r"\heldcap{%s}{%s}" % (size, letter)
    return r"\keycap{%s}{%s}{%s}{%s}" % (size, fill, txt, letter)

def system(bars, first):
    """4 bars -> TikZ picture with R row and L row of caps."""
    out = [r"\begin{tikzpicture}[x=1cm,y=1cm]"]
    x0 = 1.3
    bw = 3.2   # bar width (3 beats)
    # hand icons
    out.append(r"\node at (0.45,1.05) {\rhand};")
    out.append(r"\node at (0.45,-0.35) {\lhand};")
    for bi,(chord,pat) in enumerate(bars):
        bx = x0 + bi*bw
        toks = pat.split(); prev=None
        for b in range(3):
            out.append(r"\node[font=\large\sffamily\bfseries\color{%s}] at (%.2f,-1.35) {%d};" % ("ink" if b==0 else "dim", bx+0.5+b, b+1))
        for b,t in enumerate(toks):
            cx = bx + 0.5 + b
            if t == "-":
                out.append(r"\node at (%.2f,1.05) {%s};" % (cx, cap(prev, held=True)))
            else:
                out.append(r"\node at (%.2f,1.05) {%s};" % (cx, cap(t))); prev=t
        bass,(u,l) = CHORDS[chord]
        out.append(r"\node at (%.2f,-0.35) {%s};" % (bx+0.5, cap(bass)))
        for b in (1,2):
            cx = bx+0.5+b
            out.append(r"\node at (%.2f,-0.05) {%s};" % (cx, cap(u, small=True)))
            out.append(r"\node at (%.2f,-0.68) {%s};" % (cx, cap(l, small=True)))
        # bar line
        out.append(r"\draw[dim,line width=0.5pt] (%.2f,-1.6) -- (%.2f,1.6);" % (bx+bw-0.0, bx+bw-0.0))
    out.append(r"\draw[dim,line width=0.5pt] (%.2f,-1.6) -- (%.2f,1.6);" % (x0, x0))
    out.append(r"\end{tikzpicture}")
    return "\n".join(out)

def page(wz):
    systems = "\n\n\\vspace{34pt}\n\n".join("\\noindent" + system(wz["bars"][i:i+4], i==0) for i in range(0,16,4))
    return r"""
\begin{center}
{\fontsize{32}{36}\selectfont\bfseries %s}\\[6pt]
{\large %d bpm \quad count 1 2 3}
\end{center}
\vspace{18pt}
%s
\clearpage""" % (wz["title"], wz["bpm"], systems)

def doc():
    pages = "".join(page(wz) for wz in WALTZES)
    tpl = r"""\documentclass[11pt,letterpaper]{article}
\usepackage[top=0.6in,bottom=0.5in,left=0.55in,right=0.55in]{geometry}
\usepackage{fontspec}\usepackage{graphicx}\graphicspath{{../../../../papers/notepat-charts/}}
\usepackage{xcolor}\usepackage{amsmath}\usepackage{tikz}\usepackage{ifthen}
\setmainfont[Extension=.otf,UprightFont=lmroman10-regular,BoldFont=lmroman10-bold,ItalicFont=lmroman10-italic]{lmroman10}
\setsansfont[Extension=.otf,UprightFont=lmsans10-regular,BoldFont=lmsans10-bold]{lmsans10}
\setmonofont[Extension=.otf,UprightFont=lmmono10-regular,BoldFont=lmmonolt10-bold]{lmmono10}
\definecolor{cream}{HTML}{F6F0E2}\definecolor{ink}{HTML}{1E1A14}\definecolor{red}{HTML}{C03A2B}\definecolor{dim}{HTML}{9A8F7A}\definecolor{skin}{HTML}{E8C9A6}
\pagecolor{cream}\color{ink}\pagestyle{empty}
\newcommand{\keycap}[4]{\definecolor{capfill}{HTML}{#2}\begin{tikzpicture}[baseline=-0.5ex]
\ifthenelse{\equal{#1}{big}}{\def\cw{0.78}\def\ch{0.78}\def\fs{\fontsize{22}{22}\selectfont}}{\def\cw{0.78}\def\ch{0.5}\def\fs{\fontsize{15}{15}\selectfont}}
\fill[capfill!25!cream,draw=capfill!70!ink,line width=1pt,rounded corners=3pt] (-\cw/2,-\ch/2) rectangle (\cw/2,\ch/2);
\node[font=\ttfamily\bfseries\fs,text=#3] at (0,0) {#4};\end{tikzpicture}}
\newcommand{\heldcap}[2]{\begin{tikzpicture}[baseline=-0.5ex]
\ifthenelse{\equal{#1}{big}}{\def\cw{0.78}\def\ch{0.78}\def\fs{\fontsize{22}{22}\selectfont}}{\def\cw{0.78}\def\ch{0.5}\def\fs{\fontsize{15}{15}\selectfont}}
\draw[dim,dashed,line width=0.8pt,rounded corners=3pt] (-\cw/2,-\ch/2) rectangle (\cw/2,\ch/2);
\node[font=\ttfamily\fs,text=dim] at (0,0) {#2};\end{tikzpicture}}
\newcommand{\handpic}[1]{\begin{tikzpicture}[scale=0.13,xscale=#1,baseline=0]
\fill[skin,draw=ink,line width=0.6pt,rounded corners=1.2pt] (-2.2,-3) rectangle (2.2,0.6);
\foreach \x/\h in {-1.65/2.6,-0.55/3.2,0.55/3.0,1.65/2.3}{\fill[skin,draw=ink,line width=0.6pt,rounded corners=1.4pt] (\x-0.5,0.2) rectangle (\x+0.5,\h);}
\fill[skin,draw=ink,line width=0.6pt,rounded corners=1.4pt,rotate around={-35:(2.2,-1.6)}] (1.8,-2.2) rectangle (3.0,0.3);
\end{tikzpicture}}
\newcommand{\rhand}{\handpic{1}}
\newcommand{\lhand}{\handpic{-1}}
\begin{document}
\begin{center}{\fontsize{36}{40}\selectfont\bfseries QWERTY Waltzes}\\[30pt]
\includegraphics[width=0.8\textwidth]{keys-strip.png}\\[40pt]
{\LARGE \rhand\quad right hand row}\\[14pt]
{\LARGE \lhand\quad left hand row}\\[30pt]
{\LARGE one cap per beat}\\[14pt]
{\LARGE count \textbf{1} 2 3 \quad \textbf{1} 2 3}\\[30pt]
{\LARGE \keycap{big}{3278FF}{ink}{g}\quad press}\\[14pt]
{\LARGE \heldcap{big}{g}\quad still holding}\\[14pt]
{\LARGE \begin{tikzpicture}[baseline=-0.5ex]\node at (0,0.32) {\keycap{small}{FFE600}{ink}{e}};\node at (0,-0.32) {\keycap{small}{3278FF}{ink}{g}};\end{tikzpicture}\quad press both}
\end{center}
\clearpage
@@PAGES@@
\end{document}"""
    return tpl.replace("@@PAGES@@", pages)

if __name__ == "__main__":
    here = os.path.dirname(os.path.abspath(__file__))
    open(os.path.join(here, "cards-hands.tex"), "w").write(doc()); print("wrote cards-hands.tex")
