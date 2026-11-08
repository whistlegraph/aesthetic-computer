"""Hand-notation score cards: two keycap rows per system, one per hand.

    python3 make-hands.py                  -> cards-hands.tex (all waltzes)
    python3 make-hands.py home-row-waltz   -> cards-hands-home-row-waltz.tex

A held key is one long cap stretched over its beats. Thin lines split the
beats, a heavier line ends the bar. Under each beat sits its group: the
right-hand letter, then the left-hand letters, so a bar reads as a line,
`kd jfa kfa`.
"""
import sys, importlib.util, os
spec = importlib.util.spec_from_file_location("w", os.path.join(os.path.dirname(os.path.abspath(__file__)), "make.py"))
w = importlib.util.module_from_spec(spec); spec.loader.exec_module(w)
WALTZES, CHORDS, L2M = w.WALTZES, w.CHORDS, w.L2M
COLOR = {0:"FF3232",2:"FFA000",4:"FFE600",5:"32C832",7:"3278FF",9:"8232C8",11:"B450FF"}
def capcolor(letter):
    pc = L2M[letter] % 12
    return COLOR.get(pc, "333333"), ("white" if pc not in COLOR else "ink")

def cap(letter, small=False):
    fill, txt = capcolor(letter)
    return r"\keycap{%s}{%s}{%s}{%s}" % ("small" if small else "big", fill, txt, letter)

def longcap(letter, beats):
    """One cap across `beats` cells (1cm each), letter centred in the first cell."""
    fill, txt = capcolor(letter)
    return r"\longcap{%.2f}{%.2f}{%s}{%s}{%s}" % (beats - 0.22, 0.5 - beats / 2, fill, txt, letter)

def pills(toks):
    """'k - -' -> [(k, start, beats)]; a dash extends the previous key."""
    out = []
    for i, t in enumerate(toks):
        if t == "-" and out: out[-1][2] += 1
        else: out.append([t, i, 1])
    return out

def system(bars):
    """4 bars -> TikZ picture with R row and L row of caps, beat lines and the linear line."""
    out = [r"\begin{tikzpicture}[x=1cm,y=1cm]"]
    x0 = 1.3
    bw = 3.2   # bar width (3 beats)
    top, bot = 1.6, -2.25
    out.append(r"\node at (0.45,1.05) {\rhand};")
    out.append(r"\node at (0.45,-0.35) {\lhand};")
    # Lines first, caps on top: a long cap covers the beat line it crosses.
    out.append(r"\draw[ink!55!cream,line width=0.9pt] (%.2f,%.2f) -- (%.2f,%.2f);" % (x0, bot, x0, top))
    for bi in range(len(bars)):
        bx = x0 + bi * bw
        for b in (1, 2):
            out.append(r"\draw[dim!40!cream,line width=0.4pt] (%.2f,%.2f) -- (%.2f,%.2f);" % (bx + b, bot, bx + b, top))
        out.append(r"\draw[ink!55!cream,line width=0.9pt] (%.2f,%.2f) -- (%.2f,%.2f);" % (bx + bw, bot, bx + bw, top))
    for bi, (chord, pat) in enumerate(bars):
        bx = x0 + bi * bw
        toks = pat.split()
        bass, (u, l) = CHORDS[chord]
        for b in range(3):
            out.append(r"\node[font=\large\sffamily\bfseries\color{%s}] at (%.2f,-1.35) {%d};" % ("ink" if b == 0 else "dim", bx + 0.5 + b, b + 1))
            rh = toks[b] if toks[b] != "-" else ""
            lh = bass if b == 0 else u + l
            out.append(r"\node at (%.2f,-1.92) {\grp{%s}{%s}};" % (bx + 0.5 + b, rh.replace(";", r"\,;"), lh))
        for t, start, n in pills(toks):
            if n == 1:
                out.append(r"\node at (%.2f,1.05) {%s};" % (bx + 0.5 + start, cap(t)))
            else:
                out.append(r"\node at (%.2f,1.05) {%s};" % (bx + start + n / 2, longcap(t, n)))
        out.append(r"\node at (%.2f,-0.35) {%s};" % (bx + 0.5, cap(bass)))
        for b in (1, 2):
            cx = bx + 0.5 + b
            out.append(r"\node at (%.2f,-0.05) {%s};" % (cx, cap(u, small=True)))
            out.append(r"\node at (%.2f,-0.68) {%s};" % (cx, cap(l, small=True)))
    out.append(r"\end{tikzpicture}")
    return "\n".join(out)

def page(wz):
    systems = "\n\n\\vspace{10pt}\n\n".join("\\noindent\\resizebox{\\textwidth}{!}{" + system(wz["bars"][i:i+4]) + "}" for i in range(0, 16, 4))
    return r"""
\begin{center}
{\fontsize{32}{36}\selectfont\bfseries %s}\\[6pt]
{\large %d bpm \quad count 1 2 3}
\end{center}
\vspace{6pt}
%s
\clearpage""" % (wz["title"], wz["bpm"], systems)

def doc(waltzes):
    pages = "".join(page(wz) for wz in waltzes)
    tpl = r"""\documentclass[11pt,letterpaper]{article}
\usepackage[top=0.6in,bottom=0.5in,left=0.55in,right=0.55in]{geometry}
\usepackage{fontspec}\usepackage{graphicx}\graphicspath{{../../../../papers/notepat-charts/}}
\usepackage{xcolor}\usepackage{amsmath}\usepackage{tikz}\usepackage{ifthen}
\setmainfont[Extension=.otf,UprightFont=lmroman10-regular,BoldFont=lmroman10-bold,ItalicFont=lmroman10-italic]{lmroman10}
\setsansfont[Extension=.otf,UprightFont=lmsans10-regular,BoldFont=lmsans10-bold]{lmsans10}
\setmonofont[Extension=.otf,UprightFont=lmmono10-regular,BoldFont=lmmonolt10-bold]{lmmono10}
\definecolor{cream}{HTML}{F6F0E2}\definecolor{ink}{HTML}{1E1A14}\definecolor{red}{HTML}{C03A2B}\definecolor{dim}{HTML}{9A8F7A}\definecolor{skin}{HTML}{E8C9A6}\definecolor{blue}{HTML}{2457A8}
\pagecolor{cream}\color{ink}\pagestyle{empty}
\newcommand{\keycap}[4]{\definecolor{capfill}{HTML}{#2}\begin{tikzpicture}[baseline=-0.5ex]
\ifthenelse{\equal{#1}{big}}{\def\cw{0.78}\def\ch{0.78}\def\fs{\fontsize{22}{22}\selectfont}}{\def\cw{0.78}\def\ch{0.5}\def\fs{\fontsize{15}{15}\selectfont}}
\fill[capfill!25!cream,draw=capfill!70!ink,line width=1pt,rounded corners=3pt] (-\cw/2,-\ch/2) rectangle (\cw/2,\ch/2);
\node[font=\ttfamily\bfseries\fs,text=#3] at (0,0) {#4};\end{tikzpicture}}
% \longcap{width}{letter dx}{fill}{text color}{letter}: one cap held across beats, a tail through the rest.
\newcommand{\longcap}[5]{\definecolor{capfill}{HTML}{#3}\begin{tikzpicture}[baseline=-0.5ex]
\fill[capfill!25!cream,draw=capfill!70!ink,line width=1pt,rounded corners=3pt] (-#1/2,-0.39) rectangle (#1/2,0.39);
\node[font=\ttfamily\bfseries\fontsize{22}{22}\selectfont,text=#4] at (#2,0) {#5};
\draw[capfill!70!ink,line width=1.6pt,line cap=round] (#2+0.42,0) -- (#1/2-0.18,0);\end{tikzpicture}}
% \grp{right}{left}: one beat as text, right-hand letter then left-hand letters.
\newcommand{\grp}[2]{{\ttfamily\bfseries\Large #1}{\ttfamily\Large\color{blue}#2}}
\newcommand{\handpic}[1]{\begin{tikzpicture}[scale=0.13,xscale=#1,baseline=0]
\fill[skin,draw=ink,line width=0.6pt,rounded corners=1.2pt] (-2.2,-3) rectangle (2.2,0.6);
\foreach \x/\h in {-1.65/2.6,-0.55/3.2,0.55/3.0,1.65/2.3}{\fill[skin,draw=ink,line width=0.6pt,rounded corners=1.4pt] (\x-0.5,0.2) rectangle (\x+0.5,\h);}
\fill[skin,draw=ink,line width=0.6pt,rounded corners=1.4pt,rotate around={-35:(2.2,-1.6)}] (1.8,-2.2) rectangle (3.0,0.3);
\end{tikzpicture}}
\newcommand{\rhand}{\handpic{1}}
\newcommand{\lhand}{\handpic{-1}}
\begin{document}
\begin{center}{\fontsize{36}{40}\selectfont\bfseries QWERTY Waltzes}\\[24pt]
\includegraphics[width=0.8\textwidth]{keys-strip.png}\\[30pt]
{\LARGE \rhand\quad right hand row}\\[12pt]
{\LARGE \lhand\quad left hand row}\\[24pt]
{\LARGE one cap per beat \quad count \textbf{1} 2 3 \quad \textbf{1} 2 3}\\[12pt]
{\LARGE thin lines split the beats, a heavy line ends the bar}\\[24pt]
{\LARGE \keycap{big}{3278FF}{ink}{g}\quad press}\\[12pt]
{\LARGE \longcap{1.78}{-0.5}{3278FF}{ink}{g}\quad hold for two beats}\\[12pt]
{\LARGE \begin{tikzpicture}[baseline=-0.5ex]\node at (0,0.32) {\keycap{small}{FFE600}{ink}{e}};\node at (0,-0.32) {\keycap{small}{3278FF}{ink}{g}};\end{tikzpicture}\quad press both}\\[24pt]
{\LARGE \grp{k}{d}\ \ \grp{j}{fa}\ \ \grp{k}{fa}\quad the same bar as a line}\\[8pt]
{\large under each beat: right letter, then left letters. a beat with no right letter keeps holding.}
\end{center}
\clearpage
@@PAGES@@
\end{document}"""
    return tpl.replace("@@PAGES@@", pages)

if __name__ == "__main__":
    here = os.path.dirname(os.path.abspath(__file__))
    slugs = sys.argv[1:]
    chosen = [wz for wz in WALTZES if not slugs or wz["slug"] in slugs]
    if not chosen: sys.exit("no waltz matches " + " ".join(slugs))
    name = "cards-hands" + ("-" + "-".join(slugs) if slugs else "")
    open(os.path.join(here, name + ".tex"), "w").write(doc(chosen)); print("wrote", name + ".tex")
