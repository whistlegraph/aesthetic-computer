;;; kidlisp-cards.el --- KidLisp reference card editing workflow -*- lexical-binding: t; -*-

;;; Commentary:
;; Edit KidLisp reference cards in org-mode, compile to SVG via LaTeX.
;; Live preview in VS Code Simple Browser via CDP.

;;; Code:

(defvar kidlisp-cards-dir "/workspaces/aesthetic-computer/system/public/kidlisp.com/cards"
  "Base directory for KidLisp cards.")

(defvar kidlisp-preview-url "https://localhost:8888/kidlisp.com"
  "URL for KidLisp.com preview.")

(defun kidlisp-org-to-latex (org-file)
  "Convert ORG-FILE to LaTeX card format."
  (with-temp-buffer
    (insert-file-contents org-file)
    (goto-char (point-min))
    ;; Extract metadata
    (let ((title (if (re-search-forward "^#\\+TITLE: \\(.+\\)$" nil t)
                     (match-string 1) "Untitled"))
          (category (progn (goto-char (point-min))
                           (if (re-search-forward "^#\\+CATEGORY: \\(.+\\)$" nil t)
                               (match-string 1) "General")))
          (description "")
          (code ""))
      ;; Extract description (first paragraph after title)
      (goto-char (point-min))
      (when (re-search-forward "^\\* .+\n\n\\([^#*]+\\)" nil t)
        (setq description (string-trim (match-string 1))))
      ;; Extract code block
      (goto-char (point-min))
      (when (re-search-forward "#\\+begin_src.*\n\\(\\(?:.\\|\n\\)*?\\)#\\+end_src" nil t)
        (setq code (string-trim (match-string 1))))
      ;; Generate LaTeX
      (kidlisp-generate-latex title category description code))))

(defun kidlisp-generate-latex (title category description code)
  "Generate LaTeX card source from TITLE, CATEGORY, DESCRIPTION, CODE."
  (format "\\documentclass[tikz,border=0pt]{standalone}
\\usepackage[utf8]{inputenc}
\\usepackage{xcolor}
\\usepackage{listings}

\\newcommand{\\cardwidth}{250pt}
\\newcommand{\\cardheight}{350pt}

\\definecolor{cardbg}{HTML}{FFF9C0}
\\definecolor{codebg}{HTML}{1F1B16}
\\definecolor{codetext}{HTML}{F4E9D7}
\\definecolor{titlecolor}{HTML}{2D2A24}
\\definecolor{bodycolor}{HTML}{4A4540}
\\definecolor{categorycolor}{HTML}{666666}

\\lstdefinestyle{kidlisp}{
  basicstyle=\\ttfamily\\fontsize{9}{11}\\selectfont\\color{codetext},
  backgroundcolor=\\color{codebg},
  frame=none,
  breaklines=true,
  showstringspaces=false,
  xleftmargin=8pt,
  xrightmargin=8pt,
}

\\begin{document}
\\begin{tikzpicture}
  \\fill[cardbg, rounded corners=12pt] (0,0) rectangle (\\cardwidth, \\cardheight);
  
  \\node[anchor=north, font=\\sffamily\\fontsize{11}{13}\\selectfont\\color{categorycolor}] 
    at (0.5*\\cardwidth, \\cardheight-12pt) {%s};
  
  \\node[anchor=north, font=\\sffamily\\bfseries\\fontsize{18}{22}\\selectfont\\color{titlecolor}] 
    at (0.5*\\cardwidth, \\cardheight-32pt) {%s};
  
  \\draw[categorycolor, line width=0.5pt] (15pt, \\cardheight-55pt) -- (\\cardwidth-15pt, \\cardheight-55pt);
  
  \\node[anchor=north west, text width=\\cardwidth-30pt, font=\\sffamily\\fontsize{10}{14}\\selectfont\\color{bodycolor}] 
    at (15pt, \\cardheight-65pt) {%s};
  
  \\fill[codebg, rounded corners=8pt] (12pt, 12pt) rectangle (\\cardwidth-12pt, 130pt);
  
  \\node[anchor=north west, font=\\sffamily\\fontsize{8}{10}\\selectfont\\color{codetext}] 
    at (18pt, 124pt) {Example};
  
  \\draw[codetext, opacity=0.2, line width=0.5pt] (18pt, 112pt) -- (\\cardwidth-18pt, 112pt);
  
  \\node[anchor=north west, text width=\\cardwidth-40pt] at (18pt, 105pt) {
    \\begin{lstlisting}[style=kidlisp]
%s
    \\end{lstlisting}
  };
\\end{tikzpicture}
\\end{document}
" category title description code))

(defun kidlisp-build-card ()
  "Build current org file as a KidLisp card SVG."
  (interactive)
  (unless (and buffer-file-name (string-match "\\.org$" buffer-file-name))
    (error "Not an org file"))
  (save-buffer)
  (let* ((org-file buffer-file-name)
         (base-name (file-name-base org-file))
         (tex-file (expand-file-name (concat "src/" base-name ".tex") kidlisp-cards-dir))
         (svg-file (expand-file-name (concat "svg/" base-name ".svg") kidlisp-cards-dir))
         (latex-content (kidlisp-org-to-latex org-file)))
    ;; Write LaTeX
    (with-temp-file tex-file
      (insert latex-content))
    (message "📄 Wrote %s" tex-file)
    ;; Build SVG
    (let ((default-directory kidlisp-cards-dir))
      (if (= 0 (call-process "bash" nil nil nil "build.sh" base-name))
          (progn
            (message "✅ Built %s" svg-file)
            (kidlisp-refresh-preview))
        (error "❌ Build failed for %s" base-name)))))

(defun kidlisp-refresh-preview ()
  "Refresh the KidLisp.com preview in VS Code via CDP."
  (interactive)
  ;; Send CDP command to refresh Simple Browser
  ;; This requires the CDP tunnel to be active
  (let ((cdp-script (format "
const WebSocket = require('ws');
const ws = new WebSocket('ws://localhost:9333');
ws.on('open', () => {
  ws.send(JSON.stringify({
    id: 1,
    method: 'Page.reload',
    params: { ignoreCache: true }
  }));
  setTimeout(() => ws.close(), 100);
});
")))
    (start-process "cdp-refresh" nil "node" "-e" cdp-script)))

(defun kidlisp-preview-svg ()
  "Open the current card's SVG in a viewer."
  (interactive)
  (let* ((base-name (file-name-base buffer-file-name))
         (svg-file (expand-file-name (concat "svg/" base-name ".svg") kidlisp-cards-dir)))
    (if (file-exists-p svg-file)
        (browse-url (concat "file://" svg-file))
      (message "SVG not found. Run kidlisp-build-card first."))))

(defun kidlisp-new-card (category name)
  "Create a new card org file for CATEGORY and NAME."
  (interactive "sCategory: \nsCard name: ")
  (let* ((filename (concat (downcase category) "-" (downcase (replace-regexp-in-string " " "-" name)) ".org"))
         (filepath (expand-file-name (concat "org/" filename) kidlisp-cards-dir)))
    (find-file filepath)
    (insert (format "#+TITLE: %s
#+CATEGORY: %s
#+STARTUP: showall

* %s

Description of %s goes here.

#+begin_src lisp
(%s)
#+end_src
" name category name name (downcase name)))
    (goto-char (point-min))
    (search-forward "Description")))

;; Keybindings for org-mode
(with-eval-after-load 'org
  (define-key org-mode-map (kbd "C-c C-b") #'kidlisp-build-card)
  (define-key org-mode-map (kbd "C-c C-p") #'kidlisp-preview-svg))

(provide 'kidlisp-cards)
;;; kidlisp-cards.el ends here
