;=======================================================================
; General settings
;=======================================================================
;; location of external packages
(add-to-list 'load-path "~/.emacs.d/site-lisp")
(progn (cd "~/.emacs.d/site-lisp")
       (normal-top-level-add-subdirs-to-load-path)
)

;; frame and fonts
;(fixed-width-set-fontset "msgothic" 14)
(if (string-equal system-name "TOSHIBA")
    (progn 
      (setq initial-frame-alist
	    (append (list
;                  '(foreground-color . "white")
;                  '(background-color . "#333366")
;                  '(border-color . "black")
;                  '(mouse-color . "white")
;                  '(cursor-color . "white")
                  '(width . 100)
                  '(height . 30)
                  '(top . 0)
;                  '(left . 340)
		  )
		    initial-frame-alist
		    )
	    )
      ;; display time on mode line
      (setq display-time-string-forms
	    '(day "/" month "/" year " " dayname " " 24-hours ":" minutes " "))
      (display-time)
    )
)

(setq default-frame-alist initial-frame-alist)

;; emacs server
(server-start)

;; coloring characters
;(global-font-lock-mode t)

;; displaying line number
(setq line-number-mode t)

;; displaying the scroll bar on the right
;(set-scroll-bar-mode 'right)

;; set window title
;(setq frame-title-format
;        (concat "%b - emacs@" system-name))

;; enable paren mode
(show-paren-mode t)

;; disable splash screen
(setq inhibit-splash-screen t)

;; disable the warning bell
;; (setq visible-bell t)
(setq ring-bell-function 'ignore)

;; line wrapping at word boundary avialable only emacs 23 or later
(if (>= emacs-major-version 23)
    (global-visual-line-mode t))

;=======================================================================
; machine dependent variables
;=======================================================================
;; path to .emacs file, macro.tex
(if (string-equal system-name "CSLP009526") ;Dell laptop
    (progn
        (setq dotemacsfile "~/MyPrograms/dot_emacs/trunk/my.el")
	(setq macrotexfile "~/Files/TeX/marco.tex")
    )
    (if (string-equal system-name "Hirokis-MacBook-Air.local")
	(progn
	    (setq dotemacsfile "~/.emacs.d/.emacs-git/my.el")
	    (setq macrotexfile "~/Library/texmf/tex/latex/macro/macro.tex")
	)
        (progn
            (setq dotemacsfile "~/.emacs.d/my.el")
	    (setq macrotexfile "~/Files/TeX/macro.tex")
	)
    )
)

;===================================
; switch buffer
;===================================
(require 'swbuff-y)
(swbuff-y-mode t)

;===================================
; kill ring
;===================================
(require 'browse-kill-ring)
(define-key ctl-x-map (kbd "C-y") 'browse-kill-ring)

;=======================================================================
; git
;=======================================================================

;(require 'git-mswin)
;(require 'git)
;(require 'git-emacs)

;=======================================================================
; auto-install
;=======================================================================

(require 'auto-install)
(setq auto-install-directory "~/.emacs.d/site-lisp/")

;=======================================================================
; anything
;=======================================================================

;(require 'anything-config)
;(add-to-list 'anything-sources 'anything-c-source-emacs-commands)
;; (define-key ctl-x-map (kbd "C-a") 'anything) ;Ctrl-x Ctrl-a for anything command
;; (setq anything-sources
;;       '(anything-c-source-buffers+
;; 	anything-c-source-recentf
;; 	anything-c-source-emacs-variables
;; 	anything-c-source-emacs-commands
;; 	anything-c-source-emacs-functions
;; 	anything-c-source-files-in-current-dir
;; 	anything-c-source-kill-ring
;; 	))

;=======================================================================
; yasnippet
;=======================================================================

(require 'yasnippet-bundle)
(yas/define-snippets 'python-mode '(("env" "#!/usr/bin/env python
# -*- coding: iso-8859-15 -*-" "#!/usr...")))

(yas/define-snippets 'html-mode '(("doctype" "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01 Transitional//EN\" \"http://www.w3.org/TR/html4/loose.dtd\">" "<!DOCTYPE ...")))

(yas/define-snippets 'yatex-mode `(
("preamble latexmk" "%#!latexmk
\\documentclass[a4paper,11pt]{article}
\\usepackage[dvipdfm]{graphicx}
\\usepackage{amsmath, amssymb}
\\usepackage{import}
\\usepackage{simplemargins}
\\usepackage{titling}
\\setallmargins{1in}
\\setlength{\droptitle}{-1in}
" "%#!latexmk \\documentclass...")

("preamble pdflatex" "%#!pdflatex
\\documentclass[a4paper,11pt]{article}
\\usepackage{graphicx}
\\usepackage{amsmath, amssymb}
\\usepackage{import}
\\usepackage{simplemargins}
\\usepackage{titling}
\\setallmargins{1in}
\\setlength{\droptitle}{-1in}
" "%#!pdflatex \\documentclass...")

("preamble tikz standalone" "%#!pdflatex
\\documentclass[class=minimal,border=0pt]{standalone}
\\usepackage{graphicx}
\\usepackage{amsmath, amssymb}
\\pagestyle{empty}
\\usepackage{tikz}
\\usetikzlibrary{arrows,decorations.pathmorphing}
" "\\documentclass[class=minimal,border=0pt]{standalone}")

("beamer two-column" "\\begin{columns}
\\column{0.5\\textwidth}
\\column{0.5\\textwidth}
\\end{columns}
" "\\begin{columns}...")

("title" "\\title{}
\\author{}
\\date{}
" "\\title...")

;; Using backquote to selectively evaluate the following function while quoting others
("import" ,(format "\\import{%s}{%s}" (file-name-directory (expand-file-name macrotexfile)) (file-name-nondirectory (expand-file-name macrotexfile))) "\\import{...")

("figure" "\\begin{figure}[htbp]
 \\begin{center}
  \\includegraphics[width=\\linewidth]{}
  \\caption{}
 \\end{center}
\\end{figure}
" "\\begin{figure}...")

("wrapfigure" "\\begin{wrapfigure}{r}{0.5\\textwidth}
 \\begin{center}
  \\includegraphics[width=\\linewidth]{}
  \\caption{}
 \\end{center}
\\end{wrapfigure}
" "\\begin{wrapfig}...")
)
)

;=======================================================================
; YaTeX
;=======================================================================
(autoload 'yatex-mode "yatex" "Yet Another LaTeX mode" t)
(setq auto-mode-alist (cons '("\\.tex$" . yatex-mode) auto-mode-alist))
(add-to-list 'load-path "~/.emacs.d/site-lisp/yatex1.76")
;(setq YaTeX-kanji-code 1)

;; latex command
(if (string-equal system-name "TOSHIBA")
    (setq tex-command "platex")
  (setq tex-command "latex") ;else part
 )

(setq dvi2-command "C:/dviout/dviout.exe") ; previewer command
;(setq bibtex-command "jbibtex") ;bibtex command
(setq dviprint-command-format "dvipdfm %s ") ;print to pdf file

(setq YaTeX-use-AMS-LaTeX t)
(add-hook 'yatex-mode-load-hook
	  (function
	  (lambda ()
	   (YaTeX-define-begend-key "ba" "align")
	   (YaTeX-define-begend-key "bf" "figure")
	   (YaTeX-define-begend-key "bF" "frame")
	   )
	  ))

;; Enable local-variable
(add-hook 'yatex-mode-hook
          '(lambda ()
               (setq enable-local-variables t)
	       ))

;; Disable auto-fill
(add-hook 'yatex-mode-hook
 '(lambda () (auto-fill-mode -1))
)

;; comment, uncomment commands
;(add-hook 'yatex-mode-hook
;	  '(lambda ()
;	     (local-set-key "\C-c\C-c" 'comment-region)
;	     (local-set-key "\C-c\C-u" 'uncomment-region) ))

;=======================================================================
; RefTeX
;=======================================================================
;; (add-hook 'yatex-mode-hook 'turn-on-reftex)
(add-hook 'yatex-mode-hook '(lambda ()
			      (reftex-mode t)
			      (define-key reftex-mode-map
				(concat YaTeX-prefix ">") 'YaTeX-comment-region)
			      (define-key reftex-mode-map
				(concat YaTeX-prefix "<") 'YaTeX-uncomment-region))) 

;; (setq auto-mode-alist (cons '("\\.tex$" . reftex-mode) auto-mode-alist))
;; (setq TeX-parse-self t) ; Enable auto parsing of file
;; (setq-default TeX-master nil) ; Set master-file-name manually

;; (setq reftex-label-alist
;;       '(
;; ;	(nil ?e nil "~\\refeq{%s}" nil nil)
;; 	(nil ?f nil "~\\reffig{%s}" nil nil)
;; 	(nil ?s nil "~\\refsec{%s}" nil nil)
;; 	(nil ?t nil "~\\reftab{%s}" nil nil))) ;Change the reference commands for equation, figure, and section.

(setq reftex-insert-label-flags '("s" "sfte")) ;In a section enviroment, the label is derived automatically. In other enviroments, it prompts a user.

;=======================================================================
; Emacs lisp mode
;=======================================================================
(defun elisp-comment-block ()
  (interactive)
  (save-excursion
   (insert ";=======================================================================\n")
   (insert "; \n")
   (insert ";=======================================================================\n")
   )
  (next-line)
  (forward-char 2)
  )

(add-hook 'emacs-lisp-mode-hook
	  '(lambda ()
	     (define-key emacs-lisp-mode-map (kbd "C-c C-c") 'elisp-comment-block)
	     )
	  )

;=======================================================================
; Printing
;=======================================================================

(setq print-region-function
      (lambda (start end
                     &optional lpr-prog
                     delete-text buf display
                     &rest rest)
        (let* ((procname (make-temp-name "w32-print-"))
               (tempfile
                (subst-char-in-string
                 ?/ ?\\
                 (expand-file-name procname temporary-file-directory)))
               (coding-system-for-write 'sjis-dos))
          (write-region start end tempfile)
          (set-process-sentinel
           (start-process procname nil "notepad.exe" tempfile)
           (lambda (process event)
             (let ((tempfile
                    (expand-file-name (process-name process)
                                      temporary-file-directory)))
               (when (file-exists-p tempfile)
                 (delete-file tempfile))))
	   )
	  )
	)
      )

;=======================================================================
; my own functions
;=======================================================================

;; Open ~/.emacs
(defun open-dot-emacs ()
  "open ~/.emacs file."
  (interactive)
  (find-file dotemacsfile)
)

;; Open macro.tex
(defun open-macro-tex ()
  "open macro.tex file."
  (interactive)
  (find-file macrotexfile)
)

;=======================================================================
; mic-paren
;=======================================================================
(if window-system
    (progn
      (require 'mic-paren)
      (paren-activate)     ; activating
      (setq paren-match-face 'bold)
      (setq paren-sexp-mode t)
      )
  )

(progn
  (defvar com-point nil
    "Remember com point as a marker. \(buffer specific\)")
  (set-default 'com-point (make-marker))
  (defun getcom (arg)
    "Get com part of prefix-argument ARG."
    (cond ((null arg) nil)
          ((consp arg) (cdr arg))
          (t nil)))
  (defun paren-match (arg)
    "Go to the matching parenthesis."
    (interactive "P")
    (let ((com (getcom arg)))
      (if (numberp arg)
          (if (or (> arg 99) (< arg 1))
              (error "Prefix must be between 1 and 99.")
            (goto-char
             (if (> (point-max) 80000)
                 (* (/ (point-max) 100) arg)
               (/ (* (point-max) arg) 100)))
            (back-to-indentation))
        (cond ((looking-at "[\(\[{]")
               (if com (move-marker com-point (point)))
               (forward-sexp 1)
               (if com
                   (paren-match nil com)
                 (backward-char)))
              ((looking-at "[])}]")
               (forward-char)
               (if com (move-marker com-point (point)))
               (backward-sexp 1)
               (if com (paren-match nil com)))
              (t (error "not found"))
	      )
	)
      )
    )
  (define-key ctl-x-map "%" 'paren-match)
  )

;=======================================================================
; word-count
;=======================================================================
(autoload 'word-count-mode "word-count" "Minor mode to count words." t nil)
(global-set-key "\M-+" 'word-count-mode)

;=======================================================================
; aspell
;=======================================================================
(setq-default ispell-program-name "aspell") ;to execute, type M-x ispell

;=======================================================================
; cua-mode
;=======================================================================
(cua-mode t)
(setq cua-enable-cua-keys nil) ;disable accompanying key strokes
;(require 'sense-region)
;(sense-region-on)

;=======================================================================
; whitespace
;=======================================================================
(require 'whitespace)

;=======================================================================
; Global key-bindings
;=======================================================================

;; undo
(global-set-key (kbd "C-\\") 'undo)
;; (global-set-key (kbd "<kanji>") 'toggle-input-method)
