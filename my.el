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
               initial-frame-alist)
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
(setq visible-bell t)

;; line wrapping at word boundary
(global-visual-line-mode t)

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
(yas/define-snippets 'python-mode '(("env" "#!/usr/bin/env python" "#!/usr...")))
(yas/define-snippets 'html-mode '(("doctype" "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01 Transitional//EN\" \"http://www.w3.org/TR/html4/loose.dtd\">" "<!DOCTYPE ...")))

;=======================================================================
; YaTeX
;=======================================================================
(autoload 'yatex-mode "yatex" "Yet Another LaTeX mode" t)
(setq auto-mode-alist (cons '("\\.tex$" . yatex-mode) auto-mode-alist))
(add-to-list 'load-path "~/.emacs.d/site-lisp/yatex")
;(setq YaTeX-kanji-code 1)

(setq tex-command "latex") ; tex command
(setq dvi2-command "dviout") ; previewer command
;(setq bibtex-command "jbibtex") ;bibtex command
(setq dviprint-command-format "dvipdfmx %s ") ;print to pdf file

(setq YaTeX-use-AMS-LaTeX t)
(add-hook 'yatex-mode-load-hook
	  (function
	  (lambda ()
	   (YaTeX-define-begend-key "ba" "align")
	   (YaTeX-define-begend-key "bf" "figure")
	   )
	  ))

;; Enable local-variable
(add-hook 'yatex-mode-hook
          '(lambda ()
               (setq enable-local-variables t)
	       ))

;; comment, uncomment commands
(add-hook 'yatex-mode-hook
	  '(lambda ()
	     (local-set-key "\C-c\C-c" 'comment-region)
	     (local-set-key "\C-c\C-u" 'uncomment-region) ))

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
(setq dotemacsfile "~/.emacs.d/my.el")
(defun open-dot-emacs ()
  "open ~/.emacs file."
  (interactive)
  (find-file dotemacsfile)
)

;=======================================================================
; Global key-bindings
;=======================================================================

;; undo
(global-set-key (kbd "C-\\") 'undo)
