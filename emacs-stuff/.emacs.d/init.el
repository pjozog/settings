;; Recite the holy words
(defun confess-faith ()
  (interactive)
  (message "There is no system but GNU and Linux is one of its kernels"))

(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(LaTeX-command "latex -shell-escape")
 '(inhibit-default-init t)
 '(inhibit-startup-screen t)
 '(jabber-account-list (quote (("pjozog@gmail.com" (:network-server . "talk.google.com") (:connection-type . ssl)))))
 '(org-hide-leading-stars nil)
 '(uniquify-buffer-name-style (quote forward) nil (uniquify))
 '(vc-follow-symlinks nil)
 '(visible-bell t))
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "#000000" :foreground "#ffffff" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 100 :width normal :foundry "unknown" :family "DejaVu Sans Mono"))))
 '(cursor ((t (:background "red"))))
 '(font-latex-string-face ((((class color) (background dark)) (:foreground "red"))))
 '(font-lock-string-face ((nil (:foreground "red")))))

(setq my-emacs-version (nth 2 (split-string (emacs-version))))

;; --------------------------------------------------
;; Function definitions
;; --------------------------------------------------
(defun word-count nil "Count words in buffer" (interactive)
  (shell-command-on-region (point-min) (point-max) "wc -w"))

(defun char-count nil "Count words in buffer" (interactive)
  (shell-command-on-region (point-min) (point-max) "wc -m"))

(defun paren-latex ()
  (interactive)
  (replace-regexp "(" "\\\\left(")
  (exchange-point-and-mark)
  (replace-regexp ")" "\\\\right)"))

(defun goto-tag ()
  (interactive)
  (let ((start (point)))
    (search-forward-regexp " \\|$\\|(\\|,\\|\\.")
    (backward-char)
    (let ((end (point)))
      (kill-ring-save start end)
      (find-tag (car kill-ring)))))

(defun find-files-recursively (dir)
  (let (returnList fileList)
    (setq returnList ())
    (setq fileList (file-expand-wildcards (concat dir "/*")))
    (while fileList
      (let (currentFile)
	(setq currentFile (car fileList))
	(if (not (car (file-attributes currentFile)))
	    (setq returnList (cons currentFile returnList))
	  (if (string-equal (car (file-attributes currentFile)) "t")
	      (setq returnList (append (find-files-recursively currentFile) returnList)))))
      (pop fileList))
    returnList))

(defun open-filelist (fileList)
  (while fileList
    (let ((currentFile (car fileList)))
      (find-file (file-truename currentFile)))
    (setq fileList (cdr fileList))))
      
(defun file-basename (file)
  (car (reverse (split-string file "/"))))

(defun filter-from-filelist (list expr)
  (let ((returnList ()))
    (while list
      (let ((currentItem (car list)))
	(if (not (string-match expr currentItem))
	    (setq returnList (cons currentItem returnList)))
	(setq list (cdr list))))
    returnList))

(defun extract-from-filelist (list expr)
  (let ((returnList ()))
    (while list
      (let ((currentFile (car list)))
	(if (string-match expr currentFile)
	    (setq returnList (cons currentFile returnList))))
      (setq list (cdr list)))
    returnList))

(defun find-file-this ()
  (interactive)
   (find-file buffer-file-name))

(defun command-line-diff (switch)
  (let ((file1 (pop command-line-args-left))
	(file2 (pop command-line-args-left)))
    (ediff file1 file2)))
(add-to-list 'command-switch-alist '("-diff" . command-line-diff))

;; --------------------------------------------------
;; Packages / Minor modes / Keybindings
;; --------------------------------------------------
;; append my stuff to load-path
(add-to-list 'load-path
	     "~/.emacs.d/lisp")
(add-to-list 'load-path
	     "~/.emacs.d/plugins/yasnippet-0.6.1c")
(add-to-list 'load-path
	     "~/.emacs.d/plugins")
(add-to-list 'load-path
	     "~/.emacs.d/plugins/org")

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

(ido-mode 1)

(transient-mark-mode -1)

;; This makes color work in 'M-x shell'
(autoload 'ansi-color-for-comint-mode-on "ansi-color" nil t)

(setq mouse-yank-at-point t)

(show-paren-mode)


(require 'htmlize)
(require 'matlab)
(require 'cmake-mode)
(require 'lua-mode)
(require 'autopair)
(require 'unbound)
(require 'yasnippet)
(require 'color-theme)
(require 'linum)
(require 'flymake)
(require 'flymake-cursor)
(require 'org-install)
(require 'uniquify)

;;turn on autopair-mode
(autopair-global-mode 1)

(global-set-key [(meta return)] 'shell-resync-dirs)
(global-set-key [(control c) (f)] 'find-file-this)
;(global-set-key [(meta .)] 'goto-tag)
(global-set-key [(control x) (v) (=)] 'ediff-revision)
(global-set-key [(control c) (control g)] 'compile)
(global-set-key [(control o)] 'other-window)
(global-set-key [(control meta |)] 'fill-region)

(global-linum-mode)

(setq ediff-split-window-function 'split-window-horizontally)

(put 'upcase-region 'disabled nil)

;; Saumitro's color theme
(load "twilight")
(color-theme-twilight)

(if (file-exists-p "~/TAGS")
    (visit-tags-table "~/TAGS"))

(yas/initialize)
(yas/load-directory "~/.emacs.d/plugins/yasnippet-0.6.1c/snippets")

(put 'downcase-region 'disabled nil)

(load "auctex.el" nil t t)
(setq TeX-auto-save t)
(setq TeX-parse-self t)

(load "preview-latex.el" nil t t)

;; Set to the location of your Org files on your local system
(setq org-directory "~/Dropbox/MainOrg")
;; Set to the name of the file where new notes will be stored
(setq org-mobile-inbox-for-pull "~/org/flagged.org")
;; Set to <your Dropbox root directory>/MobileOrg.
(setq org-mobile-directory "~/Dropbox/MobileOrg")
(setq org-agenda-files '("~/Dropbox/MainOrg/test.org"))
(setq org-mobile-inbox-for-pull "~/Dropbox/MainOrg/inbox.org")

;; --------------------------------------------------
;; Language-specific settings
;; --------------------------------------------------
(setq auto-mode-alist (cons '("\\.m$" . matlab-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("CMakeLists\\.txt$" . cmake-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.lcm$" . java-mode) auto-mode-alist))
(setq matlab-fill-code nil)

;; LaTeX: Enable flymake for texlive distribution of LaTeX
(defun flymake-get-tex-args (file-name)
  (list "pdflatex" (list "-shell-escape" "-draftmode" "-file-line-error" "-interaction=nonstopmode" file-name)))

;; --------------------------------------------------
;; My hooks
;; --------------------------------------------------
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

(defun my-camel-case-hook ()
  (if (version< my-emacs-version "23.2.1")
      (c-subword-mode 1)
    (subword-mode 1)))

;; apply CamelCase hooks
(let (modeList)
  (setq modeList '(python-mode-hook 
		   java-mode-hook
		   c-mode-hook
		   c++-mode-hook
		   matlab-mode-hook))
  (while modeList
    (add-hook (car modeList) (lambda ()
			       (my-camel-case-hook)
			       (hs-minor-mode)
			       (global-set-key [(control tab)] 'hs-toggle-hiding)))
    (setq modeList (cdr modeList))))

;; apply LaTeX hooks (spellcheck, etc.)
(add-hook 'LaTeX-mode-hook (lambda ()
			     (flyspell-mode)
			     (outline-minor-mode)
			     (auto-fill-mode)
			     (flymake-mode)))

;; this makes control-tab function like org-mode
(add-hook 'outline-minor-mode-hook
	  (lambda ()
	    (define-key outline-minor-mode-map [(control tab)] 'org-cycle)))

;; elisp documentation in minibuffer
(add-hook 'emacs-lisp-mode-hook (lambda ()
				  (turn-on-eldoc-mode)))

;;--------------------------------------------------
;; Open my favorite files and start rocking!
;;--------------------------------------------------
(open-filelist '("~/.emacs.d/init.el" "~/.bashrc"
		 "~/.config/openbox/autostart.sh"
		 "~/.config/awesome/rc.lua"))
(switch-to-buffer "*scratch*")

(server-start)
;; diable warning when killing buffers opened with emacsclient 
;; (must be set after calling (server-start))
(remove-hook 'kill-buffer-query-functions 'server-kill-buffer-query-function)
