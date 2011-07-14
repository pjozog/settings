;; Recite the holy words
(defun confess-faith ()
  (interactive)
  (message "There is no system but GNU and Linux is one of its kernels"))

(setq my-emacs-version (nth 2 (split-string (emacs-version))))

;; --------------------------------------------------
;; Function definitions
;; --------------------------------------------------
(defun word-count nil "Count words in buffer" (interactive)
  (shell-command-on-region (point-min) (point-max) "wc -w"))

(defun char-count nil "Count chars in buffer" (interactive)
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
      (if (file-exists-p currentFile)
	(find-file (file-truename currentFile))))
    (setq fileList (cdr fileList))))
      
(defun file-basename (file)
  (let ((file-no-dash (replace-regexp-in-string "/$" "" file)))
    (car (reverse (split-string file-no-dash "/")))))

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

(defun f ()
  (interactive)
  (set-face-attribute 'default nil :height 120))

(defun insert-amps ()
  (interactive)
  (insert (format "& %s &" (read-from-minibuffer "Symbol? "))))

(defun dired-view-file-other-window ()
  (interactive)
  (let ((file (dired-get-file-for-visit)))
      (view-file-other-window file)))

(defun dired-find-file-other-window ()
  (interactive)
  (let ((file (dired-get-file-for-visit)))
      (find-file-other-window file)))

(defun dired-find-file-mod ()
  (interactive)
  (let ((file (dired-get-file-for-visit)))
    (if (car (file-attributes file))
	(dired-find-file)
      (dired-find-file-other-window))))

(defun dired-kill-and-next-subdir ()
  (interactive)
  ;; Had to look at the source code: dired-prev-subdir's first arg is
  ;; undocumented.  Just use 0 for this defun
  (dired-prev-subdir 0)
  (let* ((subdir-name (dired-get-subdir))
	 (search-term (concat " " (file-basename subdir-name))))
    (dired-kill-subdir)
    (dired-prev-subdir 0)
    (search-forward search-term)))

(defun run-bash ()
  (interactive)
  (term "/bin/bash"))

(defun dired-get-size ()
  (interactive)
  (let ((files (dired-get-marked-files)))
    (with-temp-buffer
      (apply 'call-process "/usr/bin/du" nil t nil "-sch" files)
      (message "Size of all marked files: %s"
               (progn 
                 (re-search-backward "\\(^[0-9.,]+[A-Za-z]+\\).*total$")
		 (match-string 1))))))

;; --------------------------------------------------
;; Packages / Minor modes / Keybindings
;; --------------------------------------------------
;; append my stuff to load-path
(add-to-list 'load-path
	     "~/.emacs.d/lisp")
(add-to-list 'load-path
	     "~/.emacs.d/plugins")
(add-to-list 'load-path
	     "~/.emacs.d/plugins/yasnippet-0.6.1c")
(add-to-list 'load-path
	     "~/.emacs.d/plugins/emacs-w3m")
(add-to-list 'load-path
	     "~/.emacs.d/plugins/org")

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
(require 'rainbow-mode)
(require 'smooth-scroll)
(require 'reftex)
(require 'w3m-load)
(require 'mediawiki)
(require 'highline)

;; Turn off the bad shit
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(transient-mark-mode -1)

;; Turn on the good shit
(ido-mode 1)
(show-paren-mode 1)
(setq mouse-yank-at-point 1)
(autopair-global-mode 1)
(global-linum-mode 1)

;; Make dired only search for filenames, not the entire buffer text
(setq dired-isearch-filenames 't)
(setq wdired-allow-to-change-permissions 't)
(setq dired-listing-switches "-alh")

;; This makes color work in 'M-x shell'
(autoload 'ansi-color-for-comint-mode-on "ansi-color" nil t)

;; My key bindings
(global-set-key [(meta return)] 'shell-resync-dirs)
(global-set-key [(control c) (f)] 'find-file-this)
;(global-set-key [(meta .)] 'goto-tag)
(global-set-key [(control x) (v) (=)] 'ediff-revision)
(global-set-key [(control c) (control g)] 'compile)
(global-set-key [(control o)] 'other-window)
(global-set-key [(control meta |)] 'fill-region)
(global-set-key [(meta h)] 'ff-find-other-file)
(global-unset-key (kbd "C-7"))
(global-set-key (kbd "C-7") 'insert-amps)
(global-set-key (kbd "C-c k") 'ido-kill-buffer)

;; Fix linum margin when running in terminal mode
(unless (window-system)
  (setq linum-format "%d "))

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

;; Activate occur easily inside isearch
(define-key isearch-mode-map (kbd "C-o")
  (lambda () (interactive)
    (let ((case-fold-search isearch-case-fold-search))
      (occur (if isearch-regexp isearch-string (regexp-quote isearch-string))))))

;;Enable smooth scrolling
;; (setq scroll-step           1
;;       scroll-conservatively 9999999)
;; (smooth-scroll-mode 1 )

(global-highline-mode 1)

;; --------------------------------------------------
;; org-mode settings
;; --------------------------------------------------
(load "myOrgSettings.el")

;; --------------------------------------------------
;; Language-specific settings
;; --------------------------------------------------
(setq auto-mode-alist (cons '("\\.m$" . matlab-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.txt$" . mediawiki-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("CMakeLists\\.txt$" . cmake-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.lcm$" . java-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("Doxyfile$" . conf-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.SRC$" . asm-mode) auto-mode-alist))
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
		   matlab-mode-hook
		   cmake-mode-hook))
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
			     (flymake-mode)
			     (turn-on-reftex)))

;; this makes control-tab function like org-mode
(add-hook 'outline-minor-mode-hook
	  (lambda ()
	    (define-key outline-minor-mode-map [(control tab)] 'org-cycle)))

;; elisp documentation in minibuffer
(add-hook 'emacs-lisp-mode-hook (lambda ()
				  (turn-on-eldoc-mode)
				  (rainbow-mode)))

;; go into wdired by pushing e...
(add-hook 'dired-mode-hook (lambda ()
			     (define-key dired-mode-map "e" 'wdired-change-to-wdired-mode)
			     (define-key dired-mode-map "/" 'dired-isearch-filenames)
			     (define-key dired-mode-map "v" 'dired-view-file-other-window)
			     (define-key dired-mode-map "f" 'dired-find-file-other-window)
			     (define-key dired-mode-map "F" 'find-name-dired)
			     (define-key dired-mode-map "c" 'run-bash)
			     (define-key dired-mode-map "k" 'dired-kill-and-next-subdir)
			     (define-key dired-mode-map (kbd "?") 'dired-get-size)
			     (define-key dired-mode-map (kbd "RET") 'dired-find-file-mod)
			     (define-key dired-mode-map (kbd "C-o") 'other-window)
			     (define-key dired-mode-map (kbd "M-p") 'dired-up-directory)))

;;--------------------------------------------------
;; Open my favorite files and start rocking!
;;--------------------------------------------------
(open-filelist '("~/.emacs.d/init.el" "~/.bashrc"
		 "~/.config/openbox/autostart.sh"
		 "~/.config/awesome/rc.lua"
		 "~/Dropbox/org/projects.org"
		 "~/perl/perl-svn/references/bibtex/references.bib"))
(dired (getenv "HOME"))
(switch-to-buffer "~")
(split-window-horizontally)
(switch-to-buffer "init.el")

(server-start)
;; diable warning when killing buffers opened with emacsclient 
;; (must be set after calling (server-start))
(remove-hook 'kill-buffer-query-functions 'server-kill-buffer-query-function)

(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(LaTeX-command "latex -shell-escape")
 '(ido-ignore-files (quote ("\\`CVS/" "\\`#" "\\`.#" "\\`\\.\\./" "\\`\\./" "_region_" "\\\\.prv/" "\\auto/" "__flymake")))
 '(inhibit-default-init t)
 '(inhibit-startup-screen t)
 '(jabber-account-list (quote (("pjozog@gmail.com" (:network-server . "talk.google.com") (:connection-type . ssl)))))
 '(org-agenda-files (quote ("~/Dropbox/org/projects.org")))
 '(org-hide-leading-stars nil)
 '(uniquify-buffer-name-style (quote forward) nil (uniquify))
 '(vc-follow-symlinks nil)
 '(visible-bell t))
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "#FFFFFF" :foreground "#000000" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 105 :width normal :foundry "unknown" :family "DejaVu Sans Mono"))))
 '(cursor ((t (:background "red"))))
 '(dired-directory ((t (:inherit font-lock-builtin-face))))
 '(dired-symlink ((t (:inherit font-lock-comment-face))))
 '(font-latex-string-face ((((class color) (background dark)) (:foreground "#A2AB64"))))
 '(org-column ((t (:background "#000000" :strike-through nil :underline nil :slant normal :weight normal :height 98 :family "DejaVu Sans Mono")))))

(if (or (string-equal "paul-laptop" system-name)
	(string-equal "perl-paulozog" system-name)
	(string-equal "paul-box" system-name))
    (set-face-attribute 'default nil :height 80))
