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

(defun kill-all-dired-buffers (exclude)
  "Kill all dired buffers."
  (interactive)
  (save-excursion
    (let((count 0))
      (dolist(buffer (buffer-list))
	(set-buffer buffer)
	(when (and (equal major-mode 'dired-mode)
		   (not (string-equal (buffer-name) exclude)))
	  (setq count (1+ count))
	  (kill-buffer buffer)))
      (message "Killed %i dired buffer(s)." count ))))

;; --------------------------------------------------
;; Aliases
;; --------------------------------------------------
(defalias 'yes-or-no-p 'y-or-n-p) ;; That 'yes' or 'no' shit is anoying:
(defalias 'man 'woman) ;; Doesn't work at startup?
(defalias 'cf 'customize-face)

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
(require 'diff-mode-)
(require 'dired+)
(require 'smarttabs)
(require 'browse-kill-ring)
(require 'naquadah-theme)
(require 'dired-mod)            ;; mine
(require 'myOrgSettings)        ;; mine


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
(global-auto-revert-mode t)
(column-number-mode 1)

;; Woman is pretty cool
(setq woman-use-own-frame nil)

;; This makes color work in 'M-x shell'
(autoload 'ansi-color-for-comint-mode-on "ansi-color" nil t)

;; My key bindings
(global-set-key [(meta return)] 'shell-resync-dirs)
(global-set-key (kbd "C-c C-f") 'find-file-this)
;(global-set-key [(meta .)] 'goto-tag)
(global-set-key [(control o)] 'other-window)
(global-set-key [(control meta |)] 'fill-region)
(global-set-key [(meta h)] 'ff-find-other-file)
(global-set-key (kbd "C-c k") 'ido-kill-buffer)
(global-set-key (kbd "C-S-k") (lambda () (interactive)
				(kill-all-dired-buffers (user-login-name))))
(global-set-key (kbd "C-S-a") 'align-regexp)
(global-set-key (kbd "C-x f") 'find-name-dired)
(global-set-key (kbd "C-x m") 'comment-region)
(global-set-key (kbd "C-X b") 'ibuffer)
(global-set-key (kbd "<f5>") 'shrink-window-horizontally)
(global-set-key (kbd "<f6>") 'enlarge-window-horizontally)
(global-set-key (kbd "<f7>") 'shrink-window)
(global-set-key (kbd "<f8>") 'enlarge-window)


(setq ediff-split-window-function 'split-window-horizontally)

(put 'upcase-region 'disabled nil)

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

(global-highline-mode 1)

;; --------------------------------------------------
;; Language-specific settings
;; --------------------------------------------------
(setq auto-mode-alist (cons '("\\.m$" . matlab-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.txt$" . mediawiki-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("CMakeLists\\.txt$" . cmake-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.lcm$" . java-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("Doxyfile$" . conf-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.shell_aliases$" . sh-mode) auto-mode-alist))
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
			       (define-key hs-minor-mode-map [(control tab)] 'hs-toggle-hiding)))
    (setq modeList (cdr modeList))))

;; apply LaTeX hooks (spellcheck, etc.)
(add-hook 'LaTeX-mode-hook (lambda ()
			     (flyspell-mode)
			     (outline-minor-mode)
			     (auto-fill-mode)
			     (flymake-mode)
			     (turn-on-reftex)
			     (define-key LaTeX-mode-map (kbd "C-7") 'insert-amps)))

;; this makes control-tab function like org-mode
(add-hook 'outline-minor-mode-hook
	  (lambda ()
	    (define-key outline-minor-mode-map [(control tab)] 'org-cycle)))

;; elisp documentation in minibuffer
(add-hook 'emacs-lisp-mode-hook (lambda ()
				  (turn-on-eldoc-mode)
				  (rainbow-mode)))

(add-hook 'vc-dir-mode-hook (lambda ()
			      (define-key vc-dir-mode-map (kbd "RET") 'vc-dir-find-file-other-window)))

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
 '(vc-hg-log-switches (quote ("-v")))
 '(visible-bell t))
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(cursor ((t (:background "red"))))
 '(dired-directory ((t (:inherit font-lock-builtin-face))))
 '(dired-symlink ((t (:inherit font-lock-comment-face))))
 '(diredp-dir-heading ((((background dark)) (:inherit font-lock-comment-face))))
 '(diredp-dir-priv ((((background dark)) (:foreground "#7474FFFFFFFF"))))
 '(font-latex-string-face ((((class color) (background dark)) (:foreground "#A2AB64"))))
 '(org-column ((t (:background "#000000" :strike-through nil :underline nil :slant normal :weight normal :height 98 :family "DejaVu Sans Mono")))))

(if (or (string-equal "paul-laptop" system-name)
	(string-equal "paul-box" system-name))
    (set-face-attribute 'default nil :height 100))

(if (or (string-equal "perl-paulozog" system-name))
    (set-face-attribute 'default nil :height 80))

;; Fix linum margin when running in terminal mode
(unless (window-system)
  (setq linum-format "%d "))

;;--------------------------------------------------
;; Open my favorite files and start rocking!
;;--------------------------------------------------
(open-filelist '("~/.emacs.d/init.el" "~/.shell_aliases" "~/.profile"
		 "~/.config/openbox/autostart.sh"
		 "~/.config/awesome/rc.lua"
		 "~/Dropbox/org/projects.org"
		 "~/perl/perl-svn/references/bibtex/references.bib"))
(dired (getenv "HOME"))
(switch-to-buffer (user-login-name))
(split-window-sensibly (selected-window))
(switch-to-buffer "init.el")
