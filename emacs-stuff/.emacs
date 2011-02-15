(defun confess-faith ()
  (interactive)
  (message "There is no system but GNU and Linux is one of its kernels"))

(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(LaTeX-command "latex -shell-escape")
 '(inhibit-startup-screen t)
 '(jabber-account-list (quote (("pjozog@gmail.com" (:network-server . "talk.google.com") (:connection-type . ssl)))))
 '(org-hide-leading-stars t)
 '(vc-follow-symlinks nil))
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "#000000" :foreground "#ffffff" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 75 :width normal :foundry "unknown" :family "DejaVu Sans Mono"))))
 '(cursor ((t (:background "red"))))
 '(font-latex-string-face ((((class color) (background dark)) (:foreground "red"))))
 '(font-lock-string-face ((nil (:foreground "red")))))

(menu-bar-mode -1)
(tool-bar-mode -1)
(ido-mode 1)

;; disable highlighting when selecting mark
(transient-mark-mode)

(setq my-emacs-version (nth 2 (split-string (emacs-version))))

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

;; Add color to a shell running in emacs 'M-x shell'
(autoload 'ansi-color-for-comint-mode-on "ansi-color" nil t)
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

;;Make CamelCase the default
(defun my-camel-case-hook ()
  (if (version< my-emacs-version "23.2.1")
      (c-subword-mode 1)
    (subword-mode 1)))

(let (modeList)
  (setq modeList '(python-mode-hook 
		   java-mode-hook
		   c-mode-hook
		   c++-mode-hook
		   matlab-mode-hook))
  (while modeList
    (add-hook (car modeList) (lambda ()
			       (if (version< my-emacs-version "23.2.1")
				   (c-subword-mode 1)
				 (subword-mode 1))
			       (hs-minor-mode)
			       (global-set-key [f1] 'hs-hide-all)
			       (global-set-key [(control tab)] 'hs-toggle-hiding)))

    (setq modeList (cdr modeList))))

;;(server-start)
(setq mouse-yank-at-point t)
(show-paren-mode)

(add-to-list 'load-path
	     "~/.emacs.d/lisp")

(require 'htmlize)
(require 'matlab)
(require 'cmake-mode)
(require 'lua-mode)
(require 'paredit)

;;associate .m files with matlab-mode
(setq auto-mode-alist (cons '("\\.m$" . matlab-mode) auto-mode-alist))
;;associate CMakeLists.txt files with matlab-mode
(setq auto-mode-alist (cons '("CMakeLists\\.txt$" . cmake-mode) auto-mode-alist))
;;turn off that god-damned line wrapping
(setq matlab-fill-code nil)

(global-set-key "\M-\r" 'shell-resync-dirs)
(global-set-key "\C-cf" 'find-file-this)
;(global-set-key "\M-." 'goto-tag)
(global-set-key "\C-xv=" 'ediff-revision)
(global-set-key "\C-c\C-g" 'compile)

(require 'linum)
(global-linum-mode)

(setq ediff-split-window-function 'split-window-horizontally)

(put 'upcase-region 'disabled nil)

;; Saumitro's color theme
(require 'color-theme)
(load "twilight")
(color-theme-twilight)

(if (file-exists-p "~/TAGS")
    (visit-tags-table "~/TAGS"))

(defun find-file-this ()
  (interactive)
   (find-file buffer-file-name))

(add-to-list 'load-path
	     "~/.emacs.d/plugins/yasnippet-0.6.1c")
(add-to-list 'load-path
	     "~/.emacs.d/plugins")

(require 'yasnippet) ;; not yasnippet-bundle
(yas/initialize)
(yas/load-directory "~/.emacs.d/plugins/yasnippet-0.6.1c/snippets")

(open-filelist '("~/.emacs" "~/.bashrc" "~/.config/openbox/menu.xml"
		 "~/.config/openbox/rc.xml" "~/.config/openbox/autostart.sh"
		 "~/.config/awesome/rc.lua"))
(switch-to-buffer "*scratch*")


(put 'downcase-region 'disabled nil)

(load "auctex.el" nil t t)
(setq TeX-auto-save t)
(setq TeX-parse-self t)
(add-hook 'LaTeX-mode-hook (lambda ()
			     (flyspell-mode)
			     (outline-minor-mode)
			     (auto-fill-mode)))

(load "preview-latex.el" nil t t)

(add-hook 'outline-minor-mode-hook
  (lambda ()
    (define-key outline-minor-mode-map [(control tab)] 'org-cycle)))

(defun word-count nil "Count words in buffer" (interactive)
  (shell-command-on-region (point-min) (point-max) "wc -w"))

(defun char-count nil "Count words in buffer" (interactive)
  (shell-command-on-region (point-min) (point-max) "wc -m"))

(defun paren-latex ()
  (interactive)
  (replace-regexp "(" "\\\\left(")
  (exchange-point-and-mark)
  (replace-regexp ")" "\\\\right)"))
