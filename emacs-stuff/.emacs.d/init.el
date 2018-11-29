;; Recite the holy words
(defun confess-faith ()
  (interactive)
  (message "There is no system but GNU and Linux is one of its kernels"))

(setq my-emacs-version (nth 2 (split-string (emacs-version))))

(setq my-emacs-dir (file-name-directory load-file-name))

;; My key bindings
(defvar paul-keys-minor-mode-map (make-keymap) "paul-keys-minor-mode keymap")

(define-key paul-keys-minor-mode-map (kbd "C-x h") 'global-highline-mode)
(define-key paul-keys-minor-mode-map (kbd "C-x l") 'linum-mode)
(define-key paul-keys-minor-mode-map (kbd "M-RET") 'shell-resync-dirs)
(define-key paul-keys-minor-mode-map (kbd "C-c C-f") 'find-file-this)
(define-key paul-keys-minor-mode-map (kbd "C-o") 'other-window)
(define-key paul-keys-minor-mode-map (kbd "C-M-|") 'fill-region)
(define-key paul-keys-minor-mode-map (kbd "M-h") 'ff-find-other-file)
(define-key paul-keys-minor-mode-map (kbd "C-c k") 'ido-kill-buffer)
(define-key paul-keys-minor-mode-map
  (kbd "C-S-k") (lambda () (interactive)
                  (kill-all-dired-buffers (user-login-name))))
(define-key paul-keys-minor-mode-map (kbd "C-S-a") 'align-regexp)
(define-key paul-keys-minor-mode-map (kbd "C-x f") 'find-name-dired)
(define-key paul-keys-minor-mode-map (kbd "C-x C-j") 'dired-jump)
(define-key paul-keys-minor-mode-map (kbd "M-;") 'comment-dwim) ; damn you, matlab-mode
(define-key paul-keys-minor-mode-map (kbd "M-*") 'pop-tag-mark) ; damn you, Emacs 25!
(define-key paul-keys-minor-mode-map (kbd "C-x C-;") (lambda ()
                                                       (interactive)
                                                       (comment-or-uncomment-region (line-beginning-position)
                                                                                    (line-end-position))))
(define-key paul-keys-minor-mode-map (kbd "C-x C-b") 'ibuffer)
(define-key paul-keys-minor-mode-map
  (kbd "M-e") (lambda ()
                (interactive)
                (setq this-command 'next-line)
                (next-line 3)))
(define-key paul-keys-minor-mode-map
  (kbd "M-a") (lambda ()
                (interactive)
                (setq this-command 'previous-line)
                (previous-line 3)))
(define-key paul-keys-minor-mode-map (kbd "<f5>") 'shrink-window-horizontally)
(define-key paul-keys-minor-mode-map (kbd "<f6>") 'enlarge-window-horizontally)
(define-key paul-keys-minor-mode-map (kbd "<f7>") 'shrink-window)
(define-key paul-keys-minor-mode-map (kbd "<f8>") 'enlarge-window)

(define-key paul-keys-minor-mode-map (kbd "C-c u") 'cua-mode)
(define-key paul-keys-minor-mode-map (kbd "M-Q") 'unfill-paragraph)
(define-key paul-keys-minor-mode-map (kbd "C-x v =") 'vc-ediff)
(define-key paul-keys-minor-mode-map [C-M-tab] 'clang-format-region)
(define-key paul-keys-minor-mode-map (kbd "C-c C-r") 'cua-rectangle-mark-mode)
(define-key paul-keys-minor-mode-map (kbd "C-M-o") 'swiper)

;; Increase/decrease the font in all buffers (not just the current one like you
;; get with C-x C-=, etc.).
(define-key paul-keys-minor-mode-map (kbd "C-c C-=")
  (lambda ()
    (interactive)
    (let ((old-face-attribute (face-attribute 'default :height)))
      (set-face-attribute 'default nil :height (+ old-face-attribute 30)))))

(define-key paul-keys-minor-mode-map (kbd "C-c C--")
  (lambda ()
    (interactive)
    (let ((old-face-attribute (face-attribute 'default :height)))
      (set-face-attribute 'default nil :height (- old-face-attribute 30)))))

(define-minor-mode paul-keys-minor-mode
  "A minor mode so that my key settings override annoying major modes."
  t " paul-keys" 'paul-keys-minor-mode-map)
(paul-keys-minor-mode 1)

;; This basically will only work with emacs24.
(when (>= emacs-major-version 24)

  ;; do melpa packages first
  (require 'package)
  (add-to-list 'package-archives
               '("melpa" . "http://melpa.org/packages/") t)
  (package-initialize)
  ;; NOTE: uncomment on first run to download packages
  ;; (package-refresh-contents)

  (defvar my-melpa-packages '(lua-mode auctex w3m org
                                       smex ido-completing-read+ idle-highlight-mode
                                       yasnippet auto-complete
                                       autopair markdown-mode
                                       rainbow-mode color-theme flymake-cursor
                                       cmake-mode browse-kill-ring
                                       ycmd company-ycmd flycheck-ycmd py-yapf
				       swiper powerline)
    "A list of melpa packages to ensure are installed at launch.")

  (dolist (p my-melpa-packages)
    (when (not (package-installed-p p))
      (package-install p)))

  (ido-mode t)
  (ido-ubiquitous-mode)
  ;; If having a problem with certain commands and ido, do the following: First,
  ;; enable debugger (setq debug-on-error 1), run the function, ensure ido has
  ;; crashed, and add the function *immediately preceding* the call to ido to
  ;; this blacklist.
  (setq ido-cr+-function-blacklist (append ido-cr+-function-blacklist '(etags-select-complete-tag)))
  ;; Use regular find-tag (no ido)
  (setq ido-use-virtual-buffers t recentf-max-saved-items 500)
  (setq ido-default-buffer-method 'selected-window)
  (setq ido-default-file-method 'selected-window)

  ;; Autocomplete in M-x
  (smex-initialize)
  (define-key paul-keys-minor-mode-map (kbd "M-x") 'smex)

  ;; replace find-tags with etags-select
  (define-key paul-keys-minor-mode-map (kbd "M-.") 'etags-select-find-tag)

  (add-hook
   'prog-mode-hook (lambda ()
		     (font-lock-add-keywords
		      nil '(("\\<\\(FIX\\|TODO\\|TODO\\(.+\\)\\|FIXME\\|NOTE\\|HACK\\|REFACTOR\\|DEBUG\\):"
			     1 font-lock-warning-face t))))))

;; --------------------------------------------------
;; Function definitions
;; --------------------------------------------------
(defun open-filelist (fileList)
  (while fileList
    (let ((currentFile (car fileList)))
      (if (file-exists-p currentFile)
          (find-file-noselect (file-truename currentFile) t)))
    (setq fileList (cdr fileList))))

(defun c-c++-header ()
  "sets either c-mode or c++-mode, whichever is appropriate for
header, based on presence of .c file"
  (interactive)
  (let ((c-file (concat (substring (buffer-file-name) 0 -1) "c")))
    (if (file-exists-p c-file)
        (c-mode)
      (c++-mode))))

(defun parent-directory (dir)
  (unless (equal "/" dir)
    (file-name-directory (directory-file-name dir))))

(defun update-git-repo-tags ()
  "Recursively looks in parent directories for an executable file
called '.git/hooks/ctags'. If it's found, then that hook will be
executed (thus updating the TAGS file). "
  (interactive)
  (let* ((old-dir default-directory)
         (maybe-git-repo (locate-dominating-file old-dir ".git/hooks/ctags"))
	 (ctags-script (concat maybe-git-repo ".git/hooks/ctags")))
    (unless (not maybe-git-repo)
      (if (file-executable-p ctags-script)
          (progn
            (message (concat "Running ctags script " ctags-script))
            (cd maybe-git-repo)
            (call-process-shell-command "./.git/hooks/ctags &")
            (cd old-dir))))))

;; opposite of fill-paragraph
(defun unfill-paragraph (&optional region)
  "Takes a multi-line paragraph and makes it into a single line of text."
  (interactive (progn (barf-if-buffer-read-only) '(t)))
  (let ((fill-column (point-max)))
    (fill-paragraph nil region)))

;; --------------------------------------------------
;; Aliases
;; --------------------------------------------------
(defalias 'yes-or-no-p 'y-or-n-p) ;; That 'yes' or 'no' shit is anoying:
(if (file-exists-p "~/Dropbox/code/private/compile-aliases.el")
    (load "~/Dropbox/code/private/compile-aliases.el"))

;; --------------------------------------------------
;; Packages / Minor modes / Keybindings
;; --------------------------------------------------
;; append my stuff to load-path
(add-to-list 'load-path
             (concat (file-name-as-directory my-emacs-dir) "lisp"))

;; No longer in Melpa. See https://github.com/melpa/melpa/pull/5008.
(require 'etags-select)
(require 'unbound)

(require 'matlab)
(require 'cmake-mode)
(require 'lua-mode)
(require 'autopair)
(require 'yasnippet)
(require 'color-theme)
(require 'linum)
(require 'flymake)
(require 'flymake-cursor)
(require 'org-install)
(require 'uniquify)
(require 'rainbow-mode)
(require 'highline)
(require 'diff-mode-)
(require 'smarttabs)
(require 'browse-kill-ring)
(show-paren-mode 1) ;;This needs to be enabled before changing color theme
(require 'subword)
(require 'paul-themes)
(paul-themes-sublime)
(require 'dired-mod)            ;; mine
(require 'myOrgSettings)        ;; mine
(require 'markdown-mode)
(require 'fill-column-indicator)
(require 'google-c-style)
(add-hook 'c-mode-common-hook 'google-set-c-style)
(require 'clang-format)
(require 'py-yapf)
(require 'idle-highlight-mode)
(require 'powerline)
(powerline-paul-theme)

;; Turn off the bad shit
(menu-bar-mode -1)
(tool-bar-mode -1)
(condition-case nil
    (scroll-bar-mode -1)
  (error nil))
(global-auto-complete-mode -1)
(setq flyspell-issue-message-flag nil)

;; Turn on the good shit
(transient-mark-mode 1)
(ido-mode t)
(setq ido-enable-prefix nil
      ido-enable-flex-matching t
      ido-auto-merge-work-directories-length nil
      ido-create-new-buffer 'always
      ido-use-filename-at-point nil
      ido-handle-duplicate-virtual-buffers 2
      ido-max-prospects 10)
(setq mouse-yank-at-point 1)
(autopair-global-mode 1)
(setq autopair-autowrap t)
;; searching for ' ' shows all whitespace
(setq search-whitespace-regexp "[ \t\r\n]+")

;; (global-linum-mode 1)
(global-auto-revert-mode t)
(column-number-mode 1)
(setq default-fill-column 80)
(setq scroll-preserve-screen-position 1)
(setq woman-fill-column default-fill-column)
(setq-default indicate-empty-lines t)
(setq gdb-many-windows t)
(setq ns-command-modifier 'meta)
(setq bibtex-align-at-equal-sign t)
(setq ediff-window-setup-function 'ediff-setup-windows-plain)
(setq ediff-split-window-function 'split-window-horizontally)

;; Restore ediff layout when quitting
(defvar ediff-last-windows nil
  "Last ediff window configuration.")

(defun ediff-restore-windows ()
  "Restore window configuration to `ediff-last-windows'."
  (set-window-configuration ediff-last-windows)
  (remove-hook 'ediff-after-quit-hook-internal
               'ediff-restore-windows))

(defadvice ediff-buffers (around ediff-restore-windows activate)
  (setq ediff-last-windows (current-window-configuration))
  (add-hook 'ediff-after-quit-hook-internal 'ediff-restore-windows)
  ad-do-it)

;; rebalance windows when you split them (C-x 2, C-x 3)
(defadvice split-window-below (after rebalance-windows activate)
  (balance-windows))
(ad-activate 'split-window-below)
(defadvice split-window-right (after rebalance-windows activate)
  (balance-windows))
(ad-activate 'split-window-right)
(defadvice delete-window (after rebalance-windows activate)
  (balance-windows))
(ad-activate 'delete-window)

;; Woman is pretty cool
(setq woman-use-own-frame nil)

;; Show time in mode line
(display-time)

;; This makes color work in 'M-x shell'
(autoload 'ansi-color-for-comint-mode-on "ansi-color" nil t)
(if (string-equal system-type "gnu/linux")
    (setq shell-file-name "/bin/bash"))

;; Number of killed things to remember
(setq kill-ring-max 6000)

(put 'upcase-region 'disabled nil)

;; HACK: disable the default snippets packaged with melpa
(setq yas-snippet-dirs (cons (concat (file-name-as-directory my-emacs-dir) "snippets") ()))
(yas/initialize)

(put 'downcase-region 'disabled nil)

(setq TeX-auto-save t)
(setq TeX-parse-self t)

;; --------------------------------------------------
;; Language-specific settings
;; --------------------------------------------------
(add-to-list 'auto-mode-alist '("CMakeLists\\.txt$" . cmake-mode))
(add-to-list 'auto-mode-alist '("\\.lcm$" . java-mode))
(add-to-list 'auto-mode-alist '("Doxyfile$" . conf-mode))
(add-to-list 'auto-mode-alist '("\\.shell_aliases$" . sh-mode))
(add-to-list 'auto-mode-alist '("\\.SRC$" . asm-mode))
(add-to-list 'auto-mode-alist '("\\.isam$" . python-mode))
(add-to-list 'auto-mode-alist '("\\.moos$" . conf-mode))
(add-to-list 'auto-mode-alist '("\\.h\\'" . c-c++-header))
(setq matlabf-fill-code nil)

;; Also make function calls highlighted for common programming modes
(let (modeList)
  (setq modeList '(c-mode
                   c++-mode
                   python-mode
                   java-mode))
  (while modeList
    (font-lock-add-keywords (car modeList)
                            '(("\\s\"?\\(\\(\\sw\\|\\s_\\)+\\(<-\\)?\\)\\s\"?*\\s-*("
                               (1 font-lock-function-name-face))) t)
    (setq modeList (cdr modeList))))
;; Special case: require space before opening parenthesis:
(font-lock-add-keywords 'matlab-mode
  '(("\\s\"?\\(\\(\\sw\\|\\s_\\)+ \\(<-\\)?\\)\\s\"?*\\s-*("
    (1 font-lock-function-name-face))) t)
(font-lock-add-keywords 'octave-mode
  '(("\\s\"?\\(\\(\\sw\\|\\s_\\)+ \\(<-\\)?\\)\\s\"?*\\s-*("
    (1 font-lock-function-name-face))) t)

;; LaTeX: Enable flymake for texlive distribution of LaTeX
(defun flymake-get-tex-args (file-name)
  (list "pdflatex" (list "-shell-escape" "-draftmode" "-file-line-error"
                         "-interaction=nonstopmode" file-name)))

;; --------------------------------------------------
;; My hooks
;; --------------------------------------------------
(add-hook 'before-save-hook (lambda () (delete-trailing-whitespace)))
(add-hook 'after-save-hook (lambda () (update-git-repo-tags)))

(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

(add-hook 'ibuffer-mode-hook (lambda ()
                               (define-key ibuffer-mode-map (kbd "C-o") 'other-window)))

(condition-case nil
    (global-subword-mode 1)
  (error nil))

;; apply hooks for programming modes
(add-hook
 'prog-mode-hook (lambda ()
                   (flyspell-prog-mode)
                   (hs-minor-mode)
                   (define-key hs-minor-mode-map [(control tab)]
                     'hs-toggle-hiding)
                   ;; Only supported in emacs 24
                   (condition-case nil
                       (progn
                         (idle-highlight-mode t))
                     (error nil))))

;; apply hooks for text modes
(add-hook
 'text-mode-hook (lambda ()
                   (flyspell-mode)))

;; apply LaTeX hooks (ensures output to pdf, for example)
(add-hook 'LaTeX-mode-hook (lambda ()
                             (outline-minor-mode)
                             (define-key LaTeX-mode-map (kbd "C-7") 'insert-amps)
                             (orgtbl-mode)
                             (TeX-PDF-mode-on)
                             (TeX-global-PDF-mode t)))

;; this makes control-tab function like org-mode
(add-hook 'outline-minor-mode-hook
          (lambda ()
            (define-key outline-minor-mode-map [(control tab)] 'org-cycle)))

;; elisp documentation in minibuffer
(add-hook 'emacs-lisp-mode-hook (lambda ()
                                  (turn-on-eldoc-mode)
                                  (rainbow-mode)))

;; matlab rebinds M-e and M-a
(add-hook 'matlab-mode-hook (lambda ()
                              (define-key
                                matlab-mode-map (kbd "M-e") (lambda ()
                                                              (interactive)
                                                              (next-line 6)))
                              (define-key
                                matlab-mode-map (kbd "M-a") (lambda ()
                                                              (interactive)
                                                              (previous-line 6)))))

(add-hook 'vc-dir-mode-hook (lambda ()
                              (define-key vc-dir-mode-map (kbd "RET")
                                'vc-dir-find-file-other-window)))

;; check the buffer when flyspell loads
(add-hook 'flyspell-mode-hook (lambda ()
                                (flyspell-buffer)))

;; Don't indent extern "C"
(add-hook 'c-mode-hook (lambda ()
                         (c-set-offset 'inextern-lang 0)))

;; Don't add extra indentations when inside a namespace (c++)
(add-hook 'c++-mode-hook (lambda ()
                           (c-set-offset 'innamespace 0)
                           (font-lock-add-keywords
                            nil '(("\\<\\(that\\)->"
                                   1 font-lock-keyword-face)))))

;; This can be a bit disruptive (e.g., when writing bazel BUILD files), but it's
;; nice to have when actually writing python
;; (add-hook 'python-mode-hook 'py-yapf-enable-on-save)

;; helper function for commenting code inside an #if0 ... #endif block
(defun c-mode-font-lock-if0 (limit)
  (save-restriction
    (widen)
    (save-excursion
      (goto-char (point-min))
      (let ((depth 0) str start start-depth)
        (while (re-search-forward "^\\s-*#\\s-*\\(if\\|else\\|endif\\)" limit 'move)
          (setq str (match-string 1))
          (if (string= str "if")
              (progn
                (setq depth (1+ depth))
                (when (and (null start) (looking-at "\\s-+0"))
                  (setq start (match-end 0)
                        start-depth depth)))
            (when (and start (= depth start-depth))
              (c-put-font-lock-face start (match-beginning 0) 'font-lock-comment-face)
              (setq start nil))
            (when (string= str "endif")
              (setq depth (1- depth)))))
        (when (and start (> depth 0))
          (c-put-font-lock-face start (point) 'font-lock-comment-face)))))
  nil)

;; use '/* ... */' for comments in c-like modes.  Comment #if0 ... #endif blocks
(add-hook 'c-mode-common-hook (lambda ()
                                (font-lock-add-keywords
                                 nil
                                 '((c-mode-font-lock-if0 (0 font-lock-comment-face prepend))) 'add-to-end)))

;; Use % for octave
(add-hook 'octave-mode-hook (lambda ()
                              (setq comment-start "% ")))

;; diable warning when killing buffers opened with emacsclient
;; (must be set after calling (server-start))
(remove-hook 'kill-buffer-query-functions 'server-kill-buffer-query-function)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(LaTeX-command "latex -shell-escape")
 '(TeX-view-program-list (quote (("Evince" "evince --page-index=%(outpage) %o"))))
 '(c-doc-comment-style (quote gtkdoc))
 '(ido-ignore-files
   (quote
    ("\\`CVS/" "\\`#" "\\`.#" "\\`\\.\\./" "\\`\\./" "_region_" "\\\\.prv/" "\\auto/" "__flymake")))
 '(inhibit-default-init t)
 '(inhibit-startup-screen t)
 '(org-agenda-files (quote ("~/Dropbox/org/projects.org")))
 '(org-hide-leading-stars nil)
 '(package-selected-packages
   (quote
    (powerline paradox yasnippet w3m swiper smex rainbow-mode py-yapf modern-cpp-font-lock markdown-mode lua-mode ido-ubiquitous idle-highlight-mode flymake-cursor flycheck-ycmd company-ycmd color-theme cmake-mode browse-kill-ring autopair auto-complete auctex)))
 '(paradox-github-token t)
 '(uniquify-buffer-name-style (quote forward) nil (uniquify))
 '(vc-follow-symlinks t)
 '(vc-hg-log-switches (quote ("-v"))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(dired-directory ((t (:inherit font-lock-builtin-face))))
 '(dired-symlink ((t (:inherit font-lock-comment-face))))
 '(diredp-dir-heading ((((background dark)) (:inherit font-lock-comment-face))))
 '(diredp-dir-priv ((((background dark)) (:foreground "#7474FFFFFFFF"))))
 '(font-latex-string-face ((((class color) (background dark)) (:foreground "#A2AB64"))))
 '(org-column ((t (:background "#000000" :strike-through nil :underline nil :slant normal :weight normal :height 98 :family "DejaVu Sans Mono")))))

;; OS X fonts
(if (string-equal "darwin" system-type)
    (progn
      (set-face-attribute 'default nil :height 80 :family "monospace")
      (require 'ls-lisp)
      (setq ls-lisp-use-insert-directory-program t)
      (setq insert-directory-program "~/bin/ls")
      (setq dired-listing-switches "-CF --group-directories-first -alh")
      (setq ns-command-modifier 'meta)))

;; Linux fonts
(if (string-equal "gnu/linux" system-type)
    (set-face-attribute 'default nil :height 70 :family "terminesspowerline"))

;; Fix linum margin when running in terminal mode
(setq linum-format "%d ")
(setq split-height-threshold 90)

;; --------------------------------------------------
;; My private emacs stuff (for work etc.)
;; --------------------------------------------------
(if (file-exists-p "~/Dropbox/code/private/my-private-emacs.el")
    (load "~/Dropbox/code/private/my-private-emacs.el"))

(if (file-exists-p "~/Dropbox/code/private/tag-table-list.el")
    (load "~/Dropbox/code/private/tag-table-list.el"))


(open-filelist '("~/.emacs.d/init.el" "~/.shell_aliases" "~/.profile"
                 "~/.config/openbox/autostart.sh"
                 "~/.i3/config"
                 "~/Dropbox/personal/org/projects.org"))

(get-buffer-create "*compilation*")

;; Set scratch buffer to org-mode
(setq initial-scratch-message "* Scratch buffer
  - Place any notes you would like here.  Use org-mode syntax.

")
(org-mode)

;; auto-complete setup for C and C++
(require 'ycmd)
(require 'company-ycmd)
(require 'flycheck-ycmd)
(setq ycmd-server-command (list "python" (file-truename "~/documents/git/ycmd/ycmd")))
(ycmd-setup)
(company-ycmd-setup)
(flycheck-ycmd-setup)
(defun enable-ycmd ()
  (ycmd-mode) (company-mode) (flycheck-mode))
(add-hook 'c++-mode-hook 'enable-ycmd)
(add-hook 'c-mode-hook 'enable-ycmd)
(add-hook 'python-mode-hook 'enable-ycmd)
(setq company-idle-delay 0.05)
(setq company-ycmd-request-sync-timeout 0)

;; Bind Shift + TAB to force semantic completion
(defun company-ycmd-semantic-complete ()
  (interactive)
  (let ((ycmd-force-semantic-completion t))
    (company-complete)))
(define-key paul-keys-minor-mode-map (kbd "<backtab>") 'company-ycmd-semantic-complete)
