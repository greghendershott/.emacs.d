;;; init.el --- Emacs init file

(eval-when-compile
  (require 'cl))

;; OS
(defvar mswindows-p (string-match "windows" (symbol-name system-type)))
(defvar macosx-p (string-match "darwin" (symbol-name system-type)))
(defvar linux-p (and (not mswindows-p) (not macosx-p)))
(defvar wsl-p (and linux-p (getenv "WSL") t))

;; Things to do early in startup, e.g. to avoid momentary display

(add-to-list 'initial-frame-alist
             (if macosx-p
                 '(fullscreen . fullboth)
               '(fullscreen . maximized)))

(setq inhibit-startup-message t)
(when (boundp 'scroll-bar-mode) (scroll-bar-mode -1))
(when (boundp 'tool-bar-mode) (tool-bar-mode -1))
(cond (macosx-p (setq ns-command-modifier 'meta
                      ns-auto-hide-menu-bar t))
      ((boundp 'menu-bar-mode) (menu-bar-mode -1)))

(case window-system
  (x (set-frame-font "Ubuntu Mono 12")))

(setq text-scale-mode-step 1.1)         ;finer inc/dec than default 1.2

(setq-default cursor-type 'box)
(setq-default cursor-in-non-selected-windows '(hbar . 3))
(setq blink-cursor-alist '((box . bar)))
(set-face-attribute 'cursor nil :background "orange")
(blink-cursor-mode 1)
(when (boundp 'blink-cursor-blinks)
  (setq blink-cursor-blinks 0)) ;forever

(setq custom-file "~/.emacs.d/custom.el")
(load custom-file t)

;; I seem to be hitting this accidentally and I never use fill-prefix.
(global-unset-key (kbd "C-x ."))

(when mswindows-p
  (setq w32-pass-lwindow-to-system nil)
  (setq w32-lwindow-modifier 'meta)
  (w32-register-hot-key [M-])
  (w32-unregister-hot-key [M-tab]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Theme hooks

(defvar gh/theme-hooks nil
  "((theme-id . function) ...)")

(defun gh/add-theme-hook (theme-id hook-func)
  (add-to-list 'gh/theme-hooks (cons theme-id hook-func)))

(defun gh/disable-all-themes ()
  (interactive)
  (mapc #'disable-theme custom-enabled-themes))

(defun gh/load-theme-advice (f theme-id &optional no-confirm no-enable &rest args)
  "Enhances `load-theme' in two ways:
1. Disables enabled themes for a clean slate.
2. Calls functions registered using `gh/add-theme-hook'."
  (unless no-enable
    (gh/disable-all-themes))
  (prog1
      (apply f theme-id no-confirm no-enable args)
    (unless no-enable
      (pcase (assq theme-id gh/theme-hooks)
        (`(,_ . ,f) (funcall f))))))

(advice-add 'load-theme
            :around
            #'gh/load-theme-advice)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Misc settings

(setq apropos-do-all t)

;;(setq visible-bell nil)
(setq ring-bell-function 'ignore)

;; Other customizations
(column-number-mode t)
(line-number-mode t)
(setq make-backup-files nil)         ;stop creating those backup~ files
(setq auto-save-default nil)         ;stop creating those #autosave# files
(setq-default major-mode 'text-mode)
(setq-default indent-tabs-mode nil)  ;spaces instead of TAB chars
(setq-default tab-width 4)
(add-hook 'emacs-lisp-mode-hook (lambda () (setq tab-width 8)))
(setq echo-keystrokes 0.1)           ;show keys immediately
(setq scroll-down-aggressively 0.1)
(setq scroll-up-aggressively 0.1)

(setq tab-always-indent 'complete)
(delete-selection-mode 1)          ;especially nice with expand-region

;; Show trailing whitespace in certain modes.
(setq-default show-trailing-whitespace nil)
;; Although it may seem silly to define these as named functions, nicer to
;; view/debug hook variables:
(defun gh/show-trailing-whitespace-yes () (setq show-trailing-whitespace t))
(defun gh/show-trailing-whitespace-no ()  (setq show-trailing-whitespace nil))
(dolist (hook '(prog-mode-hook))
  (add-hook hook #'gh/show-trailing-whitespace-yes))
(dolist (hook '(magit-mode-hook))
  (add-hook hook #'gh/show-trailing-whitespace-no 'append))

(setq safe-local-variable-values
      '((require-final-newline . t)))

;; Show empty lines at buffer end.
(set-default 'indicate-empty-lines t)

;; Sentences do not need double spaces to end.
(set-default 'sentence-end-double-space nil)

(setq browse-url-browser-function
      (cond (macosx-p #'browse-url-default-browser)
            (t #'browse-url-firefox)))

;; Emacs 25 enables global-eldoc-mode by default. No.
(when (fboundp 'global-eldoc-mode)
  (global-eldoc-mode -1))

(require 'rx)
(defun gh/set-page-delimiter ()
  (setq-local page-delimiter (rx bol (or "" ";;; "))))
(dolist (m '(clojure-mode-hook
             emacs-lisp-mode-hook
             racket-mode-hook))
  (add-hook m #'gh/set-page-delimiter))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; use-package

(require 'package)
(setq package-enable-at-startup nil) ;; for `use-package' :defer
(setq package-archives '(("elpa" .  "https://elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")))
(package-initialize)
(unless (package-installed-p 'use-package)
  (message "use-package not installed. Trying to install")
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)
(require 'bind-key)

(when (or linux-p macosx-p)
  (use-package exec-path-from-shell ;do this early
    :ensure t
    :init (exec-path-from-shell-initialize)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Packages, alphabetically.

;; Use `use-package' for built-in packages, too -- :load-path instead of
;; :ensure

(use-package ace-window
  :ensure t
  :defer t
  ;; Why bind to C-x 9? (a) Some other common window cmds are C-x <number>,
  ;; and this is next to C-x o (b) The "9" is right middle finger (c) The left
  ;; hand can tap the window key.
  :bind (("C-x 9" . ace-window))
  ;; Use home-row letters instead of the default numbers
  :config
  (require 'avy)
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
  (setq aw-dispatch-always t)
  (setq aw-background t)
  (put 'aw-leading-char-face 'face-alias 'avy-lead-face)
  (put 'aw-background-face 'face-alias 'avy-background-face))

(use-package all-the-icons
  :ensure t
  :config
  ;; Racket: 1.2 scale and bright red color is too much, for me.
  ;; Also, match more file extensions.
  (add-to-list 'all-the-icons-icon-alist
               '("\\.rkt[ld]?$" all-the-icons-fileicon "racket"
                 :height 0.8
                 :face all-the-icons-purple))
  ;; Scheme: ditto.
  (add-to-list 'all-the-icons-icon-alist
               '("\\.s\\(s\\|cm\\|ls\\)$" all-the-icons-fileicon "scheme"
                 :height 0.8
                 :face all-the-icons-purple)))

(use-package all-the-icons-dired
  :ensure t
  :init (add-hook 'dired-mode-hook 'all-the-icons-dired-mode))

(when macosx-p
  (use-package buffer-face-mode
    ;; built-in
    :defer t
    :init
    (defun gh/buffer-face-fixed ()
      "Use a fixed font in the current buffer."
      (interactive)
      (setq buffer-face-mode-face '(:family "Menlo" :width semi-condensed)) ;macosx specific
      (buffer-face-mode))
    (defun gh/buffer-face-variable ()
      "Use a variable font in the current buffer."
      (interactive)
      (setq buffer-face-mode-face '(:family "Helvetica" :height 140)) ;macosx specific
      (buffer-face-mode))
    (dolist (hook '(Info-mode-hook))
      (add-hook hook #'gh/buffer-face-variable))))

(use-package cider
  :ensure t
  :defer t
  :config (setq cider-cljs-lein-repl
                "(do (require 'figwheel-sidecar.repl-api)
                     (figwheel-sidecar.repl-api/start-figwheel!)
                     (figwheel-sidecar.repl-api/cljs-repl))"))

(use-package clojure-mode
  :ensure t
  :defer t
  :config (put-clojure-indent 'match 1))

(use-package company
  :ensure t
  :defer t
  :diminish company-mode
  ;; The following for testing racket-mode/issues/318
  :config
  (setq company-idle-delay 0.1
        company-minimum-prefix-length 2
        company-tooltip-align-annotations t
        company-show-numbers t
        company-require-match nil))

(use-package compilation-mode
  ;; built-in
  :defer t
  :init (setq compilation-scroll-output 'next-error))

(use-package copyright
  ;; built-in
  :init (setq copyright-names-regexp "Greg Hendershott"
              copyright-year-ranges t))

(use-package dash
  :ensure t)

(use-package dash-functional
  :ensure t)

(use-package deadgrep
  :ensure t)

(use-package dired
  :init (add-hook 'dired-mode-hook 'dired-hide-details-mode)
  :config (bind-keys :map dired-mode-map
                     (")" . dired-hide-details-mode)
                     ("(" . dired-hide-details-mode)
                     ("C-M-u" . dired-subtree-up)
                     ("C-M-d" . dired-subtree-down)
                     ("C-M-p" . dired-subtree-previous-sibling)
                     ("C-M-n" . dired-subtree-next-sibling)))

(use-package diminish
  :ensure t)

(use-package elisp-slime-nav
  :ensure t
  :diminish 'elisp-slime-nav-mode
  :init (dolist (hook '(emacs-lisp-mode-hook ielm-mode-hook))
          (add-hook hook #'turn-on-elisp-slime-nav-mode)))

(use-package engine-mode
  :ensure t
  :config
  (defengine my-github-issues
    "https://github.com/issues?utf8=true&q=is:open+user:greghendershott+%s"
    :keybinding "i"
    :docstring "Search my GitHub open issues.")
  (defengine duckduckgo
    "https://duckduckgo.com/?q=%s"
    :keybinding "d")
  (engine-mode 1))

(use-package expand-region
  :ensure t
  :bind (("M-SPC" . er/expand-region)))

(use-package faceup
  :ensure t
  :defer t)

(use-package find-func ;built-in
  ;; http://emacsredux.com/blog/2014/06/18/quickly-find-emacs-lisp-sources/
  :init (bind-keys ("C-h C-l" . find-library)
                   ("C-h C-f" . find-function)
                   ("C-h C-k" . find-function-on-key)
                   ("C-h C-v" . find-variable)))

(use-package flycheck
  :ensure t
  :defer t)

(use-package flyspell
  :config (bind-keys :map flyspell-mode-map
                     ("C-." . gh/next-window)
                     ("C-," . gh/prev-window)))

(use-package font-lock-studio
  :ensure t
  :defer t)

(use-package frame
  :bind (("C-c w F" . toggle-frame-fullscreen))
  :init
  ;; Unbind `suspend-frame'
  (global-set-key (kbd "C-z") nil)
  (global-set-key (kbd "C-x C-z") nil))

(use-package ggtags
  :ensure t
  :config
  (add-hook 'c-mode-common-hook
            (lambda ()
              (when (derived-mode-p 'c-mode 'c++-mode 'java-mode)
                (ggtags-mode 1)))))

(use-package greg-theme
  :load-path "~/src/elisp/greg-theme/"
  :defer t
  :init
  (add-to-list 'custom-theme-load-path "~/src/elisp/greg-theme/"))

(use-package haskell-mode
 :ensure t
 :defer t
 :init
 (add-hook 'haskell-mode-hook #'turn-on-haskell-indentation)
 (add-hook 'haskell-mode-hook #'interactive-haskell-mode)
 (add-hook 'haskell-mode-hook #'haskell-doc-mode)
 :config
 (bind-keys :map haskell-mode-map
            ("C-M-x"   . inferior-haskell-send-decl)
            ("C-c C-k" . inferior-haskell-load-file)
            ("<f5>"    . inferior-haskell-load-file)))

(use-package helpful
  :ensure t
  :bind (("C-h f" . helpful-callable)
         ("C-h v" . helpful-variable)
         ("C-h k" . helpful-key)))

(use-package highlight-parentheses
  :ensure t
  :defer t)

(use-package hl-line-mode
  :disabled t
  :init
  (setq hl-line-sticky-flag nil)
  (add-hook 'prog-mode-hook #'hl-line-mode))

(use-package hydra
  :ensure t
  :bind (("C-c b z" . gh/hydra-zoom/body))
  :config
  (setq hydra-lv nil) ;use echo area
  (defhydra gh/hydra-zoom ()
    "zoom"
    ("i" text-scale-increase "in")
    ("o" text-scale-decrease "out")
    ("SPC" nil)
    ("RET" nil)))

(use-package ibuffer-projectile
  :defer t
  :ensure t
  :init (add-hook 'ibuffer-hook #'ibuffer-projectile-set-filter-groups))

(use-package ido
  :ensure t
  :init
  (setq ido-enable-prefix nil
        ido-enable-flex-matching t
        ido-case-fold nil
        ido-auto-merge-work-directories-length -1
        ido-create-new-buffer 'always
        ido-use-filename-at-point nil
        ido-max-prospects 10
        ido-use-virtual-buffers nil)
  (ido-mode 1)

  (use-package flx-ido ;for better flex matching between words
    :ensure t
    :init
    (setq ido-use-faces nil) ;disable ido faces to see flx highlights.
    (flx-ido-mode 1))

  (use-package ido-vertical-mode ;flx-ido looks better vertically
    :ensure t
    :init
    (add-hook 'ido-setup-hook
              (lambda ()
                (bind-keys :map ido-completion-map
                           ("C-n"    . ido-next-match)
                           ("<down>" . ido-next-match)
                           ("C-p"    . ido-prev-match)
                           ("<up>"   . ido-prev-match))))
    (ido-vertical-mode 1))

  (use-package idomenu
    :ensure t
    :bind (("C-c i"   . idomenu))
    :init
    (autoload 'idomenu "idomenu" nil t) ;do I really need this?
    (add-hook 'emacs-lisp-mode #'gh/emacs-lisp-mode-imenu-hook)))

(use-package interaction-log
  :defer t
  :ensure t
  :commands 'interaction-log-mode)

(use-package ispell
  :config (setq ispell-program-name (if macosx-p "/usr/local/bin/aspell" "aspell")
                ispell-list-command "list"
                ispell-extra-args '("--sug-mode=ultra")))

(use-package json-mode
  :ensure t
  :defer t
  :config
  (add-hook 'json-mode-hook
            ;; Fix JSON mode indentation
            (lambda () (setq-local js-indent-level 4))))

(use-package magit
  :ensure t
  :bind (("C-x g" . magit-status)
         ("C-c g c" . magit-clone)
         ("C-c g s" . magit-status)
         ("C-c g b" . magit-blame)
         ("C-c g l" . magit-log-buffer-file)
         ("C-c g p" . magit-pull))
  :config
  (add-hook 'git-commit-setup-hook #'git-commit-turn-on-flyspell)
  (add-to-list 'display-buffer-alist
               `(,(rx bos "*magit: ")
                 (display-buffer-reuse-window)
                 (reusable-frames . visible))))

(use-package markdown-mode
  :ensure t
  :mode (("\\.text\\'"     . markdown-mode)
         ("\\.markdown\\'" . markdown-mode)
         ("\\.md\\'"       . markdown-mode))
  :config
  (bind-key "C-c k" #'gh/insert-key markdown-mode-map)
  (setq markdown-command "/usr/racket/bin/racket /home/greg/src/racket/collects/markdown/markdown/main.rkt"))

(use-package material-theme
  :ensure t
  :defer t
  :init
  (defun gh/material-theme-hook ()
    (set-face-attribute 'which-key-key-face nil :foreground
                        (face-attribute 'error :foreground))
    (loop for n from 1 to 8
          do (set-face-attribute (intern-soft (format "org-level-%s" n))
                                 nil
                                 :height     'unspecified
                                 :background 'unspecified
                                 :box        'unspecified)))
  (gh/add-theme-hook 'material       #'gh/material-theme-hook)
  (gh/add-theme-hook 'material-light #'gh/material-theme-hook))

(use-package mmm-mode
  :ensure t)

(when linux-p
  (use-package mu4e
    :load-path "/usr/share/emacs/site-lisp/mu4e"
    :config
    (require 'org-mu4e)
    (bind-keys :map mu4e-headers-mode-map
               ("C-c C-c" . org-mu4e-store-and-capture))
    (bind-keys :map mu4e-view-mode-map
               ("C-c C-c" . org-mu4e-store-and-capture))
    ;; use mu4e for e-mail in emacs
    (setq mail-user-agent 'mu4e-user-agent)

    (setq mu4e-maildir "/home/greg/Maildir")

    (setq mu4e-trash-folder  "/greg-gmail/trash")
    (setq mu4e-drafts-folder "/greg-gmail/drafts")

    ;; We don't actually mbsync /sent -- Gmail automatically adds its `Sent`
    ;; and `All Mail` labels when we use its SMTP. However mu4e will complain
    ;; unless this is set to a folder, even if it doesn't use it.
    (setq mu4e-sent-folder            "/greg-gmail/sent"
          mu4e-sent-messages-behavior 'delete)

    ;; (See the documentation for `mu4e-sent-messages-behavior' if you have
    ;; additional non-Gmail addresses and want assign them different
    ;; behavior.)

    ;; Rename files when moving -- needed for mbsync to avoid duplicate UID
    ;; errors!
    (setq mu4e-change-filenames-when-moving t)

    ;; Important: The (d)elete action doesn't work properly with Gmail IMAP.
    ;; Instead want to (m)ove the email to the "[Gmail]/Trash" folder (which
    ;; I've set mbsync to rename to simply "trash").
    (defun gh/mu4e-headers-move-to-trash ()
      (interactive)
      (mu4e-mark-set 'move mu4e-trash-folder)
      (mu4e-headers-next))
    (defun gh/mu4e-view-move-to-trash (&optional n)
      (interactive "P")
      (mu4e~view-in-headers-context
       (gh/mu4e-headers-move-to-trash)
       (mu4e~headers-move (or n 1))))
    (bind-key "d" #'gh/mu4e-headers-move-to-trash mu4e-headers-mode-map)
    (bind-key "d" #'gh/mu4e-view-move-to-trash    mu4e-view-mode-map)

    ;; setup some handy shortcuts for Gmail IMAP semantics
    (setq mu4e-maildir-shortcuts
          '(("/greg-gmail/INBOX" . ?i)
            ("/greg-gmail/trash" . ?t)
            ("/greg-gmail/all"   . ?a)))

    ;; allow for updating mail using 'U' in the main view:
    (setq mu4e-get-mail-command "mbsync greg-gmail")

    (setq mu4e-bookmarks
          (list
           (make-mu4e-bookmark
            :name "Unread"
            :query "flag:unread AND NOT flag:trashed AND NOT maildir:/greg-gmail/trash AND NOT maildir:/greg-gmail/spam"
            :key ?u)
           (make-mu4e-bookmark
            :name "Sent"
            :query "from:greghendershott@gmail.com AND NOT flag:trashed AND NOT maildir:/greg-gmail/trash"
            :key ?s)
           (make-mu4e-bookmark
            :name "Last week"
            :query "date:7d..now"
            :key ?w)
           (make-mu4e-bookmark
            :name "Pictures"
            :query "mime:image/*"
            :key ?p)
           (make-mu4e-bookmark
            :name "Racket Users"
            :query "list:racket-users.googlegroups.com OR to:racket-users@googlegroups.com OR cc:racket-users@googlegroups.com  OR users.racket-lang.org OR to:users@lists.racket-lang.org OR list:plt-scheme.list.cs.brown.edu"
            :key ?r)
           (make-mu4e-bookmark
            :name "Racket Dev"
            :query "list:racket-dev.googlegroups.com OR to:racket-dev@googlegroups.com OR cc:racket-dev@googlegroups.com  OR list:dev.racket-lang.org OR to:dev@lists.racket-lang.org OR cc:dev@lists.racket-lang.org"
            :key ?R)))

    ;; something about ourselves
    (setq user-mail-address "greghendershott@gmail.com"
          user-full-name    "Greg Hendershott")
    (setq mu4e-compose-signature "Greg Hendershott"
          mu4e-compose-signature-auto-include nil)

    ;; sending mail -- also, make sure the gnutls command line utils are
    ;; installed package 'gnutls-bin' in Debian/Ubuntu
    (require 'smtpmail)
    (setq message-send-mail-function 'smtpmail-send-it
          starttls-use-gnutls t
          smtpmail-starttls-credentials '(("smtp.gmail.com" 587 nil nil))
          smtpmail-auth-credentials
          '(("smtp.gmail.com" 587 "greghendershott@gmail.com" nil))
          smtpmail-default-smtp-server "smtp.gmail.com"
          smtpmail-smtp-server "smtp.gmail.com"
          smtpmail-smtp-service 587)
    (setq mu4e-compose-dont-reply-to-self t)

    (setq mu4e-attachment-dir  "~/Downloads") ;not ~/

    ;; rice
    (setq mu4e-view-show-addresses t)
    (setq mu4e-use-fancy-chars nil)
    (setq mu4e-headers-fields '( (:human-date     .   12)
                                 (:flags          .    6)
                                 (:mailing-list   .   10)
                                 (:from           .   22)
                                 (:thread-subject .   nil)))

    ;; don't keep message buffers around
    (setq message-kill-buffer-on-exit t)))

(use-package multiple-cursors
  :ensure t
  :bind (("C-c c <SPC>" . mc/vertical-align-with-space)
         ("C-c c a"     . mc/vertical-align)
         ("C-c c e"     . mc/mark-more-like-this-extended)
         ("C-c c h"     . mc/mark-all-like-this-dwim)
         ("C-c c l"     . mc/edit-lines)
         ("C-c c n"     . mc/mark-next-like-this)
         ("C-c c p"     . mc/mark-previous-like-this)
         ("C-c c r"     . vr/mc-mark)
         ("C-c c C-a"   . mc/edit-beginnings-of-lines)
         ("C-c c C-e"   . mc/edit-ends-of-lines)
         ("C-c c C-s"   . mc/mark-all-in-region)))

(use-package neotree
  :ensure t
  :bind (("C-c f t" . neotree-toggle)
         ("C-c f f" . neotree-find))
  :config
  (setq neo-theme 'icons
        neo-window-width 24
        neo-create-file-auto-open t
        neo-banner-message nil
        neo-show-updir-line t
        neo-mode-line-type 'neotree
        neo-smart-open t
        neo-dont-be-alone t
        neo-persist-show nil
        neo-show-hidden-files t
        neo-auto-indent-point nil)
  (apply
   #'custom-set-faces
   `((neo-file-link-face ((t (:inherit default))))
     (neo-dir-link-face ((t (:inherit dired-directory)))))))

(use-package nxml-mode
  :init (apply
         #'custom-set-faces
         `(;; De-emphasize the tag, emphasize the data
           (nxml-name                 ((t (:foreground "wheat3"))))
           (nxml-ref                  ((t (:foreground "wheat3"))))
           (nxml-element-prefix       ((t (:foreground "wheat3"))))
           (nxml-element-local-name   ((t (:foreground "wheat3"))))
           (nxml-attribute-prefix     ((t (:foreground "wheat3" :slant italic))))
           (nxml-attribute-local-name ((t (:foreground "wheat3" :slant italic))))
           (nxml-delimiter            ((t (:foreground "wheat3")))))))

(require 'org)
(use-package org
  :mode (("\\.org\\'" . org-mode))
  :defer t
  :bind (("C-c o a" . org-agenda)
         ("C-c o b" . org-iswitchb)
         ("C-c o c" . org-capture)
         ("C-c o l" . org-store-link))
  :config
  (setq org-agenda-files '("~/Documents/todo.org"
                           "~/Documents/greg.org"
                           "~/Documents/biomantica.org"
                           "~/README.org")
        org-startup-indented t
        org-todo-keyword-faces
        '(("TODO" . (:foreground "IndianRed" :weight bold))
          ("WAIT" . (:foreground "DarkOrange2" :weight bold))
          ("DONE" . (:foreground "SeaGreen" :weight normal))))
  (setq org-capture-templates
        '(("2" "deadline 2 days" entry (file+headline "~/Documents/todo.org" "Todo")
           "* TODO %a %?\nDEADLINE: %(org-insert-time-stamp (org-read-date nil t \"+2d\"))")))
  (setq-default org-catch-invisible-edits 'smart)
  (bind-key "C-c k" #'gh/insert-key org-mode-map))

;; (use-package org-present
;;   :load-path "~/src/elisp/org-present")

(use-package package-lint
  :ensure t)

(use-package paredit
  :ensure t
  :config
  (dolist (m '(clojure-mode-hook
               cider-repl-mode-hook
               emacs-lisp-mode-hook
               racket-mode-hook
               racket-repl-mode-hook))
    (add-hook m #'paredit-mode))
  (bind-keys :map paredit-mode-map
             ("{"   . paredit-open-curly)
             ("}"   . paredit-close-curly)
             ("C-)" . paredit-forward-slurp-sexp)
             ("C-(" . paredit-backward-slurp-sexp))
  (unless terminal-frame
    (bind-keys :map paredit-mode-map
               ("M-[" . paredit-wrap-square)
               ("M-{" . paredit-wrap-curly))))

(use-package paren-face
  :ensure t
  :config
  (setq paren-face-regexp (rx (any "()[]{}")))
  (add-to-list 'paren-face-modes 'racket-mode)
  (add-to-list 'paren-face-modes 'racket-repl-mode)
  (global-paren-face-mode))

(use-package projectile
  :ensure t
  :bind (:map projectile-command-map
              ("t" . gh/neotree-project-root)
              ("T" . projectile-toggle-between-implementation-and-test)
              ("s g" . deadgrep)
              ("s G" . projectile-grep))
  :init (projectile-global-mode)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  :config
  ;; Remove dead projects when Emacs is idle
  (run-with-idle-timer 10 nil #'projectile-cleanup-known-projects)

  (setq projectile-find-dir-includes-top-level t)

  ;; Ignore Racket bytecode dirs
  (add-to-list 'projectile-globally-ignored-directories "compiled")

  (projectile-register-project-type 'racket '("info.rkt")
                                    :compile "make setup"
                                    :test "make test")

  (defun gh/neotree-project-root (&optional directory)
    "Open a NeoTree browser for a project DIRECTORY."
    (interactive)
    (let ((default-directory (or directory default-directory)))
      (if (and (fboundp 'neo-global--window-exists-p)
               (neo-global--window-exists-p))
          (neotree-hide)
        (neotree-find (projectile-project-root)))))
  :diminish projectile-mode)

(use-package python
  :defer t
  :config (bind-keys :map python-mode-map
                     ("C-m" . newline-and-indent)))

(defvar gh/racket-mode-load-path
  (if wsl-p
      "/mnt/c/Users/greg/src/elisp/racket"
    "~/src/elisp/racket"))

(use-package racket-mode
  :load-path gh/racket-mode-load-path
  :init
  (apply
   #'custom-set-faces
   `((racket-keyword-argument-face ((t (:foreground "IndianRed3"))))))
  :config
  (cond (macosx-p
         (setq racket-program
               "/Applications/Racket_v6.10/bin/racket"
               ;;"/Applications/Racket_v7.0/bin/racket"
               ))
        (wsl-p
         ;; This config is actually for using Windows Subsystem for Linux
         ;; to run _Windows_ Racket.exe on _Windows_ files outside the
         ;; WSL file system.
         ;;
         ;; Otherwise, if Linux Racket is installed, could also run
         ;; _that_ with the usual vanilla Linux setup.
         (require 'racket-wsl)
         ;; Assumes we have /mnt/c/Program Files/Racket-7.2 on WSL PATH.
         ;; So all we need to do is specify "Racket.exe" not "racket".
         (setq racket-program "Racket.exe")
         ;; Then the path translation functions:
         (setq racket-path-from-emacs-to-racket-function #'racket-wsl-to-windows)
         (setq racket-path-from-racket-to-emacs-function #'racket-windows-to-wsl)
         ;; Only because we're developing racket-mode and have the source code
         ;; under /mnt/c, we also need this:
         (setq racket-adjust-run-rkt #'racket-wsl-to-windows))
        (linux-p
         (setq racket-program "~/racket/7.3/bin/racket")
         (setq racket-path-from-emacs-to-racket-function #'identity)
         (setq racket-path-from-racket-to-emacs-function #'identity)
         (setq racket-adjust-run-rkt #'identity))
        (mswindows-p
         (setq racket-program "C:\\Program Files\\Racket-7.2\\Racket.exe")))
  (setq racket-error-context 'medium)  ; 'high
  (diminish 'hs-minor-mode)
  (unless terminal-frame
    (bind-keys :map racket-mode-map
               ("M-]" . racket-align)
               ("M-}" . racket-unalign))))
(use-package scribble-mode
  :ensure t
  :defer t)


(use-package rainbow-delimiters
  :ensure t)

(use-package rust-mode
  :ensure t
  :defer t)
(use-package cargo
  :ensure t
  :defer t
  :config
  (add-hook 'rust-mode-hook #'cargo-minor-mode))
(use-package racer
  :ensure t
  :defer t
  :config
  (setq racer-cmd           "~/.cargo/bin/racer"
        racer-rust-src-path "~/src/rust/rust/src")
  (add-hook 'rust-mode-hook #'racer-mode)
  (add-hook 'racer-mode-hook #'eldoc-mode)
  (add-hook 'racer-mode-hook #'company-mode))
(use-package flycheck-rust
  :ensure t
  :defer t
  :config
  (add-hook 'flycheck-mode-hook #'flycheck-rust-setup))

(use-package re-builder
  :defer
  :config (setq reb-re-syntax 'rx)) ;I love using rx for regexps

(use-package smex
  :ensure t
  :bind (("M-x"   . smex)
         ("M-X"   . smex-major-mode-commands)
         ("C-x m" . smex)))

(use-package solarized
  :ensure solarized-theme
  :defer t
  :init
  (defun gh/solarized-theme-hook ()
    (set-face-attribute 'cursor nil :background "orange")
    (set-face-attribute 'font-lock-constant-face nil :weight 'normal)
    (set-face-attribute 'font-lock-function-name-face nil :weight 'bold)
    (set-face-attribute 'which-key-key-face nil :foreground
                        (face-attribute 'error :foreground))
    (set-face-attribute 'parenthesis nil :foreground
                        (face-attribute 'shadow :foreground)))
  (gh/add-theme-hook 'solarized-dark  #'gh/solarized-theme-hook)
  (gh/add-theme-hook 'solarized-light #'gh/solarized-theme-hook)
  :config
  (setq solarized-use-variable-pitch nil
        solarized-use-less-bold t       ;prefer italics
        solarized-use-more-italic nil
        solarized-distinct-doc-face t
        solarized-high-contrast-mode-line t
        ;; I find different font sizes irritating.
        solarized-height-minus-1 1.0
        solarized-height-plus-1 1.0
        solarized-height-plus-2 1.0
        solarized-height-plus-3 1.0
        solarized-height-plus-4 1.0))

(use-package tramp
  :config
  (setq tramp-default-method "ssh"))

(use-package uniquify
  :config (setq uniquify-buffer-name-style 'forward))

(use-package visual-fill-column
  ;; http://www.lunaryorn.com/posts/center-buffer-text-in-emacs.html
  :disabled t
  :ensure t
  :defer t
  :bind (("C-c t v" . visual-fill-column-mode))
  :init (dolist (hook '(visual-line-mode-hook
                        prog-mode-hook
                        text-mode-hook))
          (add-hook hook #'visual-fill-column-mode))
  :config (setq-default visual-fill-column-center-text t
                        visual-fill-column-fringes-outside-margins nil))

(use-package which-key
  :ensure t
  :init (which-key-mode)
  :config
  (setq which-key-idle-delay 0.4
        which-key-sort-order 'which-key-prefix-then-key-order
        ;; which-key-key-replacement-alist
        ;; '(("<\\([[:alnum:]-]+\\)>" . "\\1")
        ;;   ("up"                    . "↑")
        ;;   ("right"                 . "→")
        ;;   ("down"                  . "↓")
        ;;   ("left"                  . "←")
        ;;   ("DEL"                   . "⌫")
        ;;   ("deletechar"            . "⌦")
        ;;   ("RET"                   . "⏎"))
        which-key-description-replacement-alist
        '(("Prefix Command" . "prefix")
          ;; Lambdas
          ("\\`\\?\\?\\'"   . "λ")
          ;; Prettify hydra entry points
          ("/body\\'"       . " |=")
          ;; Drop my personal prefix
          ("\\`gh/"  . "")))
  (which-key-declare-prefixes
    ;; "C-c a" "applications"
    "C-c b" "buffers"
    "C-c c" "cursors"
    ;; "C-c e" "errors"
    "C-c f" "files"
    "C-c g" "git"
    "C-c h" "help"
    ;; "C-c i" "insert"
    ;; "C-c i l" "licenses"
    ;; "C-c j" "jump"
    ;; "C-c m" "major mode"
    "C-c o" "org"
    "C-c p" "projects"
    "C-c s" "search"
    "C-c t" "toggle"
    "C-c w" "windows/frames")

  (which-key-declare-prefixes-for-mode 'markdown-mode
    "C-c TAB" "markdown/images"
    "C-c C-a" "markdown/links"
    "C-c C-c" "markdown/process"
    "C-c C-s" "markdown/style"
    "C-c C-t" "markdown/header"
    "C-c C-x" "markdown/structure"
    "C-c m" "markdown/personal")

  (bind-key "C-c h b" #'describe-personal-keybindings)

  :diminish which-key-mode)

(use-package yaml-mode
  :ensure t
  :mode "\\.yml\\'")

(use-package yasnippet
  :disabled t
  :ensure t
  :diminish yas-minor-mode
  :config
  (yas-global-mode 1)
  (setq-default yas-prompt-functions '(yas-ido-prompt yas-dropdown-prompt)))

(use-package zenburn-theme
  :ensure t
  :defer t
  :init
  (defun gh/zenburn-theme-hook ()
    (set-face-attribute 'font-lock-function-name-face nil :weight 'bold)
    (set-face-attribute 'font-lock-keyword-face nil :weight 'normal)
    (set-face-attribute 'font-lock-builtin-face nil :weight 'normal)
    (set-face-attribute 'which-key-key-face nil :foreground
                        (face-attribute 'error :foreground)))
  (gh/add-theme-hook 'zenburn  #'gh/zenburn-theme-hook))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Other customizations

;;; mode line

(setq-default mode-line-format
              '("%e"
                mode-line-front-space
                mode-line-client
                mode-line-modified
                mode-line-remote
                mode-line-frame-identification
                mode-line-buffer-identification
                " " (vc-mode gh/vc-mode-line)
                " " mode-line-position
                " " mode-line-modes
                " " (:propertize (:eval mode-line-misc-info) face italic)
                mode-line-end-spaces))

(setq-default mode-line-buffer-identification
              (propertized-buffer-identification "%b"))

(setq-default mode-line-position
              '("("
                (-3 "%p")
                (size-indication-mode ("/ " (-4 "%I")))
                " "
                (line-number-mode ("%l" (column-number-mode ":%c")))
                ")"))

(defconst gh/vc-mode-line
  '((:propertize
     (:eval (format "%s" (all-the-icons-octicon "git-branch")))
     face (:height 1.0 :family "github-octicons")
     display (raise -0.1))
    (:propertize
     ;; Strip the prefix (e.g. "Git-") leaving just the branch name.
     (:eval (let ((backend (symbol-name (vc-backend (buffer-file-name)))))
              (substring vc-mode (+ (length backend) 2))))
     face bold-italic))
  "Mode line format for VC Mode: ' on <branch-name>'.")
(put 'gh/vc-mode-line 'risky-local-variable t)

(setq display-time-load-average-threshold nil)
;; (setq-default battery-mode-line-format " %b%p%%/%t")
;; (display-battery-mode 1)
(setq display-time-format "%b %-d %-l:%M") ;see `format-time-string'
(display-time)

;; multi monitor

(defun gh/set-font-height (height)
  (interactive
   (let* ((height (read-from-minibuffer "Height: " nil nil nil nil "90"))
          (height (string-to-number height)))
     (list height)))
  (set-face-attribute 'default (selected-frame) :height height))

(defun gh/set-font-height-for-current-monitor ()
  (interactive)
  (let* ((as  (frame-monitor-attributes (selected-frame)))
         (geo (assq 'geometry as))
         (h   (nth 4 geo)))
    ;; This is completely specific to my LG 4K secondary monitor.
    (gh/set-font-height (cond ((>= h 2160) 160) ;LG not HiDPI
                              ((>= h 1080)  80) ;LG HiDPI
                              (t           120)))))
(bind-key "C-c w H" #'gh/set-font-height)

(defun gh/other-monitor-workarea ()
  (let ((xs (display-monitor-attributes-list)))
    (cdr (assq 'workarea
               (or (cl-some (lambda (x)
                              (if (member (selected-frame) (assq 'frames x))
                                  nil x))
                            xs)
                   (car xs))))))

(defun gh/move-frame-to-other-monitor ()
  "Move Emacs frame (OS window) to another monitor.
   Preserve full-screen mode."
  (interactive)
  (let ((fullp (memq (frame-parameter nil 'fullscreen) '(fullscreen fullboth))))
    (when fullp (toggle-frame-fullscreen))
    (sit-for 1)
    (pcase (gh/other-monitor-workarea)
      (`(,x ,y ,cx ,cy)
       (set-frame-position (selected-frame) x y)))
    (sit-for 0.1)
    (gh/set-font-height-for-current-monitor)
    (sit-for 1)
    (when fullp (toggle-frame-fullscreen))))
(bind-key "C-c w O" #'gh/move-frame-to-other-monitor)

;; imenu
(defconst gh/emacs-lisp-mode-imenu-generic-expression
  `((nil
     ,(rx bol (* (syntax -))
          "(def" (or "un" "subst" "macro" "advice")
          (+ (syntax -))
          (group (+ (syntax word) (syntax symbol))))
     1)
    ("*Vars*"
     ,(rx bol (* (syntax -))
          "(def" (or "var" "const")
          (+ (syntax -))
          (group (+ (syntax word) (syntax symbol))))
     1)
    ("*Types*"
     "^\\s-*\
          (def\\(type\\|struct\\|class\\|ine-condition\\)\
          \\s-+\\([-A-Za-z0-9+]+\\)"
     ,(rx bol (* (syntax -))
          "(def" (or "type" "struct" "class" "ine-condition")
          (+ (syntax -))
          (group (+ (syntax word) (syntax symbol))))
     1)))

(defun gh/emacs-lisp-mode-imenu-hook ()
  (setq imenu-generic-expression gh/emacs-lisp-mode-imenu-generic-expression))

;; From http://dfan.org/blog/2009/02/19/emacs-dedicated-windows/
;;;###autoload
(defun gh/toggle-current-window-dedication ()
  "Toggle dedication state of a window."
  (interactive)
  (let* ((window    (selected-window))
         (dedicated (window-dedicated-p window)))
    (set-window-dedicated-p window (not dedicated))
    (message "Window %sdedicated to %s"
             (if dedicated "no longer " "")
             (buffer-name))))

;; Previous and next window.

(defun gh/next-window (count)
  "Alias for `other-window'."
  (interactive "p")
  (other-window count))

(defun gh/prev-window (count)
  "Like `other-window' but negated COUNT."
  (interactive "p")
  (other-window (- count)))

;; Window splits that move to the new window

(defun gh/split-window-below ()
  (interactive)
  (split-window-below)
  (other-window 1))

(defun gh/split-window-right ()
  (interactive)
  (split-window-right)
  (other-window 1))


;; Splitter drag

(require 'windmove)

(defun gh/find-other-window (dir)
  (let* ((windmove-wrap-around nil)
         (win (windmove-find-other-window dir)))
    (and win
         (not (or (minibufferp (window-buffer win))
                  (window-preserved-size win
                                         (member dir '(left right)))))
         win)))

(defun gh/move-splitter (dir arg)
  (pcase-let
      ((`(,enlarge . ,shrink)
        (case dir
          ((left right) (cons #'enlarge-window-horizontally
                              #'shrink-window-horizontally))
          ((up   down)  (cons #'enlarge-window
                              #'shrink-window))
          (otherwise    (error "unknown dir: %s" dir)))))
    (funcall (if (gh/find-other-window dir) enlarge shrink)
             arg)))

(defhydra gh/splitter-drag-hydra ()
  ("h" (gh/move-splitter 'left  1) "left")
  ("j" (gh/move-splitter 'down  1) "down")
  ("k" (gh/move-splitter 'up    1) "up")
  ("l" (gh/move-splitter 'right 1) "right")
  ("o" gh/next-window "next window") ;like C-x o
  ("n" gh/next-window "next window")
  ("p" gh/prev-window "prev window")
  ("=" balance-windows "balance")
  ("RET" nil))

;; Themes

(defhydra gh/themes-hydra (:hint nil)
  "
Themes

^Solarized^   ^Material^   ^Other^
----------------------------------------------------
_s_: Dark     _m_: Dark    _z_: Zenburn  _DEL_: none
_S_: Light    _M_: Light   _g_: Greg
"
  ("s" (load-theme 'solarized-dark  t))
  ("S" (load-theme 'solarized-light t))
  ("m" (load-theme 'material        t))
  ("M" (load-theme 'material-light  t))
  ("z" (load-theme 'zenburn         t))
  ("g" (load-theme 'greg            t))
  ("DEL" (gh/disable-all-themes))
  ("RET" nil "done" :color blue))


;;; Various window and buffer key bindings

(bind-keys ("C-x o"   . gh/next-window)
           ("C-x p"   . gh/prev-window)
           ("C-."     . gh/next-window)
           ("C-,"     . gh/prev-window)

           ("C-x 1"   . delete-other-windows-vertically)
           ("C-x 2"   . gh/split-window-below)
           ("C-x 3"   . gh/split-window-right)

           ("C-c w =" . balance-windows)
           ("C-c w k" . delete-window)
           ("C-c w /" . gh/split-window-right)
           ("C-c w -" . gh/split-window-below)
           ("C-c w s" . gh/splitter-drag-hydra/body)
           ("C-c w m" . delete-other-windows)
           ("C-c w d" . gh/toggle-current-window-dedication)
           ("C-c w t" . gh/themes-hydra/body)

           ("C-c b B" . ibuffer))

(bind-keys :map org-mode-map ("C-," . gh/prev-window))

;; "Fix" linum display problem on OS X
(setq linum-format "  %d ")

(defun gh/goto-line-with-feedback ()
  "Show line numbers temporarily, while prompting for the line number input."
  (interactive)
  (unwind-protect
      (progn
        (linum-mode 1)
        (goto-line (read-number "Goto line: ")))
    (linum-mode -1)))
(bind-key [remap goto-line] #'gh/goto-line-with-feedback)

(defun gh/isearch-yank-symbol ()
  "Yank the symbol at point into the isearch minibuffer.

C-w does something similar in isearch, but it only looks for
the rest of the word. I want to look for the whole string. And
symbol, not word, as I need this for programming the most.

http://blog.jorgenschaefer.de/2012/11/emacs-search-for-symbol-at-point.html"
  (interactive)
  (isearch-yank-string
   (save-excursion
     (when (and (not isearch-forward)
                isearch-other-end)
       (goto-char isearch-other-end))
     (thing-at-point 'symbol))))
(bind-key "C-d" #'gh/isearch-yank-symbol isearch-mode-map)

(defun gh/multi-occur-in-this-mode ()
  "Show all lines matching REGEXP in buffers with this major mode.

http://www.masteringemacs.org/articles/2011/07/20/searching-buffers-occur-mode/"
  (interactive)
  (multi-occur (let ((first-major-mode major-mode)
                     (buffer-mode-matches nil))
                 (dolist (buf (buffer-list))
                   (with-current-buffer buf
                     (if (eq first-major-mode major-mode)
                         (add-to-list 'buffer-mode-matches buf))))
                 buffer-mode-matches)
               (car (occur-read-primary-args))))
(bind-key "C-c s o" #'gh/multi-occur-in-this-mode)
;; I never use the 2-column commands on <f2>. Bind to `rgrep' instead.
(bind-key "C-c s g" #'rgrep)

(defun gh/narrow-or-widen-dwim (p)
  "If the buffer is narrowed, it widens. Otherwise, it narrows intelligently.
Intelligently means: region, subtree, or defun, whichever applies
first.

With prefix P, don't widen, just narrow even if buffer is already
narrowed.

http://endlessparentheses.com/emacs-narrow-or-widen-dwim.html"
  (interactive "P")
  (declare (interactive-only))
  (cond ((and (buffer-narrowed-p) (not p))
         (widen))
        ((region-active-p)
         (narrow-to-region (region-beginning) (region-end)))
        ((and (derived-mode-p 'org-mode)
              (fboundp 'org-narrow-to-subtree))
         (org-narrow-to-subtree))
        (t (narrow-to-defun))))

(defun gh/insert-key (key)
  "Prompt to type a key sequence. Insert its description as HTML <kbd> element.

Credit: <http://emacs.stackexchange.com/questions/2206/i-want-to-have-the-kbd-tags-for-my-blog-written-in-org-mode>

But simplified because I only use org-mode to take notes that
might later end up in markdown-mode blog post."
  (interactive "kType key sequence: ")
  (let ((tag "<kbd>%s</kbd>"))
    (if (null (equal key "\r"))
        (insert
         (format tag (help-key-description key nil)))
      (insert (format tag ""))
      (forward-char -6))))

(defun gh/smarter-move-beginning-of-line (arg)
  "Move point back to indentation of beginning of line.

Move point to the first non-whitespace character on this line.
If point is already there, move to the beginning of the line.
Effectively toggle between the first non-whitespace character and
the beginning of the line.

If ARG is not nil or 1, move forward ARG - 1 lines first.  If
point reaches the beginning or end of the buffer, stop there.

Credit: <http://emacsredux.com/blog/2013/05/22/smarter-navigation-to-the-beginning-of-a-line/>"
  (interactive "^p")
  (setq arg (or arg 1))

  ;; Move lines first
  (when (/= arg 1)
    (let ((line-move-visual nil))
      (forward-line (1- arg))))

  (let ((orig-point (point)))
    (back-to-indentation)
    (when (= orig-point (point))
      (move-beginning-of-line 1))))

(bind-key "C-a" #'gh/smarter-move-beginning-of-line)

(defun gh/comment-box (b e)
  "Draw a box comment around the region but arrange for the region
to extend to at least the fill column. Place the point after the
comment box.

Credit: <http://irreal.org/blog/?p=374>"
  (interactive "r")
  (let ((e (copy-marker e t)))
    (goto-char b)
    (end-of-line)
    (insert-char ?  (- fill-column (current-column)))
    (comment-box b e 1)
    (goto-char e)
    (set-marker e nil)))

(defun gh/toggle-show-trailing-whitespace ()
  (interactive)
  (setq show-trailing-whitespace (not show-trailing-whitespace))
  (redraw-display)
  (message "show-trailing-whitespace %s" show-trailing-whitespace))

(bind-keys ("C-c t c" . column-number-mode)
           ("C-c t d" . toggle-debug-on-error)
           ("C-c t f" . auto-fill-mode)
           ("C-c t l" . toggle-truncate-lines)
           ("C-c t n" . gh/narrow-or-widen-dwim)
           ("C-c t q" . toggle-debug-on-quit)
           ("C-c t v" . visual-line-mode)
           ("C-c t w" . gh/toggle-show-trailing-whitespace))

(defun gh/isearch-delete-something ()
  "Delete non-matching text or the last character.

Credit: <https://gist.github.com/johnmastro/508fb22a2b4e1ce754e0>"
  ;; Mostly copied from `isearch-del-char' and Drew's answer on the page above
  (interactive)
  (if (= 0 (length isearch-string))
      (ding)
    (setq isearch-string
          (substring isearch-string
                     0
                     (or (isearch-fail-pos) (1- (length isearch-string)))))
    (setq isearch-message
          (mapconcat #'isearch-text-char-description isearch-string "")))
  (if isearch-other-end (goto-char isearch-other-end))
  (isearch-search)
  (isearch-push-state)
  (isearch-update))

(bind-key [remap isearch-delete-char]
          #'gh/isearch-delete-something
          isearch-mode-map)

(defun gh/rotate-windows ()
  "Rotate your windows.

Credit: <http://whattheemacsd.com/buffer-defuns.el-02.html>"
  (interactive)
  (let ((num-windows (count-windows))
        (i 1))
    (unless (< 1 num-windows)
      (user-error "You can't rotate a single window"))
    (while  (< i num-windows)
      (let* ((w1 (elt (window-list) i))
             (w2 (elt (window-list) (+ (% i num-windows) 1)))
             (b1 (window-buffer w1))
             (b2 (window-buffer w2))
             (s1 (window-start w1))
             (s2 (window-start w2)))
        (set-window-buffer w1  b2)
        (set-window-buffer w2 b1)
        (set-window-start w1 s2)
        (set-window-start w2 s1)
        (setq i (1+ i))))))
(defhydra gh/hydra-rotate-windows (:body-pre (gh/rotate-windows))
  ("r" gh/rotate-windows "rotate")
  ("RET" nil))
(bind-key "C-c w r" #'gh/hydra-rotate-windows/body)

(defun gh/quit-other-window ()
  "Quit other window, such as *Help*"
  (interactive)
  (other-window 1)
  (quit-window))
(bind-key "C-c w q" #'gh/quit-other-window)

;; Local Variables:
;; fill-column: 78
;; End:

;;; init.el ends here
