;;; init.el --- Emacs init file  -*- lexical-binding: t; -*-

;; OS
(defvar mswindows-p (string-match "windows" (symbol-name system-type)))
(defvar macosx-p (string-match "darwin" (symbol-name system-type)))
(defvar linux-p (and (not mswindows-p) (not macosx-p)))

(eval-when-compile
  (require 'cl-macs))

(when linux-p
  (set-frame-font "IBM Plex Mono 12" nil t t))

(setq text-scale-mode-step 1.1)         ;finer inc/dec than default 1.2

(setq-default cursor-type 'box)
(setq-default cursor-in-non-selected-windows 'hollow)
(setq blink-cursor-alist '((box . bar)))
(set-face-attribute 'cursor nil :background "orange")
(blink-cursor-mode 1)
(when (boundp 'blink-cursor-blinks)
  (setq blink-cursor-blinks 4))

(setq custom-file "~/.emacs.d/custom.el")
(load custom-file t)

;; Misc tweaks

;; I hit this accidentally and I never use fill-prefix.
(keymap-unset ctl-x-map "." t)
;; I hit this accidentally when typing too fast and meaning "C-x b" for
;; switch-to-buffer.
(keymap-unset ctl-x-map "C-b" t)

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
        (`(,_ . ,f) (funcall f)))
      ;; Regardless of theme, use consistent family and size for
      ;; mode line faces.
      (dolist (f '(mode-line mode-line-inactive))
        (set-face-attribute f nil
                            :family "Noto Sans"
                            :height 'unspecified)))))

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
(setq make-backup-files nil)            ;stop creating those backup~ files
(setq auto-save-default nil)            ;stop creating those #autosave# files
(setq-default major-mode 'text-mode)
(setq-default indent-tabs-mode nil)     ;spaces instead of TAB chars
(setq-default tab-width 4)              ;existing files with TABs get width of 4
(add-hook 'emacs-lisp-mode-hook         ;but Emacs' Elisp wants 8 for alignment
          (lambda () (setq tab-width 8)))
(setq tab-always-indent 'complete)
(setq completion-cycle-threshold nil)
(setq echo-keystrokes 0.1)              ;show keys immediately
(setq scroll-down-aggressively 0.1)
(setq scroll-up-aggressively 0.1)
(setq help-window-select t)             ;so I can scroll etc. then nav back or quit
(delete-selection-mode 1)               ;especially nice with expand-region

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

(require 'rx)
(defun gh/set-page-delimiter ()
  (setq-local page-delimiter (rx bol (or "" ";;; "))))
(dolist (m '(clojure-mode-hook
             emacs-lisp-mode-hook
             racket-mode-hook))
  (add-hook m #'gh/set-page-delimiter))


;;; Comint

;; From https://github.com/grettke/lolsmacs/blob/master/lolsmacs.el
;;
;; Here is the scenario for this style of Comint configuration:
;;
;; You are doing a lot of interactive work via various Comint-supported
;; buffers. You are working in one buffer (the one with focus) while the
;; others are doing their own thing. They are probably doing work and output
;; is scrolling by and that is fine because you are not reading it. In the
;; buffer you are working in though, you want to go back and read something.
;; So although it its process continues to output information, you want to
;; keep the cursor in the same spot. Then when you are ready to type a
;; command (suppose you know the output has stopped) to do something else,
;; when you type the cursor will go to the end of the buffer. That is why
;; you prevent the focused buffer from auto-scrolling and moving the mark,
;; and leave the other ones alone.
(setq comint-scroll-to-bottom-on-input 'this)
(setq comint-scroll-to-bottom-on-output 'others)
(setq comint-move-point-for-output 'others)
(setq comint-scroll-show-maximum-output t)
(setq comint-prompt-read-only nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; use-package

(require 'package)
(setq package-enable-at-startup nil) ;for `use-package' :defer
(setq package-archives '(("elpa" .  "https://elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")))
(package-initialize)

;; In newer Emacs use-package is built in. So don't use `package-installed-p'
;; here. Instead `require` with noerror, and check if that worked.
(require 'use-package nil t)
(unless (memq 'use-package features)
  (message "use-package not built in or installed; trying to install")
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)
(require 'bind-key)

(use-package exec-path-from-shell ;do this early
  :if macosx-p
  :ensure t
  :init (exec-path-from-shell-initialize))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Packages, alphabetically.

;; Use `use-package' for built-in packages, too -- just omit :ensure.
;; And for stuff installed from source, can use :load-path.

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

(use-package buffer-face-mode
  :if macosx-p
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
    (add-hook hook #'gh/buffer-face-variable)))

(use-package calendar
  ;; I'm using this only for history from exported Gmail calendar.
  ;; For new/active appointments and events, using calendar.org.
  :config (setq diary-file "~/Documents/diary"))

;; (use-package company
;;   :ensure t
;;   :defer t
;;   :diminish company-mode
;;   ;; The following for testing racket-mode/issues/318
;;   :config
;;   (setq company-idle-delay 0.1
;;         company-minimum-prefix-length 2
;;         company-tooltip-align-annotations t
;;         company-show-numbers t
;;         company-require-match nil))

;; (use-package company-quickhelp
;;   :ensure t
;;   :after company)

(use-package compilation-mode
  ;; built-in
  :defer t
  :init (setq compilation-scroll-output 'next-error))

(use-package consult
  :ensure t
  :bind (("C-c i" . consult-imenu))
  :init
  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref))

(use-package context-menu-mode
  ;; built-in
  :defer t
  :init (context-menu-mode 1))

(use-package copyright
  ;; built-in
  :init (setq copyright-names-regexp "Greg Hendershott"
              copyright-year-ranges t))

(use-package dash
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

(use-package doom-themes
  :ensure t
  :defer t
  :init
  (defun gh/nord-theme-hook ()
    (doom-themes-org-config) ;this is nice but...
    (org-bullets-mode 0)    ;need to toggle this off...
    (org-bullets-mode 1)    ;and on again, afterwards
    (doom-themes-neotree-config)
    (setq doom-themes-neotree-file-icons t)) ;not default 'simple
  (gh/add-theme-hook 'doom-nord       #'gh/nord-theme-hook)
  (gh/add-theme-hook 'doom-nord-light #'gh/nord-theme-hook))

(use-package eink-theme
  :ensure t
  :defer t
  :init
  (defun gh/eink-theme-hook ()
    ;; I need more contrast in active vs inactive
    (set-face-attribute 'mode-line nil
                        :background "grey75")
    ;; Um I need fringe for step debug, error position, etc.
    (set-face-attribute 'fringe nil
                        :foreground (face-attribute 'default :foreground)
                        :background (face-attribute 'default :background))
    ;; A /little/ less monochromatic: I like function names to be bold, but
    ;; not comments.
    (set-face-attribute 'font-lock-function-name-face nil
                        :weight 'bold)
    (set-face-attribute 'cursor nil
                        :background "orange")
    (dolist (face '(font-lock-comment-face
                    font-lock-doc-face))
      (set-face-attribute face nil
                          :weight 'normal
                          :slant 'normal
                          :foreground (face-attribute
                                       'font-lock-comment-delimiter-face
                                       :foreground)))
    ;; Use color for strings and constants.
    (dolist (face '(font-lock-string-face
                    font-lock-constant-face))
      (set-face-attribute face nil
                          :foreground "SeaGreen")))
  (gh/add-theme-hook 'eink #'gh/eink-theme-hook))

(use-package eldoc-box
  :ensure t
  :config
  (setopt eldoc-box-lighter nil)
  (set-face-attribute 'eldoc-box-body nil
                      :inherit 'tooltip
                      :height 0.8)
  :hook
  ((prog-mode . eldoc-box-hover-mode)))

(use-package elisp-slime-nav
  :ensure t
  :diminish 'elisp-slime-nav-mode
  :init (dolist (hook '(emacs-lisp-mode-hook ielm-mode-hook))
          (add-hook hook #'turn-on-elisp-slime-nav-mode)))

(use-package envrc
  :ensure t)

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

(use-package forge
  ;; FIXME: Having trouble with magit-status (used to work on old Emacs on
  ;; Ubuntu 18.04) due to issues with sqlite.
  :disabled t
  :ensure t
  :after magit)

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

(use-package highlight-indent-guides
  :ensure t
  :defer t)

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

(use-package imenu
  :bind (("C-c i" . imenu)))

;; See `marginalia' for completion matching styles configuration.
(use-package vertico
  :ensure t
  :init (vertico-mode))

(use-package savehist
  :init (savehist-mode))

;; I want differing completion styles. For instance with `completion-at-point'
;; I want 'basic' style. [I'm very used to typing a prefix and hitting TAB,
;; maybe typing more to narrow it down and TAB-ing again.]
;;
;; The Emacs completion machinery seems to lack any way to specify a style to
;; use when no category metadata is specified.
;;
;; Many (most?) CAPFs don't supply a category! Argh.
;;
;; So use `maginalia' for its ability to add category metadata in many more
;; cases (it advises `completion-metadata-get'; see also
;; `marginalia-annotator-registry' and `marginalia-classifiers'). This is my
;; original motivation to use marginalia at all; the annotations are so far
;; IMHO just a nice-to-have.
(use-package marginalia
  :ensure t
  :bind (:map minibuffer-local-map
              ("M-A" . marginalia-cycle))
  :init
  (marginalia-mode)
  ;; Default the completion style for most categories.
  (setq completion-styles '(basic initials flex))
  ;; Then override the style for the `file' category, plus use `basic' style
  ;; for various programming categories where I want that in the minibuffer
  ;; and especially for `completion-at-point'.
  (setq completion-category-overrides
        `((file (styles basic partial-completion))
          (racket-identifier (styles basic))
          (symbol (styles basic))
          (variable (styles basic))
          (function (styles basic))))
  ;; Ignore case for completions.
  (setq completion-ignore-case t)
  (setq read-file-name-completion-ignore-case t)
  (setq read-buffer-completion-ignore-case t))

(use-package corfu
  :ensure t
  :config
  (global-corfu-mode)
  (corfu-popupinfo-mode))

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
  :diminish 'auto-revert-mode
  :config
  (unbind-key "C-<tab>" magit-section-mode-map) ;want next-tab instead
  (setq magit-completing-read-function #'magit-ido-completing-read)
  (setq magit-diff-use-overlays nil)
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
    (cl-loop for n from 1 to 8
             do (set-face-attribute (intern-soft (format "org-level-%s" n))
                                    nil
                                    :height     'unspecified
                                    :background 'unspecified
                                    :box        'unspecified)))
  (gh/add-theme-hook 'material       #'gh/material-theme-hook)
  (gh/add-theme-hook 'material-light #'gh/material-theme-hook))

(use-package mmm-mode
  :ensure t)

(use-package mu4e
  :if linux-p
  :load-path "/usr/share/emacs/site-lisp/elpa/mu4e/"
  :config
  (bind-key "C-c a m" #'mu4e)
  (bind-keys :map mu4e-headers-mode-map
             ("C-c C-c" . mu4e-org-store-and-capture))
  (bind-keys :map mu4e-view-mode-map
             ("C-c C-c" . mu4e-org-store-and-capture))
  ;; I'm using C-<tab> for tab-bar-mode
  (unbind-key "C-<tab>" mu4e-thread-mode-map)

  (setq mail-user-agent 'mu4e-user-agent)

  (setq mu4e-maildir "/home/greg/Maildir")

  (setq mu4e-trash-folder  "/greg/Trash")
  (setq mu4e-drafts-folder "/greg/Drafts")
  (setq mu4e-sent-folder   "/greg/Sent")
  (setq mu4e-sent-messages-behavior 'sent)

  (setq mu4e-attachment-dir  "~/Downloads") ;not ~/

  ;; Rename files when moving -- needed with mbsync to avoid duplicate UID
  ;; errors!
  (setq mu4e-change-filenames-when-moving t)

  (setq mu4e-maildir-shortcuts
        '(("/greg/INBOX"  . ?i)
          ("/greg/Drafts" . ?d)
          ("/greg/Sent"   . ?s)
          ("/greg/Trash"  . ?t)
          ("/archive"     . ?a)))

  (setq mu4e-get-mail-command "mbsync greg")

    ;;; Sending
  (require 'smtpmail)
  (setq message-send-mail-function 'smtpmail-send-it
        starttls-use-gnutls t
        smtpmail-starttls-credentials '(("smtp.fastmail.com" 587 nil nil))
        smtpmail-auth-credentials '(("smtp.fastmail.com" 587 "mail@greghendershott.com" nil))
        smtpmail-default-smtp-server "smtp.fastmail.com"
        smtpmail-smtp-server "smtp.fastmail.com"
        smtpmail-smtp-service 587)

  (setq mu4e-user-mail-address-list '("mail@greghendershott.com"
                                      "git@greghendershott.com"
                                      "racket@greghendershott.com"
                                      "greghendershott@gmail.com"
                                      "greghendershott@yahoo.com"
                                      "greghendershott@hotmail.com"))
  (setq user-mail-address "mail@greghendershott.com"
        user-full-name    "Greg Hendershott")
  (setq message-signature nil)

  (setq mu4e-compose-dont-reply-to-self t)

  ;; customize the reply-quote-string
  (setq message-citation-line-format
        "\nOn %a %d %b %Y at %R, %f wrote:")
  ;; choose to use the formatted string
  (setq message-citation-line-function
        'message-insert-formatted-citation-line)

  (add-hook 'mu4e-view-mode-hook 'visual-line-mode)

  ;; https://github.com/djcb/mu/issues/2337
  (with-eval-after-load "mm-decode"
    (add-to-list 'mm-discouraged-alternatives "text/html")
    (add-to-list 'mm-discouraged-alternatives "text/richtext")
    (add-to-list 'mm-discouraged-alternatives "multipart/related"))

  ;; rice
  (setq mu4e-view-show-addresses t)
  (setq mu4e-use-fancy-chars nil)
  (setq mu4e-headers-fields '( (:human-date     .   12)
                               (:flags          .    6)
                                        ;(:mailing-list   .   11)
                               (:from           .   16)
                               (:thread-subject .   nil)))
  ;; TODO: Add this to make it apparent to whom the email was sent,
  ;; e.g. info@greghendershott.com spam.
  ;;
  ;; (add-to-list 'mu4e-view-fields
  ;;              '(:X-Delivered-To .
  ;;                                (:name
  ;;                                 "X-Delivered-To"
  ;;                                 :function
  ;;                                 (lambda (msg)
  ;;                                   (or (mu4e-message-field msg :X-Delivered-To)
  ;;                                       "")))))

  ;; don't keep message buffers around
  (setq message-kill-buffer-on-exit t)

  ;; Contexts
  (defun gh/racket-mailing-list-p (msg)
    "Is MSG from one of the Racket lists?"
    (and (member (mu4e-message-field msg :mailing-list)
                 '("racket-users.googlegroups.com"
                   "racket-dev.googlegroups.com"
                   "racket-money.googlegroups.com"))
         t))
  (setq mu4e-contexts
        `(,(make-mu4e-context
            :name "Personal"
            :vars '((user-mail-address . "mail@greghendershott.com"))
            :match-func (lambda (msg)
                          (and msg (not (gh/racket-mailing-list-p msg)))))
          ,(make-mu4e-context
            :name "Racket"
            :vars '((user-mail-address . "racket@greghendershott.com"))
            :match-func (lambda (msg)
                          (and msg (gh/racket-mailing-list-p msg))))))
  (setq mu4e-context-policy 'pick-first)
  (setq mu4e-compose-context-policy 'ask))

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
  :custom-face
  (neo-file-link-face ((t (:inherit default))))
  (neo-dir-link-face ((t (:inherit dired-directory)))))

(use-package nxml-mode
  :custom-face
  ;; De-emphasize the tag, emphasize the data
  (nxml-name                 ((t (:foreground "wheat3"))))
  (nxml-ref                  ((t (:foreground "wheat3"))))
  (nxml-element-prefix       ((t (:foreground "wheat3"))))
  (nxml-element-local-name   ((t (:foreground "wheat3"))))
  (nxml-attribute-prefix     ((t (:foreground "wheat3" :slant italic))))
  (nxml-attribute-local-name ((t (:foreground "wheat3" :slant italic))))
  (nxml-delimiter            ((t (:foreground "wheat3")))))

(use-package org
  :mode (("\\.org\\'" . org-mode))
  :defer t
  :bind (("C-c o a" . org-agenda)
         ("C-c o b" . org-iswitchb)
         ("C-c o c" . org-capture)
         ("C-c o l" . org-store-link))
  :config
  (setq org-agenda-window-setup 'current-window)
  (setq org-agenda-files '("~/Documents/todo.org"
                           "~/Documents/calendar.org"
                           "~/Documents/greg.org"
                           "~/README.org")
        org-default-notes-file "~/Documents.greg.org")
  (setq org-agenda-start-on-weekday 1)
  (setq org-agenda-span 14)
  (setq org-agenda-prefix-format
        '((agenda   . "  %?-12t% s")
          (timeline . "  % s")
          (todo     . "")
          (tags     . "")
          (search   . "")))
  (setq org-deadline-warning-days 0)
  (setq org-capture-templates
        '(("t" "todo no deadline" entry (file+headline "~/Documents/todo.org" "Todo")
           "* TODO %? %a")
          ("0" "todo 0d deadline" entry (file+headline "~/Documents/todo.org" "Todo")
           "* TODO %? %a\nDEADLINE: %(org-insert-time-stamp (org-read-date nil t \"+0d\"))")
          ("2" "todo 2d deadline" entry (file+headline "~/Documents/todo.org" "Todo")
           "* TODO %? %a\nDEADLINE: %(org-insert-time-stamp (org-read-date nil t \"+2d\"))")
          ;; FIXME: improve org-read-date getting _time_ out of selected text
          ("c" "calendar item" entry (file+headline "~/Documents/calendar.org" "Items")
           "* %? %a %(org-insert-time-stamp (org-read-date t t \"%i\"))")
          ;; For use by `org-protocol-capture'.
          ("n" "misc note" entry (file+headline "~/Documents/greg.org" "Web Captures")
           "* %:annotation\n%u\n#+BEGIN_QUOTE\n%i\n#+END_QUOTE\n")))
  (setq org-protocol-default-template-key "n")
  (setq org-startup-indented t)
  (setq org-todo-keyword-faces
        '(("TODO" . (:foreground "IndianRed" :weight bold))
          ("WAIT" . (:foreground "DarkOrange2" :weight bold))
          ("DONE" . (:foreground "SeaGreen" :weight normal))))
  (setq org-ellipsis " ⋯ ")
  (setq-default org-catch-invisible-edits 'smart)
  (bind-key "C-c k" #'gh/insert-key org-mode-map))

(use-package org-protocol
  :after org
  :config (server-start))

(use-package org-bullets
  :ensure t
  :init (add-hook 'org-mode-hook 'org-bullets-mode)
  :config (setq org-bullets-bullet-list '("▸")))

(use-package org-present
  ;; Note: When I installed from ELPA, it wanted a newer version of Org, which
  ;; led to some other problems. But AFAICT it doesn't need a newer version of
  ;; Org, and it's just a single .el file, so just git clone it.
  :bind (("C-c o p" . org-present))
  :load-path "~/src/elisp/org-present")

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
               ("M-{" . paredit-wrap-curly)))
  ;; Newer versions of paredit bind these, but that's bad in interactive modes
  ;; like racket-repl-mode.
  (dolist (k '("RET" "C-m" "C-j"))
    (define-key paredit-mode-map (kbd k) nil)))

(use-package paren-face
  :ensure t
  :config
  (setq paren-face-regexp (rx (any "()[]{}")))
  (add-to-list 'paren-face-modes 'racket-mode)
  (add-to-list 'paren-face-modes 'racket-repl-mode)
  (global-paren-face-mode))

(use-package project
  :bind-keymap ("C-c p" . project-prefix-map)
  :bind (:map project-prefix-map
              ("t" . gh/toggle-neotree-for-project)
              ("g" . deadgrep))
  :custom
  (project-switch-use-entire-map nil)
  (project-switch-commands '((project-find-file "Find file")
                             (deadgrep "deadgrep")))
  :init
  (remove-hook 'project-find-functions #'project-try-vc)
  (add-hook 'project-find-functions #'gh/find-project)
  ;; Tell `project-ignores' to ignore Racket bytecode dirs. Another option is
  ;; to add globs to `grep-find-ignored-files'.
  (add-to-list 'vc-directory-exclusion-list "compiled"))

(defun gh/project-root (start-dir)
  "Find project directory, if any, dominating START-DIR."
  (locate-dominating-file
   start-dir
   (lambda (candidate-dir)
     (seq-some (lambda (marker-file)
                 (and (file-exists-p
                       (expand-file-name marker-file candidate-dir))
                      (file-name-directory (expand-file-name candidate-dir))))
               '(".git" ".projectile")))))

(defun gh/find-project (dir)
  "Create a transient project dominating DIR."
  (when-let ((root (gh/project-root dir)))
    (cons 'transient root)))

(defun gh/toggle-neotree-for-project (&optional directory)
  "Toggle a NeoTree browser for root of project containing DIRECTORY."
  (interactive)
  (if (and (fboundp 'neo-global--window-exists-p)
           (neo-global--window-exists-p))
      (neotree-hide)
    (let ((dir (or directory default-directory)))
      (if-let (root (gh/project-root dir))
          (neotree-find root)
        (user-error "can't find project root for %S" dir)))))

(use-package python
  :defer t
  :config (bind-keys :map python-mode-map
                     ("C-m" . newline-and-indent)))

(use-package racket-mode
  :load-path "~/src/elisp/racket-mode"
  :custom-face
  (racket-keyword-argument-face ((t (:foreground "IndianRed3"))))
  :config
  ;; Use racket-pdb-mode when available (on the "pdb" branch) else
  ;; racket-xp-mode.
  (if (require 'racket-pdb "racket-pdb.el" t)
      (progn
        (add-hook 'racket-mode-hook #'racket-pdb-mode)
        (add-hook 'racket-hash-lang-mode-hook #'racket-pdb-mode)
        (bind-keys :map racket-pdb-mode-map
                   ("M-n" . racket-pdb-next-use)
                   ("M-p" . racket-pdb-previous-use)))
    (require 'racket-xp)
    (add-hook 'racket-mode-hook #'racket-xp-mode)
    (add-hook 'racket-hash-lang-mode-hook #'racket-xp-mode))
  (require 'racket-hash-lang)
  ;;(add-to-list 'auto-mode-alist '("\\.rkt\\'" . racket-hash-lang-mode))
  (add-to-list 'auto-mode-alist '("\\.scrbl\\'" . racket-hash-lang-mode))
  (add-to-list 'auto-mode-alist '("\\.rhm\\'" . racket-hash-lang-mode))
  (add-hook 'racket-hash-lang-module-language-hook
            #'gh/racket-hash-lang-on-module-language)
  (require 'racket-cmd)
  (setq racket-repl-buffer-name-function
        #'racket-repl-buffer-name-project)
  (add-to-list 'racket-logger-config '(racket-mode . debug))
  (cond
   (linux-p
    (setq racket-program "~/src/racket-lang/racket/bin/racket")
    (when (fboundp 'racket-add-back-end)
      ;; For the multi-back-end branch
      ;; 1. The automatic configuration for local files is fine.
      ;; 2. Slightly tweak the configuration for "/linode:*"
      ;; to use :racket-program "racket" instead of the default.
      (racket-add-back-end "/linode:"
                           :racket-program "racket")
      ;; 3. Experiment using multi back ends on same host. Files under
      ;; /var/tmp/8.0 will use a back end using Racket 8.9
      (racket-add-back-end "/var/tmp/8.0"
                           :racket-program "~/racket/8.6-cs/bin/racket")))
   (macosx-p
    (setq racket-program "/Applications/Racket_v8.10/bin/racket"))
   (mswindows-p
    (setq racket-program "C:\\Program Files\\Racket-7.2\\Racket.exe")))
  (setq racket-error-context 'medium)  ; 'high
  (unless terminal-frame
    (bind-keys :map racket-mode-map
               ("M-]" . racket-align)
               ("M-}" . racket-unalign))))

(defun gh/racket-hash-lang-on-module-language (mod-lang)
  (let ((rackety
         (member mod-lang (list "racket" "racket/base"
                                "typed/racket" "typed/racket/base"))))
    (paredit-mode (if rackety 1 -1))
    ;; Note: `racket-font-lock-keywords' and `racket-font-lock-level-0' etc.
    ;; are for use in `font-lock-defaults' which picks levels based on
    ;; `font-lock-maximum-decoration'. Instead, here, for use directly with
    ;; `font-lock-add-keywords' we want to use only a couple specific lists,
    ;; and /not/ the -0 or -1 lists that do syntactic stuff that we're
    ;; handling with tokens. We just want the keywords for imported symbols.
    ;; Obviously if this approach turns out to be OK this should be documented
    ;; and/or simpified for use in the hook here.
    (if rackety
        (font-lock-add-keywords nil (append racket-font-lock-keywords-2
                                            racket-font-lock-keywords-3))
      (font-lock-remove-keywords nil (append racket-font-lock-keywords-2
                                             racket-font-lock-keywords-3)))))

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

(use-package sudo-edit
  :ensure t)

(use-package toml-mode
  :ensure t)

(use-package tramp
  :config
  (setq tramp-default-method "ssh"))

(use-package uniquify
  :config (setq uniquify-buffer-name-style 'forward))

(use-package which-key
  :ensure t
  :init (dolist (hook '(prog-mode-hook text-mode-hook special-mode-hook))
          (add-hook hook #'which-key-mode))
  :config
  (setq which-key-idle-delay 0.4
        which-key-idle-secondary-delay 0
        which-key-sort-order 'which-key-prefix-then-key-order
        which-key-popup-type 'side-window
        which-key-show-docstrings nil
        which-key-max-description-length nil)
  (which-key-add-key-based-replacements
    "C-c a" "applications"
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
    "C-c w" "windows/frames"
    "C-c x" "xrefs")

  (which-key-add-major-mode-key-based-replacements 'markdown-mode
    "C-c TAB" "markdown/images"
    "C-c C-a" "markdown/links"
    "C-c C-c" "markdown/process"
    "C-c C-s" "markdown/style"
    "C-c C-t" "markdown/header"
    "C-c C-x" "markdown/structure"
    "C-c m" "markdown/personal")

  (bind-key "C-c h b" #'describe-personal-keybindings)

  :diminish which-key-mode)

(use-package xref
  :bind (("C-c x a"   . #'xref-find-apropos)
         ("C-c x . ." . #'xref-find-definitions)
         ("C-c x . w" . #'xref-find-definitions-other-window)
         ("C-c x . f" . #'xref-find-definitions-other-frame)
         ("C-c x r"   . #'xref-find-references)))

(use-package yaml-mode
  :ensure t
  :mode "\\.yml\\'")

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
                ;; " " (:propertize (:eval mode-line-misc-info) face italic)
                mode-line-end-spaces))

(setq-default mode-line-buffer-identification
              (propertized-buffer-identification "%b"))

(setq-default mode-line-position
              '("("
                (-3 "%p")
                (size-indication-mode ("/ " (-4 "%I")))
                " "
                (line-number-mode ("%l" (column-number-mode ":%c")))
                " "
                (:propertize (:eval (format "%s" (point)))
                             face (:slant italic))
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

;; (setq display-time-load-average-threshold nil)

;; (setq-default battery-mode-line-format " %b%p%%/%t")
;; (display-battery-mode 1)

;; (setq display-time-format "%b %-d %-l:%M") ;see `format-time-string'
;; (display-time)

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
        (cl-case dir
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

^Solarized^   ^Material^   ^Nord^       ^Other^
-----------------------------------------------------------------
_s_: Dark     _m_: Dark    _n_: Dark    _z_: Zenburn
_S_: Light    _M_: Light   _N_: Light   _e_: Eink     _DEL_: none
"
  ("s" (load-theme 'solarized-dark  t))
  ("S" (load-theme 'solarized-light t))
  ("m" (load-theme 'material        t))
  ("M" (load-theme 'material-light  t))
  ("n" (load-theme 'doom-nord       t))
  ("N" (load-theme 'doom-nord-light t))
  ("z" (load-theme 'zenburn         t))
  ("e" (load-theme 'eink            t))
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

           ("C-c b b" . ibuffer))

(bind-keys :map org-mode-map ("C-," . gh/prev-window))

(require 'display-line-numbers nil t) ;Emacs 29.1+
(unless (memq 'display-line-numbers features)
  (require 'linum))
(defun gh/goto-line-with-feedback ()
  "Like `goto-line' but show line numbers temporarily if not already shown.

Use `display-line-numbers-mode' in Emacs 29.1+, else `linum-mode'."
  (interactive)
  (let ((mode-var (if (boundp 'display-line-numbers-mode)
                      display-line-numbers-mode linum-mode))
        (mode-proc (if (fboundp 'display-line-numbers-mode)
                       #'display-line-numbers-mode #'linum-mode)))
    (unwind-protect
        (progn
          (unless mode-var
            (funcall mode-proc 1))
          (goto-line (read-number "Goto line: ")))
      (unless mode-var
        (funcall mode-proc -1)))))
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Initial tabs.
;;
;; Do this AFTER use-package declarations for things like mu4e and org-mode.
(when linux-p
  ;;(toggle-frame-fullscreen)
  (tab-bar-mode 1)
  (tab-rename "Mail/Agenda" 1)
  (tab-bar-new-tab-to 2)
  (tab-rename "Other" 2)
  (tab-bar-select-tab 1)
  (mu4e)
  (let ((timer nil))
    (setq timer
          (run-with-timer 0.1 0.1
                          (lambda ()
                            (when-let (mu4e-win (get-buffer-window "*mu4e-main*"))
                              (let ((win (selected-window)))
                                (unwind-protect
                                    (progn
                                      (select-window mu4e-win)
                                      (split-window-horizontally)
                                      (other-window 1)
                                      (org-agenda-list)))
                                (select-window win)
                                (cancel-timer timer))))))))

;; Local Variables:
;; fill-column: 78
;; End:

;;; init.el ends here
