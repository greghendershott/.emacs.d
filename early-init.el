;;; early-init.el --- Emacs early init file  -*- lexical-binding: t; -*-

;; `load-prefer-newer': This is nil by default -- Emacs always prefers .elc,
;; even when .el is newer. Instead, I want to load .el when newer than .elc.
;; This helps when e.g. I'm working on Racket Mode. It means I'll never get
;; confused by Emacs using stale bytecode. I'm in the habit of doing a `make
;; clean` to avoid this... but eliminating a possible confusion is always
;; good.
(setq load-prefer-newer t)

(setq inhibit-startup-message t)

;; No title bar, min/max boxes, external borders
(when (eq 'gnu/linux system-type)
  (add-to-list 'default-frame-alist '(undecorated . t)))

;; Initial frame size/state
(add-to-list
 'initial-frame-alist
 (pcase system-type
   ('gnu/linux  '(width . (text-pixels . 2800))) ;let paperWM set height
   ('darwin     '(fullscreen . fullboth))
   ('windows-nt '(fullscreen . maximized))))

(when (boundp 'scroll-bar-mode) (scroll-bar-mode -1))
(when (boundp 'tool-bar-mode) (tool-bar-mode -1))
(cond ((boundp 'ns-command-modifier)
       (setq ns-command-modifier 'meta
             ns-auto-hide-menu-bar t))
      ((boundp 'menu-bar-mode)
       (menu-bar-mode -1)))
