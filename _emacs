;; @file: .emacs
;; @author: wuxi
;; @date: 2011.11.19
;;

(add-to-list 'load-path "~/.emacs.d")


;;; --- color-theme ---
;;; http://www.nongnu.org/color-theme/
(require 'color-theme)
(setq color-theme-is-global t)
(color-theme-initialize)
;(color-theme-midnight)
(color-theme-icoder-dark)


;;;--------------------------------------------------------------------
;;; === Language environment === {{{
(let ((lang (getenv "LANG")) (lc_all (getenv "LC_ALL")))
  (unless lang (setq lang ""))
  (unless lc_all (setq lc_all ""))
  (if (or (string-match "utf-8" (downcase lang))
          (string-match "utf-8" (downcase lc_all)))
      ;; UTF-8 settings:
      (progn
        (set-language-environment "UTF-8")
        (set-terminal-coding-system 'utf-8)
        (set-keyboard-coding-system 'utf-8)
        (set-clipboard-coding-system 'utf-8)
        (set-buffer-file-coding-system 'utf-8)
        (set-selection-coding-system 'utf-8)
        (modify-coding-system-alist 'process "*" 'utf-8)
        (setq default-process-coding-system '(utf-8-unix . utf-8-unix)))
    ;; GBK settings:
    (set-language-environment 'Chinese-gbk)
    (set-terminal-coding-system 'chinese-gbk)
    (set-keyboard-coding-system 'chinese-gbk)
    (set-clipboard-coding-system 'chinese-gbk)
    (set-buffer-file-coding-system 'chinese-gbk)
    (set-selection-coding-system 'chinese-gbk)
    (modify-coding-system-alist 'process "*" 'chinese-gbk)
    (setq default-process-coding-system '(chinese-gbk . chinese-gbk))))
;;; }}}


;;;--------------------------------------------------------------------
;;; === GUI/Terminal difference settings === {{{
(if (not window-system)
    ;; --- in Terminal environment: ---
    (progn
      ;; hide menu
      (menu-bar-mode 0)
      )

  ;; --- in GUI environment: ---
  ;; font:
  ;(set-frame-font "Courier_New-15")
  ;; set frame arguments:
  (setq default-frame-alist
		'((width . 86) (height . 36) (font . "Courier_New-15")))
)
;;; }}}


;; --- ELPA: Emacs Lisp Package Archive ---
;; http://tromey.com/elpa/index.html
;;
(add-to-list 'load-path "~/.emacs.d/elpa")
(load "package")
(package-initialize)
(setq package-archives '(("ELPA" . "http://tromey.com/elpa/")
                         ("gnu" . "http://elpa.gnu.org/packages/")))


;;;--------------------------------------------------------------------
;;; === General Settings === {{{

;; disable toolbar:
(setq tool-bar-mode nil)

;; tab width and uses spaces instead of tab
(setq tab-stop-list '(4 8 12 16 20 24 28 32 36 40 44 48 52 56 60 64 68 72 76 80 84 88 92 96 100 104 108 112 116 120 124 128 132 136 140 144 148 152 156 160))
(setq tab-width 4)
(setq-default tab-width 4)
(setq-default indent-tabs-mode nil)

;; yes-no shortcuts
(fset 'yes-or-no-p 'y-or-n-p)

;; default input method, use C-\ to toggle input method
(setq default-input-method 'chinese-py-punct)

;; display time in mode-line:
;(display-time-mode 1)
;(setq display-time-23hr-format t)
;(setq display-time-day-and-date t)

;; visible-bell
;(setq visible-bell t)

;; turn on mouse in xterm
(xterm-mouse-mode t)

;; enable mouse wheel
(mouse-wheel-mode t)

;; show uesless tailing whitespaces:
(setq-default show-trailing-whitespace t)

;; display line num and column num
(column-number-mode t)
(line-number-mode t)

;; display image
(auto-image-file-mode 1)

;; scroll margin
(setq scroll-step 1
      scroll-margin 7
      scroll-conservatively 10000)

;; don't blink
(blink-cursor-mode 0)

;; highlight selection region
(transient-mark-mode t)

;; inhibit startup message:
(setq inhibit-startup-message t)

;; show parenthesis:
(show-paren-mode 1)

;; disable backup
(setq-default make-backup-files nil)

;; turn off auto-save-mode
(setq-default auto-save-mode nil)

;; kill-ring size
(setq kill-ring-max 500)

;; turn on debug on error occurs
;(setq debug-on-error 1)

;; uses the system clipboard
(setq x-select-enable-clipboard t)

;; frame title
(setq frame-title-format "%b - Emacs")

;; show equivalent key-binding for a command
(setq suggest-key-bindings 1)

;; syntax highlighting
(global-font-lock-mode 1)
(setq font-lock-maximum-decoration t)

;; Disable Version Control
(setq vc-handled-backends nil)

;;; }}}


;;;--------------------------------------------------------------------
;;; === GENERAL extensions === {{{

;; --- ctab ---
(require 'ctab)
(ctab-mode t)

;; --- line number ---
;; http://stud4.tuwien.ac.at/~e0225855/linum/linum.html
(require 'linum)
(global-linum-mode t)
(set-face-foreground 'linum "yellow")

;; --- session ---
;; http://emacs-session.sourceforge.net/
(require 'session)
(add-hook 'after-init-hook 'session-initialize)

;; --- open recent file ---
;; http://www.emacswiki.org/emacs/RecentFiles
(require 'recentf)
(recentf-mode 1)
(setq recentf-max-menu-items 10)
(global-set-key (kbd "C-x C-r") 'recentf-open-files)
(global-set-key (kbd "<f5>") 'recentf-open-files)


;;; }}}


;;;--------------------------------------------------------------------
;;; === Programming settings === {{{
(setq compile-command "make -k")
(global-set-key (kbd "<f7>") 'compile)

;; --- Cscope ---
;; http://cscope.sourceforge.net/
;; http://www.emacswiki.org/emacs/CScopeAndEmacs
(require 'xcscope)

;; --- CC-mode ---
;; http://cc-mode.sourceforge.net/
(require 'cc-mode)

;; my style
(defconst my-programming-style
  '((c-tab-always-indent        . t)
    (c-comment-only-line-offset . 4)
    (c-hanging-braces-alist     . ((substatement-open after)
                                   (brace-list-open)))
    (c-hanging-colons-alist     . ((member-init-intro before)
                                   (inher-intro)
                                   (case-label after)
                                   (label after)
                                   (access-label after)))
    (c-cleanup-list             . (scope-operator
                                   empty-defun-braces
                                   defun-close-semi))
    (c-doc-comment-style        . ((c++-mode          . javadoc)
                                   (c-mode            . javadoc)
                                   (java-mode         . javadoc)))
    (c-offsets-alist            . (
                                   (innamespace       . 0)
                                   (comment-intro     . 0)
                                   (substatement-open . 0)
                                   (statement-case-open . +)
                                   (label             . 0)
                                   (cpp-macro         . -)
                                   (inline-open       . 0)
                                   (objc-method-args-cont . ++)
                                   ))
    (c-echo-syntactic-information-p . t))
  "My Common Programming Style")

(c-add-style "my-programming" my-programming-style)

;; my common hook
(defun my-c-mode-common-hook ()
  ;; set my personal style for the current buffer
  (c-set-style "my-programming")
  ;; other customizations
  (setq tab-width 4
        ;; this will make sure spaces are used instead of tabs
        indent-tabs-mode nil)

  ;; turn off auto-newline
  (c-toggle-auto-newline -1)
  ;; turn on hungry-dele
  (c-toggle-hungry-state 1)
  ;; ctrl+`: fold
  (define-key c-mode-base-map (kbd "C-\`") 'hs-toggle-hiding)
  ;; indent when type new line
  (define-key c-mode-base-map (kbd "<RET>") 'newline-and-indent)
  ;; F7: compile
  (define-key c-mode-base-map (kbd "<f7>") 'compile)
  ;; F9: continue: gud-cont
  ;(define-key c-mode-base-map [(f9)] 'gud-cont)
  ;; F10: gud-next
  ;(define-key c-mode-base-map [(f10)] 'gud-next)
  ;; F11: gud-step
  ;(define-key c-mode-base-map [(f11)] 'gud-step)

  ;; auto-complete:
  ;;(define-key c-mode-base-map (kbd "M-.") 'ac-complete-gccsense)

) ; end my-c-mode-common-hook

(add-hook 'c-mode-common-hook 'my-c-mode-common-hook)
(add-to-list 'auto-mode-alist '("\\.h$" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.m$" . objc-mode))
(add-to-list 'auto-mode-alist '("\\.mm$" . objc-mode))


;; --- CEDET ---
;; http://cedet.sourceforge.net/
;; here uses built-in cedet:
(require 'cedet)
(require 'semantic/bovine/gcc)

(setq semantic-default-submodes '(global-semantic-idle-scheduler-mode
                                  global-semanticdb-minor-mode
                                  global-semantic-idle-summary-mode
                                  global-semantic-mru-bookmark-mode))
(semantic-mode 1)

(require 'semantic/analyze/refs)
(defadvice push-mark (around semantic-mru-bookmark activate)
  "Push a mark at LOCATION with NOMSG and ACTIVATE passed to `push-mark'.
If `semantic-mru-bookmark-mode' is active, also push a tag onto
the mru bookmark stack."
  (semantic-mrub-push semantic-mru-bookmark-ring
                      (point)
                      'mark)
  ad-do-it)
(defun semantic-ia-fast-jump-back ()
  (interactive)
  (if (ring-empty-p (oref semantic-mru-bookmark-ring ring))
      (error "Semantic Bookmark ring is currently empty"))
  (let* ((ring (oref semantic-mru-bookmark-ring ring))
         (alist (semantic-mrub-ring-to-assoc-list ring))
         (first (cdr (car alist))))
    (if (semantic-equivalent-tag-p (oref first tag) (semantic-current-tag))
        (setq first (cdr (car (cdr alist)))))
    (semantic-mrub-switch-tags first)))
(defun semantic-ia-fast-jump-or-back (&optional back)
  (interactive "P")
  (if back
      (semantic-ia-fast-jump-back)
    (semantic-ia-fast-jump (point))))
(define-key semantic-mode-map [f12] 'semantic-ia-fast-jump-or-back)
(define-key semantic-mode-map [S-f12] 'semantic-ia-fast-jump-back)




;;; }}}


;; === Global key map === {{{

;; Meta-g:
(global-set-key (kbd "M-g") 'goto-line)

;; window and buffer
(global-set-key (kbd "M-`") 'other-window)
(global-set-key (kbd "M-o") 'other-window)
(global-set-key (kbd "M-1") 'delete-other-windows)
(global-set-key (kbd "M-2") 'split-window-vertically)
(global-set-key (kbd "M-3") 'split-window-horizontally)
(global-set-key (kbd "M-0") 'kill-buffer-and-window)
(global-set-key (kbd "M-4") '(lambda () (interactive) (kill-buffer (current-buffer))))
(global-set-key (kbd "M-]") 'enlarge-window-horizontally)
(global-set-key (kbd "M-6") 'enlarge-window)

;; move cursor:
(global-set-key (kbd "<end>") 'end-of-line)
(global-set-key (kbd "<home>") 'beginning-of-line)
(global-set-key (kbd "S-<right>") 'forward-word)
(global-set-key (kbd "S-<left>") 'backward-word)
(global-set-key (kbd "S-<down>") 'scroll-up)
(global-set-key (kbd "S-<up>") 'scroll-down)
(global-set-key (kbd "<mouse-4>") 'previous-line)
(global-set-key (kbd "<mouse-5>") 'forward-line)

;; Set mark: S-SPC, M-SPC = C-SPC
(global-set-key (kbd "C-SPC") 'nil)
(global-set-key (kbd "M-SPC") 'set-mark-command)
(global-set-key (kbd "S-SPC") 'set-mark-command)


;; for ignore ssh auto-translates [backspace] to [Ctrl-h]
;(global-set-key [?\C-h] 'delete-backward-char)
(global-set-key [?\C-x ?h] 'help-command)

;; point-register
(global-set-key (kbd "M-p") 'point-to-register)
(global-set-key (kbd "C-j") 'jump-to-register)

;; for puTTY:
(global-set-key (kbd "<select>") 'end-of-line)

;; for Mac
(when (string= "ns" window-system)
  ;; (setq mac-option-modifier nil)
  (setq mac-option-modifier 'meta)
  (setq mac-option-key-is-meta nil)
  ;; (setq mac-command-key-is-meta t)
  ;; (setq mac-command-modifier 'meta)
  )

;;; }}}
(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(safe-local-variable-values (quote ((encoding . utf-8) (encoding . gbk))))
 '(session-use-package t nil (session)))
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 )
