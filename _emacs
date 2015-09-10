;;; @file: .emacs
;;; @author: wuxi
;;; @date: 2015.07.07
;;;
;;; CHANGE-LIST:
;;;   - 2015.07.07, wuxi: create.
;;;

;;; set my default load-path
(add-to-list 'load-path "~/.emacs.d")


;;; === Global key bindings: === {{{

(global-set-key (kbd "M-g") 'goto-line)

;;; window and buffer
(global-set-key (kbd "M-`") 'other-window)
(global-set-key (kbd "M-o") 'other-window)
(global-set-key (kbd "M-1") 'delete-other-windows)
(global-set-key (kbd "M-2") 'split-window-vertically)
(global-set-key (kbd "M-3") 'split-window-horizontally)
(global-set-key (kbd "M-0") 'kill-buffer-and-window)
(global-set-key (kbd "M-4") '(lambda () (interactive) (kill-buffer (current-buffer))))
(global-set-key (kbd "M-]") 'enlarge-window-horizontally)
;;(global-set-key (kbd "M-[") 'shrink-window-horizontally)
(global-set-key (kbd "M-6") 'enlarge-window)

;;; move cursor:
(global-set-key (kbd "<end>") 'end-of-line)
(global-set-key (kbd "<home>") 'beginning-of-line)
(global-set-key (kbd "S-<right>") 'forward-word)
(global-set-key (kbd "S-<left>") 'backward-word)
;(global-set-key (kbd "S-<down>") 'scroll-up)
;(global-set-key (kbd "S-<up>") 'scroll-down)
(global-set-key (kbd "<mouse-4>") 'previous-line)
(global-set-key (kbd "<mouse-5>") 'forward-line)

;;; Set mark: S-SPC, M-SPC = C-SPC
(global-set-key (kbd "C-SPC") 'nil)
(global-set-key (kbd "M-SPC") 'set-mark-command)
(global-set-key (kbd "S-SPC") 'set-mark-command)

;;; for ignore ssh auto-translates [backspace] to [Ctrl-h]
;(global-set-key [?\C-h] 'delete-backward-char)
(global-set-key [?\C-x ?h] 'help-command)

;;; point-register
(global-set-key (kbd "M-p") 'point-to-register)
(global-set-key (kbd "C-j") 'jump-to-register)

;;; for puTTY:
(global-set-key (kbd "<select>") 'end-of-line)

;;; for Mac
(when (string= "ns" window-system)
  ;; (setq mac-option-modifier nil)
  (setq mac-option-modifier 'meta)
  (setq mac-option-key-is-meta nil)
  ;; (setq mac-command-key-is-meta t)
  ;; (setq mac-command-modifier 'meta)
  )

;;; Global key binding }}}


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


;;; === General Settings === {{{

;;; disable toolbar:
(setq tool-bar-mode nil)

;;; tab width and uses spaces instead of tab
(setq tab-stop-list '(4 8 12 16 20 24 28 32 36 40 44 48 52 56 60 64 68 72 76 80 84 88 92 96 100 104 108 112 116 120 124 128 132 136 140 144 148 152 156 160))
(setq tab-width 4)
(setq-default tab-width 4)
(setq-default indent-tabs-mode nil)

;;; yes-no shortcuts
(fset 'yes-or-no-p 'y-or-n-p)

;;; default input method, use C-\ to toggle input method
(setq default-input-method 'chinese-py-punct)

;;; display time in mode-line:
;(display-time-mode 1)
;(setq display-time-23hr-format t)
;(setq display-time-day-and-date t)

;;; visible-bell
;(setq visible-bell t)

;;; turn on mouse in xterm
(xterm-mouse-mode t)

;;; enable mouse wheel
(mouse-wheel-mode t)

;;; show uesless tailing whitespaces:
(setq-default show-trailing-whitespace t)

;;; display line num and column num
(column-number-mode t)
(line-number-mode t)

;;; display image
(auto-image-file-mode 1)

;;; scroll margin
(setq scroll-step 1
      scroll-margin 7
      scroll-conservatively 10000)

;;; don't blink
(blink-cursor-mode 0)

;;; highlight selection region
(transient-mark-mode t)

;;; inhibit startup message:
(setq inhibit-startup-message t)

;;; show parenthesis:
(show-paren-mode 1)

;;; disable backup
(setq-default make-backup-files nil)

;;; turn off auto-save-mode
(setq-default auto-save-mode nil)

;;; kill-ring size
(setq kill-ring-max 1000)

;;; turn on debug on error occurs
;(setq debug-on-error 1)

;;; uses the system clipboard
(setq x-select-enable-clipboard t)

;;; frame title
(setq frame-title-format "%b - Emacs")

;;; show equivalent key-binding for a command
(setq suggest-key-bindings 1)

;;; syntax highlighting
(global-font-lock-mode 1)
(setq font-lock-maximum-decoration t)

;;; Disable Version Control
(setq vc-handled-backends nil)

;;; }}}


;;;--------------------------------------------------------------------
;;; === General extensions === {{{

;;; --- color-theme ---
;;; http://www.nongnu.org/color-theme/
(require 'color-theme)
(setq color-theme-is-global t)
(color-theme-initialize)
(color-theme-icoder-dark)

;;; --- line number ---
;;; built-in
(require 'linum)
(global-linum-mode t)
(set-face-foreground 'linum "yellow")

;;; --- ELPA: Emacs Lisp Package Archive ---
;;; http://tromey.com/elpa/index.html
;;; http://melpa.org/
(add-to-list 'load-path "~/.emacs.d/elpa")
(load "package")
(package-initialize)
(setq package-archives '(("melpa" . "http://melpa.org/packages/")
                         ("ELPA" . "http://tromey.com/elpa/")
                         ("gnu" . "http://elpa.gnu.org/packages/")))

;;; --- open recent file ---
;;; http://www.emacswiki.org/emacs/RecentFiles
(require 'recentf)
(recentf-mode 1)
(setq recentf-max-menu-items 10)
(global-set-key (kbd "C-x C-r") 'recentf-open-files)
(global-set-key (kbd "<f5>") 'recentf-open-files)

;;; --- ctab-mode ---
;;; my ctab mode
(require 'ctab)
(ctab-mode t)

;;; --- flex and bison(yacc) mode ---
;;; get make-regexp first from here: http://web.mit.edu/majapw/OldFiles/MacData/afs/athena/software/r_v2.14.1/ess-5.14/lisp/
;;; flex-mode: http://www.emacswiki.org/emacs/FlexMode
;;; bison-mode: http://www.emacswiki.org/emacs/BisonMode
(require 'flex-mode)
(require 'bison-mode)
(add-to-list 'auto-mode-alist '("\\.l\\'" . flex-mode))
(add-to-list 'auto-mode-alist '("\\.y\\'" . bison-mode))

;;; --- session ---
;;; http://emacs-session.sourceforge.net/
;;; NOTE: install with ELPA(recommand):
;;;   M-x package-install [RET] session [RET]
;;; or manually install and uncomment:
;;; (require 'session)
;;; (add-hook 'after-init-hook 'session-initialize)

;;; --- markdown-mode ---
;;; http://jblevins.org/projects/markdown-mode/
;;; NOTE: install with ELPA(recommand):
;;;   M-x package-install [RET] markdown-mode [RET]
;;; or manually install and uncomment:
;;; (autoload 'markdown-mode "markdown-mode"
;;;   "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

;;; General extensions }}}


;;;--------------------------------------------------------------------
;;; === PROGRAMMING settings === {{{
(setq compile-command "make -k")
(global-set-key (kbd "<f7>") 'compile)

;;; --- CC-mode ---
;;; http://cc-mode.sourceforge.net/
(require 'cc-mode)

;;; --- CEDET ---
;;; http://cedet.sourceforge.net/
;;; here uses built-in cedet:
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

;;; --- my code-style --- {{{
(defconst my-code-style
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

(c-add-style "my-code-style" my-code-style)

;;; my common hook
(defun my-c-mode-common-hook ()
  ;;; set my personal style for the current buffer
  (c-set-style "my-code-style")
  ;;; other customizations
  ;;; this will make sure spaces are used instead of tabs
  (setq tab-width 4 indent-tabs-mode nil)
  ;;; turn off auto-newline
  (c-toggle-auto-newline -1)
  ;;; turn on hungry-dele
  (c-toggle-hungry-state 1)
  ;;; ctrl+`: fold
  (define-key c-mode-base-map (kbd "C-\`") 'hs-toggle-hiding)
  ;;; indent when type new line
  (define-key c-mode-base-map (kbd "<RET>") 'newline-and-indent)
  ;;; F7: compile
  (define-key c-mode-base-map (kbd "<f7>") 'compile)
  ;;; F9: continue: gud-cont
  ;(define-key c-mode-base-map [(f9)] 'gud-cont)
  ;;; F10: gud-next
  ;(define-key c-mode-base-map [(f10)] 'gud-next)
  ;;; F11: gud-step
  ;(define-key c-mode-base-map [(f11)] 'gud-step)
  ;;; auto-complete:
  ;;(define-key c-mode-base-map (kbd "M-.") 'ac-complete-gccsense)
) ; end my-c-mode-common-hook

(add-hook 'c-mode-common-hook 'my-c-mode-common-hook)
(add-to-list 'auto-mode-alist '("\\.h$" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.m$" . objc-mode))
(add-to-list 'auto-mode-alist '("\\.mm$" . objc-mode))
;;; my code-style }}}

;;; --- Cscope ---
;;; https://github.com/dkogan/xcscope.el
;;; https://raw.githubusercontent.com/dkogan/xcscope.el/master/xcscope.el
(require 'xcscope)
(cscope-setup)
;;; NOTE: to use GNU Global back-end: http://www.gnu.org/s/global/
;;;       install GNU Global and uncomment following region to enable:
(setq cscope-program "gtags-cscope")

;;; --- Auto-complete ---
;;; http://auto-complete.org/
;;; https://github.com/auto-complete/auto-complete
;;; http://www.emacswiki.org/emacs/AutoComplete
;;; NOTE: install with ELPA(recommand):
;;;   M-x package-install [RET] auto-complete [RET]

;;; --- Go ---
;;; https://github.com/dominikh/go-mode.el
;;; http://tleyden.github.io/blog/2014/05/22/configure-emacs-as-a-go-editor-from-scratch/
(require 'go-mode-autoloads)
;(setenv "GOPATH" "/Users/wuxi/gocode")
;(setq exec-path (cons "/usr/local/opt/go/libexec/bin/go" exec-path))
;(add-to-list 'exec-path "/Users/wuxi/gocode/bin")

;;; --- godef ---
;;; Install: $ go get code.google.com/p/rog-go/exp/cmd/godef
(defun my-go-mode-hook ()
  ; Call Gofmt before saving
  ;(add-hook 'before-save-hook 'gofmt-before-save)
  ;; (local-set-key (kbd "C-c C-j") 'godef-jump) ;; default jumb key
  (local-set-key (kbd "C-c C-b") 'pop-tag-mark))
(add-hook 'go-mode-hook 'my-go-mode-hook)

;;; --- gocode: ---
;;; Go aware Autocomplete
;;; https://github.com/nsf/gocode
;;; Depends:
;;;   auto-complete.
;;
;;; Install:
;;;   $ go get -u github.com/nsf/gocode
;;;   $ cp $GOPATH/src/github.com/nsf/gocode/emacs/go-autocomplete.el $HOME/.emacs.d/
;;;   uncomment following:
;;(require 'go-autocomplete)
;;(require 'auto-complete-config)
;;(ac-config-default)

;;; --- go-eldoc ---
;;; https://github.com/syohex/emacs-go-eldoc
;;; Depends:
;;;   go-mode
;;;   gocode
;;; Install: using package:
;;;   M-x package-install go-eldoc
;;;   and uncomment following:
(add-hook 'go-mode-hook 'go-eldoc-setup)

;;; --- go oracle ---
;;; User Manual:
;;;   https://docs.google.com/document/d/1SLk36YRjjMgKqe490mSRzOPYEDe0Y_WQNRv-EiFYUyw/view
;;; Install:
;;;   $ go get golang.org/x/tools/cmd/oracle
;;; then uncomment:
(load-file "__GOPATH_1ST__/src/golang.org/x/tools/cmd/oracle/oracle.el")

;;; Programming settings }}}
