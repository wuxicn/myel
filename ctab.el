;;; ctab.el --- Display a new tab bar in the header line

;; Copyright (C) 2012 Wu Xi

;; Author: Wu Xi <wuxi.cn@gmail.com>
;; Created: Jul 7, 2012
;; Keywords: convenience
;; Revision: $Id$

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:
;;
;; This library provides a minor mode to display tabs in the header
;; line.
;;
;; INSTALL:
;;   Copy ctab.el to your load-path and add to your .emacs:
;;
;;     (require 'ctab)
;;     (ctab-mode t)
;;
;; SHORTCUT KEYS:
;;   M-<right>  `ctab-next'      Select next tab.
;;   M-<left>   `ctab-previous'  Select previous tab.
;;
;;
;; Toggle display of ctab in header-line with M-x `ctab-mode'.
;;

;;; Changelist:
;;  2012.7.17, wuxi: fix file-extension match bug.
;;

;;; Code:

(defconst ctab-version "0.4.0")

(defgroup ctab nil
  "Display a tab bar for normal buffers in the header line."
  :group 'convenience)


;;;
;;; Faces: {{{

(defface ctab-default-face
  '((t (:inherit default
                 :foreground "white"
                 :background "grey20"
                 :underline t)))
  "Default face used in the tab bar."
  :group 'ctab)

(defface ctab-head-face
  '((t (:inherit ctab-default-face
                 :background "black")))
  "Face for displaying head of cTab bar."
  :group 'ctab)

(defface ctab-current-face
  '((t (:inherit ctab-default-face
                 :foreground "#ffffff"
                 :background "black"
                 :underline nil)))
  "Face for displaying tab of current buffer."
  :group 'ctab)

;;; face }}}

;;;
;;; Implementation: {{{

;;; Variables:

(defvar ctab-blist nil
  "The ctab buffer-list.")

(defvar ctab-blist-num 0
  "Number of buffers in ctab-blist")

(defvar ctab-enable-default-filter t)

(defvar ctab-filter-list nil
  "Buffer filter function list.
A buffer can go into ctab-blist only if all functions in filter list return t")

(defvar ctab-index 0
  "Index of current buffer in tabset.")
(make-variable-buffer-local 'ctab-index)

(defvar ctab-old-header nil
  "Old default-value of `header-line-format'.")
(make-variable-buffer-local 'ctab-old-header)

(defvar ctab-header-ext (list "h" "hpp" "hxx" "h++" "hh")
  "List of header file extensions")

;;; Functions:

(defun ctab-default-filter (buffer)
  "Default filter: filter all buffers have a star at beginning of buffer name,
except '*scratch*', eg: *Message*"
  (when ctab-enable-default-filter
    (let* ((bname (buffer-name buffer)) (c (substring bname 0 1)))
      (or (string= bname "*scratch*")
          (not (or (string= c "*") (string= c " ")))))
    ) ;; end when
  ) ;; end ctab-default-filter
(add-to-list 'ctab-filter-list (function ctab-default-filter))

(defun ctab-smart-insert (buffer)
  "Insert `buffer' into `ctab-blist' at the right place it needs to be.
The right place is: if buffer name=='file.cpp', then the right place is the
place next to 'file.h'."
  (if (not ctab-blist)
      ;; ctab-blist is empty:
      (progn
        (add-to-list 'ctab-blist buffer)
        (setq ctab-blist-num (1+ ctab-blist-num)))

    ;; else:
    (let* ((blist ctab-blist)
           (name (file-name-sans-extension (buffer-name buffer)))
           (ext (downcase (concat "" (file-name-extension (buffer-name buffer)))))
           (not-in-list t) (last blist) pos tmp-list)

      ;; check `blist' for buffer with the name identical to `buffer'
      (while (and blist not-in-list)
        (if (equal buffer (car blist))
            ;; `buffer' is already in `blist':
            (setq not-in-list nil)
          ;; else:
          (when (string= name (file-name-sans-extension (buffer-name (car blist))))
            (if (member ext ctab-header-ext)
                ;; buffer is c/c++ header:
                (progn
                  (setq blist nil)
                  (setq pos last))
              ;; buffer is not c/c++ header:
              (setq pos blist))))
        (setq last blist)
        (setq blist (cdr blist)))

      ;; not in `blist', insert:
      (when not-in-list
        (if (not pos)
            ;; not found, append:
            (setcdr last (list buffer))
          ;; else: found, insert after pos:
          (setq tmp-list (cons buffer (cdr pos)))
          (setcdr pos tmp-list))
        ;; inc ctab-blist-num:
        (setq ctab-blist-num (1+ ctab-blist-num)))

      ) ;; end let*
    ) ;; end if
  ) ;; end ctab-tab-list

(defun ctab-insert (buffer)
  "Insert `buffer' into `ctab-blist'."
  (let* ((flist ctab-filter-list) (pass t))
    ;; apply `buffer' to each filter function
    (while (and pass flist)
      (setq filter (car flist))
      (setq pass (funcall (car flist) buffer))
      (setq flist (cdr flist)))
    ;; insert if all filters are passed:
    (when pass
      (ctab-smart-insert buffer))
    ) ;; end let
  ) ;; end ctab-insert

(defun ctab-delete (buffer)
  "Remove `buffer' from `ctab-blist'."
  (when ctab-blist
    (if (equal buffer (car ctab-blist))
        ;; delete first element:
        (pop ctab-blist)
      ;; else:
      (delete buffer ctab-blist))
    (setq ctab-blist-num (1- ctab-blist-num)))
  ) ;; end ctab-delele

(defsubst ctab-free-blist ()
  "Free the blist store."
  (setq ctab-blist nil
        ctab-blist-num 0
        ctab-index nil))

(defun ctab-init-blist ()
  "Init blist of ctab."
  ;; release previous tabset:
  (ctab-free-blist)
  ;; reset offset
  (set-frame-parameter nil 'ctab-offset 0)
  ;(setq ctab-offset 0)
  ;; insert all existing buffer into tabset:
  (dolist (buffer (buffer-list))
    (ctab-insert buffer)))

;; format tab line and get offset:
(defun ctab-format-line (curbuf)
  "Format output line for `header-line-format'.
Returns list (head-str tail-str begin-offset end-offset), if `curbuf' is in
 `ctab-blist'; otherwise, returns nil.
header-line eg: [5/17]:_ctab.el__worker.h_|worker.cpp|_main.cpp_...
                 ^       ^                  ^               ^
                 |       |- normal tab      |- current tab  |
               head-str                                   tail-str
"
  (let ((n 0) (tabset ctab-blist) index (offset 0) beg end (str "") tail-str)
    (dolist (buf tabset)
      ;; format tab:
      (if (equal buf curbuf)
          ;; format current tab:
          (progn
            ;; save index of current tab:
            (setq index n)
            ;; format string:
            (setq str (propertize (format "|%s|" buf)
                                  'face 'ctab-current-face))
            ;; set offsets:
            (setq beg offset)
            (setq end (+ offset (length str))))
        ;; else: format normal tab:
        (setq str (propertize (format " %s " buf)
                              'face 'ctab-default-face)))
      ;; append to line:
      (setq tail-str (concat tail-str str))
      ;; count tabs:
      (setq n (1+ n))
      ;; calc offset:
      (setq offset (+ offset (length str)))
      ) ;; end dolist

    ;; check if curbuf is in list:
    (when index
      ;; `curbuf' is in `list':
      ;; set new index:
      (setq ctab-index index)
      ;; return the result list: (head-str tail-str begin-offset end-offset)
      (list
       ;; head string: "[i/n]"
       (propertize (format "[%d/%d]:" (1+ index) n) 'face 'ctab-head-face)
       ;; tail string:
       tail-str
       ;; begin-offset:
       beg
       ;; end-offset:
       end)
      )

    ) ;; end let
  ) ;; end ctab-format-line

(defun ctab-compute-offset (win-width head-width beg end)
  "Compute tab line offset according to line length and window width."
  (let* ((width (- win-width head-width))
         (ctab-offset (frame-parameter nil 'ctab-offset))
         (offset (- end ctab-offset)))
    (cond
     ;; case 1: left-margin of current tab is out of display region:
     ((< beg ctab-offset) (setq ctab-offset beg))
     ;; case 2: right-margin of current tab is out of display region:
     ((> offset width) (setq ctab-offset (+ ctab-offset (- offset width))))
     ;; case 3: current tab is in display region: do nothing
     )
    (set-frame-parameter nil 'ctab-offset ctab-offset)
    ) ;; end let
  ) ;; end ctab-compute-offset

(defun ctab-display-header ()
  "Insert current buffer into `ctab-blist', if current buffer is not in it, and
 then display the header-line."
  ;; insert buffer if needed:
  (ctab-insert (current-buffer))

  ;; display:
  (let ((quad (ctab-format-line (current-buffer))))
    (if quad
        ;; non-nil, display ctab in header:
        (progn
          ;; move display offset:
          (ctab-compute-offset
           (window-width) (length (car quad)) (nth 2 quad) (nth 3 quad))
          ;; output:
          (concat (car quad) (substring (nth 1 quad)
                                        (frame-parameter nil 'ctab-offset)))
          ) ;; end progn
      ;; else: nil, display orginal header:
      ctab-old-header)

    ) ;; end let
  ) ;; end ctab-display-header

(defsubst ctab-move (index)
  "Move buffer to index-th buffer in ctab-blist"
  (when (member (current-buffer) ctab-blist)
    (switch-to-buffer (nth index ctab-blist))))

;;; public function:
;;;###autoload
(defun ctab-previous ()
  "Move to previous buffer."
  (interactive)
  (if (> ctab-index 0)
      (ctab-move (1- ctab-index))
    (ctab-move (1- (length ctab-blist)))))

;;; public function:
;;;###autoload
(defun ctab-next ()
  "Move to next buffer."
  (interactive)
  (ctab-move (% (1+ ctab-index) (length ctab-blist))))

;;; Bind/unbind shortcut keys:
(defvar ctab-old-key-next nil "Old key-binding of `M-<right>'.")
(defvar ctab-old-key-pre nil "Old key-binding of `M-<left>'.")

(defun ctab-bind-keys ()
  "Bind shortcut keys for moving."
  ;; save old key-bindings:
  (setq ctab-old-key-next (global-key-binding (kbd "M-<right>")))
  (setq ctab-old-key-pre (global-key-binding (kbd "M-<left>")))
  ;; bind keys:
  (define-key global-map (kbd "M-<right>") 'ctab-next)
  (define-key global-map (kbd "M-<left>") 'ctab-previous))

(defun ctab-unbind-keys ()
  "Unbind shortcut keys."
  (define-key global-map (kbd "M-<right>") 'ctab-old-key-next)
  (define-key global-map (kbd "M-<left>") 'ctab-old-key-pre))

;;; hook:
(defun ctab-kill-buffer-hook ()
  "Hook run just before actually killing a buffer."
  (ctab-delete (current-buffer))
  t)

(defun ctab-find-file-hook ()
  "Hook run after a file is visited."
  (ctab-insert (current-buffer))
  t)

(defconst ctab-header-line '(:eval (ctab-display-header))
  "The ctab bar header line format.")

;;;###autoload
(define-minor-mode ctab-mode
  "Toggle display of tabs in the header line."
  :global t
  :group 'ctab
  ;; body:
  (if ctab-mode
      ;; ON:
      (progn
        (message "ctab-mode: ON")
        (unless (eq header-line-format ctab-header-line)
          ;; save current default-value of `header-line-format'.
          (setq ctab-old-header (default-value 'header-line-format))
          ;; add hooks:
          (add-hook 'kill-buffer-hook 'ctab-kill-buffer-hook)
          (add-hook 'find-file-hook 'ctab-find-file-hook)
          ;; bind keys:
          (ctab-bind-keys)
          ;; init:
          (ctab-init-blist)
          (setq-default header-line-format ctab-header-line)
          )
        ) ;; END OF progn

    ;; else: OFF:
    (message "ctab-mode: OFF")
    ;; restore previous `header-line-format' if needed:
    (when (eq (default-value 'header-line-format) ctab-header-line)
      ;; restore default-value of `header-line-format':
      (setq-default header-line-format ctab-old-header)
      ;; remove hooks:
      (remove-hook 'kill-buffer-hook 'ctab-kill-buffer-hook)
      (remove-hook 'find-file-hook 'ctab-find-file-hook)
      ;; unbind keys:
      (ctab-unbind-keys)
      ;; release vars:
      (ctab-free-blist))
    ) ;; END OF 'else'
  )

;;; implementation }}}

(provide 'ctab)
;;; ctab.el ends here