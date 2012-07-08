;; File: color-theme-icoder-dark.el
;; Revision 1
;;
;; Copyright (C) 2010 Wu Xi http://icoder.me/
;;
;; Permission is hereby granted, free of charge, to any person
;; obtaining a copy of this software and associated documentation
;; files (the "Software"), to deal in the Software without
;; restriction, including without limitation the rights to use,
;; copy, modify, merge, publish, distribute, sublicense, and/or sell
;; copies of the Software, and to permit persons to whom the
;; Software is furnished to do so, subject to the following
;; conditions:
;;
;; The above copyright notice and this permission notice shall be
;; included in all copies or substantial portions of the Software.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
;; OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
;; HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
;; WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
;; FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
;; OTHER DEALINGS IN THE SOFTWARE.

(require 'color-theme)

(defun color-theme-icoder-dark ()
  "iCoder color theme for Emacs by Wu Xi."
  (interactive)
  (color-theme-install
   '(color-theme-gruber-darker
     ((foreground-color . "#b2b2b2")
      (background-color . "#000000")
      (background-mode . dark)
      (cursor-color . "#00ff00")
      (mouse-color . "#00ff00"))

     ;; Standard font lock faces
     (default ((t (nil))))
     (font-lock-comment-face ((t (:foreground "#005faf"))))
     (font-lock-comment-delimiter-face ((t (:foreground "#005faf"))))
     (font-lock-doc-face ((t (:foreground "tomato3"))))
     (font-lock-doc-string-face ((t (:foreground "tomato3"))))
     (font-lock-string-face ((t (:foreground "tomato3"))))
     (font-lock-keyword-face ((t (:foreground "cyan"))))
     (font-lock-builtin-face ((t (:foreground "sandybrown"))))
     (font-lock-function-name-face ((t (:foreground "lightskyblue"))))
     (font-lock-variable-name-face ((t (:foreground "darkgoldenrod"))))
     (font-lock-preprocessor-face ((t (:foreground "magenta"))))
     ;(font-lock-constant-face ((t (:foreground "orange"))))
     (font-lock-constant-face ((t (:foreground "sandybrown"))))
     (font-lock-type-face ((t (:foreground "green3"))))
     (font-lock-warning-face ((t (:foreground "orange"))))
     (font-lock-reference-face ((t (:foreground "#95a99f"))))
     (trailing-whitespace ((t (:foreground "white" :background "grey12"))))
     (link ((t (:foreground "steelblue" :underline t))))

     ;; Compilation
     (compilation-warning ((t (:foreground "orange"))))
     (compilation-error ((t (:foreground "violet"))))
     (compilation-line-number ((t (:foreground "gold"))))
     (compilation-info ((t (:foreground "green1"))))

     ;; Search
     (isearch ((t (:background "grey35" :foreground "snow"))))
     (isearch-lazy-highlight-face ((t (:foreground "#f4f4ff" :background "#5f627f"))))
     (isearch-fail ((t (:foreground "#000" :background "#f43841"))))

     ;; User interface
     (fringe ((t (:background "#111" :foreground "#444"))))
     (border ((t (:background "#111" :foreground "#444"))))
     (mode-line ((t (:background "grey20" :foreground "white"))))
     (mode-line-buffer-id ((t (:background "grey25" :foreground "snow"))))
     (mode-line-inactive ((t (:background "grey10" :foreground "grey25"))))
     (minibuffer-prompt ((t (:foreground "#96A6C8"))))
     (region ((t (:background "grey15"))))
     (secondary-selection ((t (:background "#484951" :foreground "#F4F4FF"))))
     (tooltip ((t (:background "#52494e" :foreground "#fff"))))

     ;; Parenthesis matching
     (show-paren-match-face ((t (:background "grey32" :foreground "snow"))))
     (show-paren-mismatch-face ((t (:background "#c73c3f" :foreground "white"))))

     ;; Line highlighting
     ;(highlight ((t (:background "#282828" :foreground nil))))
     ;(highlight-current-line-face ((t (:background "#282828" :foreground nil))))

     ;; Info
     ;(info-xref ((t (:foreground "#96a6c8"))))
     ;(info-visited ((t (:foreground "#9e95c7"))))
     )))

(provide 'color-theme-icoder-dark)
