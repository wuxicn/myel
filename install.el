(add-to-list 'load-path "~/.emacs.d")

;; --- ELPA: Emacs Lisp Package Archive ---
;; http://tromey.com/elpa/index.html
;; http://melpa.org/
(add-to-list 'load-path "~/.emacs.d/elpa")
(load "package")
(package-initialize)
(setq package-archives '(("melpa" . "http://melpa.org/packages/")
                         ("ELPA" . "http://tromey.com/elpa/")
                         ("gnu" . "http://elpa.gnu.org/packages/")))
(package-list-packages)

(package-install 'session)
(package-install 'markdown-mode)
(package-install 'auto-complete)
