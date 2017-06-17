(when (>= emacs-major-version 24)
  (require 'package)
  (setq package-archives '(("gnu"       . "http://elpa.gnu.org/packages/")
                           ("marmalade" . "http://marmalade-repo.org/packages/")
                           ("melpa"     . "http://melpa.milkbox.net/packages/")))
  (package-initialize))

;;------------------------------------------------------------------------------
;; site-specific (e.g. windows/peppermint/bodhi etc) settings
(load "~/.emacs.d/site-specific.el")

;;------------------------------------------------------------------------------
(require 'cc-mode) ;- so that c-mode-map exists for key redef
(require 'dired) ;- so that dired-mode-map exists for key redef
(require 'python) ;- so that python-mode-map exists for key redef
(require 'scheme)
(load "~/.emacs.d/lua-mode") ;- custom version


;;------------------------------------------------------------------------------
(use-package bury-successful-compilation
  :config
  (bury-successful-compilation 1))

;;------------------------------------------------------------------------------
(use-package recentf
  :init
  (setq recentf-max-menu-items 25)
  (setq recentf-exclude '(".*-autoloads\\.el\\'"
                          "[/\\]\\.elpa/"
                          ".*.ido\\.last.*"
                          ".*/session.*"
                          ))
  :config
  (recentf-mode 1))

;;------------------------------------------------------------------------------
(use-package multiple-cursors
  :init
  (setq mc/mode-line
        (quote
         (" mc:"
          (:eval
           (format
            #("%d" 0 2
              (face mode-line))
            (mc/num-cursors)))))))

;;------------------------------------------------------------------------------
(use-package smex
  :init
  (smex-initialize)
  (setq smex-max-prospects 10)
  :bind
  ("M-x" . smex))

;;------------------------------------------------------------------------------
(use-package ido-ubiquitous
  :config
  (ido-ubiquitous-mode t))

;;------------------------------------------------------------------------------
(use-package bm
  :init
  (setq bm-highlight-style (quote bm-highlight-only-fringe))
  :bind
  (("C-c b"       . tkf-bm-cmd-list)
   ("C-M-<left>"  . bm-toggle)
   ("C-M-<right>" . bm-toggle)
   ("C-M-<up>"    . bm-previous)
   ("C-M-<down>"  . bm-next)))

;;------------------------------------------------------------------------------

(use-package idomenu
  :bind
  (("C-c i" . idomenu)))

;;------------------------------------------------------------------------------
(use-package eldoc
  :config
  (add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
  (add-hook 'lisp-interaction-mode-hook 'turn-on-eldoc-mode)
  (add-hook 'ielm-mode-hook 'turn-on-eldoc-mode))

;;------------------------------------------------------------------------------
(use-package org
  :init
  (setq-default org-export-html-preamble nil)
  (setq-default org-export-html-postamble nil)
  (setq org-hide-leading-stars t)
  (setq org-log-done 'time)   ; insert a timestamp when org item marked DONE
  (setq org-default-notes-file (concat org-directory "/notes.org"))
  (setq org-default-diary-file (concat org-directory "/diary.org"))
  (setq org-file-list '("notes.org" "diary.org"))
  (setq org-agenda-files `(,org-default-notes-file))
  ;;(setq org-remember-default-headline nil)
  (setq org-capture-templates
        '(("n"    "Note"  entry   (file+headline org-default-notes-file "Notes")
           "* %?")
          ("t"    "Todo"  entry   (file+headline org-default-notes-file "Todos")
           "* TODO %?")
          ("d"    "Diary" entry   (file+datetree org-default-diary-file "Diary")
           "* %U %?")
          )))

;;------------------------------------------------------------------------------
(use-package speedbar
  :init
  (setq speedbar-directory-unshown-regexp "^$")
  (setq speedbar-fetch-etags-arguments (quote ("-I" "-o" "-")))
  (setq speedbar-fetch-etags-command "etags")
  (setq speedbar-show-unknown-files nil)
  (setq speedbar-smart-directory-expand-flag t)
  (setq speedbar-supported-extension-expressions
   (quote
    (".lua" ".org" ".[ch]\\(\\+\\+\\|pp\\|c\\|h\\|xx\\)?"
     ".tex\\(i\\(nfo\\)?\\)?" ".el" ".emacs" ".l" ".lsp"
     ".p" ".java" ".js" ".f\\(90\\|77\\|or\\)?" ".scm")))
  (setq speedbar-tag-group-name-minimum-length 10)
  (setq speedbar-update-flag t)
  (setq speedbar-use-images nil)
  (setq speedbar-use-imenu-flag nil)
  (setq speedbar-verbosity-level 0)
  :config
  (speedbar-add-supported-extension ".org")
  (speedbar-add-supported-extension ".scm")
  (speedbar-add-supported-extension ".lua"))

(use-package sr-speedbar
  :init
  (setq sr-speedbar-auto-refresh nil)
  (setq sr-speedbar-default-width 31)
  (setq sr-speedbar-max-width 31)
  (setq sr-speedbar-right-side nil))

;;------------------------------------------------------------------------------
(use-package eshell
  :init
  (setq eshell-highlight-prompt t)
  (setq eshell-ls-use-colors t)
  (setq eshell-prompt-function
    (lambda nil
      (if
          (=
           (user-uid)
           0)
          "ESHELL (ROOT) »"
        #("ESHELL »" 0 8
          (rear-nonsticky
           (font-lock-face read-only)
           front-sticky
           (font-lock-face read-only)
           font-lock-face eshell-prompt read-only t)))))
  (add-hook 'eshell-mode-hook
            (lambda ()
              (local-set-key (kbd "M-p") 'eshell-previous-input)
              (local-set-key (kbd "M-n") 'eshell-next-input)
              (local-set-key (kbd "C-c h")
                             (lambda ()
                               (interactive)
                               (insert
                                (ido-completing-read "Eshell history: "
                                                     (delete-dups
                                                      (ring-elements eshell-history-ring))))))
              (local-set-key (kbd "C-c C-h") 'eshell-list-history))))

;;------------------------------------------------------------------------------
(use-package ido
  :init
  ;; This (below) effectively disables the following annoying behavior of
  ;; ido-find-file:
  ;;      1. C-x C-f
  ;;      2. Go to some random directory.
  ;;      3. Type in the name of a file which exists in ANOTHER DIRECTORY
  ;;      4. ido-find-file jumps to the OTHER DIRECTORY and waits for you
  ;;         to confirm opening the OTHER FILE.
  ;;      5. Swearing ensues.
  (setq ido-auto-merge-delay-time 99999)
  (setq ido-auto-merge-delay-time 99999)
  (setq ido-enable-flex-matching t)
  (setq ido-everywhere t)
  (setq ido-max-prospects 10)
  :config
  (ido-mode (quote both)))

;;------------------------------------------------------------------------------
(use-package ido-vertical-mode
  :init
  (setq ido-vertical-indicator "»")
  :config
  (ido-vertical-mode t)
  (setq ido-vertical-define-keys 'C-n-and-C-p-only))

;;------------------------------------------------------------------------------
(use-package cc-mode
  :config
  (setq-default c-basic-offset 4)
  (setq c-default-style
        (quote
         ((c-mode . "ellemtel")
          (java-mode . "java")
          (awk-mode . "awk")
          (other . "gnu"))))
  (add-hook 'c-mode-common-hook (lambda ()
                                  (modify-syntax-entry ?_ "w" c-mode-syntax-table))))

;;------------------------------------------------------------------------------
(use-package c-eldoc
  :config
  (add-hook 'c-mode-hook 'c-turn-on-eldoc-mode))

;;------------------------------------------------------------------------------
(use-package bury-successful-compilation
  :config
  (bury-successful-compilation 1))

;;------------------------------------------------------------------------------
;; Info setup for custom info files.
(use-package info
  :init
  (add-hook 'Info-mode-hook
            (lambda ()
              (setq Info-additional-directory-list Info-default-directory-list))))

;;------------------------------------------------------------------------------
(use-package cua-mode
  :init
  (setq cua-enable-cua-keys nil)
  (cua-mode t)
  :bind
  (("C-c d"   . cua-delete-region)
   ("C-c RET" . cua-cancel)
   ("C-c TAB" . cua-set-rectangle-mark)))

;;------------------------------------------------------------------------------
(use-package lua-mode
  :init
  (setq lua-always-show nil)
  (setq lua-indent-level 4)
  (setq lua-prefix-key "C-c"))

;;------------------------------------------------------------------------------
(use-package dired
  :init
  (setq dired-find-subdir t)
  (setq dired-use-ls-dired nil))

;;------------------------------------------------------------------------------
(use-package grep
  :init
  (setq grep-command "grep -in")
  (setq grep-find-command (quote ("find . -type f -exec grep /dev/null {} +" . 26)))
  (setq grep-find-template
   "find . <X> -type f <F> -exec grep <C> -n -e <R> /dev/null {} +")
  (setq grep-highlight-matches (quote auto))
  (setq grep-template "grep <X> <C> -n -e <R> <F>")
  (setq grep-use-null-device t))

;;------------------------------------------------------------------------------
(use-package remember
  :init
  (setq remember-leader-text "--------------- ")
  (setq remember-mode-hook (quote (org-remember-apply-template))))

;;------------------------------------------------------------------------------
;; World's best modeline
(setq-default mode-line-format
              (list mode-line-buffer-identification
                    ;; '%0x' - fixed-width prevents flickering
                    " ("
                    ") (%06p C%02c"
                    ") ("
                    '(:eval (if (buffer-modified-p) "M" "-"))
                    '(:eval (if buffer-read-only    "R" "-"))
                    '(:eval (if (window-dedicated-p (selected-window)) "D" "-"))
                    ") ("
                    '(:eval (last-dir default-directory))
                    ")  "
                    mode-line-modes
                    '(:eval vc-mode)
                    ))

;;------------------------------------------------------------------------------
;; work around the bogus "-remote" flag specified by browse-url-firefox
(setq browse-url-browser-function 'browse-url-generic)

;;------------------------------------------------------------------------------
;; Misc initialization
(server-start)
(window-numbering-mode t)
(setq-default indent-tabs-mode nil)
(setq scroll-step 1)
(setq-default tab-width 4)
(setq compilation-scroll-output t)
(column-number-mode 2)
(setq scroll-preserve-screen-position nil)
(setq scroll-conservatively 1)
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1) ((control) . nil)))
(setq mouse-wheel-progressive-speed t)
(put 'set-goal-column 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'scroll-left 'disabled nil)
(setq make-backup-files nil)
(setq-default ring-bell-function 'ignore)             ; God I hate that flashing
(setq-default fill-column 80)
(setq initial-scratch-message nil)
(setq-default cursor-type 'bar)
(fringe-mode '(15 . 15))
(setq-default max-comment-column 80)
(defalias 'list-buffers 'ibuffer)
(add-hook 'before-save-hook (lambda () (delete-trailing-whitespace)))
(set-fringe-mode 10)
(setq-default indicate-buffer-boundaries '((top . left) (bottom . right)))
(electric-indent-mode t)
(winner-mode 1)
(setq-default truncate-lines t)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(Man-notify-method (quote pushy))
 '(ac-etags-requires 1)
 '(align-text-modes (quote (text-mode outline-mode fundamental-mode)))
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-color-names-vector
   ["black" "red3" "ForestGreen" "yellow3" "blue" "magenta3" "DeepSkyBlue" "gray50"])
 '(ansi-term-color-vector
   [unspecified "#1b181b" "#ca402b" "#918b3b" "#bb8a35" "#516aec" "#7b59c0" "#516aec" "#ab9bab"] t)
 '(auto-hscroll-mode t)
 '(blink-cursor-delay 1.1)
 '(blink-cursor-interval 0.6)
 '(blink-cursor-mode t)
 '(blink-matching-delay 0)
 '(column-number-mode t)
 '(custom-safe-themes
   (quote
    ("cc5f20e3da891112f77f14d3d1650f7faa3e1051b3f9f527dcf8d71039893ac9" "3cbcddac24a78361e0d69af42f5970ab99b0ba6ca9cb47bbbfbbe680362b0b20" "c5206ba2411b3665e02f3ce0fc5d8461790a9bad8b2bb7db6604856bcfcf8b3d" "fcfcd70b25718334c188f41b33f96da3b46b6093fc4cb94cd0b0d3063ed48ec5" "3a5007fa3d1fe8fee567c59e6cdd43f1924ab320cb938fe11745710a44cac30b" "157956dd11bb3f8cd6856cc32c6df31583086da13c96b480e25e6705c3765dcd" "868360d9afa25cb16ea603a5c78655acc21db2ea508acdc63b8acb93880b3440" "8e2ebb60baf42758a76565808251aa197556d9f751e6c6a55e811ecdbd69deca" "8464a1275056fb10d7f2e712c7e3750abbf6c3ac952a005ad7c1a8e2eece88ea" "df745aa43d57560c339941c1d68d4d91d0df6e984ac511a68209960b6abd0f6d" "23b0a10ce874449818aa478c63265755639ac12ba5e1562ea012f99c3fdccea7" "d145690625dc0b4f86fbdd8651fbbb861572c57505edf4fd91be5fead58d692d" "0ee3fc6d2e0fc8715ff59aed2432510d98f7e76fe81d183a0eb96789f4d897ca" "98d0ff69fd11d6fca210b5068022d504b6eea5208233dec38212baf7201c811f" "b04153b12fbb67935f6898f38eb985ec62511fd1df6e2262069efa8565874195" "98e568e0b988a0ef8c9abdb9730ee909929167ff8932ecfb33d8cec8c3432935" "cdc683669f9425d9faf91f2fb07a508178c9e9c20ec3ce10cf6f6c2e6ac628c0" "df97fc9066acac64a021021021a809e7c421ba7c8bc7669095c6cf32f72edc22")))
 '(default-justification (quote full))
 '(desktop-save nil)
 '(desktop-save-mode nil)
 '(diff-command "diff")
 '(diff-switches "-C 5")
 '(fancy-splash-image nil)
 '(fci-rule-color "#3E4451")
 '(frame-background-mode nil)
 '(grep-command "grep -in")
 '(grep-find-command (quote ("find . -type f -exec grep /dev/null {} +" . 26)))
 '(highlight-changes-colors nil)
 '(hl-paren-colors
   (quote
    ("#B9F" "#B8D" "#B7B" "#B69" "#B57" "#B45" "#B33" "#B11")))
 '(hscroll-margin 0)
 '(hscroll-step 5)
 ;'(imenu-auto-rescan t)
 ;'(imenu-max-items 100)
 '(inhibit-startup-echo-area-message t)
 '(inhibit-startup-screen t)
 '(initial-buffer-choice nil)
 '(isearch-lazy-highlight t)
 '(kill-ring-max 200)
 '(kill-whole-line t)
 '(lazy-highlight-cleanup t)
 '(linum-format "%-4d")
 '(make-backup-files nil)
 '(menu-bar-mode nil)
 '(message-default-charset (quote iso-8859-1))
 '(mouse-wheel-mode t)
 '(nrepl-message-colors
   (quote
    ("#CC9393" "#DFAF8F" "#F0DFAF" "#7F9F7F" "#BFEBBF" "#93E0E3" "#94BFF3" "#DC8CC3")))
 '(org-fontify-whole-heading-line t)
 '(package-selected-packages
   (quote
    (sr-speedbar imenu-anywhere c-eldoc ac-clang ac-etags creamsody-theme nasm-mode bury-successful-compilation markdown-mode+ ido-vertical-mode syntax-subword idomenu ido-ubiquitous ido-select-window use-package window-numbering bm move-text smex magit multiple-cursors visual-regexp expand-region popup-kill-ring peg)))
 '(pdf-view-midnight-colors (quote ("#DCDCCC" . "#383838")))
 '(pos-tip-background-color "#1A3734")
 '(pos-tip-foreground-color "#FFFFC8")
 '(ruler-mode-show-tab-stops nil)
 '(save-place t nil (saveplace))
 '(scroll-bar-mode nil)
 '(scroll-conservatively 1)
 '(scroll-preserve-screen-position t)
 '(scroll-step 1)
 '(send-mail-function (quote mailclient-send-it))
 '(shift-select-mode nil)
 '(show-paren-mode t)
 '(standard-indent 4)
 '(tool-bar-mode nil)
 '(tooltip-mode nil)
 '(uniquify-buffer-name-style (quote forward) nil (uniquify))
 '(vc-annotate-background nil)
 '(vc-annotate-color-map
   (quote
    ((20 . "#bf616a")
     (40 . "#DCA432")
     (60 . "#ebcb8b")
     (80 . "#B4EB89")
     (100 . "#89EBCA")
     (120 . "#89AAEB")
     (140 . "#C189EB")
     (160 . "#bf616a")
     (180 . "#DCA432")
     (200 . "#ebcb8b")
     (220 . "#B4EB89")
     (240 . "#89EBCA")
     (260 . "#89AAEB")
     (280 . "#C189EB")
     (300 . "#bf616a")
     (320 . "#DCA432")
     (340 . "#ebcb8b")
     (360 . "#B4EB89"))))
 '(vc-annotate-very-old-color nil)
 '(visible-bell nil)
 '(window-divider-default-right-width 1)
 '(window-divider-mode t)
 '(woman-locale "en_US.UTF-8"))

(load-theme 'tkf-dark t)

;;------------------------------------------------------------------------------
;; MISC FUNCTIONS THAT I'VE SNAGGED OR WRITTEN

;;(defun switch-to-scratch-buffer ()
;;  (interactive)
;;  (switch-to-buffer "*scratch*"))
;;
;;(defun buffer-vector ()
;;  (apply 'vector (buffer-list)))
;;
;;
;;
;;(defun switch-to-file-buff ()
;;  (interactive)
;;  (let ((buffers (buffer-vector)))
;;    (do ((i 0 (1+ i)))
;;        (

(defun push-mark-no-activate ()
  "Pushes `point' to `mark-ring' and does not activate the region Equivalent to \\[set-mark-command] when \\[transient-mark-mode] is disabled"
  (interactive)
  (push-mark (point) t nil)
  (message "Pushed mark to ring"))

;;------------------------------------------------------------------------------

(defun jump-to-mark ()
  "Jumps to the local mark, respecting the `mark-ring' order. This is the same as using \\[set-mark-command] with the prefix argument."
  (interactive)
  (set-mark-command 1))

;;------------------------------------------------------------------------------
(defun tkf-make-frame ()
  (interactive)
  (make-frame `((font . ,default-font))))

;;------------------------------------------------------------------------------
;; From emacswiki
(defun copy-line (arg)
      "Copy lines (as many as prefix argument) in the kill ring"
      (interactive "p")
      (kill-ring-save (line-beginning-position)
                      (line-beginning-position (+ 1 arg)))
      (message "%d line%s copied" arg (if (= 1 arg) "" "s")))

;;------------------------------------------------------------------------------
;; From stackoverflow
(defun my-ido-find-tag ()
  "Find a tag using ido"
  (interactive)
  (tags-completion-table)
  (let* ((initial-input
          (funcall (or find-tag-default-function
                       (get major-mode 'find-tag-default-function)
                       'find-tag-default)))
         (initial-input-regex (concat "\\(^\\|::\\)" initial-input "$")))
    (find-tag (ido-completing-read
               "Tag: "
               (sort
                (remove nil
                        (mapcar (lambda (tag) (unless (integerp tag)
                                                (prin1-to-string tag 'noescape)))
                                tags-completion-table))
                ;; put those matching initial-input first:
                (lambda (a b) (string-match initial-input-regex a)))
               nil
               'require-match
               initial-input))))

(defun xah-search-current-word (arg)
  "Call `isearch' on current word or text selection.
“word” here is A to Z, a to z, and hyphen 「-」 and underline 「_」, independent of syntax table.
URL `http://ergoemacs.org/emacs/modernization_isearch.html'
Version 2015-04-09"
  (interactive)
  (if (eq arg 1)
      (message "arg is 1")
    (message "arg is not 1"))
  (let ( ξp1 ξp2 )
    (if (use-region-p)
        (progn
          (setq ξp1 (region-beginning))
          (setq ξp2 (region-end)))
      (save-excursion
        (skip-chars-backward "-_A-Za-z0-9")
        (setq ξp1 (point))
        (right-char)
        (skip-chars-forward "-_A-Za-z0-9")
        (setq ξp2 (point))))
    (setq mark-active nil)
    (when (< ξp1 (point))
      (goto-char ξp1))
    (cond ((eq arg 1)
           (isearch-mode t))
          ((eq arg -1)
           (isearch-mode nil))
          (t
           (isearch-mode t)))
    (isearch-yank-string (buffer-substring-no-properties ξp1 ξp2))))

(defun last-dir (p)
  (if (equal system-type 'windows-nt)
      (last-dir-windows p)
    (last-dir-unix p)))

(defun last-dir-windows (p)
  (let ((dirs (reverse (split-string p "\\(\\\\\\|/\\)"))))
    (cond ((equal (car dirs) "")
           (cadr dirs))
          (t (car dirs)))))

(defun last-dir-unix (p)
  (let ((dirs (reverse (split-string p "/"))))
    (cond ((and (equal (car dirs) "")
                (equal (cadr dirs) ""))
           "/")   ; root dir
          ((and (equal (car dirs) "")
                (equal (cadr dirs) "~"))
           "~")   ; home dir
          (t (cadr dirs)))))

(defun tkf-back-blank ()
  (interactive)
  (if (bolp)
      (progn (previous-line)
             (end-of-line))
    (if (looking-back " ")
        (skip-chars-backward " ")
        (skip-chars-backward "^ "))))

(defun tkf-forward-blank ()
  (interactive)
  (if (eolp)
      (progn (beginning-of-line)
             (next-line))
    (if (looking-at " ")
        (skip-chars-forward " ")
        (skip-chars-forward "^ "))))

(defun tkf-beginning-of-line ()
  "Move cursor to first nonblank character on current line
   preceeding point. If no such character exists, then leave cursor at the
   beginning of the line.  If cursor is already at the beginning of the line,
   then move to the first nonblank character on the line."
  (interactive)
  (let ((pos (if (= (point) (line-beginning-position))
                 (line-end-position)
                 (point))))
    (move-beginning-of-line nil)
    (while (and (looking-at " ")
                (< (point) pos))
      (forward-char))
    (if (>= (point) pos)
        (move-beginning-of-line nil))))

(defun tkf-newline-and-indent ()
  "Similar to the ordinary Emacs 'newline-and-indent' but no indentation
   occurs if the previous line is all whitespace"
  (interactive "")
  (let ((do-newline-and-indent t))
    (save-excursion
      ;(forward-line -1)
      (beginning-of-line)
      (if (looking-at " *$")
          (setq do-newline-and-indent nil)))
    (if do-newline-and-indent
        (newline-and-indent)
      (newline))))

(defun tkf-fullscreen (&optional f)
  (interactive)
  (set-frame-parameter f 'fullscreen
                       (if (frame-parameter f 'fullscreen) nil 'fullboth)))

(defun toggle-sticky-buffer-window ()
  "Toggle whether this window is dedicated to this buffer."
  (interactive)
  (set-window-dedicated-p
   (selected-window)
   (not (window-dedicated-p (selected-window))))
  (if (window-dedicated-p (selected-window))
      (message "Window is now dedicated.")
  (message "Window is no longer dedicated.")))

(defun tkf-insert-divider-comment ()
  (interactive "*")
  (let ((dashes #'(lambda ()
                    (let ((mc 0))
                      (if (not (boundp 'max-comment-column))
                          (progn (message "max-comment-column not defined.  Using 80 as a default")
                                 (setq mc 80))
                        (setq mc max-comment-column))
                      (if (< (current-column) max-comment-column)
                          (let ((c (current-column)))
                            (dotimes (i (- max-comment-column c))
                              (insert "-"))))))))
    (cond ((or (eq major-mode 'lisp-interaction-mode)
               (eq major-mode 'lisp-mode)
               (eq major-mode 'scheme-mode)
               (eq major-mode 'emacs-lisp-mode))
           (insert ";;")
           (funcall dashes))
          ((or (eq major-mode 'c++-mode)
               (eq major-mode 'c-mode)
               (eq major-mode 'java-mode)
               (eq major-mode 'asy-mode))
           (insert "//")
           (funcall dashes))
          ((or (eq major-mode 'awk-mode)
               (eq major-mode 'python-mode))
           (insert "#")
           (funcall dashes))
          (t
           (funcall dashes)))))

(if (eq system-type 'windows-nt)
    ;; setting the PC keyboard's various keys to
    ;; Super or Hyper, for emacs running on Windows.
    ;; From: http://xahlee.blogspot.com/2010/08/emacs-hyper-and-super-keys.html
    (setq w32-pass-lwindow-to-system    nil
          w32-pass-rwindow-to-system    nil
          w32-pass-apps-to-system       nil
          w32-lwindow-modifier          'super   ;; Left Windows key
          w32-rwindow-modifier          'super)) ;; Right Windows key

(defun leftmost-nonblank-position ()
  (let ((pos nil))
    (save-excursion
      (beginning-of-line)
      (setq pos (point))
      (while (and (< (point) (line-end-position))
                  (looking-at "\\s-"))
        (forward-char))
      (if (not (looking-at "\\s-"))
          (setq pos (point)))
    pos)))

(defun xahlee-open-in-external-app ()
  "Open the current file or dired marked files in external app."
  (interactive)
  (let ( doIt
         (myFileList
          (cond
           ((string-equal major-mode "dired-mode") (dired-get-marked-files))
           (t (list (buffer-file-name))))))
    (setq doIt (if (<= (length myFileList) 5)
                   t
                 (y-or-n-p "Open more than 5 files?")))
    (when doIt
      (cond
       ((string-equal system-type "windows-nt")
        (mapc (lambda (fPath) (w32-shell-execute "open" (replace-regexp-in-string "/" "\\" fPath t t))) myFileList))
       ((string-equal system-type "darwin")
        (mapc (lambda (fPath) (shell-command (format "open \"%s\"" fPath)))  myFileList))
       ((string-equal system-type "gnu/linux")
        (mapc (lambda (fPath) (let ((process-connection-type nil)) (start-process "" nil "xdg-open" fPath)) ) myFileList))))))

(defun tkf-bm-cmd-list ()
  (interactive)
  (let ((c (ido-completing-read "BM:" '("mrk" "nxt" "prv" "regx" "clr") )))
    (cond
     ((string= c "mrk")
      (bm-toggle))
     ((string= c "nxt")
      (bm-next))
     ((string= c "prv")
      (bm-previous))
     ((string= c "regx")
      (bm-bookmark-regexp))
     ((string= c "clr")
      (bm-remove-all-current-buffer)))))

(defun tkf-org-open-file ()
  (interactive)
  (let ((f (ido-completing-read "ORG:" org-file-list)))
    (find-file (concat org-directory "/" f))))

;; END OF MISC FUNCTIONS THAT I'VE SNAGGED
;;------------------------------------------------------------------------------

;;------------------------------------------------------------------------------
;; Custom bindings

(global-set-key (kbd "C-x k")           'kill-this-buffer)
(global-set-key (kbd "C-M-;")           'next-buffer)
(global-set-key (kbd "C-M-'")           'previous-buffer)
;(global-set-key (kbd "C-M-'")           'push-mark-no-activate)
;(global-set-key (kbd "C-M-;")           'jump-to-mark)
(global-set-key (kbd "C-c k")             (lambda ()
                                          (interactive)
                                          (let ((col (current-column)))
                                            (kill-whole-line)
                                            (move-to-column col t))))
(global-set-key (kbd "C-c c")           'tkf-insert-divider-comment)
(global-set-key (kbd "C-c j")           (lambda () (interactive) (join-line -1)))
(global-set-key (kbd "C-c n")           'org-capture)
(global-set-key (kbd "C-c p")           'toggle-sticky-buffer-window)
(global-set-key (kbd "C-c o")           'tkf-org-open-file)
(global-set-key (kbd "C-c t")           'my-ido-find-tag)
(global-set-key (kbd "C-c u")           (lambda () (interactive)
                                          (backward-word)
                                          (upcase-word 1)))
(global-set-key (kbd "C-c s")           (lambda () (interactive)
                                          (xah-search-current-word 1)))
(global-set-key (kbd "C-c r")           (lambda () (interactive)
                                          (xah-search-current-word -1)))
(global-set-key (kbd "C-c y")           'popup-kill-ring)
(global-set-key (kbd "C-c l")           'copy-line)
(global-set-key (kbd "C-.")             'mc/mark-next-like-this)
(global-set-key (kbd "C-,")             'mc/mark-previous-like-this)
(global-set-key (kbd "C-/")             'mc/mark-all-like-this)
(global-set-key (kbd "M-z")             'zap-up-to-char)
(global-set-key (kbd "M-Z")             'zap-to-char)
(global-set-key (kbd "C-z")             'undo)
(global-set-key (kbd "M-[")             'move-text-up)
(global-set-key (kbd "M-]")             'move-text-down)
(global-set-key (kbd "C-a")             'tkf-beginning-of-line)
(global-set-key (kbd "C-=")             'er/expand-region)
(global-set-key (kbd "C-x C-r")         'recentf-open-files)
(global-set-key (kbd "<C-right>")       'tkf-forward-blank)
(global-set-key (kbd "<C-left>")        'tkf-back-blank)
(global-set-key (kbd "C-M-<return>")    'hs-toggle-hiding)
(global-set-key (kbd "<M-up>")          'windmove-up)
(global-set-key (kbd "<M-down>")        'windmove-down)
(global-set-key (kbd "<M-left>")        'windmove-left)
(global-set-key (kbd "<M-right>")       'windmove-right)


;;--- The "a" key won't work properly in dired mode when a window is protected
;;    so unprotect it briefly, do the "a"-thing, then reprotect.
(define-key dired-mode-map (kbd "a")
  #'(lambda ()
      (interactive "")
      (let ((ded (window-dedicated-p (selected-window))))
        (if ded
            (set-window-dedicated-p (selected-window) nil))
        (dired-find-alternate-file)
        (if ded
            (set-window-dedicated-p (selected-window) t)))))

(define-key dired-mode-map (kbd "o") 'xahlee-open-in-external-app)

;;------------------------------------------------------------------------------
;; Lisp/scheme mode customizations

(cond ((eq system-type 'windows-nt)
       (setq scheme-program-name "gracket-text.exe"))
      ((eq system-type 'gnu/linux)
       (setq scheme-program-name "gracket-text")))

;;------------------------------------------------------------------------------
;; Python-mode customizationsp

(setq-default python-indent 4)
(setq-default py-indent-offset 4)

(put 'narrow-to-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'dired-find-alternate-file 'disabled nil)
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
