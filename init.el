(package-initialize)

(add-to-list 'load-path "~/.emacs.d/lisp")

(load "~/site-specific.el")

;;* Requires/includes

(require 'cc-mode) ;- so that c-mode-map exists for key redef
(require 'dired) ;- so that dired-mode-map exists for key redef
(require 'python) ;- so that python-mode-map exists for key redef
(require 'scheme)
(require 'kill-ring-ido)
(require 'subr-x)

;;* World's best modeline
;; Note: mode-line-format must occur before (window-numbering-mode 1)
;;       otherwise window numbers won't show.
(setq-default mode-line-format
              (list mode-line-buffer-identification
                    " │ "
                    ;; window number goes here
                    ;; '%0x' - fixed-width prevents flickering
                    " │ %06p C%02c │ "
                    '(:eval (if (buffer-modified-p) "M" "-"))
                    '(:eval (if buffer-read-only    "R" "-"))
                    '(:eval (if overwrite-mode      "O" "I"))
                    '(:eval (if (window-dedicated-p (selected-window)) "D" "-"))
                    " │ "
                    '(:eval (last-dir default-directory))
                    " │ "
                    mode-line-modes
                    ;;minor-mode-alist
                    '(:eval (if current-input-method
                                (concat  " │ " current-input-method " │ ")
                              ""))
                    '(:eval vc-mode)
                    ))

(use-package fill-column-indicator
  :ensure t
  :init
  (setq-default fill-column 82))

(use-package git-gutter
  :ensure t
  :init
  (global-git-gutter-mode t)
  :bind
  (("C-c <up>" . git-gutter:previous-diff)
   ("C-c <down>" . git-gutter:next-diff)))

(use-package form-feed
  :ensure t
  :init
  (add-hook 'emacs-lisp-mode-hook (lambda () (form-feed-mode t)))
  (add-hook 'c-mode-common-hook  (lambda ()
                                   (form-feed-mode t)))
  (setq-default form-feed-line-width (- fill-column 1)))


(use-package window-numbering
  :ensure t
  :init
  (window-numbering-mode 1))

(use-package iedit
  :ensure t)

(use-package windresize
  :ensure t
  :bind
  (("C-c w" . windresize)))

(use-package magit
  :ensure t)

(use-package dired
  :init
  (add-hook 'dired-mode-hook 'all-the-icons-dired-mode))

(use-package ace-jump-mode
  :ensure t
  :bind
  (("C-c j" . ace-jump-mode)))

;; TODO: figure out why python-mode cannot be loaded here.
;; (use-package python-mode
;;   :config
;;   (setq python-indent-offset 2))

(use-package ido-occur
  :ensure t
  :bind
  (("C-c l" . ido-occur)
   ("C-c L" . ido-occur-at-point)
   :map isearch-mode-map
   ("C-o" . ido-occur-from-isearch)))

(use-package visual-regexp
  :ensure t)

(use-package multiple-cursors
  :ensure t
  :init
  (setq mc/mode-line
        (quote
         (" mc:"
          (:eval
           (format
            #("%d" 0 2
              (face mode-line))
            (mc/num-cursors))))))
  :bind
  (("C-." . mc/mark-next-like-this)
   ("C-," . mc/mark-previous-like-this)
   ("C-/" . mc/mark-all-like-this)))

(use-package move-text
  :ensure t
  :bind
  ("<M-up>" .   move-text-up)
  ("<M-down>" . move-text-down))

(use-package smex
  :ensure t
  :init
  (smex-initialize)
  (setq smex-max-prospects 10)
  :bind
  ("M-x" . smex))

(use-package ido-completing-read+
  :ensure t
  :config
  (ido-ubiquitous-mode t))

(use-package bm
  :ensure t
  :init
  (setq bm-highlight-style (quote bm-highlight-only-fringe))
  :bind
  (("M-]"   . bm-next)
   ("M-["   . bm-previous)
   ("C-c m" . bm-toggle)))

(use-package expand-region
  :ensure t
  :bind
  (("C-=" . er/expand-region)))



;; just go with something simple that works in all conceivable situations
(defun tkf-open-note ()
  (interactive "")
  (ido-find-file-in-dir org-directory))

(defun tkf-insert-file-link ()
  (interactive "")
  (let ((file-name (read-file-name "File to link:" org-directory))
        (title (read-from-minibuffer "Name (enter for none):")))
    (if (equal (string-trim title) "")
        (insert (concat "[[" file-name "]]"))
      (insert (concat "[[file:" file-name "][" title "]]")))))



(use-package org
  :init
  (setq-default org-export-html-preamble nil)
  (setq-default org-export-html-postamble nil)
  (setq org-hide-leading-stars t)
  (setq org-log-done 'time)   ; insert a timestamp when org item marked DONE
  ;;(setq org-default-diary-file (concat org-directory "/diary.org"))
  ;;(setq org-default-checkitem-file (concat org-directory "/checkitem.org"))
  ;;(setq org-default-log-file (concat org-directory "/log.org"))
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode t)))
  (setq org-todo-keywords
       '((sequence "TODO" "WRITTEN" "TESTED" "|" "DONE" "FIX")))
  ;;(setq org-remember-default-headline nil)
  ;;(setq-default org-capture-templates
  ;;              '(("n" "Note" plain
  ;;                 ;;(file tkf-new-note-filepath)
  ;;                 (file "/home/todd/Documents/Notes/test.org")
  ;;                 "* %?")
  ;;                ("t" "Todo"  checkitem
  ;;                 (file+olp+datetree org-default-checkitem-file "Checkitems")
  ;;                 "[ ] %?")
  ;;        ;; ("d"    "Diary" entry     (file+olp+datetree org-default-diary-file "Diary")
  ;;        ;;  "* %U %?")
  ;;        ;;("T"    "TEST"  plain     (file (gen-file
  ;;        ;("l"    "Log"
  ;;        ))
  :bind
  (("C-c o" . tkf-open-note)
   ("C-c L" . tkf-insert-file-link)))
   ;;("C-c n" . org-capture)))  ;; never used org-capture

(use-package org-journal
  :ensure t)

(defun imenu-lisp ()
  (add-to-list 'imenu-generic-expression
               '("defrule" "^\\s-*(defrule\\s-+\\([^[:space:]]+\\)\\s-*$" 1))
  (add-to-list 'imenu-generic-expression
               '("Sect" "^\\s-*;;\\*\\s-*\\(.*\\)$" 1))
  (add-to-list 'imenu-generic-expression
               '("defbinaryop" "^\\s-*(defbinaryop\\s-+\\([^[:space:]]+\\).*$" 1)))

(defun imenu-emacs-lisp ()
  (add-to-list 'imenu-generic-expression
               '("Sect" "^\\s-*;;\\*\\s-*\\(.*\\)$" 1))
  (add-to-list 'imenu-generic-expression
               '("use-package" "^\\s-*(use-package\\s-*\\(.+\\)$" 1)))

(defun imenu-c ()
  (add-to-list 'imenu-generic-expression
               '("Macro"
                 "^\\s-*\#\\s-*define\\s-+\\([a-zA-Z_][a-zA-Z_0-9]*\\)"
                 1))
  (add-to-list 'imenu-generic-expression
               '("enum"
                 "^\\s-*enum\\s-+\\([a-zA-Z_][a-zA-Z_0-9]*\\)"
                 1))
  (add-to-list 'imenu-generic-expression
               '("Section"
                 "^\\s-*///\\s-*\\(.*\\)$"
                 1))
  (add-to-list 'imenu-generic-expression
               '("typedef struct"
                 "^\\s-*typedef struct\\s-+\\([a-zA-Z_][a-zA-Z_0-9]*\\)\\s-*{\\s-*$"
                 1)))

(use-package imenu
  :init
  (setq imenu-auto-rescan t)
  :bind
  (("C-c i" . imenu))
  :hook
  (lisp-mode . imenu-lisp)
  (emacs-lisp-mode . imenu-emacs-lisp)
  (c-mode-common-hook . imenu-c))

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

(use-package ido-vertical-mode
  :ensure t
  :init
  (setq ido-vertical-indicator "»")
  (setq ido-vertical-show-count t)
  :config
  (ido-vertical-mode nil)
  (setq ido-vertical-define-keys 'C-n-and-C-p-only))

(use-package cc-mode
  :config
  (setq-default c-basic-offset 2)
  (setq c-default-style
        (quote
         ((c-mode    . "ellemtel")
          (java-mode . "java")
          (awk-mode  . "awk")
          (other     . "gnu"))))
  :init
  (add-hook 'c-mode-common-hook  (lambda ()
                                   (modify-syntax-entry ?_ "w" c-mode-syntax-table)
                                   (fci-mode t))))

;; Info setup for custom info files.
(use-package info
  :init
  (add-hook 'Info-mode-hook
            (lambda ()
              (setq Info-additional-directory-list '("/home/todd/info")))))

(use-package slime
  :ensure t
  :init
  (setq inferior-lisp-program "/usr/bin/ecl"))

;; Unfortunately cua mode must be enabled to use its awesome rectangle commands
;; We just disable the cua keys here and pretend that it doesn't exist.
(use-package cua-mode
  :init
  (setq cua-enable-cua-keys nil)
  (cua-mode)
  :bind
  ("C-c d" . cua-delete-region)
  ("C-c RET" . cua-cancel)
  ("C-c TAB" . cua-set-rectangle-mark))

;;(use-package lua-mode
;;  :ensure t
;;  :init
;;  (setq lua-always-show nil)
;;  (setq lua-indent-level 2)
;;  (setq lua-prefix-key "C-c"))

(use-package dired
  :init
  (setq dired-find-subdir t)
  (setq dired-use-ls-dired nil))

(use-package grep
  :init
  (setq grep-command "grep -in")
  (setq grep-find-command (quote ("find . -type f -exec grep /dev/null {} +" . 26)))
  (setq grep-find-template
   "find . <X> -type f <F> -exec grep <C> -n -e <R> /dev/null {} +")
  (setq grep-highlight-matches (quote auto))
  (setq grep-template "grep <X> <C> -n -e <R> <F>")
  (setq grep-use-null-device t))

(use-package remember
  :init
  (setq remember-leader-text "--------------- ")
  (setq remember-mode-hook (quote (org-remember-apply-template))))

;;* Vars set by 'custom'
;; TODO: someday move all of this crap out into 'use-package' blocks where it belongs.
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(Man-notify-method 'pushy)
 '(align-text-modes '(text-mode outline-mode fundamental-mode))
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-color-names-vector
   ["black" "red3" "ForestGreen" "yellow3" "blue" "magenta3" "DeepSkyBlue" "gray50"])
 '(ansi-term-color-vector
   [unspecified "#1b181b" "#ca402b" "#918b3b" "#bb8a35" "#516aec" "#7b59c0" "#516aec" "#ab9bab"] t)
 '(auto-dim-other-buffers-mode nil)
 '(auto-hscroll-mode t)
 '(beacon-color "#ff9da4")
 '(blink-cursor-blinks 0)
 '(blink-cursor-delay 1.1)
 '(blink-cursor-interval 0.45)
 '(blink-cursor-mode t)
 '(blink-matching-delay 0)
 '(byte-compile-delete-errors t)
 '(byte-compile-warnings '(not cl-functions))
 '(column-number-mode t)
 '(company-quickhelp-color-background "#4F4F4F")
 '(company-quickhelp-color-foreground "#DCDCCC")
 '(compilation-message-face 'default)
 '(compilation-scroll-output t)
 '(compile-command "./build.sh")
 '(cua-global-mark-cursor-color "#2aa198")
 '(cua-normal-cursor-color "#657b83")
 '(cua-overwrite-cursor-color "#b58900")
 '(cua-read-only-cursor-color "#859900")
 '(custom-safe-themes
   '())
 '(default-input-method "sgml")
 '(default-justification 'full)
 '(deft-use-filename-as-title nil)
 '(deft-use-filter-string-for-filename t)
 '(desktop-save nil)
 '(desktop-save-mode nil)
 '(diff-command "diff")
 '(diff-switches "-C 5")
 '(dired-sidebar-one-instance-p t)
 '(doom-modeline-mode t)
 '(fancy-splash-image nil)
 '(fci-rule-color "#383838")
 '(flycheck-color-mode-line-face-to-color 'mode-line-buffer-id)
 '(font-lock-global-modes '(not speedbar-mode))
 '(frame-background-mode nil)
 '(grep-command "grep -in")
 '(grep-find-command '("find . -type f -exec grep /dev/null {} +" . 26))
 '(highlight-changes-colors '("#d33682" "#6c71c4"))
 '(highlight-symbol-foreground-color "#586e75")
 '(highlight-tail-colors
   '(("#eee8d5" . 0)
     ("#B4C342" . 20)
     ("#69CABF" . 30)
     ("#69B7F0" . 50)
     ("#DEB542" . 60)
     ("#F2804F" . 70)
     ("#F771AC" . 85)
     ("#eee8d5" . 100)))
 '(hl-bg-colors
   '("#DEB542" "#F2804F" "#FF6E64" "#F771AC" "#9EA0E5" "#69B7F0" "#69CABF" "#B4C342"))
 '(hl-fg-colors
   '("#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3"))
 '(hl-paren-colors '("#B9F" "#B8D" "#B7B" "#B69" "#B57" "#B45" "#B33" "#B11"))
 '(hl-todo-keyword-faces
   '(("HOLD" . "#e5f040")
     ("TODO" . "#feacd0")
     ("NEXT" . "#b6a0ff")
     ("THEM" . "#ed92f8")
     ("PROG" . "#00d3d0")
     ("OKAY" . "#4ae8fc")
     ("DONT" . "#58dd13")
     ("FAIL" . "#ff8059")
     ("DONE" . "#44bc44")
     ("NOTE" . "#f0ce43")
     ("KLUDGE" . "#eecc00")
     ("HACK" . "#eecc00")
     ("TEMP" . "#ffcccc")
     ("FIXME" . "#ff9977")
     ("XXX+" . "#f4923b")
     ("REVIEW" . "#6ae4b9")
     ("DEPRECATED" . "#aaeeee")))
 '(hscroll-margin 0)
 '(hscroll-step 5)
 '(ibuffer-deletion-face 'dired-flagged)
 '(ibuffer-filter-group-name-face 'dired-mark)
 '(ibuffer-marked-face 'dired-marked)
 '(ibuffer-title-face 'dired-header)
 '(inhibit-startup-echo-area-message t)
 '(inhibit-startup-screen t)
 '(initial-buffer-choice nil)
 '(isearch-lazy-highlight t)
 '(jdee-db-active-breakpoint-face-colors (cons "#161a2a" "#82aaff"))
 '(jdee-db-requested-breakpoint-face-colors (cons "#161a2a" "#c3e88d"))
 '(jdee-db-spec-breakpoint-face-colors (cons "#161a2a" "#444a73"))
 '(kill-ring-max 200)
 '(kill-whole-line t)
 '(lazy-highlight-cleanup t)
 '(line-number-mode nil)
 '(linum-format "%-4d")
 '(lsp-ui-doc-border "#93a1a1")
 '(lua-always-show nil t)
 '(lua-default-application "lua5.3")
 '(lua-indent-level 2 t)
 '(magit-diff-use-overlays nil)
 '(menu-bar-mode nil)
 '(message-default-charset 'iso-8859-1)
 '(mouse-wheel-mode t)
 '(nrepl-message-colors
   '("#CC9393" "#DFAF8F" "#F0DFAF" "#7F9F7F" "#BFEBBF" "#93E0E3" "#94BFF3" "#DC8CC3"))
 '(objed-cursor-color "#ff757f")
 '(org-fontify-whole-heading-line t)
 '(package-selected-packages
   '(iedit org-journal git-gutter ace-jump-mode slime graphviz-dot-mode form-feed all-the-icons-dired all-the-icons org-bullets window-numbering windresize ido-occur fill-column-indicator rectangle-utils imenu-anywhere nasm-mode markdown-mode+ ido-vertical-mode syntax-subword ido-ubiquitous ido-select-window use-package bm move-text smex magit multiple-cursors visual-regexp expand-region peg))
 '(pdf-view-midnight-colors '("#DCDCCC" . "#383838"))
 '(pos-tip-background-color "#eee8d5")
 '(pos-tip-foreground-color "#586e75")
 '(python-indent-offset 2)
 '(python-shell-interpreter "python3")
 '(recentf-exclude '("session.*"))
 '(recentf-mode t)
 '(ruler-mode-show-tab-stops nil)
 '(rustic-ansi-faces
   ["#212337" "#ff757f" "#c3e88d" "#ffc777" "#82aaff" "#c099ff" "#b4f9f8" "#c8d3f5"])
 '(save-place t nil (saveplace))
 '(scroll-bar-mode nil)
 '(scroll-conservatively 1)
 '(scroll-preserve-screen-position t)
 '(scroll-step 1)
 '(send-mail-function 'mailclient-send-it)
 '(shift-select-mode nil)
 '(show-paren-mode t)
 '(show-paren-style 'parenthesis)
 '(show-paren-when-point-inside-paren t)
 '(smartrep-mode-line-active-bg (solarized-color-blend "#859900" "#eee8d5" 0.2))
 '(speedbar-directory-unshown-regexp "^$")
 '(speedbar-fetch-etags-arguments '("-I" "-o" "-"))
 '(speedbar-fetch-etags-command "etags")
 '(speedbar-ignored-modes '(fundamental-mode Info-mode))
 '(speedbar-tag-group-name-minimum-length 25)
 '(speedbar-tag-regroup-maximum-length 32)
 '(speedbar-tag-split-minimum-length 10)
 '(speedbar-update-flag t)
 '(speedbar-use-imenu-flag t)
 '(speedbar-verbosity-level 0)
 '(sr-speedbar-auto-refresh nil)
 '(sr-speedbar-default-width 20 t)
 '(sr-speedbar-max-width 20 t)
 '(sr-speedbar-right-side nil)
 '(standard-indent 2)
 '(term-default-bg-color "#fdf6e3")
 '(term-default-fg-color "#657b83")
 '(tool-bar-mode nil)
 '(tooltip-mode nil)
 '(uniquify-buffer-name-style 'forward nil (uniquify))
 '(vc-annotate-background "#2B2B2B")
 '(vc-annotate-background-mode nil)
 '(vc-annotate-color-map
   '((20 . "#BC8383")
     (40 . "#CC9393")
     (60 . "#DFAF8F")
     (80 . "#D0BF8F")
     (100 . "#E0CF9F")
     (120 . "#F0DFAF")
     (140 . "#5F7F5F")
     (160 . "#7F9F7F")
     (180 . "#8FB28F")
     (200 . "#9FC59F")
     (220 . "#AFD8AF")
     (240 . "#BFEBBF")
     (260 . "#93E0E3")
     (280 . "#6CA0A3")
     (300 . "#7CB8BB")
     (320 . "#8CD0D3")
     (340 . "#94BFF3")
     (360 . "#DC8CC3")))
 '(vc-annotate-very-old-color "#DC8CC3")
 '(visible-bell nil)
 '(weechat-color-list
   '(unspecified "#fdf6e3" "#eee8d5" "#990A1B" "#dc322f" "#546E00" "#859900" "#7B6000" "#b58900" "#00629D" "#268bd2" "#93115C" "#d33682" "#00736F" "#2aa198" "#657b83" "#839496"))
 '(which-key-mode t)
 '(window-divider-default-right-width 1)
 '(window-divider-mode t)
 '(woman-locale "en_US.UTF-8")
 '(xterm-color-names
   ["#eee8d5" "#dc322f" "#859900" "#b58900" "#268bd2" "#d33682" "#2aa198" "#073642"])
 '(xterm-color-names-bright
   ["#fdf6e3" "#cb4b16" "#93a1a1" "#839496" "#657b83" "#6c71c4" "#586e75" "#002b36"]))

;;* Misc functions that i've snagged or written

(defun prev-window ()
  (interactive)
  (other-window -1))

(defun switch-to-next-file-buffer ()
  (interactive)
  (dolist (buff (reverse (cdr (buffer-list))))
    (when (and (buffer-file-name buff)
               (not (member "TAGS"
                            (split-string (buffer-file-name buff)
                                          "/"))))
      (switch-to-buffer buff)
      (return))))

(setq-default *uninteresting-buffer-regexps*
              '("TAGS"
                "\*Messages\*"
                "\*Backtrace\*"
                "\*Warnings\*"
                "^ *\*code-conversion-work\*"
                " *\*Minibuf-[0-9]+\*"
                " *\*Echo Area [0-9]+\*"
                " *\*which-key\*"
                " *\*code-conversion-work\*"
                ))

(defun string!= (s0 s1)
  (or (or (not (eq (type-of s0) 'string))
          (not (eq (type-of s1) 'string))
          (not (string= s0 s1)))))

(defmacro fn (args &rest body)
  `(lambda ,args ,@body))

(defun switch-to-next-interesting-buffer ()
  (interactive)
  (dolist (buff (reverse (cdr (buffer-list))))
    (when (and (not (seq-filter (fn (r) (string-match r (buffer-name buff)))
                                *uninteresting-buffer-regexps*))
               (string!= (buffer-file-name)
                         (buffer-file-name buff)))
      (switch-to-buffer buff)
      (return))))

(defun copy-whole-line-trim ()
  (interactive)
  (kill-new (string-trim-left (string-trim-right
                               (buffer-substring (line-beginning-position)
                                                 (line-end-position))))))

(defun kill-whole-line-trim ()
  (interactive)
  (kill-new (buffer-substring (line-beginning-position)
                              (line-end-position)))
  (kill-whole-line)
  (setq kill-ring (cdr kill-ring))
  (setq kill-ring-yank-pointer kill-ring))

(defun push-mark-no-activate ()
  "Pushes `point' to `mark-ring' and does not activate the region Equivalent to \\[set-mark-command] when \\[transient-mark-mode] is disabled"
  (interactive)
  (push-mark (point) t nil)
  (message "Pushed mark to ring"))

(defun jump-to-mark ()
  "Jumps to the local mark, respecting the `mark-ring' order. This is the same as using \\[set-mark-command] with the prefix argument."
  (interactive)
  (set-mark-command 1))

(defun tkf-make-frame ()
  (interactive)
  (make-frame `((font . ,default-font))))

;; From:
;; https://zhangda.wordpress.com/2009/05/21/customize-emacs-automatic-scrolling-and-stop-the-cursor-from-jumping-around-as-i-move-it/
(defun stop-cursor-jump ()
  (setq scroll-margin 1
        scroll-conservatively 0
        scroll-up-aggressively 0.01
        scroll-down-aggressively 0.01)
  (setq-default scroll-up-aggressively 0.01
                scroll-down-aggressively 0.01))

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

(defun my-ido-complete-tag ()
  "Find a tag using ido"
  (interactive)
  (tags-completion-table)
  (let* ((initial-input
          (funcall (or find-tag-default-function
                       (get major-mode 'find-tag-default-function)
                       'find-tag-default)))
         (initial-input-regex (concat "\\(^\\|::\\)" initial-input "$")))
    (complete-symbol (ido-completing-read
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
    (while (and (looking-at "[\t ]")
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
                                 (setq mc 120))
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

(defun tkf-org-open-file ()
  (interactive)
  (find-file (read-file-name "" org-directory)))

;;* Misc. keybindings
(global-set-key (kbd "C-x k")           'kill-this-buffer)
(global-set-key (kbd "C-x K")           'kill-buffer)
;;(global-set-key (kbd "C-;")             'switch-to-next-interesting-buffer)
;;(global-set-key (kbd "M-;")             'switch-to-next-file-buffer)
(global-set-key (kbd "M-P")             'push-mark-no-activate)
(global-set-key (kbd "M-J")             'jump-to-mark)
(global-set-key (kbd "C-c b")           'compile)
(global-set-key (kbd "C-c k")           (lambda ()
                                          (interactive)
                                          (let ((col (current-column)))
                                            (kill-whole-line)
                                            (move-to-column col t))))
(global-set-key (kbd "C-c =")           'tkf-in-csert-divider-comment)
(global-set-key (kbd "C-c c")           'copy-whole-line-trim)
(global-set-key (kbd "C-c x")           'kill-whole-line-trim)
;(global-set-key (kbd "C-c l")           (lambda () (interactive) (join-line -1)))
(global-set-key (kbd "C-c p")           'toggle-sticky-buffer-window)
(global-set-key (kbd "C-c t")           'my-ido-find-tag)
(global-set-key (kbd "C-c u")           (lambda () (interactive)
                                          (backward-word)
                                          (upcase-word 1)))
(global-set-key (kbd "C-c s")           (lambda () (interactive)
                                          (xah-search-current-word 1)))
(global-set-key (kbd "C-c r")           (lambda () (interactive)
                                          (xah-search-current-word -1)))
(global-set-key (kbd "C-c y")           'kill-ring-ido)
;; C-c <digit> deletes a numbered window (indicated by the presence of a second argument to
;;                                        (select-window-by-number ...)
(global-set-key (kbd "C-c 1")           (lambda () (interactive) (select-window-by-number 1 t)))
(global-set-key (kbd "C-c 2")           (lambda () (interactive) (select-window-by-number 2 t)))
(global-set-key (kbd "C-c 3")           (lambda () (interactive) (select-window-by-number 3 t)))
(global-set-key (kbd "C-c 4")           (lambda () (interactive) (select-window-by-number 4 t)))
(global-set-key (kbd "C-c 5")           (lambda () (interactive) (select-window-by-number 5 t)))
(global-set-key (kbd "C-c 6")           (lambda () (interactive) (select-window-by-number 6 t)))
(global-set-key (kbd "C-c 7")           (lambda () (interactive) (select-window-by-number 7 t)))
(global-set-key (kbd "C-c 8")           (lambda () (interactive) (select-window-by-number 8 t)))
(global-set-key (kbd "C-c 9")           (lambda () (interactive) (select-window-by-number 9 t)))
(global-set-key (kbd "C-c 0")           (lambda () (interactive) (select-window-by-number 0 t)))
(global-set-key (kbd "M-z")             'zap-up-to-char)
(global-set-key (kbd "M-Z")             (lambda (c)
                                          (interactive "cZap backward upto char:")
                                          (zap-up-to-char -1 c)))
(global-set-key (kbd "C-x C-r")         'recentf-open-files)
(global-set-key (kbd "C-z")             'undo)
(global-set-key (kbd "C-a")             'tkf-beginning-of-line)
(global-set-key (kbd "C-=")             'er/expand-region)
(global-set-key (kbd "C-M-<return>")    'hs-toggle-hiding)
(global-set-key (kbd "M-f")             'forward-to-word)
(global-set-key (kbd "M-F")             'forward-word)
(global-set-key (kbd "M-b")             'backward-to-word)
(global-set-key (kbd "M-B")             'backward-word)

;; The "a" key won't work properly in dired mode when a window is protected
;; so unprotect it briefly, do the "a"-thing, then reprotect.
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

;;* Python-mode customizationsp

(setq-default python-indent 2)
(setq-default py-indent-offset 2)

(put 'narrow-to-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'dired-find-alternate-file 'disabled nil)

;;* Misc initialization

(server-start)
;; work around the bogus "-remote" flag specified by browse-url-firefox
(setq browse-url-browser-function 'browse-url-generic)
(setq-default indent-tabs-mode nil)
(setq scroll-step 1)
(setq-default tab-width 2)
(setq compilation-scroll-output t)
(column-number-mode 2)
(setq scroll-preserve-screen-position nil)
(setq scroll-conservatively 10000)
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1) ((control) . nil)))
(setq mouse-wheel-progressive-speed t)
(put 'set-goal-column 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'scroll-left 'disabled nil)
(setq-default ring-bell-function 'ignore)             ; God I hate that flashing
(setq initial-scratch-message nil)
(setq-default cursor-type 'bar)
(fringe-mode '(15 . 15))
(setq-default max-comment-column 120)
(defalias 'list-buffers 'ibuffer)
(add-hook 'before-save-hook (lambda () (delete-trailing-whitespace)))
(set-fringe-mode 10)
(setq-default indicate-buffer-boundaries '((top . left) (bottom . right)))
(electric-indent-mode t)
(recentf-mode 1)
(winner-mode t)
(setq-default truncate-lines t)
(setq make-backup-files t
      backup-by-copying t      ; don't clobber symlinks
      backup-directory-alist
      '(("." . ".backup"))
      delete-old-versions t
      ;kept-new-versions 6
      kept-old-versions 10
      version-control t)
(defalias 'yes-or-no-p 'y-or-n-p)
(setq tags-revert-without-query 1)
(stop-cursor-jump)

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
