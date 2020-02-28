(package-initialize)

(add-to-list 'load-path "~/.emacs.d/lisp")

(load "~/.emacs.d/site-specific.el")

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
  (setq-default fill-column 132))

(use-package window-numbering
  :ensure t
  :init
  (window-numbering-mode 1))

(use-package iedit
  :ensure t)

(use-package geiser
  :ensure t
  :init
  (setq geiser-active-implementations '(guile)))

(use-package windresize
  :ensure t
  :bind
  (("C-c w" . windresize)))

(use-package magit
  :ensure t)

(use-package dired-sidebar
  :ensure t
  :bind
  (("C-x C-n" . dired-sidebar-toggle-sidebar))
  :custom
  (dired-sidebar-one-instance-p t))

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

(use-package idomenu
  :ensure t
  :bind
  (("C-c i" . idomenu)))

(use-package expand-region
  :ensure t
  :bind
  (("C-=" . er/expand-region)))

(use-package org
  :init
  (setq-default org-export-html-preamble nil)
  (setq-default org-export-html-postamble nil)
  (setq org-hide-leading-stars t)
  (setq org-log-done 'time)   ; insert a timestamp when org item marked DONE
  (setq org-default-notes-file (concat org-directory "/notes.org"))
  (setq org-default-diary-file (concat org-directory "/diary.org"))
  (setq org-default-log-file (concat org-directory "/log.org"))
  (setq org-file-list '("notes.org" "diary.org" "log.org"))
  (setq org-agenda-files `(,org-default-notes-file))
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode t)))
  (setq org-todo-keywords
       '((sequence "TODO" "WRITTEN" "TESTED" "|" "DONE" "FIX")))
  ;;(setq org-remember-default-headline nil)
  (setq-default org-capture-templates
        '(("n"    "Note"  entry     (file+headline org-default-notes-file "Notes")
           "* %?")
          ("t"    "Todo"  checkitem (file+olp+datetree org-default-notes-file "Todos")
           "[ ] %?")
          ("d"    "Diary" entry     (file+olp+datetree org-default-diary-file "Diary")
           "* %U %?")
          ;("l"    "Log"
          ))
  :bind
  (("C-c o" . tkf-org-open-file)
   ("C-c n" . org-capture)))

(defun imenu-lisp ()
  (add-to-list 'imenu-generic-expression
               '("defrule" "^\\s-*(defrule\\s-+\\([^[:space:]]+\\)\\s-*$" 1))
  (add-to-list 'imenu-generic-expression
               '("Sect" "^\\s-*;;\\*\\s-*\\(.*\\)$" 1))
  (add-to-list 'imenu-generic-expression
               '("defbinaryop" "^\\s-*(defbinaryop\\s-+\\([^[:space:]]+\\).*$" 1)))

(defun imenu-emacs-lisp ()
  (add-to-list 'imenu-generic-expression
               '("use-package" "^\\s-*(use-package\\s-*\\(.+\\)$" 1))
  (add-to-list 'imenu-generic-expression
               '("Sect" "^\\s-*;;\\*\\s-*\\(.*\\)$" 1)))

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
  (setq ido-max-prospects 4)
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

;;* Vars set by 'custom'
;; TODO: someday move all of this crap out into 'use-package' blocks where it belongs.
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(Man-notify-method (quote pushy))
 '(align-text-modes (quote (text-mode outline-mode fundamental-mode)))
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-color-names-vector
   ["black" "red3" "ForestGreen" "yellow3" "blue" "magenta3" "DeepSkyBlue" "gray50"])
 '(ansi-term-color-vector
   [unspecified "#1b181b" "#ca402b" "#918b3b" "#bb8a35" "#516aec" "#7b59c0" "#516aec" "#ab9bab"] t)
 '(auto-hscroll-mode t)
 '(beacon-color "#ff9da4")
 '(blink-cursor-blinks 0)
 '(blink-cursor-delay 1.1)
 '(blink-cursor-interval 0.45)
 '(blink-cursor-mode t)
 '(blink-matching-delay 0)
 '(byte-compile-delete-errors t)
 '(byte-compile-warnings (quote (not cl-functions)))
 '(column-number-mode t)
 '(company-quickhelp-color-background "#4F4F4F")
 '(company-quickhelp-color-foreground "#DCDCCC")
 '(compilation-message-face (quote default))
 '(compilation-scroll-output t)
 '(compile-command "./build.sh")
 '(cua-global-mark-cursor-color "#2aa198")
 '(cua-normal-cursor-color "#657b83")
 '(cua-overwrite-cursor-color "#b58900")
 '(cua-read-only-cursor-color "#859900")
 '(custom-safe-themes
   (quote
    ("479eaf3a52999cf6149d958aea6dc321f6124441341ae276181cca226a1ff891" "3fc8191e0db6a9cbbc7ed52d1201bb948ab2b5ecb82a3d9044ab60252f394d67" "76ab82bb457ffe05c89f3fb925c017a7b4dda10a07523e937ea15b0e4ea497b6" "c9206fd8b9b4f4969102201a627d260c82cf1ce7e26dda1a2a51617aa4f03fa0" "765cca79196ad45df1ad89d6746dbe12a5055d0a0a128a4251504b6af5e2aa00" "03a12775e0f940de84902ed75a36de55b179fcb454827f05c274f409ac4e1de1" "bfd7765141d012e8c2566bc2d03f8198438297157dfec59ff2c66fc4983c229e" "e873fb4809c80706a0d789cc2a08b412b20d29d07c2b04c61849a80ba7b760a0" "ba7ea9b452077d6cdf38775fe1b4ce52a01ebe1965ef6ae900355abbc823724b" "5b672901298bf0107eb514847adcb15a4b368e9330998aaa9137f0c438d5775e" "ee2a93650207d9973e58ed39416488abb0ccceb75021734d2a2e372a280c94db" "7f982b98b9909862f0ea8f49132b20e7e5ed0f2f8de10dd464c05047fcc197aa" "00327567e30a5e5506a725a68dd71f694386d7ffb9f36b721022811af7ba46a3" "91935fa44b3edc7ed1e4f673e89c3198275e2caa1a3e5c21bfcb23cf28b13070" "bd2970981c4ec44b443284b64c0bff2e9e0a3c879fd88f1dc2ba2f34be1dfa49" "e8cef8258baa4527c2a8cefed287437c110436126a320629c0da7c321b2d8b2e" "e58fbefb33317cd7172fe3a02ab7eb7ae7d3dcd8930c3e401c291a77243591d9" "6fe36528adf16d1e96a93bc23736a9b6392d2a596e61f1df7d5683179a9ba78c" "242527ce24b140d304381952aa7a081179a9848d734446d913ca8ef0af3cef21" "3f44e2d33b9deb2da947523e2169031d3707eec0426e78c7b8a646ef773a2077" "44247f2a14c661d96d2bff302f1dbf37ebe7616935e4682102b68c0b6cc80095" default)))
 '(default-input-method "sgml")
 '(default-justification (quote full))
 '(desktop-save nil)
 '(desktop-save-mode nil)
 '(diff-command "diff")
 '(diff-switches "-C 5")
 '(fancy-splash-image nil)
 '(fci-rule-color "#383838")
 '(flycheck-color-mode-line-face-to-color (quote mode-line-buffer-id))
 '(font-lock-global-modes (quote (not speedbar-mode)))
 '(frame-background-mode nil)
 '(grep-command "grep -in")
 '(grep-find-command (quote ("find . -type f -exec grep /dev/null {} +" . 26)))
 '(highlight-changes-colors (quote ("#d33682" "#6c71c4")))
 '(highlight-symbol-foreground-color "#586e75")
 '(highlight-tail-colors
   (quote
    (("#eee8d5" . 0)
     ("#B4C342" . 20)
     ("#69CABF" . 30)
     ("#69B7F0" . 50)
     ("#DEB542" . 60)
     ("#F2804F" . 70)
     ("#F771AC" . 85)
     ("#eee8d5" . 100))))
 '(hl-bg-colors
   (quote
    ("#DEB542" "#F2804F" "#FF6E64" "#F771AC" "#9EA0E5" "#69B7F0" "#69CABF" "#B4C342")))
 '(hl-fg-colors
   (quote
    ("#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3")))
 '(hl-paren-colors
   (quote
    ("#B9F" "#B8D" "#B7B" "#B69" "#B57" "#B45" "#B33" "#B11")))
 '(hscroll-margin 0)
 '(hscroll-step 5)
 '(inhibit-startup-echo-area-message t)
 '(inhibit-startup-screen t)
 '(initial-buffer-choice nil)
 '(isearch-lazy-highlight t)
 '(kill-ring-max 200)
 '(kill-whole-line t)
 '(lazy-highlight-cleanup t)
 '(line-number-mode nil)
 '(linum-format "%-4d")
 '(lua-always-show nil t)
 '(lua-default-application "lua5.3")
 '(lua-indent-level 2 t)
 '(magit-diff-use-overlays nil)
 '(menu-bar-mode nil)
 '(message-default-charset (quote iso-8859-1))
 '(mouse-wheel-mode t)
 '(nrepl-message-colors
   (quote
    ("#CC9393" "#DFAF8F" "#F0DFAF" "#7F9F7F" "#BFEBBF" "#93E0E3" "#94BFF3" "#DC8CC3")))
 '(org-fontify-whole-heading-line t)
 '(package-selected-packages
   (quote
    (iedit all-the-icons-dired all-the-icons dired-sidebar org-bullets geiser window-numbering windresize ido-occur rainbow-mode fill-column-indicator rectangle-utils imenu-anywhere nasm-mode markdown-mode+ ido-vertical-mode syntax-subword idomenu ido-ubiquitous ido-select-window use-package bm move-text smex magit multiple-cursors visual-regexp expand-region peg)))
 '(pdf-view-midnight-colors (quote ("#DCDCCC" . "#383838")))
 '(pos-tip-background-color "#eee8d5")
 '(pos-tip-foreground-color "#586e75")
 '(python-indent-offset 2)
 '(python-shell-interpreter "python3")
 '(recentf-exclude (quote ("session.*")))
 '(recentf-mode t)
 '(ruler-mode-show-tab-stops nil)
 '(save-place t nil (saveplace))
 '(scroll-bar-mode nil)
 '(scroll-conservatively 1)
 '(scroll-preserve-screen-position t)
 '(scroll-step 1)
 '(send-mail-function (quote mailclient-send-it))
 '(shift-select-mode nil)
 '(show-paren-mode t)
 '(show-paren-style (quote parenthesis))
 '(show-paren-when-point-inside-paren t)
 '(smartrep-mode-line-active-bg (solarized-color-blend "#859900" "#eee8d5" 0.2))
 '(speedbar-directory-unshown-regexp "^$")
 '(speedbar-fetch-etags-arguments (quote ("-I" "-o" "-")))
 '(speedbar-ignored-modes (quote (fundamental-mode Info-mode)))
 '(speedbar-tag-group-name-minimum-length 25)
 '(speedbar-tag-regroup-maximum-length 32)
 '(speedbar-tag-split-minimum-length 10)
 '(speedbar-update-flag t)
 '(speedbar-use-imenu-flag nil)
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
 '(uniquify-buffer-name-style (quote forward) nil (uniquify))
 '(vc-annotate-background "#2B2B2B")
 '(vc-annotate-background-mode nil)
 '(vc-annotate-color-map
   (quote
    ((20 . "#BC8383")
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
     (360 . "#DC8CC3"))))
 '(vc-annotate-very-old-color "#DC8CC3")
 '(visible-bell nil)
 '(weechat-color-list
   (quote
    (unspecified "#fdf6e3" "#eee8d5" "#990A1B" "#dc322f" "#546E00" "#859900" "#7B6000" "#b58900" "#00629D" "#268bd2" "#93115C" "#d33682" "#00736F" "#2aa198" "#657b83" "#839496")))
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
  (let ((f (ido-completing-read "ORG:" org-file-list)))
    (find-file (concat org-directory "/" f))))

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
(global-set-key (kbd "C-c =")           'tkf-insert-divider-comment)
(global-set-key (kbd "C-c c")           'copy-whole-line-trim)
(global-set-key (kbd "C-c x")           'kill-whole-line-trim)
(global-set-key (kbd "C-c j")           (lambda () (interactive) (join-line -1)))
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
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
