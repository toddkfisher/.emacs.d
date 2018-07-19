(when (>= emacs-major-version 24)
  (require 'package)
  (setq package-archives '(("gnu"       . "http://elpa.gnu.org/packages/")
                           ;("marmalade" . "http://marmalade-repo.org/packages/")
                           ("melpa"     . "http://melpa.milkbox.net/packages/")))
)

(add-to-list 'load-path "~/.emacs.d/lisp")
(load "~/.emacs.d/site-specific.el")

;;* Requires/includes

(require 'cc-mode) ;- so that c-mode-map exists for key redef
(require 'dired) ;- so that dired-mode-map exists for key redef
(require 'python) ;- so that python-mode-map exists for key redef
(require 'scheme)
(require 'kill-ring-ido)
(require 'subr-x)
;(load "~/.emacs.d/lua-mode") ;- custom version

(use-package windresize
  :ensure t)

(use-package magit
  :ensure t)

(use-package ido-occur
  :ensure t
  :bind (("C-c l" . ido-occur)
         ("C-c L" . ido-occur-at-point)
         :map isearch-mode-map
         ("C-o" . ido-occur-from-isearch)))

(use-package visual-regexp
  :ensure t)

(use-package bury-successful-compilation
  :ensure t
  :config
  (bury-successful-compilation 1))

(use-package zel
  :ensure t
  :demand t
  :bind (("C-x C-r" . zel-find-file-frecent))
  :config (zel-install))

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
  :config
  (ido-ubiquitous-mode t))

(use-package bm
  :ensure t
  :init
  (setq bm-highlight-style (quote bm-highlight-only-fringe))
  :bind
  (("C-c b"       . tkf-bm-cmd-list)
   ("C-M-<left>"  . bm-toggle)
   ("C-M-<right>" . bm-toggle)
   ("C-M-<up>"    . bm-previous)
   ("C-M-<down>"  . bm-next)))

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

(use-package speedbar
  :init
  (setq speedbar-directory-unshown-regexp "^$")
  (setq speedbar-fetch-etags-arguments (quote ("-I" "-o" "-")))
  (setq speedbar-fetch-etags-command "etags")
  (setq speedbar-show-unknown-files nil)
  (setq speedbar-smart-directory-expand-flag t)
  (setq speedbar-tag-hierarchy-method (quote (speedbar-prefix-group-tag-hierarchy
                                              speedbar-sort-tag-hierarchy)))
  (setq speedbar-use-images t)
  (setq speedbar-supported-extension-expressions
   (quote
    (".lua" ".org" ".[ch]\\(\\+\\+\\|pp\\|c\\|h\\|xx\\)?"
     ".tex\\(i\\(nfo\\)?\\)?" ".el" ".emacs" ".l" ".lsp"
     ".p" ".java" ".js" ".f\\(90\\|77\\|or\\)?" ".scm")))
  (setq speedbar-tag-group-name-minimum-length 25)
  (setq speedbar-tag-regroup-maximum-length 32)
  (setq speedbar-tag-split-minimum-length 10)
  (setq speedbar-update-flag t)
  (setq speedbar-use-imenu-flag t)
  (setq speedbar-verbosity-level 0)
  :config
  (speedbar-add-supported-extension ".org")
  (speedbar-add-supported-extension ".scm")
  (speedbar-add-supported-extension ".lua"))

(use-package imenu
  :init
  (setq imenu-auto-rescan t)
  (add-hook 'emacs-lisp-mode-hook
            (lambda ()
              (add-to-list 'imenu-generic-expression
                           '("use-package" "^\\s-*(use-package\\s-*\\(.+\\)$" 1))
              (add-to-list 'imenu-generic-expression
                           '("Sect" "^\\s-*;;\\*\\s-*\\(.*\\)$" 1))))
  (add-hook 'c-mode-common-hook
            (lambda ()
              (add-to-list 'imenu-generic-expression
                           '("Macro" "^\\s-*\#\\s-*define\\s-+\\([a-zA-Z_][a-zA-Z_0-9]*\\)" 1))
              (add-to-list 'imenu-generic-expression
                           '("Enum" "^\\s-*enum\\s-+\\([a-zA-Z_][a-zA-Z_0-9]*\\)" 1))
              (add-to-list 'imenu-generic-expression
                           '("Sect" "^\\s-*///\\s-*\\(.*\\)$" 1)))))

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
  (setq ido-max-prospects 30)
  :config
  (ido-mode (quote both)))

(use-package ido-vertical-mode
  :ensure t
  :init
  (setq ido-vertical-indicator "»")
  (setq ido-vertical-show-count t)
  :config
  (ido-vertical-mode t)
  (setq ido-vertical-define-keys 'C-n-and-C-p-only))

(use-package cc-mode
  :config
  (setq-default c-basic-offset 2)
  (setq c-default-style
        (quote
         ((c-mode . "ellemtel")
          (java-mode . "java")
          (awk-mode . "awk")
          (other . "gnu"))))
  (add-hook 'c-mode-common-hook (lambda ()
                                  (modify-syntax-entry ?_ "w" c-mode-syntax-table))))

;; Info setup for custom info files.
(use-package info
  :init
  (add-hook 'Info-mode-hook
            (lambda ()
              (setq Info-additional-directory-list Info-default-directory-list))))

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

(use-package lua-mode
  :ensure t
  :init
  (setq lua-always-show nil)
  (setq lua-indent-level 2)
  (setq lua-prefix-key "C-c"))

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

;;* World's best modeline

(setq-default mode-line-format
              (list mode-line-buffer-identification
                    ;; '%0x' - fixed-width prevents flickering
                    "[%06p C%02c] "
                    "["
                    '(:eval (if (buffer-modified-p) "M" "-"))
                    '(:eval (if buffer-read-only    "R" "-"))
                    '(:eval (if (window-dedicated-p (selected-window)) "D" "-"))
                    "] ["
                    '(:eval (last-dir default-directory))
                    "] "
                    mode-line-modes
                    ;;minor-mode-alist
                    '(:eval (if current-input-method
                                (concat  " [" current-input-method "] ")
                              ""))
                    '(:eval vc-mode)
                    ))

;;* Misc initialization

(server-start)
;; work around the bogus "-remote" flag specified by browse-url-firefox
(setq browse-url-browser-function 'browse-url-generic)
(setq-default indent-tabs-mode nil)
(setq scroll-step 1)
(setq-default tab-width 4)
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
(setq-default fill-column 120)
(setq initial-scratch-message nil)
(setq-default cursor-type 'bar)
(fringe-mode '(15 . 15))
(setq-default max-comment-column 120)
(defalias 'list-buffers 'ibuffer)
(add-hook 'before-save-hook (lambda () (delete-trailing-whitespace)))
(set-fringe-mode 10)
(setq-default indicate-buffer-boundaries '((top . left) (bottom . right)))
(electric-indent-mode t)
(winner-mode 1)
(setq-default truncate-lines t)
(setq make-backup-files t
      backup-by-copying t      ; don't clobber symlinks
      backup-directory-alist
      '(("." . ".backup"))
      delete-old-versions t
      ;kept-new-versions 6
      kept-old-versions 10
      version-control t)

;;* Vars set by 'custom'

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(Man-notify-method 'pushy)
 '(ac-etags-requires 1)
 '(align-text-modes '(text-mode outline-mode fundamental-mode))
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
 '(bury-successful-compilation t)
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
   '("fe99c747159b8578ea4fde7d3b515629c4da8f370c6aeee8792f1e221747a739" "b750c018f0e35398fa057744d9f55efecd4819b48d8013ea47461a79aaa39fe6" "8124f2ab6312fa896f738fad7b5fd92d1ba8dc13284dc40110a11315819d6415" "be1a08d062557870e3eb2a2a5d5fdf8f6db09958bea16388366fd35f1c24cb59" "b4a604bea8128771562be509d9eee0352aae3048f4c1aeb28b7915b180411d42" "025321e4fb425afe848bafb9310e6cbec54e7416efc317317a01e5be87419124" "dcb22110c7fc12a7dfb58756a33e48cdd62cb5065d552420693d4f241e9b4089" "a52be8eae85fad512baf6459bb32a8c3e874d8e5595aa36f3cc901d8714caeb8" "572982da96a9a253df5c452fdd6fbb1d59522dd11985320c6aa3d8b1029e38d1" "135d3eedcc9fd83ca04ef3eb95c11d6cbc49efd79b5a1c7ed96ddc44452bee59" "61e0c8c378dcc98d351e12a7d20883e2922bee4867784c31733bdcacfc80d9af" "ffdf162871f4dc43ad754e9dd0537c8ad805e9283f028ee01821b5fc6109aab0" "b848a4154d29fd04a67bb279f66b6ccf448dbe3cf18129b5608fcff341ad62de" "7057c4bf73f2ca4340a677947f2b78bf04aa221b240d9dde943abe8d7edd5470" "c3588f38d5f4c16ea25a3781f1d1097fd2ce054fea45734fed9c6a105ff954ba" "fae41d83315eb6720ecd9bf5e11b0eea1ddf704e6205839c625e3f87fdd245b7" "10ff7875889398a1868904bfc542265d714b76ec426028c1588004cff8b3cbc7" "921766350f1b1dc27011b6a9d6437350f41d6ff6124815bea6ca620bf6930f05" "ce22b808e56057364ad9ed972599b3d35184914414ed6c087aeea220de1b408f" "7770669f2c317c5e0dab6c44cf7e9d4eb28ab0406b6abffefc119788a82cba50" "1ca57385f724c4876e77ac8fdff51cb4d4e4c86c7e7eb04e36baca133ccff92f" "8fdba11aa9669a787a88dbc41b26d9948e686f8fd3e296ab1cff21fc76fe9bb5" "072f98f70368f665865a87889f2dc9d7d73098be1e7ee91376656d0dcad7213c" "941960129608a7e4ca2a64610a3e0a39580cb091f9a1f7511458fc1fb825dd87" "91085b67cda10f6d2d8b73c824cf3363cf9578e3d635953b332f3c83b6138759" "0963560bb8895d7c1d52aecfa1cca02be2811b30876714943b779f0952ce1be3" "ba65665c2118ddcc53c42a96d5dd5b68f4893b739a0b945cc5c62543bbf31db5" "44896751b727f86193c7f7318de8c8a69faa947808eaf835aece8880655d3410" "05d6197c182cea385d1a3ffdb5eba82b89b7755af879ac7bb8734a2e3df56abe" "88ddae05392fa6cc0ae7109c65338bb2f5cd9963bb08fde2350d03d74d13be2f" "ab002b214661ac4f68bf5faed0ae6b70a9e178a291baeed833ff54a68ff0f1d4" "78077ec6d72dcc2aa585b90288772cdd08847749694503e991aad7e9ceb0b005" "24f808cf30bc29c1b2f99a4eb5e1024e206d64acce912a4583518b27a7787f5e" "fa15061a40109e437d615ed4c9313d514ca153fddd4d430b668d7f0172f19fda" "d8ee6932fb2a2cfcb0cd94f533018bdcd20a3fabdb8de7ea6d949303716ed4e8" "8e57bfaf81e85fa4ef6469beab77873eebf7734033f504836339f30d9a5ceb86" "0078cd5277cc85066deea1a97acfd9a4d4a73b0bfd5bdb59208cea227da171a1" "52aa88113c248970c4e1964f8f763733bf865c999572ff2d00bb87b92c941698" "c367563852621ce9fa3812f7ce4cc7bc5aa5e2493d805dc445eb43d987bb1730" "ab5591bd1e0d4eee7499ec881824ee1bc0d1a36fcc3a173efa5723247e2e39c5" "75afe8ea80825404636682f3a7a1c0ad331231fdb6ba89a06f23cfc1904f208b" "f86d8f815535233f0dbc1a1932f98c7d4d0e02bc10fab15957fe0fb265f14f52" "c03ae142328606351a728eb8119046910bcbd82cea7bdaf35798a4f6c2c06373" "de5423a074ec045be82bd3352557c7d4fbfc43be45ed81c4a3964f8981704afc" "cddcb43d30a3df6a55fa3646c395b32d8c8cfbfc60e6670fe0677a397e43bd64" "d25b0a86cfd284b0a7defa09e7ff40912c93f8145b6b09a8b9b475dc97c607a8" "ae940575518fb9e26e7d96febabcd1323809bf88d04b690aff656d437cb47eac" "6742f4cbdc146691ba3842cdcca077ebad561b4be63aaedc2cf7b42982b8655d" "24855513cf6e7f97d6f963c1afc6f6067f6dce1292a70046db27250894a6c22f" "48fc33408da05aefa21c8c6326651469a766fdb777bb93be507d9b3edd5eccb9" "9b59e67da7e12dc445077a3de93c593de210e98024d50a2a0ff466c801492e1e" "c2daa9561c384a23996af9e133bd0f240d98d4ab582aaee15df8ff96e41018ba" "c70484fd73d7cb9e2fae2c58460ccf6f6dac2ceed24bd89e8c9bdd39e681e168" "52981f0adf8f669325d1e6abf6de4656a315afbf93389ddb17de09ca4e72ca77" "62a72129e350c17d995def2bb8b0af882aff24830fb5ef705ddd650ae650ad8b" "3fe3a51bf3112b984f0b0d8267f0c908249158af0ef302d5fa6c83e48e067f1a" "ff70ddefe53e1948ae2f343751b422deb3832e588ae45e8e7d3eed23f2efbe84" "b8c870a4600d9aee133bc1f647fcbc68f5a43ec6a0f4964c47e47ad96daf257d" "61b7b332213a2a547d4d3177b885ee6d1bddcb414aef0fa3fb00564afabddf6a" "0d120d63f525999d885d3d9347b3f366236eee6e960f4b2f590a5944d29469b8" "3713d2149fe396925d453863fc3f58ed4a26cace105442a203bca00af48cfbf3" "2c2783c9cc324367d977c3dc0b94ce5cbf164f8b3f152cb8f554edf1c833d01c" "70aa5dcc749cff913adcc28702fa1729d21835ff4f203416680b5e1827f0fd61" "2ea93fae58badcf228fa7c2a420cfb95943245ce349d8800b03373549e43c17c" "9b0a12dd4768478115702b93bb7f85890f35f761772d03a2676f7f905f521e82" "5f4081f935b1b10fa0f1040322fbcb641db826a35265e9761b4be91cbfa2210a" "f94c6e6d1c0e031ac90c2b988d01f38f3c945991743304c62e83c59e030bbdc8" "90cba39869a8e09072cac3effeb30dd8c1c62d49491b4df24d5b15a01003ecc0" "693492321f3fd81fc062a6eb15264bb6145b4c5dff4728ccf172867a472bbdef" "18e55def86740de0be000da2d86f9976d3e932abe9991ca226e27c6d6a4d4d7b" "ce93ba0f53685c7440a8743f9181c35264a8f2b90c0872755476bff2ae620592" "b5bb7fb55b23c051d0adc95dd8875673f4a9059d3ede6d2c9d977a6b87ab3667" "52241deed0a253843dc0776c927fbb06abac4007e277cac63f95dc7d92f33abf" "45e1965240aba90570e620a387d0f594fac9f0e17901a7b82d7ab4d113e37331" "6fc1119bed76e59fd1cfb5452a2bc6e205802b240cc7dec2d1f0ede55b95ec18" "40305434e57ea1e3bf73747cfc7c8d26df61ff6859c29b29ff4e8a955cbbe80f" "34a8f86186e26ee6b1b0bcff4687d3a6b69633d56cbd21a5346b892983f8f448" "4ca5c83b52a640b392dbc891d1cb785ada7eb2ff0ef56c788761592843eaaa3d" "1455129882ff12c6422edf09ba2da323ccd8d13c11da3bfe25b86e76351e0514" "e06dd28faff16af3d9578c739f113d5570b018266b21da7912d7d22ba4f6557f" "4f38110a9c9d1d4597910af77efe8610bfeb0587e0985e551c110e5f73b25131" "a88e5b9b44d52b0563a860469ccb1797c726b05c31187050dc58c6a1eb772d45" "cb9c8dd271328798c81cb403f56569a2adeff4ab3c9c5577cbdc8786b04102a0" "5b35102f9c13c0074d84cb47aee40314e63122bd04762d92d75566bfce61d1b0" "ffd3ec503a90671454fc84ecfabdced46d1da9ed2fa06ab26d6f25889bb2e690" "5c899f57121dc017cb6b83798c8f01f253960cde807986debdc8878875538e69" "9a5670f583653c63ecf6dab51904ab7f7121fd66bb7e8f1076362c6bdcba6b35" "bb240b23c066762b40c3332a3d82015d5aa2b1eaf6e41c24ba375fd8dcaf71ce" "d0b94045da3bb704ef333b535eb9e41ba54ecd90318d704ff1d8e8ac3f3338d7" "6e11380d21a1a0da27b4fba02836ef7e79147bf6eecd7255f4f68daf666c104c" "9fd4582cb7e6168273e4231caae97985ca1d862247bf4b9994d260f59462f26e" "dd4bf3946484f84970bb593054faf77e9bf9063f9840cd16c24a699bc56fa24f" "6aa7e56ad5c1ed3b9540b5c6d243c22bb29f1717e773efb08862c4f328783b15" "26de89770485887b1820744668f4ba7b43b355e9e202bd51fe2bd1b00a423868" "0226002c60dbfeb03a149ef97496e6344272e25421f1fd3d8377276d00331ccc" "042b232af1c1803a63a7060a97a502a15da77b930c8101af440c5478b4e90785" "58846296c358062b861e8d00ec217aa544be656b79f20886edebd8362b2f6299" "cd36d1d42d5bc8c656354566b4e2635e3aa52ac24e14651b4264bb7da50d2092" "11ba929b3f8434da9d4379ab3a736b7c20a44d65745fd481e648607409dd0319" "c5bb79098f2435d885314b0e22e3a843e3adacac8619c9c1c800378afdc4fed5" "6607bd23f195d3c7bdc5b5b6abd4d35f1f712d020a19c4297d93c58cd2b23441" "aaee1082cd79e8360d346037406e18a23e077ad2a1d452792805e2483420e722" "7853957ab5bcae62eb5e55cb4f2a58cb9025f22a60ea37dfbb8e37f0cd9cd1f6" "4fe5c4e9cc351cad33c67dc4c07d3a1466479774474f2f1f4d26d2e2a9d1ffe2" "c8bd098beb9106bd97af91344838c0855071c894afe29a3a168fa893b14ebb31" "1193b301cbafbea16cf88af4469808db67a91968f7fa7ade745d3d32875f8347" "c19a58cdf3de4d24de5d48b2cae3fd246f1b7b2108b644d8e28b7945e1cf7743" "ab82b505488cc26e10ab7c021638dfbe339902fd09e932083a66ff23f2833a69" "cae9e907a981ae971f45cf3ab9cf437044eee441b2b368d0cc8dd3963b03d584" "f0bc6a6691067d12b508240b41a48bdd11877e3f311702a9f5b40a7690e9ffbb" "249e66b8f5dca3f1e2413fc392d4ab1bc379b2f4acc865f86ba2d7fd90309807" "b5939987c3499fd625c7ab9b1c43a32ccb69a314290f3328b6a487090f1b67d3" "a66859f3a7ab74366d830aa1ca25a4c31d0efefeb5ac6e9a62cbba278bf385f2" "c2acfdd43dd2a28cd07ed53944179b1dc56c4e7aaa9fe969ba7a2c27f2cf7a1a" "08ba77c9d18a382c5d7ac6a3544d556d39be1197e7846f9b90cbee0cda863e3f" "c98623506a99bbbe1c859c49ef3cf0b2c659172de8e3fab7af0d1dca20dbc605" "749bc96858b0718dd8d17772171fcd6b7820dc84d559e217dd787c9d1900ccf2" "af948f0bdcd74fc8791035fcc505164b1d2cd1e8a100e98c7cf1a01a7c95f8f6" "1b78415cc4d823e3e46d517a7961b74dfc8b0ea4f552dd45aa0a010d20482c70" "cf73d964cc2b40eabbcea9b3300875994913e9016ff59d0ae9192e95b9a2ff4c" "7476638087b08d3bcfcda457f27cc13bde0e53bf69abfbe9a77d20dd9ab59e46" "5124da273fe254052e02a5324a335deb3a9846bf4b686c37b547b65898394dee" "adb5d2c497c5775fde44ad89bc45339bf2068509fdcc7694e65a76e1477e18a1" "94497b381c911a7fbd2a49315cc3f3b376e08a86637edf4da3e02e48e2deb46e" "55bea44b3044d4cbc91ce320b16b8840a3888b071765a12f384e649c56b41aa3" "f8bccc566e4014eeaf27dbb62640de7d2c37b943c6d4927fd1e4c1fb28381ef3" "1b92cc9350540756743502133b23701e0ea0647f24e3bd1c130c7510be338e8c" "a99efd3a9b5b27b919bbad0fadf74186a5398678b38c33c00bc1c06134e80652" "8816481f2043dc8cce815d40ff3e2689705911a26783266c812b491ec1757482" "220baeaee7a66b82be28e98f596e1f8164a0085a6381154ffa59bfda548f3180" "88ca1e2dd66d0b5bc8498eecea01b20ea099fb3adc4048d851a7def6d2041802" "8d8817a9191139ab8ae58e4f7ff0574581a57e3bb92ebba9871e0217d3ae8a0e" "0feca35327d813293b8941dd4a734bdd2377c941f61ec23ca683f407d276583a" "bea5fd3610ed135e6ecc35bf8a9c27277d50336455dbdd2969809f7d7c1f7d79" "39ff23a1e0a10eafc29e602ad4d7c28b7b2340816cfaf037d3d7bd7301212c38" "e2b5dbe0c4733c40c18fdfc332b7aa8eec3cb900e4c8ce4f6dca81e4cda56bc6" "b47cc20950acacba6966cc44328b33547f9b81c2005ef3a3d9107e787627fec8" "497ac578aa09866ef5ff5e3462f1e0f9b8952115f179264dea9d02c74b97203c" "251f77c9268bad9bbda49732cb92fd26a86ff5eb208327d2c2cb0bfef27f1d67" "2c97eb9fad6eff6cf93326510091dc6fa746294c5a452bbf8b4236790ce08293" "e64c067330dc249830a64577364bb140f8f2f42aa48436a3ac4a873a47d1fbc5" "ab502e5c70b0ce54a7ca1378de8805e45eaf046b335495c4fcea7ee0dee6da3b" "69c1638a52d34b1428f0dad999ca661186869ccbd9c514ed664a35ed9763b794" "771f816d64b17824bba753c6fe1f5ff32436e5c1e1f61e2f5fe0e6a2e5ab7b79" "1b2c1adbfea189cbc7e5084b3c8bd9cbb6c26cc778ea6f7e3a2f3c8189bc738b" "59fc29518e698ca6da046525a0f3cd79d3f3e26ffd1500818ab9edc5b841d657" "a3a50dac689cab998aff2fe42a8c9490d3a9c372c29712f690f1dd0e8afb68e1" "b168a9c65a1feb85417f443ce2b7f8e6df85231910a269d041da8e86006c794d" "c502dc05bc481811d9d750ad3e0f9e011b850b2fb5bc8b64a543d9cdda916020" "24fc794a16809a86a63ec2e6f8801e99141aca73fc238ea30d35f87c88847329" "f364574c20725ef3af0b1ae5f866fc3869f8ca81cbf56620920b03a96edbf5d8" "0c71e4d0b5ad79a7cb155f180adcc93f2fe5ae3d3a863de7d3a8c898087d890c" "708b21af2f90794ac274f1712ea7b9ce4debac517cb798b94cdfff9972e45eeb" "e91e4080a52195711724533a1d09fd07a8721721d76b9a3c3d7a5bc40866d174" "52e12c74da453fe7bdf85cdc2bbecb5cf2ee33c61b9b18587c1b0bcc554fdbf0" "c4deb1faca6832175f859131b21412a26a709f0cbb8a91111ea18110a1b9324a" "d38365d7b6bb2b8123b8fce8a2354060e50bff77a762750eeb475a473b0bd1a1" "de1b2ccc55142298ff342af38cd9199b4e06b663c95f88abc93a39c4a02dafac" "5c633ab11200f1e8870d9786cfe2eb3f692d03f1f552be815bcfca7dcb27ee47" "f1cd03a5f3f58290096a27fd042cb57b5b707e3bcce46672eaf1f840aa8a05a3" "cf4f2e5f7ce22a3fe1b564f3ede64b8b32ba2a4949cc485767bdff917346b2ab" "2967b42c993ffba7296744e1977692b601315bbeac31b668667ab3abba21623e" "3d55f2a28a4c8b655f4a9cc63a7dcb2a96dcabcba8ff16dbd65e282666792d8a" "f8c3518ed77dce92733c3d4372b6f1ab7c4d3a83fa37ad18968ce9af8f8a5604" "f13e22557f5e9a01b4a76fb9f725ab97ad5fc2e7edabf16e651736110eab9418" "668077061dd75b4e3b37e21aae4069b3f93ea310d24da356067e0d455449f814" "771677d7b8bd0197af7ca88ed42ca6f374d846b71dd672491501a7a82a43151c" "665013ce520e0bb98351824a2796b531c8a2476685a179382a4ba586c9f6e57d" "dd7f8ade2425cb623731789e2b2a92fc4c974ef8d859450a8beda81af008033a" "63fa7e56d9ef6767bc36d295738631689ff818afd14ef7feb962f90a27cf69e3" "cb1c1765e553dc19c6d00eadcf0fa7bfeed4d80b964a5cded931d8840d01380b" "292a9bd97cd61266865e0c9623be79623d45b5a3f2afb9028bbc2f045a0cab7b" "e50d72b780b8f74dfab46f7288a163f2e06f5d8770a6f4402d143296a215aada" "53eb2899f73ac8d0cbe67cd3c7692c9d6fe284ad51263560e2c8e529e61ef03c" "5c54ed6defd2e482bf6d156828622fdaf7f276e25f2ace3e457aaacff62eb737" "62dfac8c55a28be8af9fe151c3e9ad2b5099aaf4dbbeee1c5981a498cf575a8e" "0ac4060a9d5e17f61507fdff27a301aba10e4d61de9845ae0a4f7d77a7dc92f2" "608993116bc220bd777b20e6dfaa92edaa38e6ee684bc525f530c320b6605a8e" "edfe7112f89dced3c5dde5541c3b69100a11fecf939df8f5ff22e01a65134db0" "fe12e509d8c40bcdb78a3823aac3e992ae6c467380c1154a6b7f395957adee86" "0bcefb20af901baf2e8ccfc0c2bdf178207541e626f6485291cd1d9fa3d5127f" "91b418ebf4a63c23bf6375348c10851e9ffc5f2f290051f66a1a8208ad86f001" "77c12d2bcbbbb4124fe551fa2c5389a8bd7242caab63ce54d2cec02cf1f23ee4" "cbf24343ce42077529526757e7ea1895ce67ecc9e96424e218d5f9997b28b0a4" "ace60675ba2988c452e0f6d46c29ff112b6cedfbe136a4820c690424c7fcdf07" "92e01fe5968614a754124858a16fc7a89de0bf0396b023135e2fc6002988c63f" "4db90d604edaf1a7bdc27776575306773d47b64838c77a09ea016af439ea04a8" "683a6dbb71f726b288108cda3a39e40ed20f1a46716a6777403502ac7a466046" "827b6573e1222ca04e333e24b2e61b19a345bba973e2867867f0549328559396" "c1a2c2b4747cf5fcade8693f9c5f710af0024b12867212a5f3914ea45bc1ce25" "1b0cf956836f032ead36cb86d3ea3f50dbe6a61f239c5320425bad9456acfb69" "ba6eddb4ee087dde721808c5a4b030446a2da6e897fd894d2680474bf713cdd6" "b3afe56908d8fb3676f94590e95b02bbac544e8a12f15be76dd615377b18b10a" "bb6170fb586eeced87fbfb5fdf7ec83e60fbf173f0ace196738574fb17012305" "141e2235ba385bdb57daf674f9b76d4871133b0dde8c18fad97aa6c88b4abfb1" "4d49420b9c54687ab9f112683b55706a6908bd343d1d9e4c1b6cebead20827f8" "3a33e2050e3f55e9eb6f7973bb9b57103af456f84ef38e9770e5d6377c937fb9" "b0069479d1b6b94579b1123258152ae05d553bacfadb40fdf0d28e8d2a453b20" "a6b45207eb7c7942f13ba0cc8e016cf7ac0b530797169072f2453a7d0d0a01e0" "e8ac39a37e0ab8f5d6742c05babc6a4df26350051df55e17a75701b593423e1f" "3341a36f9a6a4032d7245c8d0454678908f4716f1d1cb08263317bb388bd493e" "e84d695fbe7844f2efbc9b1438c7ad789d21b26bba98ba0c455ec145b841aaa9" "7b415efc0067e2a8d5e5e55c889998b93171f6829642d014a954123e38a03e1d" "71f0ea740dac8663e76470c3e624ff835e5ca5f962f6e8c70c166eb28ccbb73d" "d2ba87563e7d8b1bab1dff73a5303585d67b3e0768c9047dda3ac8ac6e238705" "f5927bd02ec4d757488d3f25c5e0d8d66a2a188a0a8f0d4492f1533807c81548" "fb0367a55ffde5eb6d650ffa9ffb5477713a7fbc23b29fbd2b821f50c3a56435" "44857c9cc770b76dfef082cff01709f392e514097c38e14e06c0f9d10614eca5" "c6f3f26792aa81a8f3dd97dbf4659d9ba9e0ed04e0840afcc456a16fc31e40e6" "03cd3bc741ebaf77a92fed79edcf3082c9e34bcad8d02fb42663c25a8d9e811f" "00ce3ea2cc5965ae88f2cd41a6ae1c3ddc8ce8212491b8ff5a48b241ceef2466" "a6ffdf64892e0c4ad008ce7f57d29ba7108c8e5a3ec22603ea130b8b649ec041" "4bfced46dcfc40c45b076a1758ca106a947b1b6a6ff79a3281f3accacfb3243c" "b755c709cb46ba4599ab3a3c189a056e9736cbbb39ed9ecfc1ab1aa3d6f79021" "0e0c37ee89f0213ce31205e9ae8bce1f93c9bcd81b1bcda0233061bb02c357a8" "086970da368bb95e42fd4ddac3149e84ce5f165e90dfc6ce6baceae30cf581ef" "444238426b59b360fb74f46b521933f126778777c68c67841c31e0a68b0cc920" "992496fbb643947867fc8d952db1d9d94879b2a331474ae602300b12bae99db0" "2c7ea2026702437c288ff6faab6adddaf5e62f1bb60dc3fc6eb74dfa71d923a8" "914e6915fd67bc0720185c16f9c81192d21b97532cdf9fb340f998528dec180e" "06c31a229f9d64e96414453a8be012f19bbcd532686a144fbb03d677f6cfa1bb" "0e23c11dbb4c553485a8831549000e71d8f04b8b3d450c1d2f7a60a0a8176bde" "de72013ffe6ddab91e33b9cd0f8c64202829acc6a19c372ef5e3055ae037afcf" "fba46c4a9a74f216e7123a8403be623fccf4edfe4ecc7e057899108b53d440d5" "4d15a9e3b92eadb61f8cc1358fed27b3188d6382d0849634ebe47a30774f351c" "247c199fdd9a672fec48b34c57ddf099784587eeb0dc0da5d096ea2b05cbb475" "6314849f1c6c647abcd78319e2239520cbca09bc3bab1cc097ab7d1dd1c1d8a1" "964119526fe772767ef55cd7d5a83149a17225870514cf726c82361ad77a11d9" "2b7d7d2e6a16b32b7bd6655725d67947a26e9c659dad6806c37ac68dad1ad9ac" "1236de017ddab24ff9a0a56425e054566d548066ffcecc7e7dd37b9fe5ccde3f" "36b362851e116dffeb446c163c32dcfeb47ec7a0539ff6c5c2a26cdec2d361c8" "fa2b58bb98b62c3b8cf3b6f02f058ef7827a8e497125de0254f56e373abee088" "bffa9739ce0752a37d9b1eee78fc00ba159748f50dc328af4be661484848e476" "1b8d67b43ff1723960eb5e0cba512a2c7a2ad544ddb2533a90101fd1852b426e" "bb08c73af94ee74453c90422485b29e5643b73b05e8de029a6909af6a3fb3f58" "06f0b439b62164c6f8f84fdda32b62fb50b6d00e8b01c2208e55543a6337433a" "628278136f88aa1a151bb2d6c8a86bf2b7631fbea5f0f76cba2a0079cd910f7d" "82d2cac368ccdec2fcc7573f24c3f79654b78bf133096f9b40c20d97ec1d8016" "2607b32314f8f8439ff938a8e9253705276b616e7d22ed8d93be6fc212a31420" "233b7e41169010101e3d5758bf22c98abeb11adb5576c2856daf81e4f7079312" "f6a752f6da9ab45bab6f5fa085c41c7fc236dacfc4d0ccc0b6350eda074b62f4" "85239487bb5b3d86246d497422eee018fac239fdc9ae6cc18c735cb75819b866" "ade2eed0206eea7f9cb979a5df51c9b256995e1e3c8d6eadaade170b00c00ae1" "e8d438b5c9c4edd034e7a7b22b6975313662f5427ab92d1fe11327497687ca8e" "03d986c193f30225250d0df1db6aecf6cf7fca0c7756bda9f75150b1531478ba" "002019e8a817e4d13a6f582712d0cc5602fdff83a95e0602ba82f38fc7e87d34" "4ae67a3beefb4d6a045168806e5c816e3c9dac69891539c6de130cb9fb698fae" "d219727b6861ca7fd11f8b587c1a241a02b92f69f9054511e385a2e145f0f1e6" "3a22f7c11999bafe9c4d1a9c3726154348a2695d7f4a91df256af73fa09c9b6b" "8ee742ca7ddb4a66bbfc7c18a7932dfc84cb0c00c37c092a0b8a619f82e0742b" "113292bc2d534d9a04cd83939c5838b2b29b6d46c02c59b5659f74f6c1c0aada" "2470ada68f292e674c43f92ced6aa1aec800991bd75bcb249bd31092be6fd566" "f1b89a28a2019e57b5604936714b209c95ca9765558fa492ffae60e29ed90d29" "2122c743c1b2c6dea3c3dad668a79d402c30ce5c4753487a2d08decbcb18ac21" "47df6a14322c553c8c21566517211e0385dcfd03d316f62d4c6e910d7257b74e" "7a89347312f63dc6cce3b1c55d336973886f64e30acc70e3d5e9b2ae68955036" "f40a88e6dd9f7e0847a550ed2b5c1e27e940e77ff71b15b2874b81828ab14869" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" "a8245b7cc985a0610d71f9852e9f2767ad1b852c2bdea6f4aadc12cce9c4d6d0" "84bef22008e8d0cd5507ce3b236492f884c318fd6cd1e697803c4e759e7082fd" "057aaab02e872c0c273d1d3a01d630c573b0c166ae7904263c2ae9878a529169" "60b2441f571695e30a5bcf7fb7800262ff4e4c0325768c681b27c8a879f11c86" "7a3f91f6051da31aec071c2e7306b380d3acabe12ed76f4e4f2679f92ad3f5d2" "804a67c14a1546af549da8a37614a46f02813b80eb3592aed07590809274eb0a" "c84d5ca42cdc3e88c95ccded6a2505df31c66a54287f2479be373b3d95e0e79a" "442b9416e6d839daa902f8e4a7586b40e3c784bc3f575247094065e47eff3934" "d83f7e94a704e652c4b859ce787b3265a2bc629f261e73e32bfa6f2ac68ae7a7" "d5c0d88100fd0223e52f044524540ef22d0d0f3c31ccb1eee1f15d5ee244bcb9" "31f5321150cfaf26d63f6bc491f83ec991646fcee1f50601ef49fef55623b666" "3507ae328654b56363f7c14766f4250eab84c2e255cf44b8286d9ca9915ef83b" "c134e46874ca526ff64354bf57f52edc3bb69941024bdf49483c07d37c84df7a" "3143585a67376ff832e6847a7774e29e6a1638b94e39c8bb581279ab0bcd73d0" "4b7932f05267a85674119b642972d763eb5190beaff54eb437e6d77b80f743e7" "05eae7d14979225c76a432fd8fa07f839b890a25a4b15d632d9bda2179e612a7" "8744b50ae4390d24ceb0b98165a52bcc1831e1a8cf79c223ca20a01bec92edd9" "3124abec637403879833abc9824cd1d8e4b5deaa580a4880c3930fcadede03e0" "838193a7b08183409d4ff144c6f0453c0bddcd7a30adeb338039eff6c958bdbf" "0cc7596e605bf33daa06289b7ac812591fad4c1df28e9154d3ec7c28bffa66e5" "e11569fd7e31321a33358ee4b232c2d3cf05caccd90f896e1df6cab228191109" "1ac40990bf7aa91ad667e55654df61ab6574a5058e1a96702dd16a0a57b32693" "dafea82b8a161e62b7512b9649ea289bbfb5c6287a1929c85147daf419e842a0" "e661bf4e4f23330f46ae90fc75d6053c1ea2f832257f3b87302881b7b1a5ecf2" "7c15e252cfe7f4a7c066b0004a93ef61049ecf6131b8e43689ff6407a2156f0b" "f69293900680a0fd08769c456388e898233cb5d160d9d5fd813414b5654739f4" "9731e6f204206436f15f9f1fe5affbf007b824576a4d3e30176fbad054c72776" "89031202e9b70e1670e6f624b281bf50690c532a28cabc12fc33af50a6cf3800" "d09b53a9bccd9cda978b3ebb947f6d5cc72a13d39c644c7a42305e5617dd337f" "6eebc7bbdb88c13bc13ebe73a93365fd1857e26e26c667cc53644cce7c3f8a9e" "3b2808e9f762726e6709478ae8fa285776cc3a3859c1f60e2059a8e542987353" "c41c125fc0121f52a171de8f5a2f007a67530712b0cd8d8b1c177a62b315ce29" "e9dc855ab94003cfa30ce88f071118e1f715d3ae48ecd299be438347d95675c2" "c0406a8b3bf502f15364b8948cb36da0f9badfea5b67bcdb5bbb2fa18683f612" "ebda6542f22d83846f7d34c8e939a0cb111843bfb76929f880a9ae70e5ef41d9" "50837ed15b193c5ecdeb29dd0ad954db1776df3a034a532bc0becb3e51794e53" "0771744a1744d79361e1a1eb60b7fb1efe387ec703185e4fd05c869f5cc134f4" "ce92edfbaa59a9f404825495a0bffefb91ce8e3bd6b5021ecbece91f1e282693" "101112b5da367b0fb5e20c60f25a8ac261e6c9fd4ca19fb0c902ce671d195d60" "dc60cae3fea6f1992dd5a32541cd72778e54fe082692f7be34787c065c39e8dc" "b5815ac58b6caea83c76c2c5b41f9fb697569e8e7427ccf8387d3f44f8f20eb7" "bf932c474232789b5ebb268c747e4525b396e76df80b02d8cbfd0ba1d3fc5042" "2d383b6358fc43a383aaebcbbab9e740988f265c3af6f2eb4f98cbb6f69d26b1" "b7456eabe092035b6c524b002b7c00c18d5e2fc68979b6cdebaabdd667a76ce1" "8c49bff70df5da50a5ccd3a6d9f1d4aaccd8b622a749099561e7bdc4d7b975c4" "1e84c947cc9b323208eb61bc77a70ffa84764bb727297fb49ca6e21a2e1a932d" "eee9be7b721aa3876da05aed53e94940a95a87d6bf0a05467e6873ce6af0e08d" "ed71631d5759bb8e402863de34ad81f6277f1f832f79ebb41c5ce52154919d63" "61b9ca95a213997e4f827aa33c14a456572ddf0b5ed319e82f4793c41cf6ebbc" "9af39bee42cede35abc9181f5e916e2d2ef5e13a0902c3875966bca655ea5fce" "7b33b67c0c8c522128096d83a33755f6a41b36655bb1193462078b8067004333" "cb8e81b70db73ac2a62ee0169d4e8c28fba2227739c4cbc37d84e10ce2b6cb75" "cfc35536f111f51dcdb773fbf3245fa41629761c1f6482a09c44ff1471088bf6" "0317dbbe30b03189f6f321a33717ef540357a78bcd6e327646498a584242521b" "3a1f884fb67d56b3a9bfc808433fa4d00609bb925a5a1853461f634d4ec236bb" "fe59a4109b577cb4ee054bef31825a79642c24c70341de2ad265f1726d47507b" "778203d06bc4d2af36b3407e5ce9a9ea3c337d2a22c89d080dd922b2d39c7831" "e48290582fcf57b68f6a804386a8b4ce2f01358d9152364b7ef12200d2d9a13c" "24a35b5ad6b8a18f4bf559b081061927d550bfdaddfb0234fc0b2aaf9477d3e6" "0f80a763b8b72b173d9bdcc43474d24acea1a1d84d7ce73877d6c971fd273304" "b08fb7da6b77e11703e38986231a5985dc4249295236a23c1329c84079843722" "4d9398509a31bde6841356d1ed438da8378854d41d2463b4db67ddf4cf2c0d0e" "42a4b75476d1c83c60d94377b084e31157bbd91675dfa2a1cf04c659d2dbd6d8" "aea842b5101d415f68ec64e677dc74d31e8801da9242cc6c30e7178ddc4541c7" "f7895fa7957e0a8d3e1725a1612c25977f4bfbc0859ea2b0975678f8182b8093" "a8fb4389bae5d650b659d9650f8590226ba0e2f8095e30e3cd6736a1d87e1ddf" "03bf6ec0f6f53a6bbc4bcc7c4c5d98726a8da71ccfed429a57c1ad88954bbfcd" "50703ef64dee2bb5e5d46779d6858c95495665b4e9ceb6b849eb0c9866ecad50" "047d3d63efa5a4d3e5cfcf39219f031dc4bdd19789b41dc14b2fd6239aa7183e" "d69e04dfbd1dfcc2b405a73ede74141c617c7ad5284a845a2dc98f57bca55b6e" "b8dcd740221fd47472c679fb46fb3795d67cfb67188ce386cbdc6b0995838de9" "2f0dca1942aa538025da0bc044adb538c39dd466dbb4f5f33321041b00bdb7cd" "74fe528679e05181c0c533c4a833fa47b5ca07bfce4ba140d1ab0009093c12c9" "fed72cbc694e69a71ce25709d22ce64ee7be5a84581ba045f3cc8c749d7789d4" "511ab03e64a18e57e490c3bd51301d3f60015ed548c5b0255318cbe4396dcfd4" "30e2f9d431d7853f235a134ec06139fa7d7c13ef9e4cae2ca040629840a48e57" "a56cae836c05f77f5f782782da51a1e76de3caf99772485abcede77c05923940" "20032a6dadb9a0cff6e7d0d2974116f8f4fd31be1f66b22b885c7ed5701e5d13" "5ebb52f67be71b97ed52aa40cf3cea55f52cb0498adcb5d3cc4aa1c7e5febf29" "e822c0ecbf33f270520e9bee105457a9bdb13b7a37d10df5229c664ec115498a" "622e9f996247e2c343f93f52e269a2deadbbb0036cad49d53c7e78b4287dc0c1" "e311c97dcc3135f5c1718b0b73bd0e7e063ccd35e3d00193a55301281cd322a2" "4743df55ca27cc0e479bd4187e9b0ebd807ca23deaed3420547b4e4c49347b10" "7fe50c7a6a6b77751359d993ad4d6bcad9bf562b1ca2e71728900ca6bf5d4b01" "65a7e442aeb2c710cf7c267106e0060dd73ed06967ca9174af447c1d20427834" "e2b864ae6811e3b9c4dcb2892037ae40ee20344197f192d27f99bdc283cd06c3" "23d62536b32a966e36fdbd66b216247693231d07d79935ebfb8efac74d71ba8b" "dd7a353664de7e90e925ddb910bde13679485e24a9046ce7087f85b8b6ac1441" "d3b9b56fad56c0cc33e323d362392de692fa954833b49ef68f0f5f2fb2e77a8f" "49ec198ee2621af0223a2ba6601f3d05fe96a809853fcf32ba3643e5c42bfaa4" "911bc9b99b94db7535cb09a70c383f3a9e4a7867162920cbb6b4973190ce3476" "9a47c53847c211b05e0cc2ae3a85fa971198fb97674497d421488798df7b2ee3" "bb4f4e0948ea5a198e963c4db0299c56a877d4e640b68d244f762de4b349e58f" "858bdabe720619b9f5f4dfb58a912107d00627ee45d8089c15c2f68ad025f3eb" "bb621a789b7e3b71b3c33ad60331b452853611ef1800bd147f1f179e98f3b0ed" "8a98055e618e089d1c8283f24a7aff4bf56b296d6a5be8a9f3d131ad891f3caf" "a28fc60ba7fa78019912cafe81d0cbfd99a3bee10872c2dd92c046fd7ed92908" "791c6c9031478951bd64f91d15659a732c12ebd6a4bb4b74898d4d7053e55003" "bb4bad64428aaa700e27c3c1321c2509edef93c42d0ca84b40db45c1dc9d0718" "16a4dc3c4c0ed4f77babf59598eaca471b647402d7edb581272dcee550df99e6" "d6213620da466323b727c47c79317b74ea43b73ca729beeafd5b1eee2dc4f93d" "3b70ac136add85c476dd0c04bbee61f3195efc8f016cd0a93d77ea10bd7e7906" "4719b1ecdc61e94e547282860031144f92a89c797a9229b5aa518b959e3bf3fe" "430ca27e66133ae85f3924351060a664d95259b57aad872070b356326401c88e" "f263d12275cb28237b54f046a97858ae8ea48a1a739af3ae1d449005147fad20" "45d6370916e70bef5b2a4a93ec7fbaf4e661401d420d9c19d5c3c3397472cb5b" "2544a1fc86b09327538741db58d0da2e987d5534dca55773ab99ac5c3ecd2570" "365517e4c838143b11f7613932ecce83be64e2e4d71ce388021f71078d7a3d06" "8d08687b505e5fe588b218e3afd0e5e2a54c6c5094a1c4cc526c2fefb81672f5" "50bb44c0c0e4e3913558b78b609d1419d815db76e6a42fbb51ba6f9e6a901233" "1d8763bf89dd19012cc7f063bed309b1add9517bd8955ff7321fa13141c785d3" "d5a6b53d41badaea376729a461af4f07579073a4507061daf07067a3b3fd7f2f" "ce0e9a7516debe2950724e801cd673596df94f057691741fb3574a5bd481359b" "21685bdb4aa75e06f4f2fb608cb392cc3aa34db6d33a6ba464e69a5a5b1df0fe" "28c10800fe757cf5b2574ad66ac51f35a963c9b4ad588da17eac93cc1de78b67" "c6611bd64cdf6270e1cd94aa9b6e7a3504c2d0ec61b63241b4ef26954b1137e1" "60211ea7e924de850d592b6ac4eb29043454a35e96d1db1d71276b8682b04843" "3b88d01e19ce9541332a750865739d9d4cd6a0e7496446a45b9736c90b1d446f" "c3f971f1118446bd01f020031ebe116a82d4a58bcb745bd8f6f7af19d29f99bc" "47358f7671de40c2d2cde2b94fcc1e902fd51d882babf91ad8aaa30d79247acf" "82ef2a9cf75996ab4e76bb71ed59a312624b0d9fecdc50c24cc265cddb1891f5" "3dec9692373f56cd70f117c174f3ee850fc91886aaefcf2819b9e0d4aeb442b4" "e5184d2a9baab186ff39e1bd564d0cdf0cc1b16e6d149a29e1b2bbd544fde2f3" \`
     ("ce23eb8a41f316dd8e83d12f1102c4bc3d873f03c0840bb715dd7485a6fc4375" "4cdab1bb7daf3b5ff8730a3192f163a5ebc8dbf838a35a02ab8c4acfb9446e4d" "f8bdc0fa265732aa287ed6834d03256c7a81962f3339066b40df55b21271aecc" "14de50ac422c72fcad768c98666d2090002ce0f0b22cc8123baabcb85c357967" "d28266318e5e1f05e785e03180d9bf0f12417700e6342694d5ee85c8156dbe5d" "f21fd738fc96276330784b20dc94e2ddc7bd54f59d0339a1b9a9fc3353f81209" "d0c7d4e8ea11cee648f12807df1845d849baf5ae81db7cfef2b72884ed2f2f36" "cc63defec4021abda5d0103d3d31fb3525ef93462acda782626ba9ac517ceb2a" "3fe791f9f426f27baf7740daa97566d079a5662bec4b214658eeaf6b720dcfcb" "53dfac565c9a7931d28a81de8e01dce14ef11def9cee5df41112f4eb29608f4a" "a99608feeca62df4a617e860452a81cb339fb40cbf9a54a143f360f0d33e10e9" "60211ea7e924de850d592b6ac4eb29043454a35e96d1db1d71276b8682b04843" "d895e60012b36b17cc9fbea2c02e88572bdd56d7509c2c36dbc90b9b2703fc5a" "739992ba96433ad7945b18ef485dc2f4fc8e8419179f3c63bb443ce633b24ab4" "d58f1e4e17a7980de872cfe48ef6f710e912081c480386d948956b2e8168e0dc" "f774094839e11bd38d40331e9382ffc924d595eae7d5234981eb18131107ad76" "da4f03c1c7c54b62374d4e8ac38c782e8283ab82e77b4f4f6767a7e1ea276655" "5b4c18e8241df957091f4492d2a83a0b6b242d768b2e2518e699cab74a4b4da1" "2020ed7655cc92fcd627d976c121159d06391cdd7f746b08bf81782721bc6730" "fcc50343bbcbf3463943ceecc836658f5553af1dd97129acc8d5706672ff8e94" "ef9542ca0b692153cb3ee3bb2f6acf841491b7d38e741409b4858f535721a8ef" "35f637897db4b283bfd3dae5c1358a756061ac570f0d292b0460cc55af444553" "b0462e82dc44fab2bd94c2a7168768a0edf73f3c12c6b58c58883e5fd5d76661" "619a2779c74031000afb8a48927eab3565817fee106fbe4f06ee2c5bc5a6d351" "491a02f4fcea31534576ae91771f4d0234e6259b4e4133b67979a6d3b7b16e9b" "77ecb8dba07df2ba275bb39004a34b2db94f7a15f3c768f34bcbe8e4ead45936" "45d6370916e70bef5b2a4a93ec7fbaf4e661401d420d9c19d5c3c3397472cb5b" "9ebfb6fcd837a1e13655c17d666015a731d1824c17fd465c1a74bf04363a3e7f" "f55d21ee2bc4a9a9b41926772ec5a8fd2016e1562375d0633fc49a42bba555e6" "d89c2d2a01c0d1b3205988df818d64a5961f3569dde74aa63f205ea10d37c8e2" "786b975bae95aec37cdedb1c70441aa7ff358fc359c8e1802af7fe88b61e8de3" "430fea33773e79263cae31cfa553687aa7d1d9d618aea3e8c4e74a20d0387105" "c402aece67579c30733a5c95119a7222e2969d9bfb13fb711f8dacba428d0a01" "cc93274abd876bd7afee024fa6f0ac3ceb3cf12c34137e30b4cc30cfd05f0d90" "26ae46706518980608bc920d227d2f2e1c26b303fcb7e3fb1cbb31a09c8f358d" "601651546bd1b9223a564620c0e44e49f638f98ce7163ec24fa3db0944301b4b" "be88db69be3da30e47773e915650410950789ae4cf6d38815638e02232423979" "fc2d43ba033e9cf245161317843cba6656658b4f9a132ae25957d73cfa739152" "539532b6418ef4c4e03b927296257cd246062c4936ace5e556f82f07940c3a46" "ea163dd6e9b151c739003105c03e21ca41becd4f50316aaab57114b6bd3ec48b" "a6bd4eb1e7c3cfdfd87fec367fd44ac27d12428473db5080835a245fdc271dad" "e5f3ad79e5098b0fa58d8e2f26898c01571f9aaae074b4deeeee050b2365ad5d" "4bbad25e0215e309dccb5b25c15df55a4082111ea66ee4ae4c353403bae34b31" "49d7058b1ad35faa88cdd391e3e4bc34f78c24a6bde7dc2f9946b448574c7165" "7c3349025bf8bb160c09c4963caacce4e00be64e9a070cfabfc901ea4fbc9c12" "8fe83a62dc0f1118811cd82ebf60ab6d9471562015bc2b73ae3ec379e796c536" "12976c3dd7996ac91fe738ae8b58dbc75f7875a60cadaffd72f341a04a6b769a" "35e7319643d6f93589e8eea1f89ddf4fbcb55a2e234f3e97270c608c360f8707" "90b0191e74c531ba39ae4ea755d9c9166fdc6fa5ac00711a0d6aa39c31def652" "5763c8bd394b3439d24c2caafef458b834502e01f272ede0e789bb41fbe3dd2a" "dab1295d80638d517c0dd3312ddc604f98f96ab416d4834f93f3c30619be6a44" "815eec341b8596a0ee15adfb0dcd455d6ab22d7851804d9bf051f25c4a7e6792" "a170111e5f294b21941d40c5641c75248381a3bb335e30d9d27ed8419fd23745" "0e854ae02392414bba1a12feb907224e37e0bb745f5560bf5d79ab4bfbe9f3e6" "58545c5216b4acc0dd02cfe16d54195a424bdf9cec7e237b3dd36d04c02473af" "ef85b91c593f6549bdc9e548c2184285f5c51e974b5be3c4ed6dd454028719ae" "db0cbdaf1801059611fb8a6e339d653fbe8070cea40d4137ca84f46ea0dcb47f" "b421ae0446bd9f6db68c5936a6408ed4977d2230f16b011c8c267010ac8e085c" "0421f946e871c2ce8696e36479c17e546ef7a9f40ea75b6c9dad43043794d61a" "7ee4eae903199970c573b56c7e71615c17645ef0fe61893cc2c109514bfa57f6" "8515ae7d039f789977bc181c32b58e6d5f4fc5b4755007de0b3bd9688ff01fa4" "9dd99d56bdaf15779ca7cb73673c8ba8de4d3a2295ef83c91b9f32492e88b5ff" "f1ea3c92e3c82da1a649d23aec48f4ac222618883abcc5736df298227cd24373" "7601e46b2a73ab0d0d0b5a8e13b125f0660efebf1c7a05d87b4bd7304f90c9ad" "1a14a21d173f83ad9d95ec2af8bef5135087e9fce171730c92517dd2b33580f9" "bc982e953e8954acfd7d957038f33a3be03655b21bb18ab24323ab8ab1668e28" "d42f4f906ca69e5a5f1efa1af75b48dc865eb5b478edc2ac24b047f8aa1b5f01" "a01e0cf273ac7b5c1baeeebb296ef683deff7293b9a25c9b783883e7ebe12aed" "d9edc29a9b27d7098646c3315c5ab8fdf07638b1ab4f80360a521f845a3c5fb0" "87d34869134b5497549a25dff75367d68aed7a8e3da598c9fa4e060a4e1f948e" "e3164f469e6f7aa0ec451dd7b66ac79111bdd78d4cfec69e8bb9c66fca912a48" "a7571dfde3a54d069bc7969457b213023d69a7534a556b4c27e301cbac1be221" "5fa0947f1d0d999eee035069b8b65cc4f2f14e1717e1792ec3d80ae625e0f29f" "b335b10a1d293669d7493018b099f34cbca1e8fb8bd43db50096c9306a567f91" "e9f71e1e9e479c080612c0e6cb0109c7a9fc1dafeeec7b0fee075d23fa793370" "f2d8571772c7216c5dca60e00590f710b1c55e4b866c8c130d547e4a3f204e8b" "582e2e5490bc4416ea0481db6f5b2c7203426959f545c88273fa3220e3528d1d" "cc5f20e3da891112f77f14d3d1650f7faa3e1051b3f9f527dcf8d71039893ac9" "3cbcddac24a78361e0d69af42f5970ab99b0ba6ca9cb47bbbfbbe680362b0b20" "c5206ba2411b3665e02f3ce0fc5d8461790a9bad8b2bb7db6604856bcfcf8b3d" "fcfcd70b25718334c188f41b33f96da3b46b6093fc4cb94cd0b0d3063ed48ec5" "3a5007fa3d1fe8fee567c59e6cdd43f1924ab320cb938fe11745710a44cac30b" "157956dd11bb3f8cd6856cc32c6df31583086da13c96b480e25e6705c3765dcd" "868360d9afa25cb16ea603a5c78655acc21db2ea508acdc63b8acb93880b3440" "8e2ebb60baf42758a76565808251aa197556d9f751e6c6a55e811ecdbd69deca" "8464a1275056fb10d7f2e712c7e3750abbf6c3ac952a005ad7c1a8e2eece88ea" "df745aa43d57560c339941c1d68d4d91d0df6e984ac511a68209960b6abd0f6d" "23b0a10ce874449818aa478c63265755639ac12ba5e1562ea012f99c3fdccea7" "d145690625dc0b4f86fbdd8651fbbb861572c57505edf4fd91be5fead58d692d" "0ee3fc6d2e0fc8715ff59aed2432510d98f7e76fe81d183a0eb96789f4d897ca" "98d0ff69fd11d6fca210b5068022d504b6eea5208233dec38212baf7201c811f" "b04153b12fbb67935f6898f38eb985ec62511fd1df6e2262069efa8565874195" "98e568e0b988a0ef8c9abdb9730ee909929167ff8932ecfb33d8cec8c3432935" "cdc683669f9425d9faf91f2fb07a508178c9e9c20ec3ce10cf6f6c2e6ac628c0" "df97fc9066acac64a021021021a809e7c421ba7c8bc7669095c6cf32f72edc22")))
 '(default-input-method "sgml")
 '(default-justification 'full)
 '(desktop-save nil)
 '(desktop-save-mode nil)
 '(diff-command "diff")
 '(diff-switches "-C 5")
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
 '(lua-always-show nil)
 '(lua-default-application "lua5.3")
 '(lua-indent-level 2)
 '(lua-prefix-key "C-c")
 '(magit-diff-use-overlays nil)
 '(menu-bar-mode nil)
 '(message-default-charset 'iso-8859-1)
 '(mouse-wheel-mode t)
 '(nrepl-message-colors
   '("#CC9393" "#DFAF8F" "#F0DFAF" "#7F9F7F" "#BFEBBF" "#93E0E3" "#94BFF3" "#DC8CC3"))
 '(org-fontify-whole-heading-line t)
 '(package-selected-packages
   '(windresize ido-occur zel rainbow-mode which-key fill-column-indicator rectangle-utils imenu-anywhere nasm-mode bury-successful-compilation markdown-mode+ ido-vertical-mode syntax-subword idomenu ido-ubiquitous ido-select-window use-package bm move-text smex magit multiple-cursors visual-regexp expand-region peg))
 '(pdf-view-midnight-colors '("#DCDCCC" . "#383838"))
 '(pos-tip-background-color "#eee8d5")
 '(pos-tip-foreground-color "#586e75")
 '(ruler-mode-show-tab-stops nil)
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
 '(speedbar-ignored-modes '(fundamental-mode info-mode))
 '(speedbar-tag-group-name-minimum-length 25)
 '(speedbar-tag-regroup-maximum-length 32)
 '(speedbar-tag-split-minimum-length 10)
 '(speedbar-use-imenu-flag nil)
 '(speedbar-verbosity-level 0)
 '(sr-speedbar-auto-refresh nil)
 '(sr-speedbar-default-width 28)
 '(sr-speedbar-max-width 28)
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

(setq-default *uninteresting-buffer-names*
              '("TAGS" "zel-history" "*Messages*" "*Backtrace*" "*Warnings*"))

(defun switch-to-next-interesting-buffer ()
  (interactive)
  (dolist (buff (reverse (cdr (buffer-list))))
    (let ((bname (buffer-name buff)))
      (when (and (not (member bname *uninteresting-buffer-names*))
                 (or (< (length bname) 8)
                     (string= "*Minibuf" (substring bname 0 8)))
                 (switch-to-buffer buff)
                 (return))))))

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

;;* Custom keybindings.
(global-set-key (kbd "C-x k")           'kill-this-buffer)
(global-set-key (kbd "M-`")             'switch-to-next-interesting-buffer)
(global-set-key (kbd "C-`")             'switch-to-next-file-buffer)
(global-set-key (kbd "C-M-'")           'push-mark-no-activate)
(global-set-key (kbd "C-M-;")           'jump-to-mark)
(global-set-key (kbd "C-c b")           'compile)
(global-set-key (kbd "C-c k")           (lambda ()
                                          (interactive)
                                          (let ((col (current-column)))
                                            (kill-whole-line)
                                            (move-to-column col t))))
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
(global-set-key (kbd "M-]")             'zap-up-to-char)
(global-set-key (kbd "M-z")             'zap-up-to-char)
(global-set-key (kbd "M-[")             (lambda (c)
                                          (interactive "cZap backward upto char:")
                                          (zap-up-to-char -1 c)))
(global-set-key (kbd "C-z")             'undo)
(global-set-key (kbd "C-a")             'tkf-beginning-of-line)
(global-set-key (kbd "C-=")             'er/expand-region)
(global-set-key (kbd "<M-right>")       'other-window)
(global-set-key (kbd "<M-left>")        'prev-window)
(global-set-key (kbd "C-M-<return>")    'hs-toggle-hiding)
(global-set-key (kbd "M-f")             'forward-to-word)
(global-set-key (kbd "M-b")             'backward-to-word)

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
 '(compilation-line-number ((t (:inherit default :stipple nil :background "#696c72" :foreground "black" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :width normal))))
 '(compilation-mode-line-exit ((t nil)))
 '(compilation-mode-line-fail ((t nil)))
 '(compilation-mode-line-run ((t nil))))
