(when (>= emacs-major-version 24)
  (require 'package)
  (setq package-archives '(("gnu"       . "http://elpa.gnu.org/packages/")
                           ;("marmalade" . "http://marmalade-repo.org/packages/")
                           ("melpa"     . "http://melpa.milkbox.net/packages/")))
  (package-initialize))

(add-to-list 'load-path "~/.emacs.d/lisp")
(load "~/.emacs.d/site-specific.el")

;;* Requires/includes

(require 'cc-mode) ;- so that c-mode-map exists for key redef
(require 'dired) ;- so that dired-mode-map exists for key redef
(require 'python) ;- so that python-mode-map exists for key redef
(require 'scheme)
(require 'kill-ring-ido)
(require 'subr-x)
(load "~/.emacs.d/lua-mode") ;- custom version

(use-package bury-successful-compilation
  :config
  (bury-successful-compilation 1))

(use-package recentf
  :init
  (setq recentf-max-menu-items 25)
  (setq recentf-exclude '(".*-autoloads\\.el\\'"
                          "[/\\]\\.elpa/"
                          ".*.ido\\.last.*"
                          ".*/session.*"
                          "/home/todd/Mail.*"
                          ))
  :config
  (recentf-mode 1))

(use-package multiple-cursors
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
  :bind
  ("<M-up>" .   move-text-up)
  ("<M-down>" . move-text-down))

(use-package smex
  :init
  (smex-initialize)
  (setq smex-max-prospects 10)
  :bind
  ("M-x" . smex))

(use-package ido-completing-read+
  :config
  (ido-ubiquitous-mode t))

(use-package bm
  :init
  (setq bm-highlight-style (quote bm-highlight-only-fringe))
  :bind
  (("C-c b"       . tkf-bm-cmd-list)
   ("C-M-<left>"  . bm-toggle)
   ("C-M-<right>" . bm-toggle)
   ("C-M-<up>"    . bm-previous)
   ("C-M-<down>"  . bm-next)))

(use-package idomenu
  :bind
  (("C-c i" . idomenu)))

(use-package expand-region
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

(use-package sr-speedbar
  :init
  (setq sr-speedbar-auto-refresh nil)
  (setq sr-speedbar-default-width speedbar-scr-width)
  (setq sr-speedbar-max-width speedbar-scr-width)
  (setq sr-speedbar-right-side nil))

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
  (setq ido-max-prospects 10)
  :config
  (ido-mode (quote both)))

(use-package ido-vertical-mode
  :init
  (setq ido-vertical-indicator "»")
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
                    "│"  ; window number goes between bars
                    "│"
                    ;; '%0x' - fixed-width prevents flickering
                    "%06p C%02c"
                    "│"
                    '(:eval (if (buffer-modified-p) "M" "-"))
                    '(:eval (if buffer-read-only    "R" "-"))
                    '(:eval (if (window-dedicated-p (selected-window)) "D" "-"))
                    "│"
                    '(:eval (last-dir default-directory))
                    "│"
                    mode-line-modes
                    "│"
                    '(:eval vc-mode)
                    ))

;;* Misc initialization

(server-start)
(window-numbering-mode t)
;; work around the bogus "-remote" flag specified by browse-url-firefox
(setq browse-url-browser-function 'browse-url-generic)
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
(load-theme 'tkf-dark t)

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
 '(blink-cursor-blinks 0)
 '(blink-cursor-delay 1.1)
 '(blink-cursor-interval 0.45)
 '(blink-cursor-mode t)
 '(blink-matching-delay 0)
 '(byte-compile-delete-errors t)
 '(byte-compile-warnings '(not cl-functions))
 '(column-number-mode t)
 '(compile-command "./build.sh")
 '(custom-safe-themes
   '("e9dc855ab94003cfa30ce88f071118e1f715d3ae48ecd299be438347d95675c2" "c0406a8b3bf502f15364b8948cb36da0f9badfea5b67bcdb5bbb2fa18683f612" "ebda6542f22d83846f7d34c8e939a0cb111843bfb76929f880a9ae70e5ef41d9" "50837ed15b193c5ecdeb29dd0ad954db1776df3a034a532bc0becb3e51794e53" "0771744a1744d79361e1a1eb60b7fb1efe387ec703185e4fd05c869f5cc134f4" "ce92edfbaa59a9f404825495a0bffefb91ce8e3bd6b5021ecbece91f1e282693" "101112b5da367b0fb5e20c60f25a8ac261e6c9fd4ca19fb0c902ce671d195d60" "dc60cae3fea6f1992dd5a32541cd72778e54fe082692f7be34787c065c39e8dc" "b5815ac58b6caea83c76c2c5b41f9fb697569e8e7427ccf8387d3f44f8f20eb7" "bf932c474232789b5ebb268c747e4525b396e76df80b02d8cbfd0ba1d3fc5042" "2d383b6358fc43a383aaebcbbab9e740988f265c3af6f2eb4f98cbb6f69d26b1" "b7456eabe092035b6c524b002b7c00c18d5e2fc68979b6cdebaabdd667a76ce1" "8c49bff70df5da50a5ccd3a6d9f1d4aaccd8b622a749099561e7bdc4d7b975c4" "1e84c947cc9b323208eb61bc77a70ffa84764bb727297fb49ca6e21a2e1a932d" "eee9be7b721aa3876da05aed53e94940a95a87d6bf0a05467e6873ce6af0e08d" "ed71631d5759bb8e402863de34ad81f6277f1f832f79ebb41c5ce52154919d63" "61b9ca95a213997e4f827aa33c14a456572ddf0b5ed319e82f4793c41cf6ebbc" "9af39bee42cede35abc9181f5e916e2d2ef5e13a0902c3875966bca655ea5fce" "7b33b67c0c8c522128096d83a33755f6a41b36655bb1193462078b8067004333" "cb8e81b70db73ac2a62ee0169d4e8c28fba2227739c4cbc37d84e10ce2b6cb75" "cfc35536f111f51dcdb773fbf3245fa41629761c1f6482a09c44ff1471088bf6" "0317dbbe30b03189f6f321a33717ef540357a78bcd6e327646498a584242521b" "3a1f884fb67d56b3a9bfc808433fa4d00609bb925a5a1853461f634d4ec236bb" "fe59a4109b577cb4ee054bef31825a79642c24c70341de2ad265f1726d47507b" "778203d06bc4d2af36b3407e5ce9a9ea3c337d2a22c89d080dd922b2d39c7831" "e48290582fcf57b68f6a804386a8b4ce2f01358d9152364b7ef12200d2d9a13c" "24a35b5ad6b8a18f4bf559b081061927d550bfdaddfb0234fc0b2aaf9477d3e6" "0f80a763b8b72b173d9bdcc43474d24acea1a1d84d7ce73877d6c971fd273304" "b08fb7da6b77e11703e38986231a5985dc4249295236a23c1329c84079843722" "4d9398509a31bde6841356d1ed438da8378854d41d2463b4db67ddf4cf2c0d0e" "42a4b75476d1c83c60d94377b084e31157bbd91675dfa2a1cf04c659d2dbd6d8" "aea842b5101d415f68ec64e677dc74d31e8801da9242cc6c30e7178ddc4541c7" "f7895fa7957e0a8d3e1725a1612c25977f4bfbc0859ea2b0975678f8182b8093" "a8fb4389bae5d650b659d9650f8590226ba0e2f8095e30e3cd6736a1d87e1ddf" "03bf6ec0f6f53a6bbc4bcc7c4c5d98726a8da71ccfed429a57c1ad88954bbfcd" "50703ef64dee2bb5e5d46779d6858c95495665b4e9ceb6b849eb0c9866ecad50" "047d3d63efa5a4d3e5cfcf39219f031dc4bdd19789b41dc14b2fd6239aa7183e" "d69e04dfbd1dfcc2b405a73ede74141c617c7ad5284a845a2dc98f57bca55b6e" "b8dcd740221fd47472c679fb46fb3795d67cfb67188ce386cbdc6b0995838de9" "2f0dca1942aa538025da0bc044adb538c39dd466dbb4f5f33321041b00bdb7cd" "74fe528679e05181c0c533c4a833fa47b5ca07bfce4ba140d1ab0009093c12c9" "fed72cbc694e69a71ce25709d22ce64ee7be5a84581ba045f3cc8c749d7789d4" "511ab03e64a18e57e490c3bd51301d3f60015ed548c5b0255318cbe4396dcfd4" "30e2f9d431d7853f235a134ec06139fa7d7c13ef9e4cae2ca040629840a48e57" "a56cae836c05f77f5f782782da51a1e76de3caf99772485abcede77c05923940" "20032a6dadb9a0cff6e7d0d2974116f8f4fd31be1f66b22b885c7ed5701e5d13" "5ebb52f67be71b97ed52aa40cf3cea55f52cb0498adcb5d3cc4aa1c7e5febf29" "e822c0ecbf33f270520e9bee105457a9bdb13b7a37d10df5229c664ec115498a" "622e9f996247e2c343f93f52e269a2deadbbb0036cad49d53c7e78b4287dc0c1" "e311c97dcc3135f5c1718b0b73bd0e7e063ccd35e3d00193a55301281cd322a2" "4743df55ca27cc0e479bd4187e9b0ebd807ca23deaed3420547b4e4c49347b10" "7fe50c7a6a6b77751359d993ad4d6bcad9bf562b1ca2e71728900ca6bf5d4b01" "65a7e442aeb2c710cf7c267106e0060dd73ed06967ca9174af447c1d20427834" "e2b864ae6811e3b9c4dcb2892037ae40ee20344197f192d27f99bdc283cd06c3" "23d62536b32a966e36fdbd66b216247693231d07d79935ebfb8efac74d71ba8b" "dd7a353664de7e90e925ddb910bde13679485e24a9046ce7087f85b8b6ac1441" "d3b9b56fad56c0cc33e323d362392de692fa954833b49ef68f0f5f2fb2e77a8f" "49ec198ee2621af0223a2ba6601f3d05fe96a809853fcf32ba3643e5c42bfaa4" "911bc9b99b94db7535cb09a70c383f3a9e4a7867162920cbb6b4973190ce3476" "9a47c53847c211b05e0cc2ae3a85fa971198fb97674497d421488798df7b2ee3" "bb4f4e0948ea5a198e963c4db0299c56a877d4e640b68d244f762de4b349e58f" "858bdabe720619b9f5f4dfb58a912107d00627ee45d8089c15c2f68ad025f3eb" "bb621a789b7e3b71b3c33ad60331b452853611ef1800bd147f1f179e98f3b0ed" "8a98055e618e089d1c8283f24a7aff4bf56b296d6a5be8a9f3d131ad891f3caf" "a28fc60ba7fa78019912cafe81d0cbfd99a3bee10872c2dd92c046fd7ed92908" "791c6c9031478951bd64f91d15659a732c12ebd6a4bb4b74898d4d7053e55003" "bb4bad64428aaa700e27c3c1321c2509edef93c42d0ca84b40db45c1dc9d0718" "16a4dc3c4c0ed4f77babf59598eaca471b647402d7edb581272dcee550df99e6" "d6213620da466323b727c47c79317b74ea43b73ca729beeafd5b1eee2dc4f93d" "3b70ac136add85c476dd0c04bbee61f3195efc8f016cd0a93d77ea10bd7e7906" "4719b1ecdc61e94e547282860031144f92a89c797a9229b5aa518b959e3bf3fe" "430ca27e66133ae85f3924351060a664d95259b57aad872070b356326401c88e" "f263d12275cb28237b54f046a97858ae8ea48a1a739af3ae1d449005147fad20" "45d6370916e70bef5b2a4a93ec7fbaf4e661401d420d9c19d5c3c3397472cb5b" "2544a1fc86b09327538741db58d0da2e987d5534dca55773ab99ac5c3ecd2570" "365517e4c838143b11f7613932ecce83be64e2e4d71ce388021f71078d7a3d06" "8d08687b505e5fe588b218e3afd0e5e2a54c6c5094a1c4cc526c2fefb81672f5" "50bb44c0c0e4e3913558b78b609d1419d815db76e6a42fbb51ba6f9e6a901233" "1d8763bf89dd19012cc7f063bed309b1add9517bd8955ff7321fa13141c785d3" "d5a6b53d41badaea376729a461af4f07579073a4507061daf07067a3b3fd7f2f" "ce0e9a7516debe2950724e801cd673596df94f057691741fb3574a5bd481359b" "21685bdb4aa75e06f4f2fb608cb392cc3aa34db6d33a6ba464e69a5a5b1df0fe" "28c10800fe757cf5b2574ad66ac51f35a963c9b4ad588da17eac93cc1de78b67" "c6611bd64cdf6270e1cd94aa9b6e7a3504c2d0ec61b63241b4ef26954b1137e1" "60211ea7e924de850d592b6ac4eb29043454a35e96d1db1d71276b8682b04843" "3b88d01e19ce9541332a750865739d9d4cd6a0e7496446a45b9736c90b1d446f" "c3f971f1118446bd01f020031ebe116a82d4a58bcb745bd8f6f7af19d29f99bc" "47358f7671de40c2d2cde2b94fcc1e902fd51d882babf91ad8aaa30d79247acf" "82ef2a9cf75996ab4e76bb71ed59a312624b0d9fecdc50c24cc265cddb1891f5" "3dec9692373f56cd70f117c174f3ee850fc91886aaefcf2819b9e0d4aeb442b4" "e5184d2a9baab186ff39e1bd564d0cdf0cc1b16e6d149a29e1b2bbd544fde2f3" \`
     ("ce23eb8a41f316dd8e83d12f1102c4bc3d873f03c0840bb715dd7485a6fc4375" "4cdab1bb7daf3b5ff8730a3192f163a5ebc8dbf838a35a02ab8c4acfb9446e4d" "f8bdc0fa265732aa287ed6834d03256c7a81962f3339066b40df55b21271aecc" "14de50ac422c72fcad768c98666d2090002ce0f0b22cc8123baabcb85c357967" "d28266318e5e1f05e785e03180d9bf0f12417700e6342694d5ee85c8156dbe5d" "f21fd738fc96276330784b20dc94e2ddc7bd54f59d0339a1b9a9fc3353f81209" "d0c7d4e8ea11cee648f12807df1845d849baf5ae81db7cfef2b72884ed2f2f36" "cc63defec4021abda5d0103d3d31fb3525ef93462acda782626ba9ac517ceb2a" "3fe791f9f426f27baf7740daa97566d079a5662bec4b214658eeaf6b720dcfcb" "53dfac565c9a7931d28a81de8e01dce14ef11def9cee5df41112f4eb29608f4a" "a99608feeca62df4a617e860452a81cb339fb40cbf9a54a143f360f0d33e10e9" "60211ea7e924de850d592b6ac4eb29043454a35e96d1db1d71276b8682b04843" "d895e60012b36b17cc9fbea2c02e88572bdd56d7509c2c36dbc90b9b2703fc5a" "739992ba96433ad7945b18ef485dc2f4fc8e8419179f3c63bb443ce633b24ab4" "d58f1e4e17a7980de872cfe48ef6f710e912081c480386d948956b2e8168e0dc" "f774094839e11bd38d40331e9382ffc924d595eae7d5234981eb18131107ad76" "da4f03c1c7c54b62374d4e8ac38c782e8283ab82e77b4f4f6767a7e1ea276655" "5b4c18e8241df957091f4492d2a83a0b6b242d768b2e2518e699cab74a4b4da1" "2020ed7655cc92fcd627d976c121159d06391cdd7f746b08bf81782721bc6730" "fcc50343bbcbf3463943ceecc836658f5553af1dd97129acc8d5706672ff8e94" "ef9542ca0b692153cb3ee3bb2f6acf841491b7d38e741409b4858f535721a8ef" "35f637897db4b283bfd3dae5c1358a756061ac570f0d292b0460cc55af444553" "b0462e82dc44fab2bd94c2a7168768a0edf73f3c12c6b58c58883e5fd5d76661" "619a2779c74031000afb8a48927eab3565817fee106fbe4f06ee2c5bc5a6d351" "491a02f4fcea31534576ae91771f4d0234e6259b4e4133b67979a6d3b7b16e9b" "77ecb8dba07df2ba275bb39004a34b2db94f7a15f3c768f34bcbe8e4ead45936" "45d6370916e70bef5b2a4a93ec7fbaf4e661401d420d9c19d5c3c3397472cb5b" "9ebfb6fcd837a1e13655c17d666015a731d1824c17fd465c1a74bf04363a3e7f" "f55d21ee2bc4a9a9b41926772ec5a8fd2016e1562375d0633fc49a42bba555e6" "d89c2d2a01c0d1b3205988df818d64a5961f3569dde74aa63f205ea10d37c8e2" "786b975bae95aec37cdedb1c70441aa7ff358fc359c8e1802af7fe88b61e8de3" "430fea33773e79263cae31cfa553687aa7d1d9d618aea3e8c4e74a20d0387105" "c402aece67579c30733a5c95119a7222e2969d9bfb13fb711f8dacba428d0a01" "cc93274abd876bd7afee024fa6f0ac3ceb3cf12c34137e30b4cc30cfd05f0d90" "26ae46706518980608bc920d227d2f2e1c26b303fcb7e3fb1cbb31a09c8f358d" "601651546bd1b9223a564620c0e44e49f638f98ce7163ec24fa3db0944301b4b" "be88db69be3da30e47773e915650410950789ae4cf6d38815638e02232423979" "fc2d43ba033e9cf245161317843cba6656658b4f9a132ae25957d73cfa739152" "539532b6418ef4c4e03b927296257cd246062c4936ace5e556f82f07940c3a46" "ea163dd6e9b151c739003105c03e21ca41becd4f50316aaab57114b6bd3ec48b" "a6bd4eb1e7c3cfdfd87fec367fd44ac27d12428473db5080835a245fdc271dad" "e5f3ad79e5098b0fa58d8e2f26898c01571f9aaae074b4deeeee050b2365ad5d" "4bbad25e0215e309dccb5b25c15df55a4082111ea66ee4ae4c353403bae34b31" "49d7058b1ad35faa88cdd391e3e4bc34f78c24a6bde7dc2f9946b448574c7165" "7c3349025bf8bb160c09c4963caacce4e00be64e9a070cfabfc901ea4fbc9c12" "8fe83a62dc0f1118811cd82ebf60ab6d9471562015bc2b73ae3ec379e796c536" "12976c3dd7996ac91fe738ae8b58dbc75f7875a60cadaffd72f341a04a6b769a" "35e7319643d6f93589e8eea1f89ddf4fbcb55a2e234f3e97270c608c360f8707" "90b0191e74c531ba39ae4ea755d9c9166fdc6fa5ac00711a0d6aa39c31def652" "5763c8bd394b3439d24c2caafef458b834502e01f272ede0e789bb41fbe3dd2a" "dab1295d80638d517c0dd3312ddc604f98f96ab416d4834f93f3c30619be6a44" "815eec341b8596a0ee15adfb0dcd455d6ab22d7851804d9bf051f25c4a7e6792" "a170111e5f294b21941d40c5641c75248381a3bb335e30d9d27ed8419fd23745" "0e854ae02392414bba1a12feb907224e37e0bb745f5560bf5d79ab4bfbe9f3e6" "58545c5216b4acc0dd02cfe16d54195a424bdf9cec7e237b3dd36d04c02473af" "ef85b91c593f6549bdc9e548c2184285f5c51e974b5be3c4ed6dd454028719ae" "db0cbdaf1801059611fb8a6e339d653fbe8070cea40d4137ca84f46ea0dcb47f" "b421ae0446bd9f6db68c5936a6408ed4977d2230f16b011c8c267010ac8e085c" "0421f946e871c2ce8696e36479c17e546ef7a9f40ea75b6c9dad43043794d61a" "7ee4eae903199970c573b56c7e71615c17645ef0fe61893cc2c109514bfa57f6" "8515ae7d039f789977bc181c32b58e6d5f4fc5b4755007de0b3bd9688ff01fa4" "9dd99d56bdaf15779ca7cb73673c8ba8de4d3a2295ef83c91b9f32492e88b5ff" "f1ea3c92e3c82da1a649d23aec48f4ac222618883abcc5736df298227cd24373" "7601e46b2a73ab0d0d0b5a8e13b125f0660efebf1c7a05d87b4bd7304f90c9ad" "1a14a21d173f83ad9d95ec2af8bef5135087e9fce171730c92517dd2b33580f9" "bc982e953e8954acfd7d957038f33a3be03655b21bb18ab24323ab8ab1668e28" "d42f4f906ca69e5a5f1efa1af75b48dc865eb5b478edc2ac24b047f8aa1b5f01" "a01e0cf273ac7b5c1baeeebb296ef683deff7293b9a25c9b783883e7ebe12aed" "d9edc29a9b27d7098646c3315c5ab8fdf07638b1ab4f80360a521f845a3c5fb0" "87d34869134b5497549a25dff75367d68aed7a8e3da598c9fa4e060a4e1f948e" "e3164f469e6f7aa0ec451dd7b66ac79111bdd78d4cfec69e8bb9c66fca912a48" "a7571dfde3a54d069bc7969457b213023d69a7534a556b4c27e301cbac1be221" "5fa0947f1d0d999eee035069b8b65cc4f2f14e1717e1792ec3d80ae625e0f29f" "b335b10a1d293669d7493018b099f34cbca1e8fb8bd43db50096c9306a567f91" "e9f71e1e9e479c080612c0e6cb0109c7a9fc1dafeeec7b0fee075d23fa793370" "f2d8571772c7216c5dca60e00590f710b1c55e4b866c8c130d547e4a3f204e8b" "582e2e5490bc4416ea0481db6f5b2c7203426959f545c88273fa3220e3528d1d" "cc5f20e3da891112f77f14d3d1650f7faa3e1051b3f9f527dcf8d71039893ac9" "3cbcddac24a78361e0d69af42f5970ab99b0ba6ca9cb47bbbfbbe680362b0b20" "c5206ba2411b3665e02f3ce0fc5d8461790a9bad8b2bb7db6604856bcfcf8b3d" "fcfcd70b25718334c188f41b33f96da3b46b6093fc4cb94cd0b0d3063ed48ec5" "3a5007fa3d1fe8fee567c59e6cdd43f1924ab320cb938fe11745710a44cac30b" "157956dd11bb3f8cd6856cc32c6df31583086da13c96b480e25e6705c3765dcd" "868360d9afa25cb16ea603a5c78655acc21db2ea508acdc63b8acb93880b3440" "8e2ebb60baf42758a76565808251aa197556d9f751e6c6a55e811ecdbd69deca" "8464a1275056fb10d7f2e712c7e3750abbf6c3ac952a005ad7c1a8e2eece88ea" "df745aa43d57560c339941c1d68d4d91d0df6e984ac511a68209960b6abd0f6d" "23b0a10ce874449818aa478c63265755639ac12ba5e1562ea012f99c3fdccea7" "d145690625dc0b4f86fbdd8651fbbb861572c57505edf4fd91be5fead58d692d" "0ee3fc6d2e0fc8715ff59aed2432510d98f7e76fe81d183a0eb96789f4d897ca" "98d0ff69fd11d6fca210b5068022d504b6eea5208233dec38212baf7201c811f" "b04153b12fbb67935f6898f38eb985ec62511fd1df6e2262069efa8565874195" "98e568e0b988a0ef8c9abdb9730ee909929167ff8932ecfb33d8cec8c3432935" "cdc683669f9425d9faf91f2fb07a508178c9e9c20ec3ce10cf6f6c2e6ac628c0" "df97fc9066acac64a021021021a809e7c421ba7c8bc7669095c6cf32f72edc22")))
 '(default-justification 'full)
 '(desktop-save nil)
 '(desktop-save-mode nil)
 '(diff-command "diff")
 '(diff-switches "-C 5")
 '(fancy-splash-image nil)
 '(fci-rule-color "#3E4451")
 '(font-lock-global-modes '(not speedbar-mode))
 '(frame-background-mode nil)
 '(grep-command "grep -in")
 '(grep-find-command '("find . -type f -exec grep /dev/null {} +" . 26))
 '(highlight-changes-colors nil)
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
 '(make-backup-files nil)
 '(menu-bar-mode nil)
 '(message-default-charset 'iso-8859-1)
 '(mouse-wheel-mode t)
 '(nrepl-message-colors
   '("#CC9393" "#DFAF8F" "#F0DFAF" "#7F9F7F" "#BFEBBF" "#93E0E3" "#94BFF3" "#DC8CC3"))
 '(org-fontify-whole-heading-line t)
 '(package-selected-packages
   '(fill-column-indicator rectangle-utils sr-speedbar imenu-anywhere nasm-mode bury-successful-compilation markdown-mode+ ido-vertical-mode syntax-subword idomenu ido-ubiquitous ido-select-window use-package window-numbering bm move-text smex magit multiple-cursors visual-regexp expand-region peg))
 '(pdf-view-midnight-colors '("#DCDCCC" . "#383838"))
 '(pos-tip-background-color "#1A3734")
 '(pos-tip-foreground-color "#FFFFC8")
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
 '(standard-indent 2)
 '(tool-bar-mode nil)
 '(tooltip-mode nil)
 '(uniquify-buffer-name-style 'forward nil (uniquify))
 '(vc-annotate-background nil)
 '(vc-annotate-color-map
   '((20 . "#bf616a")
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
     (360 . "#B4EB89")))
 '(vc-annotate-very-old-color nil)
 '(visible-bell nil)
 '(window-divider-default-right-width 1)
 '(window-divider-mode t)
 '(woman-locale "en_US.UTF-8"))

;;------------------------------------------------------------------------------
;; Misc functions that i've snagged or written

(defun switch-to-next-file-buffer ()
  (interactive)
  (dolist (buff (reverse (cdr (buffer-list))))
    (when (and (buffer-file-name buff)
               (not (member "TAGS"
                            (split-string (buffer-file-name buff)
                                          "/"))))
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
(global-set-key (kbd "C-;")             'next-buffer)
(global-set-key (kbd "C-:")             'switch-to-next-file-buffer)
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
(global-set-key (kbd "C-x C-r")         'recentf-open-files)
(global-set-key (kbd "<C-right>")       'tkf-forward-blank)
(global-set-key (kbd "<C-left>")        'tkf-back-blank)
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
 )
