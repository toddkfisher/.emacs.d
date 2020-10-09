;;------------------------------------------------------------------------------
(setq bg                 "grey3"
      kw-wgt             'bold
      kw-slant           'italic
      fg                 "azure1" ;"grey87" ;"#ccdcdc" ;"#eeeee3"
      kw                 "SteelBlue4" ;"LightSteelBlue4"
      cursor             "yellow"
      fl-string          "MediumSeaGreen"
      string-slant       'normal
      fl-func-underline  nil
      fl-func-wgt        'bold
      fl-func-slant      'normal
      fl-func-fg         "SkyBlue3"
      fl-comment         "grey60"
      fl-var             "SkyBlue3"
      fl-var-wgt         'normal
      comment-slant      'italic
      bgr                'dark
      notice             "IndianRed3"
      paren-mismatch     notice
      rgn-bg             "DarkBlue"
      isrch-bg           notice
      isrch-fg           "white"
      lzy-hi             isrch-bg
      lzy-hi-bg          "#612429"
      grp-0              "red4"
      grp-1              "red3"
      grp-2              "red2"
      hl-ln              "gray12"
      ;;hl-ln-underline    t
      modeln-bg          "sandy brown"
      modeln-fg          "gray20"
      modeln-inactive-fg "light slate gray"
      modeln-inactive-bg "gray20"
      modeln-outline     "gray"
      org1               kw
      org2               "SteelBlue3"
      org3               "SteelBlue2"
      org4               "SteelBlue1"
      org5               org4
      org6               org4
      org7               org4
      org8               org4
      ;;modeln-
      err                "red"
      bm-fg              "white"
      bm-bg              isrch-bg
      par-match-fg       "purple"
      par-match-bg       bg
      par-match-wgt      'bold
      lnum-fg            fl-comment
      lnum-bg            modeln-inactive-bg
      auctex-preview-bg  "DodgerBlue3"
      hi-blu             "DodgerBlue2"
      hi-grn             "DarkOliveGreen1"
      hi-pnk             "LightPink1"
      hi-red             "Firebrick1"
      hi-yllw            "yellow"
      pmpt-fg            fl-string
      pmpt-slant         'normal
      pmpt-wgt           'bold
      ediff-bg           "gray20"
      hexl-ascii         fl-string
      wrn                "yellow2"
      crs-fg             fg
      crs-bg             "white"
      symlink-slant      'italic
      symlink-weight     'normal
      newline            "PowderBlue"
      long-lines-bg      "#FFDEEB"
      win-num            "DodgerBlue1")

(deftheme tkf-dark
  "tkf - dark")

(custom-theme-set-variables
 'tkf-dark
 )

(custom-theme-set-faces
 'tkf-dark
 `(default                               ((t (:inherit nil
                                                       :background ,bg
                                                       :foreground ,fg))))
 `(git-gutter:added                      ((t (:inherit default
                                                       :background ,bm-bg
                                                       :foreground ,modeln-fg))))
 `(git-gutter:deleted                    ((t (:inherit default
                                                       :background ,bm-bg
                                                       :foreground ,modeln-fg))))
 `(git-gutter:modified                   ((t (:inherit default
                                                       :background ,bm-bg
                                                       :foreground ,modeln-fg))))
 `(git-gutter:separator                  ((t (:inherit default
                                                       :background ,bm-bg
                                                       :foreground ,modeln-fg))))
 `(git-gutter:unchanged                  ((t (:inherit default
                                                       :background ,bm-bg
                                                       :foreground ,modeln-fg))))
 `(imenu-list-entry-face                 ((t (:inherit default
                                                       :background ,bg
                                                       :foreground ,fg))))
 `(imenu-list-entry-face-0               ((t (:inherit default
                                                       :background ,bg
                                                       :foreground ,fg))))
 `(imenu-list-entry-face-1               ((t (:inherit default
                                                       :background ,bg
                                                       :foreground ,fg))))
 `(imenu-list-entry-face-2               ((t (:inherit default
                                                       :background ,bg
                                                       :foreground ,fg))))
 `(imenu-list-entry-face-3               ((t (:inherit default
                                                       :background ,bg
                                                       :foreground ,fg))))
 `(imenu-list-entry-subalist-face-0      ((t (:inherit default
                                                       :background ,bg
                                                       :foreground ,org1))))
 `(imenu-list-entry-subalist-face-1      ((t (:inherit default
                                                       :background ,bg
                                                       :foreground ,fg))))
 `(imenu-list-entry-subalist-face-2      ((t (:inherit default
                                                       :background ,bg
                                                       :foreground ,fg))))
 `(imenu-list-entry-subalist-face-3      ((t (:inherit default
                                                       :background ,bg
                                                       :foreground ,fg))))
 `(error                                 ((t (:foreground ,err))))
 `(comint-highlight-input                ((t (:inherit default))))
 `(comint-highlight-prompt               ((t (:inherit default
                                                       :foreground ,pmpt-fg
                                                       :slant ,pmpt-slant
                                                       :weight ,pmpt-wgt))))
 `(cscope-file-face                      ((t (:inherit default
                                                       :weight bold))))
 `(cscope-function-face                  ((t (:inherit default
                                                       :slant italic))))
 `(cscope-line-face                      ((t (:inherit default))))
 `(cscope-line-number-face               ((t (:inherit default))))
 `(cscope-mouse-face                     ((t (:inherit default))))
 `(cua-global-mark                       ((t (:inherit region))))
 `(cua-rectangle                         ((t (:inherit region))))
 `(cua-rectangle-noselect                ((t (:inherit default))))
 `(cursor                                ((t (:inherit default
                                                       :background ,cursor
                                                       :foreground ,fg))))
 `(custom-button-pressed-unraised        ((t (:inherit custom-button-unraised
                                                       :background ,bg))))
 `(custom-changed                        ((t (:inherit custom-button-unraised))))
 `(custom-comment                        ((t (:inherit default))))
 `(custom-comment-tag                    ((t (:inherit default))))
 `(custom-face-tag                       ((t (:inherit default))))
 `(custom-group-tag                      ((t (:inherit default
                                                       :weight bold))))
 `(custom-group-tag-1                    ((t (:inherit default
                                                       :weight bold))))
 `(custom-invalid                        ((t (:inherit default
                                                       :strike-through t))))
 `(custom-modified                       ((t (:inherit default))))
 `(custom-rogue                          ((t (:inherit default))))
 `(custom-saved                          ((t (:inherit default))))
 `(custom-set                            ((t (:inherit default))))
 `(custom-state                          ((t (:inherit default))))
 `(custom-themed                         ((t (:inherit default))))
 `(custom-variable-button                ((t (:inherit link
                                                       :weight normal))))
 `(custom-variable-tag                   ((t (:inherit custom-variable-button
                                                       :weight normal))))
 `(custom-visibility                     ((t (:inherit link))))
 `(warning                               ((t (:inherit default
                                                       :background ,bg
                                                       :foreground ,wrn
                                                       :weight ,kw-wgt))))
 `(makefile-space                        ((t (:inherit default
                                                       :background ,wrn
                                                       :foreground ,wrn))))
 `(hexl-ascii-region                     ((t (:inherit default
                                                       :foreground ,hexl-ascii))))
 `(term-color-black                      ((t (:inherit default))))
 `(term-color-blue                       ((t (:inherit default))))
 `(term-color-cyan                       ((t (:inherit default))))
 `(term-color-green                      ((t (:inherit default))))
 `(term-color-magenta                    ((t (:inherit default))))
 `(term-color-red                        ((t (:inherit default))))
 `(term-color-white                      ((t (:inherit default))))
 `(term-color-yellow                     ((t (:inherit default))))
 `(dired-symlink                         ((t (:inherit
                                              link :slant italic))))
 `(dired-directory                       ((t (:inherit link))))
 `(dired-marked                          ((t (:inherit warning))))
 `(diredp-compressed-file-suffix         ((t (:inherit default))))
 `(diredp-date-time                      ((t (:inherit default))))
 `(diredp-deletion                       ((t (:inherit default))))
 `(diredp-deletion-file-name             ((t (:inherit default))))
 `(diredp-dir-heading                    ((t (:inherit default))))
 `(diredp-dir-priv                       ((t (:inherit default))))
 `(diredp-display-msg                    ((t (:inherit default))))
 `(diredp-exec-priv                      ((t (:inherit default))))
 `(diredp-executable-tag                 ((t (:inherit default))))
 `(diredp-file-name                      ((t (:inherit default))))
 `(diredp-file-suffix                    ((t (:inherit default))))
 `(diredp-flag-mark                      ((t (:inherit dired-marked))))
 `(diredp-flag-mark-line                 ((t (:inherit dired-marked))))
 `(diredp-ignored-file-name              ((t (:inherit default))))
 `(diredp-link-priv                      ((t (:inherit default))))
 `(diredp-no-priv                        ((t (:inherit default))))
 `(diredp-number                         ((t (:inherit default))))
 `(diredp-other-priv                     ((t (:inherit default))))
 `(diredp-rare-priv                      ((t (:inherit default))))
 `(diredp-read-priv                      ((t (:inherit default))))
 `(diredp-symlink                        ((t (:inherit link
                                                       :slant italic))))
 `(diredp-write-priv                     ((t (:inherit default))))
 `(dired-perm-write                      ((t (:inherit default))))
 `(escape-glyph                          ((t (:inherit default
                                                       :inverse-video t))))
 `(eshell-ls-archive                     ((t (:inherit default))))
 `(eshell-ls-backup                      ((t (:inherit default))))
 `(eshell-ls-clutter                     ((t (:inherit default))))
 `(eshell-ls-directory                   ((t (:inherit link))))
 `(eshell-ls-executable                  ((t (:inherit warning))))
 `(eshell-ls-missing                     ((t (:inherit default))))
 `(eshell-ls-product                     ((t (:inherit default))))
 `(eshell-ls-readonly                    ((t (:inherit default))))
 `(eshell-ls-special                     ((t (:inherit default))))
 `(eshell-ls-symlink                     ((t (:inherit link
                                                       :slant ,symlink-slant
                                                       :weight ,symlink-weight))))
 `(eshell-ls-unreadable                  ((t (:inherit default))))
 `(eshell-prompt                         ((t (:inherit comint-highlight-prompt))))
 `(file-name-shadow                      ((t (:inherit default))))
 `(fixed-pitch                           ((t (:inherit default))))
 `(shadow                                ((t (:inherit font-lock-comment-face))))
 `(font-lock-builtin-face                ((t (:inherit default))))
 `(font-lock-comment-delimiter-face      ((t (:inherit default
                                                       :slant ,comment-slant
                                                       :foreground ,fl-comment))))
 `(font-lock-comment-face                ((t (:inherit default
                                                       :slant ,comment-slant
                                                       :foreground ,fl-comment))))
 `(font-lock-constant-face               ((t (:inherit default))))
 `(font-lock-function-name-face          ((t (:inherit default
                                                       :foreground ,fl-func-fg
                                                       :slant ,fl-func-slant
                                                       :weight ,fl-func-wgt
                                                       :underline ,fl-func-underline))))
 `(font-lock-keyword-face                ((t (:inherit default
                                                       :foreground ,kw
                                                       :slant ,kw-slant
                                                       :weight ,kw-wgt))))
 `(font-lock-negation-char-face          ((t (:inherit default))))
 `(font-lock-regexp-grouping-backslash   ((t (:inherit default))))
 `(font-lock-regexp-grouping-construct   ((t (:inherit default))))
 `(font-lock-string-face                 ((t (:inherit default
                                                       :slant ,string-slant
                                                       :foreground ,fl-string))))
 `(font-lock-type-face                   ((t (:inherit default))))
 `(font-lock-variable-name-face          ((t (:foreground ,fl-var
                                              :weight ,fl-var-wgt))))
 `(font-lock-warning-face                ((t (:inherit error))))
 `(fringe                                ((t (:background ,bg
                                                          :foreground ,modeln-inactive-fg))))
 `(gnus-button                           ((t (:inherit default))))
 `(gnus-cite-1                           ((t (:inherit default))))
 `(gnus-cite-10                          ((t (:inherit default))))
 `(gnus-cite-11                          ((t (:inherit default))))
 `(gnus-cite-2                           ((t (:inherit default))))
 `(gnus-cite-3                           ((t (:inherit default))))
 `(gnus-cite-4                           ((t (:inherit default))))
 `(gnus-cite-5                           ((t (:inherit default))))
 `(gnus-cite-6                           ((t (:inherit default))))
 `(gnus-cite-7                           ((t (:inherit default))))
 `(gnus-cite-8                           ((t (:inherit default))))
 `(gnus-cite-9                           ((t (:inherit default))))
 `(gnus-cite-attribution                 ((t (:inherit default))))
 `(gnus-emphasis-bold                    ((t (:inherit default))))
 `(gnus-emphasis-bold-italic             ((t (:inherit default))))
 `(gnus-emphasis-highlight-words         ((t (:inherit default))))
 `(gnus-emphasis-italic                  ((t (:inherit default))))
 `(gnus-emphasis-strikethru              ((t (:inherit default))))
 `(gnus-emphasis-underline               ((t (:inherit default))))
 `(gnus-emphasis-underline-bold          ((t (:inherit default))))
 `(gnus-emphasis-underline-bold-italic   ((t (:inherit default))))
 `(gnus-emphasis-underline-italic        ((t (:inherit default))))
 `(gnus-group-mail-1                     ((t (:inherit default))))
 `(gnus-group-mail-1-empty               ((t (:inherit default))))
 `(gnus-group-mail-2                     ((t (:inherit default))))
 `(gnus-group-mail-2-empty               ((t (:inherit default))))
 `(gnus-group-mail-3                     ((t (:inherit default))))
 `(gnus-group-mail-3-empty               ((t (:inherit default))))
 `(gnus-group-mail-low                   ((t (:inherit default))))
 `(gnus-group-mail-low-empty             ((t (:inherit default))))
 `(gnus-group-news-1                     ((t (:inherit default))))
 `(gnus-group-news-1-empty               ((t (:inherit default))))
 `(gnus-group-news-2                     ((t (:inherit default))))
 `(gnus-group-news-2-empty               ((t (:inherit default))))
 `(gnus-group-news-3                     ((t (:inherit default))))
 `(gnus-group-news-3-empty               ((t (:inherit default))))
 `(gnus-group-news-4                     ((t (:inherit default))))
 `(gnus-group-news-4-empty               ((t (:inherit default))))
 `(gnus-group-news-5                     ((t (:inherit default))))
 `(gnus-group-news-5-empty               ((t (:inherit default))))
 `(gnus-group-news-6                     ((t (:inherit default))))
 `(gnus-group-news-6-empty               ((t (:inherit default))))
 `(gnus-group-news-low                   ((t (:inherit default))))
 `(gnus-group-news-low-empty             ((t (:inherit default))))
 `(gnus-header-content                   ((t (:inherit default))))
 `(gnus-header-from                      ((t (:inherit default))))
 `(gnus-header-name                      ((t (:inherit default))))
 `(gnus-header-newsgroups                ((t (:inherit default))))
 `(gnus-header-subject                   ((t (:inherit default))))
 `(gnus-server-agent                     ((t (:inherit default))))
 `(gnus-server-closed                    ((t (:inherit default))))
 `(gnus-server-denied                    ((t (:inherit default))))
 `(gnus-server-offline                   ((t (:inherit default))))
 `(gnus-server-opened                    ((t (:inherit default))))
 `(gnus-signature                        ((t (:inherit default))))
 `(gnus-splash                           ((t (:inherit default))))
 `(gnus-summary-cancelled                ((t (:inherit default))))
 `(gnus-summary-high-ancient             ((t (:inherit default))))
 `(gnus-summary-high-read                ((t (:inherit default))))
 `(gnus-summary-high-ticked              ((t (:inherit default))))
 `(gnus-summary-high-undownloaded        ((t (:inherit default))))
 `(gnus-summary-high-unread              ((t (:inherit default))))
 `(gnus-summary-low-ancient              ((t (:inherit default))))
 `(gnus-summary-low-read                 ((t (:inherit default))))
 `(gnus-summary-low-ticked               ((t (:inherit default))))
 `(gnus-summary-low-undownloaded         ((t (:inherit default))))
 `(gnus-summary-low-unread               ((t (:inherit default))))
 `(gnus-summary-normal-ancient           ((t (:inherit default))))
 `(gnus-summary-normal-read              ((t (:inherit default))))
 `(gnus-summary-normal-ticked            ((t (:inherit default))))
 `(gnus-summary-normal-undownloaded      ((t (:inherit default))))
 `(gnus-summary-normal-unread            ((t (:inherit default))))
 `(gnus-summary-selected                 ((t (:inherit default))))
 `(header-line                           ((t (:inherit default :underline nil))))
 `(help-argument-name                    ((t (:inherit default
                                                       :slant italic))))
 `(highlight                             ((t (:inherit default
                                                       :inverse-video t))))
 `(highlight-changes                     ((t (:inherit default
                                                       :weight bold))))
 `(highlight-changes-delete              ((t (:inherit default
                                                       :strike-through t))))
 `(hl-line                               ((t (:background ,hl-ln))))
 `(ido-vertical-first-match-face         ((t (:inherit font-lock-variable-name-face
                                                       :underline t))))
 `(ido-vertical-match-face               ((t (:inherit font-lock-variable-name-face
                                                       :underline nil))))
 `(ido-vertical-only-match-face          ((t (:inherit font-lock-variable-name-face
                                                       :underline t))))
 `(ido-indicator                         ((t (:inherit default))))
 `(ido-only-match                        ((t (:inherit default))))
 `(ido-subdir                            ((t (:inherit default
                                                       :underline t))))
 `(isearch                               ((t (:background ,isrch-bg
                                                          :foreground ,isrch-fg))))
 `(isearch-fail                          ((t (:inherit default))))
 `(lazy-highlight                        ((t (:underline nil
                                                         :background ,lzy-hi-bg
                                                         :overline nil
                                                         :foreground ,lzy-hi))))
 `(link                                  ((t (:inherit default
                                                       :underline t))))
 `(link-visited                          ((t (:inherit link))))
 `(linum                                 ((t (:inherit default
                                                       :foreground ,lnum-fg
                                                       :background ,lnum-bg))))
 `(match                                 ((t (:inherit default
                                                       :background ,lzy-hi))))
 `(message-cited-text                    ((t (:inherit default))))
 `(message-header-cc                     ((t (:inherit default))))
 `(message-header-name                   ((t (:inherit default))))
 `(message-header-newsgroups             ((t (:inherit default))))
 `(message-header-other                  ((t (:inherit default))))
 `(message-header-subject                ((t (:inherit default))))
 `(message-header-to                     ((t (:inherit default))))
 `(message-header-xheader                ((t (:inherit default))))
 `(message-mml                           ((t (:inherit default))))
 `(message-separator                     ((t (:inherit default))))
 `(minibuffer-prompt                     ((t (:inherit default))))
 `(mm-uu-extract                         ((t (:inherit default))))
 `(mode-line                             ((t (:background ,modeln-bg
                                                          :foreground ,modeln-fg
                                                          :box nil))))
 `(mode-line-highlight                   ((t (:weight bold))))
 `(mode-line-inactive                    ((t (:background ,modeln-inactive-bg
                                                          :foreground ,modeln-inactive-fg))))
 `(next-error                            ((t (:inherit error))))
 `(org-agenda-column-dateline            ((t (:inherit default))))
 `(org-block                             ((t (:inherit default))))
 `(org-block-background                  ((t (:inherit org-level-1))))
 `(org-agenda-date                       ((t (:inherit default))))
 `(org-agenda-date-weekend               ((t (:inherit default))))
 `(org-agenda-dimmed-todo-face           ((t (:inherit default))))
 `(org-agenda-restriction-lock           ((t (:inherit default))))
 `(org-agenda-structure                  ((t (:inherit default))))
 `(org-archived                          ((t (:inherit default))))
 `(org-clock-overlay                     ((t (:inherit default))))
 `(org-code                              ((t (:inherit default))))
 `(org-column                            ((t (:inherit default))))
 `(org-column-title                      ((t (:inherit default))))
 `(org-date                              ((t (:inherit default))))
 `(org-done                              ((t (:inherit default))))
 `(org-drawer                            ((t (:inherit default))))
 `(org-ellipsis                          ((t (:inherit default))))
 `(org-footnote                          ((t (:inherit default))))
 `(org-formula                           ((t (:inherit default))))
 `(org-headline-done                     ((t (:inherit default))))
 `(org-hide                              ((t (:inherit default))))
 `(org-latex-and-export-specials         ((t (:inherit default))))
 `(org-level-1                           ((t (:inherit default
                                                       :foreground ,org1))))
 `(org-level-2                           ((t (:inherit default
                                                       :foreground ,org2))))
 `(org-level-3                           ((t (:inherit default
                                                       :foreground ,org3))))
 `(org-level-4                           ((t (:inherit default
                                                       :foreground ,org4))))
 `(org-level-5                           ((t (:inherit default
                                                       :foreground ,org5))))
 `(org-level-6                           ((t (:inherit default
                                                       :foreground ,org6))))
 `(org-level-7                           ((t (:inherit default
                                                       :foreground ,org7))))
 `(org-level-8                           ((t (:inherit default
                                                       :foreground ,org8))))
 `(org-link                              ((t (:inherit link))))
 `(org-property-value                    ((t (:inherit default))))
 `(org-scheduled                         ((t (:inherit default))))
 `(org-scheduled-previously              ((t (:inherit default))))
 `(org-scheduled-today                   ((t (:inherit default))))
 `(org-sexp-date                         ((t (:inherit default))))
 `(org-special-keyword                   ((t (:inherit default))))
 `(org-table                             ((t (:inherit default))))
 `(org-tag                               ((t (:inherit default))))
 `(org-target                            ((t (:inherit default))))
 `(org-time-grid                         ((t (:inherit default))))
 `(org-todo                              ((t (:inherit default))))
 `(org-upcoming-deadline                 ((t (:inherit default))))
 `(org-verbatim                          ((t (:inherit default))))
 `(org-warning                           ((t (:inherit default))))
 `(outline-1                             ((t (:inherit default))))
 `(outline-2                             ((t (:inherit default))))
 `(outline-3                             ((t (:inherit default))))
 `(outline-4                             ((t (:inherit default))))
 `(outline-5                             ((t (:inherit default))))
 `(outline-6                             ((t (:inherit default))))
 `(outline-7                             ((t (:inherit default))))
 `(outline-8                             ((t (:inherit default))))
 `(region                                ((t (:background ,rgn-bg))))
 `(secondary-selection                   ((t (:inherit region))))
 `(show-paren-match                      ((t (:weight ,par-match-wgt
                                                      :foreground ,par-match-fg
                                                      :background ,par-match-bg))))
 `(show-paren-mismatch                   ((t (:foreground ,paren-mismatch))))
;; `(speedbar-button-face                  ((t (:inherit default
;;                                                       :height ,sb-height
;;                                                       :family ,sb-face))))
;; `(speedbar-directory-face               ((t (:inherit default
;;                                                       :height ,sb-height
;;                                                       :family ,sb-face))))
;; `(speedbar-file-face                    ((t (:inherit default
;;                                                       :height ,sb-height
;;                                                       :family ,sb-face))))
;; `(speedbar-selected-face                ((t (:inherit default
;;                                                       :height ,sb-height
;;                                                       :family ,sb-face))))
;; `(speedbar-separator-face               ((t (:inherit default
;;                                                       :height ,sb-height
;;                                                       :family ,sb-face))))
;; `(speedbar-tag-face                     ((t (:inherit default
;;                                                       :height ,sb-height
;;                                                       :family ,sb-face))))
;; `(speedbar-highlight-face               ((t (:inherit lazy-highlight))))
 `(tex-math                              ((t (:inherit default))))
 `(tex-verbatim                          ((t (:inherit default))))
 ;`(trailing-whitespace                   ((t (:inherit default :stipple nil :background ,bg :foreground ,fg :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :width normal ))))
 `(wg-brace-face                         ((t (:inherit))))
 `(wg-command-face                       ((t (:inherit))))
 `(wg-current-workgroup-face             ((t (:inherit))))
 `(wg-divider-face                       ((t (:inherit))))
 `(wg-filename-face                      ((t (:inherit))))
 `(wg-frapme-face                        ((t (:inherit default))))
 `(wg-message-face                       ((t (:inherit default))))
 `(wg-mode-line-face                     ((t (:inherit default))))
 `(wg-other-workgroup-face               ((t (:inherit default))))
 `(wg-previous-workgroup-face            ((t (:inherit default))))
 `(widget-button-pressed                 ((t (:inherit widget-button))))
 `(widget-documentation                  ((t (:inherit default))))
 `(bm-face                               ((t (:inherit default))))
 `(bm-fringe-face                        ((t (:foreground ,bm-fg
                                                          :background ,bm-bg))))
 `(bm-fringe-persistent-face             ((t (:foreground ,bm-fg
                                                          :background ,bm-bg))))
 `(bm-persistent-face                    ((t (:inherit default))))
 `(mm/master-face                        ((t (:inherit region))))
 `(mm/mirror-face                        ((t (:inherit region))))
 `(font-latex-bold-face                  ((t (:inherit bold))))
 `(font-latex-doctex-documentation-face  ((t (:inherit font-lock-string-face))))
 `(font-latex-doctex-preprocessor-face   ((t (:inherit font-lock-keyword-face))))
 `(font-latex-italic-face                ((t (:inherit italic))))
 `(font-latex-math-face                  ((t (:inherit italic))))
 `(font-latex-sectioning-0-face          ((t (:inherit bold))))
 `(font-latex-sectioning-1-face          ((t (:inherit bold))))
 `(font-latex-sectioning-2-face          ((t (:inherit bold))))
 `(font-latex-sectioning-3-face          ((t (:inherit bold))))
 `(font-latex-sectioning-4-face          ((t (:inherit bold))))
 `(font-latex-sectioning-5-face          ((t (:inherit bold))))
 `(font-latex-sedate-face                ((t (:inherit font-lock-comment-face))))
 `(font-latex-slide-title-face           ((t (:inherit normal))))
 `(font-latex-string-face                ((t (:inherit font-lock-string-face))))
 `(font-latex-subscript-face             ((t (:inherit normal))))
 `(font-latex-superscript-face           ((t (:inherit normal))))
 `(font-latex-verbatim-face              ((t (:inherit font-lock-string-face))))
 `(font-latex-warning-face               ((t (:inherit font-lock-warning-face))))
 `(preview-face                          ((t (:inherit default
                                                       :background ,auctex-preview-bg))))
 `(compilation                           ((t (:inherit nil))))
 `(compilation-enter-directory           ((t (:inherit nil))))
 `(compilation-error                     ((t (:inherit nil))))
 `(compilation-info                      ((t (:inherit nil))))
 `(compilation-info                      ((t (:inherit nil))))
 `(compilation-leave-directory           ((t (:inherit nil))))
 `(compilation-line                      ((t (:inherit nil))))
 `(compilation-line-number               ((t (:inherit nil))))
 `(compilation-message                   ((t (:inherit nil))))
 `(compilation-mode-line-exit            ((t (:inherit nil))))
 `(compilation-mode-line-fail            ((t (:inherit nil))))
 `(compilation-mode-line-run             ((t (:inherit nil))))
 `(compilation-warning                   ((t (:inherit nil))))
 `(compilation-column                    ((t (:inherit nil))))
 `(hi-blue-b                             ((t (:inherit default
                                                       :background ,hi-blu
                                                       :weight bold))))
 `(hi-green                              ((t (:inherit default
                                                       :background ,hi-grn))))
 `(hi-green-b                            ((t (:inherit default
                                                       :background ,hi-grn
                                                       :weight bold))))
 `(hi-pink                               ((t (:inherit default
                                                       :background ,hi-pnk))))
 `(hi-red-b                              ((t (:inherit default
                                                       :background ,hi-red
                                                       :weight bold))))
 `(hi-yellow                             ((t (:inherit default
                                                       :background ,hi-yllw))))
 `(ediff-current-diff-A                  ((t (:inherit default
                                                       :background ,ediff-bg))))
 `(ediff-current-diff-Ancestor           ((t (:inherit default
                                                       :background ,ediff-bg))))
 `(ediff-current-diff-B                  ((t (:inherit default
                                                       :background ,ediff-bg))))
 `(ediff-current-diff-C                  ((t (:inherit default
                                                       :background ,ediff-bg))))
 `(ediff-even-diff-A                     ((t (:inherit default
                                                       :background ,ediff-bg))))
 `(ediff-even-diff-Ancestor              ((t (:inherit default
                                                       :background ,ediff-bg))))
 `(ediff-even-diff-B                     ((t (:inherit default
                                                       :background ,ediff-bg))))
 `(ediff-even-diff-C                     ((t (:inherit default
                                                       :background ,ediff-bg))))
 `(ediff-fine-diff-A                     ((t (:inherit default
                                                       :background ,ediff-bg))))
 `(ediff-fine-diff-Ancestor              ((t (:inherit default
                                                       :background ,ediff-bg))))
 `(ediff-fine-diff-B                     ((t (:inherit default
                                                       :background ,ediff-bg))))
 `(ediff-fine-diff-C                     ((t (:inherit default
                                                       :background ,ediff-bg))))
 `(ediff-odd-diff-A                      ((t (:inherit default
                                                       :background ,ediff-bg))))
 `(ediff-odd-diff-Ancestor               ((t (:inherit default
                                                       :background ,ediff-bg))))
 `(ediff-odd-diff-B                      ((t (:inherit default
                                                       :background ,ediff-bg))))
 `(ediff-odd-diff-C                      ((t (:inherit default
                                                       :background ,ediff-bg))))
 `(diff-index                            ((t (:inherit default
                                                       :background ,ediff-bg))))
 `(diff-marked                           ((t (:inherit default
                                                       :background ,ediff-bg))))
 `(diff-mark                             ((t (:inherit default
                                                       :background ,ediff-bg))))
 `(diff-function                         ((t (:inherit default
                                                       :weight bold))))
 `(diff-header                           ((t (:inherit default
                                                       :weight bold))))
 `(diff-hunk-header                      ((t (:inherit default
                                                       :weight bold))))
 `(diff-index                            ((t (:inherit default
                                                       :weight bold))))
 `(diff-file-header                      ((t (:inherit default
                                                       :weight bold))))
 ;; MARKER : left off here
 `(diff-indicator-added                  ((t (:inherit default :foreground ,ediff-bg))))
 `(diff-indicator-changed                ((t (:inherit default :background ,ediff-bg))))
 `(diff-indicator-removed                ((t (:inherit default :background ,ediff-bg))))
 `(diff-nonexistent                      ((t (:inherit default :background ,ediff-bg))))
 `(diff-refine-added                     ((t (:inherit default :background ,ediff-bg))))
 `(diff-refine-change                    ((t (:inherit default :background ,ediff-bg))))
 `(diff-refine-removed                   ((t (:inherit default :background ,ediff-bg))))
 `(diff-removed                          ((t (:inherit default :background ,ediff-bg))))
 `(diff-added                            ((t (:inherit default :background ,ediff-bg))))
 `(vr/group-0                            ((t (:inherit default :background ,grp-0))))
 `(vr/group-1                            ((t (:inherit default :background ,grp-1))))
 `(vr/group-2                            ((t (:inherit default :background ,grp-2))))
 `(vr/match-0                            ((t (:inherit vr/group-0))))
 `(vr/match-1                            ((t (:inherit default :background ,lzy-hi))))
 `(sh-heredoc                            ((t (:inherit default :foreground ,fl-string))))
 `(sh-quoted-exec                        ((t (:inherit default :foreground ,fl-string))))
 `(kill-ring-ido-number-face             ((t (:inherit default :foreground ,err))))
 `(window-number-face                    ((t (:foreground ,modeln-fg :weight bold))))
 `(whitespace-line                       ((t (:background ,long-lines-bg))))
 `(whitespace-empty                      ((t (:inherit default))))
 `(whitespace-hspace                     ((t (:inherit default))))
 `(whitespace-indentation                ((t (:inherit default))))
 `(whitespace-newline                    ((t (:inherit default :foreground ,newline))))
 `(whitespace-space                      ((t (:inherit default))))
 `(whitespace-space-after-tab            ((t (:inherit default))))
 `(whitespace-space-before-tab           ((t (:inherit default))))
 `(whitespace-tab                        ((t (:inherit default))))
 `(whitespace-trailing                   ((t (:inherit default))))
 `(trailing-whitespace                   ((t (:inherit default))))
 `(cursor                                ((t (:inherit default :foreground ,crs-fg :background ,crs-bg))))
 `(ace-jump-face-foreground              ((t (:inherit default :foreground ,err))))
 `(ace-jump-face-background              ((t (:inherit default))))
 `(magit-bisect-bad                      ((t (:inherit default ))))
 `(magit-bisect-good                     ((t (:inherit default ))))
 `(magit-bisect-skip                     ((t (:inherit default ))))
 `(magit-blame-date                      ((t (:inherit default ))))
 `(magit-blame-hash                      ((t (:inherit default ))))
 `(magit-blame-heading                   ((t (:inherit default ))))
 `(magit-blame-name                      ((t (:inherit default ))))
 `(magit-blame-summary                   ((t (:inherit default ))))
 `(magit-branch-current                  ((t (:inherit default ))))
 `(magit-branch-local                    ((t (:inherit default ))))
 `(magit-branch-remote                   ((t (:inherit default ))))
 `(magit-cherry-equivalent               ((t (:inherit default ))))
 `(magit-cherry-unmatched                ((t (:inherit default ))))
 `(magit-diff-added                      ((t (:inherit default ))))
 `(magit-diff-added-highlight            ((t (:inherit default :background ,rgn-bg))))
 `(magit-diff-base                       ((t (:inherit default ))))
 `(magit-diff-base-highlight             ((t (:inherit default :background ,rgn-bg))))
 `(magit-diff-conflict-heading           ((t (:inherit default :weight bold))))
 `(magit-diff-context                    ((t (:inherit default ))))
 `(magit-diff-context-highlight          ((t (:inherit default :background ,rgn-bg))))
 `(magit-diff-file-heading               ((t (:inherit default :weight bold))))
 `(magit-diff-file-heading-highlight     ((t (:inherit default :background ,rgn-bg))))
 `(magit-diff-file-heading-selection     ((t (:inherit default :background ,rgn-bg))))
 `(magit-diff-hunk-heading               ((t (:inherit default :weight bold))))
 `(magit-diff-hunk-heading-highlight     ((t (:inherit default :background ,rgn-bg))))
 `(magit-diff-hunk-heading-selection     ((t (:inherit default :background ,rgn-bg))))
 `(magit-diff-hunk-region                ((t (:inherit default ))))
 `(magit-diff-lines-boundary             ((t (:inherit default ))))
 `(magit-diff-lines-heading              ((t (:inherit default :weight bold))))
 `(magit-diff-our                        ((t (:inherit default ))))
 `(magit-diff-our-highlight              ((t (:inherit default :background ,rgn-bg))))
 `(magit-diff-removed                    ((t (:inherit default ))))
 `(magit-diff-removed-highlight          ((t (:inherit default :background ,rgn-bg))))
 `(magit-diff-their                      ((t (:inherit default ))))
 `(magit-diff-their-highlight            ((t (:inherit default :background ,rgn-bg))))
 `(magit-diff-whitespace-warning         ((t (:inherit default ))))
 `(magit-diffstat-added                  ((t (:inherit default ))))
 `(magit-diffstat-removed                ((t (:inherit default ))))
 `(magit-dimmed                          ((t (:inherit default ))))
 `(magit-filename                        ((t (:inherit default ))))
 `(magit-hash                            ((t (:inherit default ))))
 `(magit-head                            ((t (:inherit default ))))
 `(magit-header-line                     ((t (:inherit default ))))
 `(magit-log-author                      ((t (:inherit default ))))
 `(magit-log-date                        ((t (:inherit default ))))
 `(magit-log-graph                       ((t (:inherit default ))))
 `(magit-popup-argument                  ((t (:inherit default ))))
 `(magit-popup-disabled-argument         ((t (:inherit default ))))
 `(magit-popup-heading                   ((t (:inherit default :weight bold))))
 `(magit-popup-key                       ((t (:inherit default ))))
 `(magit-popup-option-value              ((t (:inherit default ))))
 `(magit-process-ng                      ((t (:inherit default ))))
 `(magit-process-ok                      ((t (:inherit default ))))
 `(magit-reflog-amend                    ((t (:inherit default ))))
 `(magit-reflog-checkout                 ((t (:inherit default ))))
 `(magit-reflog-cherry-pick              ((t (:inherit default ))))
 `(magit-reflog-commit                   ((t (:inherit default ))))
 `(magit-reflog-merge                    ((t (:inherit default ))))
 `(magit-reflog-other                    ((t (:inherit default ))))
 `(magit-reflog-rebase                   ((t (:inherit default ))))
 `(magit-reflog-remote                   ((t (:inherit default ))))
 `(magit-reflog-reset                    ((t (:inherit default ))))
 `(magit-refname                         ((t (:inherit default ))))
 `(magit-refname-stash                   ((t (:inherit default ))))
 `(magit-refname-wip                     ((t (:inherit default ))))
 `(magit-section-heading                 ((t (:inherit default :weight bold))))
 `(magit-section-heading-selection       ((t (:inherit default :background ,rgn-bg))))
 `(magit-section-highlight               ((t (:inherit default :background ,rgn-bg))))
 `(magit-section-secondary-heading       ((t (:inherit default :weight bold))))
 `(magit-sequence-done                   ((t (:inherit default ))))
 `(magit-sequence-drop                   ((t (:inherit default ))))
 `(magit-sequence-head                   ((t (:inherit default ))))
 `(magit-sequence-onto                   ((t (:inherit default ))))
 `(magit-sequence-part                   ((t (:inherit default ))))
 `(magit-sequence-pick                   ((t (:inherit default ))))
 `(magit-sequence-stop                   ((t (:inherit default ))))
 `(magit-signature-bad                   ((t (:inherit default ))))
 `(magit-signature-error                 ((t (:inherit error ))))
 `(magit-signature-expired               ((t (:inherit default ))))
 `(magit-signature-expired-key           ((t (:inherit default ))))
 `(magit-signature-good                  ((t (:inherit default ))))
 `(magit-signature-revoked               ((t (:inherit default ))))
 `(magit-signature-untrusted             ((t (:inherit default ))))
 `(magit-tag                             ((t (:inherit default ))))
 `(slime-error-face                      ((t (:inherit default))))
 `(slime-highlight-face                  ((t (:inherit default :inverse t))))
 `(slime-inspector-action-face           ((t (:inherit default))))
 `(slime-inspector-label-face            ((t (:inherit default))))
 `(slime-inspector-topline-face          ((t (:inherit default))))
 `(slime-inspector-type-face             ((t (:inherit default))))
 `(slime-inspector-value-face            ((t (:inherit default))))
 `(slime-note-face                       ((t (:inherit default))))
 `(slime-reader-conditional-face         ((t (:inherit default))))
 `(slime-repl-input-face                 ((t (:inherit default))))
 `(slime-repl-inputed-output-face        ((t (:inherit default :weight bold))))
 `(slime-repl-output-face                ((t (:inherit default :weight bold))))
 `(slime-repl-output-mouseover-face      ((t (:inherit default))))
 `(slime-repl-prompt-face                ((t (:inherit comint-highlight-prompt))))
 `(slime-repl-result-face                ((t (:inherit default))))
 `(slime-style-warning-face              ((t (:inherit default))))
 `(slime-warning-face                    ((t (:inherit default))))
 `(flyspell-duplicate                    ((t (:inherit error :underline t))))
 `(flyspell-incorrect                    ((t (:inherit error :weight bold))))
 `(ac-completion-face                    ((t (:inherit default :underline t :foreground ,fl-string))))
 `(mc/cursor-face                        ((t (:interit cursor :foreground ,cursor))))
 `(mc/cursor-bar-face                    ((t (:interit cursor :foreground ,cursor))))
 `(racket-keyword-argument-face          ((t (:interit font-lock-keyword-face))))
 ;;`(racket-paren-face                     ((t (:interit font-lock-keyword-face))))
 ;;`(racket-selfeval-face                  ((t (:interit font-lock-keyword-face))))
 '(geiser-font-lock-doc-link ((t (:inherit link))))
 '(geiser-font-lock-xref-link ((t (:inherit link))))
)

(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'tkf-dark)
