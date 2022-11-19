;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;;;; VISUALS

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
;; (setq doom-font (font-spec :family "monospace" :size 12 :weight 'semi-light)
;;       doom-variable-pitch-font (font-spec :family "sans" :size 13))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme (if (equal (getenv "THEME") "light") 'doom-opera-light 'doom-one)
      doom-font (font-spec :family "monospace" :size 24 :weight 'semi-light)
      doom-variable-pitch-font (font-spec :family "sans" :size 24))

(defun load-theme-string (theme)
  (if (-contains? (custom-available-themes) (intern theme)) (load-theme (intern theme))))
(defun toggle-theme (&optional suffix)
  "Heuristically toggle between light and dark themes."
  (interactive)
  (let* ((theme (s-replace-all '(("light" . "dark") ("dark" . "light")
                     ("black" . "white") ("white" . "black")
                     ("day" . "night") ("night" . "day"))
                   (symbol-name doom-theme)))
        (theme-base (s-replace-regexp "-[^-]*$" "" theme)))
    (or (if suffix (or (load-theme-string (concat theme "-" suffix)) (load-theme-string (concat theme-base "-" suffix)))) (load-theme-string (if (and (not suffix) (equal theme (symbol-name doom-theme))) (concat theme "-light") theme)) (load-theme-string theme-base))
    )
  )

(setq display-line-numbers-type 'relative
      scroll-margin 6
      hscroll-margin 20
      )

;;;; BINDINGS

(defun xah/open-in-external-app (&optional @fname)
  "Open the current file or dired marked files in external app.
When called in emacs lisp, if @fname is given, open that.
URL `http://ergoemacs.org/emacs/emacs_dired_open_file_in_ext_apps.html'
Version 2019-11-04 2021-02-16"
  (interactive)
  (let* (
         ($file-list
          (if @fname
              (progn (list @fname))
            (if (string-equal major-mode "dired-mode")
                (dired-get-marked-files)
              (list (buffer-file-name)))))
         ($do-it-p (if (<= (length $file-list) 5)
                       t
                     (y-or-n-p "Open more than 5 files? "))))
    (when $do-it-p
      (cond
       ((string-equal system-type "windows-nt")
        (mapc
         (lambda ($fpath)
           (shell-command (concat "PowerShell -Command \"Invoke-Item -LiteralPath\" " "'" (shell-quote-argument (expand-file-name $fpath )) "'")))
         $file-list))
       ((string-equal system-type "darwin")
        (mapc
         (lambda ($fpath)
           (shell-command
            (concat "open " (shell-quote-argument $fpath))))  $file-list))
       ((string-equal system-type "gnu/linux")
        (mapc
         (lambda ($fpath) (let ((process-connection-type nil))
                            (start-process "" nil "xdg-open" $fpath))) $file-list))))))

(defun xf/dragon ()
  "Share file from current buffer via dragon."
  (interactive)
  (apply 'start-process
         `("dragon" nil
         "dragon-drop" "-a" "-x"
           ,@(dired-get-marked-files)
           ,@(unless (dired-get-marked-files)
             (list (or (buffer-file-name)
                 (image-dired-original-file-name)
                 default-directory))
             ))
         )
  )

(defun xf/org-journal-current ()
  (interactive)
  (org-journal-open-current-journal-file)
  (goto-char (point-max))
  )

;; rebing C-u - https://emacs.stackexchange.com/a/58320
(global-set-key (kbd "C-#") 'universal-argument)
(define-key universal-argument-map (kbd "C-#") 'universal-argument-more)
(global-set-key (kbd "C-*") 'universal-argument)
(define-key universal-argument-map (kbd "C-*") 'universal-argument-more)

; TODO map C-c C-c to save and quit if unmapped

;; https://emacs.stackexchange.com/questions/21335/prevent-folding-org-files-opened-by-ediff
(with-eval-after-load 'outline
  (add-hook 'ediff-prepare-buffer-hook 'outline-show-all))

(map! :n
      ;; Buffer-local font resizing
      "M-C-+"   'text-scale-increase
      "M-C--"   'text-scale-decrease
      ;; Frame-local font resizing
      "C-="     'doom/reset-font-size
      "C-+"     'doom/increase-font-size
      "C--"     'doom/decrease-font-size
      "C-u"     'evil-scroll-up
      :leader
      "u"       'evil-prev-buffer
      "i"       'evil-next-buffer
      "q"       'doom/save-and-kill-buffer
      "bq"      'doom/save-and-kill-buffer
      "#"       'xah/open-in-external-app
      "-"       'evil-quick-diff
      "_"       'ediff
      "wO"      'delete-other-windows
      "os"      'eshell
      "oj"      'xf/org-journal-current
      "njo"     'xf/org-journal-current
      "nrb"     (lambda () (interactive) (setq +org-roam-auto-backlinks-buffer (not +org-roam-auto-backlinks-buffer)) (org-roam-buffer-toggle))
      "Se"      '+snippets/edit
      "SN"      '+snippets/new
      "Sm"      'smerge-mode
      "m;"      'comment-line
      "Q"       'save-buffers-kill-terminal ; or evil-quit-all?
      :desc "Dragon current buffer" "d" 'xf/dragon
      :desc "Update DB & Quit"      "wU" (lambda () (interactive) (xf/org-roam-update) (save-buffers-kill-terminal))
      :map ctl-x-map
      "8 SPC" (lambda () (insert-char "200B"))
      :map text-mode-map
      :desc "Markdown to Zulip" "mam" "4dip
:%s/<\\/?span ?[^ >]*>//g
:%s/\\n\\n<a id=.*<\\/a>\\n\\n//g
:%s/<\\(http[^ \\n]+\\)>/\\1/g"
;:%s/\\n *\\n /\\n /
;:%s/    /  /g"
      )

; TODO use smerge-basic-map
(map! :map smerge-mode-map
      :leader
      "Ss"      'smerge-next
      "Sj"      'smerge-next
      "Sn"      'smerge-next
      "Sk"      'smerge-prev
      "Sp"      'smerge-prev
      "Sr"      'smerge-resolve
      "SR"      'smerge-refine
      "Su"      'smerge-keep-upper
      "Si"      'smerge-keep-current
      "So"      'smerge-keep-lower
      )

;;;; GLOBAL SETUP

(setq confirm-kill-emacs nil
      lazy-highlight-cleanup nil
      large-file-warning-threshold 40000000)

(whitespace-mode 0)
(auto-correct-mode)

;;; UTF-8 encoding - https://zhangda.wordpress.com/2016/02/15/configurations-for-beautifying-emacs-org-mode/
;; disable CJK coding/encoding (Chinese/Japanese/Korean characters)
(setq utf-translate-cjk-mode nil)

(set-language-environment 'utf-8)
(setq locale-coding-system 'utf-8)

;; set the default encoding system
(prefer-coding-system 'utf-8)
(setq default-file-name-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
;; backwards compatibility as default-buffer-file-coding-system
;; is deprecated in 23.2.
(if (boundp buffer-file-coding-system)
    (setq buffer-file-coding-system 'utf-8)
  (setq default-buffer-file-coding-system 'utf-8))

;; Treat clipboard input as UTF-8 string first; compound text next, etc.
(setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING))
(add-hook 'window-setup-hook #'xclip-mode)

;;; Data Preservation

;; Undo
(setq evil-want-fine-undo t)
(setq amalgamating-undo-limit 5)

;; Doom defaults: /home/janek/.config/emacs/core/core-editor.el::89
(setq auto-save-default t
      auto-save-interval 40)

(setq make-backup-files t
      delete-old-versions t
      version-control t
      vc-make-backup-files t
      kept-new-versions 5
      kept-old-versions 3
      )

;;;; Directory configuration

(defvar user-data-dir (if (file-exists-p "~/data/") "~/data" "/home/data") "Location of the main user data")

(load! "./user.el" nil t)
(load! "./local.el" nil t)

(setq backup-directory-alist (list (cons "." (concat doom-cache-dir "backup/")))
      custom-emacs-data-dir (expand-file-name "data" doom-private-dir))

(use-package! projectile
  :init
    (add-to-list 'projectile-ignored-projects (expand-file-name user-data-dir))
    (add-to-list 'projectile-ignored-projects user-data-dir)
    (projectile-add-known-project (expand-file-name "4-media/" user-data-dir))
    (after! org
      (projectile-add-known-project org-directory)
      (projectile-register-project-type 'org '(".orgids"))
      ;(setq projectile-project-search-path '((org-directory . 0) ((expand-file-name "1-projects" user-data-dir) . 3)))
      )
  )

(setq time-stamp-bare "%Y-%m-%d"
      time-stamp-format (concat "[" time-stamp-bare "]"))
;; Automatically add modified stamp - https://github.com/org-roam/org-roam/issues/1935#issuecomment-968047007
(use-package! time-stamp
  :init (setq time-stamp-start "modified:[       ]+\\\\?"
              time-stamp-end "$")
  :hook before-save)

(use-package! image
  :custom
  ;; Enable converting external formats (ie. webp) to internal ones.
  (image-use-external-converter t))
;(setq image-file-name-regexps "/preview/")
;(add-to-list 'image-file-name-regexps "/preview/")

;;;; ORG

(use-package! org
  :bind (:map org-mode-map
         ("C-c b" . org-cycle-list-bullet)
         ("C-c ." . org-time-stamp-inactive)
         ("C-c C-." . org-time-stamp)
         ("M-C-+" . org-timestamp-up)
         ("M-C--" . org-timestamp-down)
        )
  :init

  ;; Behavior
  (setq org-read-date-prefer-future nil
        org-extend-today-until 5
        )

  (setq org-id-method 'org
        org-id-ts-format "%Y%m%dT%H%M%S"
        )

  ;; Visuals
  ; https?[0-z.\/-]*\.(png|jpg)\?[^?]*
  (setq org-fold-core-style 'overlays)
  (setq org-image-actual-width nil)
  (setq org-ellipsis "◀")

  :config

  (defun xf/slugify (string) (downcase (s-replace-regexp "[^[:alnum:][:digit:]]\+"  "-" string)))

  ; the value does not matter, see https://emacs.stackexchange.com/questions/71774/pass-default-value-to-org-set-property/71777#71777
  ;(add-to-list 'org-global-properties-fixed '("ID_ALL" . "id"))
  (map! :map org-mode-map
        :leader
        "\\"    'org-ctrl-c-ctrl-c
        :desc "Agenda" "oA" (lambda () (interactive) (org-agenda nil "d"))
        :localleader
        "C" 'org-clock-in
        "v" 'org-insert-heading
        "jj" 'org-insert-heading
        "k" 'org-latex-export-to-pdf
        "t" 'org-todo-or-insert
        "n" 'org-add-note
        "y" 'org-yank-visible
        "Y" 'org-copy-visible
        "d=" 'org-timestamp-up-week
        "rt" 'org-change-todo-in-region
        "ra" 'org-change-tag-in-region
        "lk" 'counsel-org-link
        "gR" 'org-mode-restart
        :desc "Low Priority" "pc" (lambda () (interactive) (org-priority "C"))
        :desc "Medium Priority" "pb" (lambda () (interactive) (org-priority "B"))
        :desc "High Priority" "pa" (lambda () (interactive) (org-priority "A"))
        :desc "Set ID property" "lI" (lambda () (interactive)
              ;; Check out https://github.com/novoid/dot-emacs/blob/master/config.org#my-id-get-or-generate
              (org-set-property "ID"
                (let ((heading (org-get-heading t t t t)))
                  (if heading (org-read-property-value "ID" nil (xf/slugify heading)) (file-name-sans-extension (file-name-nondirectory buffer-file-name)))
                )))
        :desc "Set Roam Aliases" "la" (lambda () (interactive) (org-set-property "ROAM_ALIASES" nil))
        :desc "Add tag" "mt" 'org-roam-tag-add
        :desc "Remove tag" "mT" 'org-roam-tag-remove
        :desc "Extract node to file" "me" 'org-roam-extract-subtree
        )

  ;; Fix xdg-open & pdfs - https://depp.brause.cc/dotemacs/#orgd97f08c
  (setq org-file-apps '((remote . emacs)
                        ("\\.pdf\\'" . default)
                        (auto-mode . emacs)
                        (directory . emacs)
                        (system . "setsid -w xdg-open %s")
                        (t . system)))

  (setq org-priority-default 67
        org-priority-lowest 68
        org-priority-start-cycle-with-default nil)
  (setq org-priority-faces '((65 . error) (66 . "DarkGoldenRod") (67 . warning) (68 . "bisque") (69 . "grey")))

  (push "PERM(e)" (cdr (car org-todo-keywords)))

  ;; Org startup - https://orgmode.org/manual/In_002dbuffer-Settings.html
  (setq org-startup-folded 'show2levels
        org-display-remote-inline-images 'cache)

  ; TODO customize org-log-note-headings

  ;; Automated logging for todos - https://stackoverflow.com/questions/12262220/add-created-date-property-to-todos-in-org-mode/52815573#52815573
  (setq org-log-done 'time
        org-log-into-drawer t
        org-treat-insert-todo-heading-as-state-change t)

  (defun xf/org-attach-id-folder-format (id)
    "Translate any ID into a folder-path."
    (format "%s/%s"
            (substring id 0 2)
            (if (> (seq-length id) 2) (substring id 2) id))
    )
  (unless (file-exists-p org-attach-id-dir)
    (setq org-attach-id-dir (expand-file-name "attach" (xdg-user-dir "DOCUMENTS"))))
  (setq org-attach-method 'mv
        org-attach-preferred-new-method nil
        org-attach-id-to-path-function-list '(xf/org-attach-id-folder-format)
        )
  ;; https://stackoverflow.com/a/32353255/6723250
  (defun org-convert-csv-table (beg end)
    "convert csv to org-table considering '12,12'"
    (interactive (list (point) (mark)))
    (replace-regexp "\\(^\\)\\|\\(\".*?\"\\)\\|," (quote (replace-eval-replacement
                              replace-quote (cond ((equal "^" (match-string 1)) "|")
                                                     ((equal "," (match-string 0)) "|")
                                                     ((match-string 2))) ))  nil  beg end))

  (defun org-todo-or-insert (&optional arg)
    (interactive "P")
    (if (org-at-heading-p) (org-todo arg) (org-insert-todo-heading arg t)))

  (defun org-timestamp-up-week ()
    (interactive)
    (let ((current-prefix-arg '(7))) (call-interactively 'org-timestamp-up-day))
    )

  (defun org-change-todo-in-region ()
    "https://emacs.stackexchange.com/questions/38529/make-multiple-lines-todos-at-once-in-org-mode"
    (interactive)
    (let ((scope (if mark-active 'region 'tree))
          (state (org-fast-todo-selection))
          (org-enforce-todo-dependencies nil))
      (org-map-entries (lambda () (org-todo state)) nil scope)))

  (defun org-yank-visible ()
    (interactive)
    (if mark-active (call-interactively 'org-copy-visible) (org-copy-visible (point) (progn (end-of-line) (point)))))

  ;; Toggle source blocks with C-c t
  (defvar org-blocks-hidden nil)
  (defun org-toggle-blocks ()
    "Toggle all org blocks."
    (interactive)
    (if org-blocks-hidden
        (org-show-block-all)
      (org-hide-block-all))
    (setq-local org-blocks-hidden (not org-blocks-hidden)))
  (define-key org-mode-map (kbd "C-c t") 'org-toggle-blocks)

  ;; https://christiantietze.de/posts/2019/06/org-fold-heading/
  (defun ct/org-foldup ()
    "Hide the entire subtree from root headline at point."
    (interactive)
    (while (ignore-errors (outline-up-heading 1)))
    (org-flag-subtree t))

  (defun ct/org-shifttab (&optional arg)
    (interactive "P")
    (if (or (null (org-current-level))     ; point is before 1st heading, or
            (and (= 1 (org-current-level)) ; at level-1 heading, or
                 (org-at-heading-p))
            (org-at-table-p))              ; in a table (to preserve cell movement)
        ; perform org-shifttab at root level elements and inside tables
        (org-shifttab arg)
        ; try to fold up elsewhere
        (ct/org-foldup)))
  (define-key org-mode-map (kbd "S-<tab>") 'ct/org-shifttab)


  ;; https://blog.aaronbieber.com/2016/09/24/an-agenda-for-life-with-org-mode.html
  (defun air-org-skip-subtree-if-habit ()
    "Skip an agenda entry if it has a STYLE property equal to \"habit\"."
    (let ((subtree-end (save-excursion (org-end-of-subtree t))))
      (if (string= (org-entry-get nil "STYLE") "habit")
          subtree-end
        nil)))
  (defun air-org-skip-subtree-if-priority (priority)
    "Skip an agenda subtree if it has a priority of PRIORITY.

  PRIORITY may be one of the characters ?A, ?B, or ?C."
    (let ((subtree-end (save-excursion (org-end-of-subtree t)))
          (pri-value (* 1000 (- org-lowest-priority priority)))
          (pri-current (org-get-priority (thing-at-point 'line t))))
      (if (= pri-value pri-current)
          subtree-end
        nil)))
  (setq org-agenda-custom-commands
        '(("d" "Daily agenda and all TODOs"
           ((tags "PRIORITY=\"A\""
                  ((org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
                   (org-agenda-overriding-header "High-priority unfinished tasks:")))
            (agenda "" ((org-agenda-ndays 1)))
            (alltodo ""
                     ((org-agenda-skip-function '(or (air-org-skip-subtree-if-habit)
                                                     (air-org-skip-subtree-if-priority ?A)
                                                     (org-agenda-skip-if nil '(scheduled deadline))))
                      (org-agenda-overriding-header "ALL normal priority tasks:"))))
           ((org-agenda-compact-blocks t)))))

  (use-package! dash
    :defer t
    :config
    (add-variable-watcher 'org-display-custom-times (lambda (symbol val op wh) (cl-flet ((wrapper (if val (-cut concat "<" <> ">") 'identity))) (setq org-time-stamp-custom-formats `(,(wrapper "%d.%m.%Y %a") . ,(wrapper "%d.%m.%Y %a %H:%M"))))))
    (setq org-display-custom-times 't)
    )

)

(after! org-fancy-priorities
  ;; https://unicode-table.com/en/blocks/miscellaneous-symbols-and-arrows https://www.w3schools.com/colors/colors_names.asp
  ; (custom-reevaluate-setting 'org-fancy-priorities-list) (add-to-list 'org-fancy-priorities-list "☠" t)
  (setq org-fancy-priorities-list '("❗" "✯" "❖" "⬢" "■"))
  )

(use-package! org-journal
  ;; Prompt after idleness - Focused? ETC? (Pragmatic Programmer)
  :init
    (setq org-journal-file-type 'monthly
          org-journal-file-format "%Y%m.org"
          org-journal-created-property-timestamp-format time-stamp-format
          org-journal-carryover-delete-empty-journal 'always
          org-journal-date-format (concat "[" time-stamp-bare " %3a]")
          ;org-journal-time-format "%02H "
          )
  :config
    ; TODO map njj to open-or-create-entry

    ;; https://emacs.stackexchange.com/questions/17897/create-an-org-journal-template-for-daily-journal-entry/32655#32655
    (defun pc/new-buffer-p ()
      (not (file-exists-p (buffer-file-name))))
    (defun pc/insert-journal-template ()
      (when (pc/new-buffer-p)
        (save-excursion
          (goto-char (point-min))
          (insert (concat ":properties:\n:id:       " (file-name-base buffer-file-name) "\n:end:\n#+startup: overview noinlineimages\n#+options: \\n:t\n")))))
    (add-hook 'org-journal-after-entry-create-hook #'pc/insert-journal-template)

    (defvar xf/survey-mode-journal--timer nil)
    (defvar xf/survey-mode-journal--timer-interval 300)

    (define-minor-mode xf/survey-mode
      "New org-journal entry after long idleness"
      :group 'org-roam
      :global t
      (when xf/survey-mode-journal--timer (cancel-timer xf/survey-mode-journal--timer))
      (setq xf/survey-mode-journal--timer
            (when xf/survey-mode
              (run-with-idle-timer
               xf/survey-mode-journal--timer-interval :repeat
               #'xf/journal-survey))))

    (defun xf/journal-survey ()
      "Open a new journal entry"
      (interactive)
      (unless (equal major-mode 'org-journal-mode) (call-interactively 'org-journal-new-entry)))

    ;(if (file-exists-p org-journal-dir) (xf/survey-mode))
    ; TODO journal at start (call-interactively 'org-journal-new-entry)
  )

;; FIXME can I combine defer and after?
(use-package! org-roam
  ;:after org-mode
  :defer 3
  :init
    (setq company-minimum-prefix-length 1
          company-dabbrev-ignore-case 't
          completion-ignore-case 't)
  :config
    (require 'org-roam-protocol)

    ;; https://github.com/org-roam/org-roam/pull/833
    (defun xf/dashify-slug (slug)
      (s-replace "_" "-" slug))
    (advice-add 'org-roam-node-slug :filter-return #'xf/dashify-slug)

    (setq org-roam-db-update-on-save nil
          org-roam-extract-new-file-path "${slug}.org"
          +org-roam-auto-backlinks-buffer t)
    (add-hook 'org-capture-after-finalize-hook (lambda () (if (org-roam-file-p) (org-roam-db-sync))))

    (setq xf/org-roam-capture-props (concat ":properties:\n:id:       ${slug}\n:created:  %<" time-stamp-format ">\n:modified: <>\n:end:\n"))
    (setq xf/org-roam-capture-title "\n#+title: ${title}")
    (setq org-roam-capture-templates
          `(("d" "default" plain "%?" :target
             (file+head ,org-roam-extract-new-file-path ,(concat xf/org-roam-capture-props "#+filetags: :" xf/org-roam-capture-title))
             :unnarrowed t)
            )
          )
    (cl-loop for item in '("health" "own" "list" "notes" "project" "entity:person" "tech:software:list" "faith" "inspiration" "writing")
      do (add-to-list 'org-roam-capture-templates
            `(,(substring item 0 1) ,(car (split-string item ":")) plain "%?" :target
             (file+head ,(concat (car (split-string item ":")) "/" org-roam-extract-new-file-path) ,(concat xf/org-roam-capture-props "#+filetags: :" item ":" xf/org-roam-capture-title))
             :unnarrowed t)
            )
      )

    (defvar xf/auto-org-roam-db-sync--timer nil)
    (defvar xf/auto-org-roam-db-sync--timer-interval 10)
    (define-minor-mode xf/auto-org-roam-db-sync-mode
      "Toggle automatic `org-roam-db-sync' when Emacs is idle.
       Reference: `auto-save-visited-mode'"
      :group 'org-roam
      :global t
      (when xf/auto-org-roam-db-sync--timer (cancel-timer xf/auto-org-roam-db-sync--timer))
      (setq xf/auto-org-roam-db-sync--timer
            (when xf/auto-org-roam-db-sync-mode
              (run-with-idle-timer
               xf/auto-org-roam-db-sync--timer-interval :repeat
               #'org-roam-db-sync))))

    ;; TODO kill opened buffers
    (defun xf/org-roam-update ()
      "Update org-roam database and sync ids to orgids."
      (interactive)
      (org-roam-db-sync)
      (let ((org-display-remote-inline-images 'skip)) (org-roam-update-org-id-locations))
      (when (equal major-mode 'org-mode) (org-mode-restart)))

    (if (file-exists-p org-roam-directory) (xf/auto-org-roam-db-sync-mode))
  )

(use-package! ox
  :config
    (map! :map org-mode-map
          :leader
          "e" 'org-export-dispatch-without-time
          "E" 'org-export-repeat
          :desc "Save and Export" "be" (lambda () (interactive) (basic-save-buffer) (org-export-repeat))
          :localleader
          "E" 'org-export-repeat
          )

    (setq org-html-style "<link rel=\"stylesheet\" type=\"text/css\" href=\"https://gongzhitaao.org/orgcss/org.css\"/>"
          org-html-head-include-default-style nil
          org-html-htmlize-output-type 'css)

    (defun org-export-repeat ()
      (interactive)
      (let ((current-prefix-arg '(4))) (call-interactively 'org-export-dispatch))
      )

    ;; TODO name file according to subtree headline
    (defun org-export-dispatch-without-time ()
      (interactive)
      (let ((org-time-stamp-custom-formats
             '("%d.%m.%Y" . "%d.%m.%Y"))
            (org-display-custom-times 't))
        (org-set-property "EXPORT_FILE_NAME" (xf/slugify (org-get-heading t t t t)))
        (org-export-dispatch))
      )

    (setq org-latex-to-pdf-process '("xelatex -interaction -shell-escape nonstopmode %f" "xelatex -interaction nonstopmode -shell-escape %f"))
    ;; Exporting - https://orgmode.org/manual/Export-Settings.html
    (setq org-export-with-tags nil
          org-export-with-tasks 'done
          org-export-with-todo-keywords nil
          ;org-export-with-toc nil
          org-export-with-section-numbers nil
          org-export-with-broken-links 't
          org-ascii-text-width 999
          org-export-headline-levels 4
          org-export-with-sub-superscripts '{}
          org-use-sub-superscripts '{}
          )

  )

(use-package! ox-context
  :after ox)
(use-package! ox-bb
  :after ox)
(use-package! ox-extra
  :after ox
  :config (ox-extras-activate '(ignore-headlines)) ;; use tag :ignore: to export content without headline
  )
(use-package! ox-latex
  :after ox
  :config
    ;; Insert linebreak after headings tagged with "newpage" when exporting through latex - https://emacs.stackexchange.com/a/30892
    (defun org/get-headline-string-element (headline backend info)
      (let ((prop-point (next-property-change 0 headline)))
        (if prop-point (plist-get (text-properties-at prop-point headline) :parent))))
    (defun org/ensure-latex-clearpage (headline backend info)
      (when (org-export-derived-backend-p backend 'latex)
        (let ((elmnt (org/get-headline-string-element headline backend info)))
          (when (member "newpage" (org-element-property :tags elmnt))
            (concat "\\clearpage\n" headline)))))
    (add-to-list 'org-export-filter-headline-functions
                 'org/ensure-latex-clearpage)

    ;;(setq org-latex-toc-command "\\tableofcontents*\n\n")
    (setq org-latex-pdf-process '("latexmk -shell-escape -pdfxe -pdfxelatex=\"xelatex --shell-escape\" -outdir=/tmp/latexmk -f -pdf %F && mv %f /tmp/latexmk && mv /tmp/latexmk/%b.pdf %o") ; https://emacs.stackexchange.com/a/48351
          org-latex-packages-alist '(("" "fullpage") ("avoid-all" "widows-and-orphans") ("" "svg"))
          org-latex-src-block-backend 'minted
          org-latex-default-class "article4")
    (add-to-list 'org-latex-classes
         '("article4" "\\documentclass{article} \\usepackage{titlesec} \\titleformat{\\paragraph}{\\normalfont\\normalsize\\itshape}{\\theparagraph}{1em}{} \\titlespacing*{\\paragraph}{0pt}{2ex plus 1ex minus .2ex}{.5ex plus .2ex}"
            ("\\section{%s}" . "\\section*{%s}")
            ("\\subsection{%s}" . "\\subsection*{%s}")
            ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
            ("\\paragraph{%s}" . "\\paragraph*{%s}")
            ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))
    (add-to-list 'org-latex-classes
         '("shortreport" "\\documentclass[oneside]{memoir} \\chapterstyle{article}"
            ("\\chapter{%s}" . "\\chapter*{%s}")
            ("\\section{%s}" . "\\section*{%s}")
            ("\\subsection{%s}" . "\\subsection*{%s}")
            ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
            ("\\paragraph{%s}" . "\\paragraph*{%s}")
            ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))
    )

;; https://discord.com/channels/406534637242810369/406554085794381833/814175445004189706
;; Fix xdg-open after setting process-connection-type
;(setq process-connection-type nil)
;(after! org
;  (add-to-list 'org-file-apps '(system . "setsid -w xdg-open %s"))

;;;; PACKAGES

;;; Colors

; https://www.emacswiki.org/emacs/HexColour
;;(require 'cl)
;;(defun hexcolor-luminance (color)
;;  "Calculate the luminance of a color string (e.g. \"#ffaa00\", \"blue\").
;;This is 0.3 red + 0.59 green + 0.11 blue and always between 0 and 255."
;;  (let* ((values (x-color-values color))
;;         (r (car values))
;;         (g (cadr values))
;;         (b (caddr values)))
;;    (floor (+ (* .3 r) (* .59 g) (* .11 b)) 256)))
;;(defun hexcolor-add-to-font-lock ()
;;  (interactive)
;;  (font-lock-add-keywords nil
;;   `((,(concat "#[0-9a-fA-F]\\{3\\}[0-9a-fA-F]\\{3\\}?\\|"
;;               (regexp-opt (x-defined-colors) 'words))
;;      (0 (let ((color (match-string-no-properties 0)))
;;           (put-text-property
;;            (match-beginning 0) (match-end 0)
;;            'face `((:foreground ,(if (> 128.0 (hexcolor-luminance color))
;;                                       "white" "black"))
;;                    (:background ,color)))))))))

;;(defvar hexcolor-keywords
;;  '(("#[abcdef[:digit:]]\\{6\\}"
;;     (0 (put-text-property (match-beginning 0)
;;                           (match-end 0)
;;		    'face (list :background
;;			        (match-string-no-properties 0)))))))
;;(defun hexcolor-add-to-font-lock ()
;;  (font-lock-add-keywords nil hexcolor-keywords))

;;(add-hook 'web-mode-hook 'hexcolor-add-to-font-lock)

(after! eshell
  ; https://stackoverflow.com/questions/63469203/eshell-and-color-output
  (require 'xterm-color)
  (add-hook 'eshell-before-prompt-hook
            (lambda ()
              (setq xterm-color-preserve-properties t)))
  (add-to-list 'eshell-preoutput-filter-functions 'xterm-color-filter)
  (setq eshell-output-filter-functions (remove 'eshell-handle-ansi-color eshell-output-filter-functions)
        eshell-scroll-to-bottom-on-output 'others
        )
  (setenv "TERM" "xterm-256color")
  )

;;; Mappings

(map! :map special-mode-map
      "<tab>" 'other-window
      "q"     'kill-this-buffer
      :map nov-mode-map
      "<tab>" 'other-window
      "q"     'kill-this-buffer
      :n "q"  'kill-this-buffer
      :map image-mode-map
      "<tab>" 'other-window
      :n "q"  'kill-this-buffer
      :n "+"  'image-increase-size
      :n "-"  'image-decrease-size
      :map Info-mode-map
      :desc "Go Home" :n "gu" 'Info-directory
      :desc "Go Home" :n "gU" 'Info-directory
      :map thumbs-mode-map
      :n "q"  'thumbs-kill-buffer
      )

(after! ivy
  (ivy-define-key ivy-minibuffer-map (kbd "<S-return>") 'ivy-immediate-done)
  (ivy-define-key ivy-minibuffer-map (kbd "C-h") 'ivy-backward-kill-word)
  (ivy-define-key ivy-minibuffer-map (kbd "C-l") 'ivy-partial-or-done)
  )

;;; Dired

(use-package! dired
  :config
  ; Make dired open certain file types externally when pressing RET on a file https://pastebin.com/8QWYpCA2
  ; Alternative: https://www.emacswiki.org/emacs/OpenWith
  (defvar unsupported-mime-types
    '("image/x-xcf")) ; "application/zip"))
  (load "subr-x")
  (defun get-mimetype (filepath)
    (string-trim
     (shell-command-to-string (concat "file -b --mime-type \"" filepath "\""))))

  ;;(let ((mime "image/x-xcf")) (msg mime))

  (defun dired-find-file-dwim ()
    (interactive)
    (let* ((file (dired-get-filename nil t))
           (mime (get-mimetype file)))
      (if (or (string-suffix-p ".desktop" file) (string-prefix-p "audio" mime) (string-prefix-p "video" mime) (member mime unsupported-mime-types))
        (call-process "xdg-open" nil 0 nil file)
        (find-file file))))

  (map! :map dired-mode-map
        :n "RET" 'dired-find-file-dwim
        :n "l" 'dired-find-file-dwim
        :n "h" 'dired-up-directory
        :n "ö" 'evil-ex-search-forward
        :localleader
        :desc "Compress/Extract" "c" 'dired-do-compress
        :desc "Size information" "s"
                (lambda () (interactive) (dired-do-shell-command "s"))
        :desc "Lowercase files" "L"
                (lambda () (interactive) (dired-do-shell-command "lowercase"))
        :desc "Symlink to this" "l" 'dired-do-symlink
        :desc "Open image-dired" "i"
                (lambda () (interactive) (image-dired buffer-file-name))
        :desc "Open image externally" "I" 'image-dired-dired-display-external
        :desc "Org attach subtree" "a" 'org-attach-dired-to-subtree
        :map wdired-mode-map
        :n "RET" (lambda () (interactive) (wdired-exit) (dired-find-file-dwim))
        )

  )
(use-package! dired-ranger
  :disabled
  :config
    (ranger-override-dired-mode 0)
    (map! :map dired-mode-map
          :n "r" 'ranger
          :localleader
          :desc "Start ranger" "r" 'ranger
          :map ranger-mode-map
          "i"    'dired-toggle-read-only
          :n "r" 'ranger)
    )
(use-package! image-dired
  :init
    (setq image-dired-external-viewer "gimp"
          image-dired-thumb-size 400
          image-dired-show-all-from-dir-max-files 300)
  :config
  ; TODO map image-dired-delete-marked
    (add-to-list 'image-dired-cmd-create-thumbnail-options "-auto-orient")
    (add-to-list 'image-dired-cmd-create-temp-image-options "-auto-orient")
    (add-to-list 'image-dired-cmd-create-standard-thumbnail-options "-auto-orient")
  )
(use-package! diredfl
  :config (add-to-list 'diredfl-compressed-extensions ".nupkg")
  )
(after! dired-aux
  (add-to-list 'dired-compress-file-suffixes '("\\.nupkg\\'" "" "unzip -o -d %o %i"))
  (add-to-list 'dired-compress-file-suffixes '("\\.tar\\'" "" "tar xf %i"))
  )
(after! all-the-icons
  (add-to-list 'all-the-icons-extension-icon-alist '("nupkg" all-the-icons-octicon "file-zip" :v-adjust 0.0 :face all-the-icons-lmaroon))
  )

;;; evil
(add-hook 'visual-line-mode-hook (lambda () (setq line-move-visual nil)))
(use-package! evil
  :ensure t
  :init (setq evil-respect-visual-line-mode nil)
  :config
        (evil-set-register ?i "yiwjgriw") ; copy current word and replace down
        (after! evil-surround
          (setq-default evil-embrace-evil-surround-keys (-union evil-embrace-evil-surround-keys '(?` ?~ ?\~)))
          ;; TILDE https://github.com/emacs-evil/evil-surround/issues/20#issuecomment-471516289
          (defmacro define-and-bind-quoted-text-object (name key start-regex end-regex)
            (let ((inner-name (make-symbol (concat "evil-inner-" name)))
                  (outer-name (make-symbol (concat "evil-a-" name))))
              `(progn
                 (evil-define-text-object ,inner-name (count &optional beg end type)
                   (evil-select-paren ,start-regex ,end-regex beg end type count nil))
                 (evil-define-text-object ,outer-name (count &optional beg end type)
                   (evil-select-paren ,start-regex ,end-regex beg end type count t))
                 (define-key evil-inner-text-objects-map ,key #',inner-name)
                 (define-key evil-outer-text-objects-map ,key #',outer-name))))
          (define-and-bind-quoted-text-object "tilde" "~" "~" "~")
          )
  )

(use-package! evil-replace-with-register ; gr
  :ensure t
  :init
    (setq evil-replace-with-register-key (kbd "gr"))
  :config
    (evil-replace-with-register-install)
    (defun eval-paragraph ()
      (interactive)
      (er/mark-paragraph)
      (call-interactively '+eval:region)
    )
    (map! :n "gR" 'eval-paragraph
          :n "g%" 'eval-defun
          :v "gR" '+eval/region)
  )

(use-package! evil-args ; https://github.com/wcsmith/evil-args
  :config
    ;; bind evil-args text objects
    (define-key evil-inner-text-objects-map "a" 'evil-inner-arg)
    (define-key evil-outer-text-objects-map "a" 'evil-outer-arg)

    ;; bind evil-forward/backward-args
    (define-key evil-normal-state-map "L" 'evil-forward-arg)
    (define-key evil-normal-state-map "H" 'evil-backward-arg)
    (define-key evil-motion-state-map "L" 'evil-forward-arg)
    (define-key evil-motion-state-map "H" 'evil-backward-arg)

    ;; bind evil-jump-out-args
    (define-key evil-normal-state-map "K" 'evil-jump-out-args)
  )

;;; File Editing Modes

(setq initial-major-mode 'org-mode)
(add-to-list 'auto-mode-alist '("/journal/" . org-mode))
(add-to-list 'auto-mode-alist '("\\.jrnl\\'" . org-mode))

(add-to-list 'auto-mode-alist '("\\.el##" . emacs-lisp-mode))
(add-to-list 'auto-mode-alist `(,(getenv "CONFIG_SHELLS") . sh-mode))
(add-to-list 'auto-mode-alist `(,(getenv "CONFIG_ZSH") . sh-mode))
(add-to-list 'auto-mode-alist `("\\.local/bin" . sh-mode))

(add-to-list 'auto-mode-alist '("\\.twee\\'" . twee-chapbook-mode))
(add-hook 'twee-chapbook-mode-hook 'twee-mode)

;(add-to-list 'auto-mode-alist `("\\.scss.erb\\'" . scss-mode))
(add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode))
(use-package! web-mode
  :mode "\\.html\\'"
  :mode "\\.phtml\\'"
  :mode "\\.tpl\\.php\\'"
  :mode "\\.[agj]sp\\'"
  :mode "\\.as[cp]x\\'"
  :mode "\\.erb\\'"
  :mode "\\.mustache\\'"
  :mode "\\.djhtml\\'"
  :config (add-hook 'web-mode-hook 'rainbow-mode)
  )

(after! lsp-mode
  (setq lsp-disabled-clients '(typeprof-ls)))

(use-package! adoc-mode ; Asciidoc, a md alternative
  :mode "\\.adoc\\'"
  )

(use-package! plantuml-mode ; Diagrams
  :mode "\\.puml\\'"
  :config
    (set-file-template! 'plantuml-mode :mode 'plantuml-mode)
    (setq plantuml-executable-path "nostderr"
          plantuml-executable-args '("plantuml" "-headless")
          plantuml-default-exec-mode 'jar
          plantuml-jar-path "/usr/share/java/plantuml/plantuml.jar"
          org-plantuml-jar-path plantuml-jar-path
          plantuml-java-args '("-Djava.awt.headless=true" "-jar")
          plantuml-indent-level 4
          )
    (after! org
      (org-babel-do-load-languages 'org-babel-load-languages '((plantuml . t)))
      )
  )

(use-package vc-fossil
  ;; Keep from loading unnecessarily at startup.
  :defer t
  ;; This allows VC to load vc-fossil when needed.
  :init (add-to-list 'vc-handled-backends 'Fossil))

(use-package! chordpro-mode
  :mode "\\.cho"
  :config
    (set-file-template! 'chordpro-mode :mode 'chordpro-mode) ; TODO broken
    (define-key chordpro-mode-map (kbd "C-c C-c") 'chordpro-insert-chord)
  )
(use-package! lilypond-mode
  :mode ("\\.ly\\'" . LilyPond-mode)
  :config
    (set-file-template! 'LilyPond-mode :mode 'LilyPond-mode)
    (setq LilyPond-pdf-command "xdg-open")
    (add-hook 'LilyPond-mode-hook 'turn-on-font-lock)

    (defun xf/live-preview ()
      (interactive)
      (setq-local compile-command (format "lilypond %s" (shell-quote-argument buffer-file-name)))
      ; TODO this does not quite work
      (message mode-name)
      (message buffer-file-name)
      (message "%s" (length (window-list)))
      (unless (> (length (window-list)) 1)
        (split-window-horizontally)
        (find-file (concat (file-name-sans-extension buffer-file-name) ".pdf"))
        (find-file-other-window (concat (file-name-sans-extension buffer-file-name) ".ly"))
        )
      )

    (add-hook 'LilyPond-mode-hook 'xf/live-preview)

    ; TODO (require 'lyqi nil t)
  )

(setq js-indent-level 2)
(after! json-mode
  (defconst json-mode-comments-re (rx (group "//" (zero-or-more nonl) line-end)))
  (push '(json-mode-comments-re 1 font-lock-comment-face) json-font-lock-keywords-1)
  )

;;; Misc package config

(setq pdf-misc-print-programm "/usr/bin/lpr")

(add-hook 'pdf-view-mode-hook 'auto-revert-mode)
(setq auto-revert-interval 2)

(setq eww-search-prefix "https://safe.duckduckgo.com/html/?q=")

(use-package! activity-watch-mode
  :config
    (activity-watch--send-heartbeat (activity-watch--create-heartbeat (current-time))
      :on-success (lambda (&rest _) (global-activity-watch-mode))
      :on-error (lambda (&rest _) (message "")))
  )

(after! spell-fu
  (remove-hook 'text-mode-hook #'spell-fu-mode)
  )
(setq ispell-personal-dictionary (expand-file-name "personal-dictionary" custom-emacs-data-dir))

(use-package! rdictcc
  :if (locate-library "rdictcc")
  :bind (("C-c t". 'rdictcc-translate-word-at-point)
         ("C-c T". 'rdictcc-translate-word))
  :config

        ;; TODO what happened to q?
        (map! :leader "T" 'rdictcc-translate-word
              :map rdictcc-buffer-mode-map
              :n "q" 'kill-this-buffer
              "q" 'kill-this-buffer
              )

        (setq rdictcc-program-args "--directory $XDG_DATA_HOME/dictcc")
  )

(after! tramp
  (setq tramp-default-method "scpx")
  (add-to-list 'tramp-methods
   '("yadm"
     (tramp-login-program "yadm")
     (tramp-login-args (("enter")))
     (tramp-login-env (("SHELL") ("/bin/sh")))
     (tramp-remote-shell "/bin/sh")
     (tramp-remote-shell-args ("-c"))))
  (map! :leader
        :desc "Yadm status" "gT" (lambda () (interactive) (magit-status "/yadm::")))
  )

(use-package! magit
  :defer t
  :config
    (setq magit-clone-set-remote.pushDefault 't
          magit-clone-default-directory (expand-file-name "1-projects" user-data-dir)
          magit-blame--style (car magit-blame-styles))
    (setq magit-clone-name-alist
      '(("\\`\\(?:github:\\|gh:\\)?\\([^:]+\\)\\'" "github.com" "user.name")
        ("\\`\\(?:gitlab:\\|gl:\\)\\([^:]+\\)\\'" "gitlab.com" "user.name")
        ("\\`\\(?:gitea:\\|x:\\)\\([^:]+\\)\\'" "git.jfischer.org" "user.name")))
  )

(use-package! direnv ; nix-shell stuffs
  :defer t
  :config
    (setq direnv-always-show-summary nil)
    (direnv-mode)
  )

(use-package! nov
  :mode ("\\.epub\\'" . nov-mode)
  )

(use-package! recompile-on-save)

;; https://emacs.stackexchange.com/questions/64532/emms-and-mpd-configuration
(use-package! emms
  :disabled
  :config
    (require 'emms-setup)
    (require 'emms-player-mpd)
    (emms-all) ; don't change this to values you see on stackoverflow questions if you expect emms to work
    (setq emms-player-list '(emms-player-mpd))
    (add-to-list 'emms-info-functions 'emms-info-mpd)
    (add-to-list 'emms-player-list 'emms-player-mpd)
    (setq emms-source-file-default-directory (getenv "MUSIC"))

    ;; Socket is not supported
    (setq emms-player-mpd-server-name "localhost")
    (setq emms-player-mpd-server-port "6600")
    (setq emms-player-mpd-music-directory (expand-file-name "music" user-data-dir))
  )


(use-package! mu4e
  :defer 3
  :config
  (setq mu4e-change-filenames-when-moving t ; avoid sync conflicts
      mu4e-update-interval (* 10 60) ; check mail 10 minutes
      mu4e-compose-format-flowed t ; re-flow mail so it's not hard wrapped
      mu4e-get-mail-command "offlineimap -o"
      mu4e-maildir "~/.local/share/mail")
  (setq mu4e-drafts-folder "/mail/Drafts"
      mu4e-sent-folder   "/mail/Sent"
      mu4e-refile-folder "/mail/All Mail"
      mu4e-trash-folder  "/mail/Trash")
  (setq mu4e-maildir-shortcuts
      '(("/mail/inbox"     . ?i)
	("/mail/Sent"      . ?s)
	("/mail/Trash"     . ?t)
	("/mail/Drafts"    . ?d)
	("/mail/All Mail"  . ?a)))
  (setq message-send-mail-function 'smtpmail-send-it
      auth-sources '("~/.authinfo") ;need to use gpg version but only local smtp stored for now
      smtpmail-smtp-server "127.0.0.1"
      smtpmail-smtp-service 1025
      smtpmail-stream-type 'ssl))

;(with-eval-after-load "ispell"
;  (setq ispell-program-name "hunspell")
;  (setq hunspell-default-dict "en_US")
;  (setq ispell-dictionary "en_US,de_DE")
;  ;; ispell-set-spellchecker-params has to be called
;  ;; before ispell-hunspell-add-multi-dic will work
;  (ispell-set-spellchecker-params)
;  (ispell-hunspell-add-multi-dic ispell-dictionary)
;  )

;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.
