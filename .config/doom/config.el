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
(setq doom-theme 'doom-one
      doom-font (font-spec :family "monospace" :size 24 :weight 'semi-light)
      doom-variable-pitch-font (font-spec :family "sans" :size 24))

(setq display-line-numbers-type 'relative
      scroll-margin 6
      hscroll-margin 20
      )

;;;; BINDINGS

(defun xah-open-in-external-app (&optional @fname)
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

(defun dragon (&optional @file)
  "Share file from current buffer via dragon."
  (interactive)
  (shell-command (concat "dragon-drag-and-drop -a -x " (or @file buffer-file-name)))
  )

;; rebing C-u - https://emacs.stackexchange.com/a/58320
(global-set-key (kbd "C-#") 'universal-argument)
(define-key universal-argument-map (kbd "C-#") 'universal-argument-more)
(global-set-key (kbd "C-*") 'universal-argument)
(define-key universal-argument-map (kbd "C-*") 'universal-argument-more)

(map! ;; Buffer-local font resizing
      :n
      "M-C-+"   'text-scale-increase
      "M-C--"   'text-scale-decrease
      ;; Frame-local font resizing
      "C-="     'doom/reset-font-size
      "C-+"     'doom/increase-font-size
      "C--"     'doom/decrease-font-size
      :leader
      "u"       'evil-prev-buffer
      "i"       'evil-next-buffer
      "bq"      'doom/save-and-kill-buffer
      "d"       'dragon
      "#"       'xah-open-in-external-app
      "njo"     'org-journal-open-current-journal-file
      "Se"      '+snippets/edit
      "SN"      '+snippets/new
      "Sm"      'smerge-mode
      "m;"      'comment-line
      :desc "Update & Quit" "qu"      (lambda () (interactive) (my/org-roam-update) (save-buffers-kill-terminal))
      :map text-mode-map
      :desc "Markdown to Zulip" "mam" ":%s/\\n\\n<a id=.*<\\/a>\\n\\n//
:%s/\\n *\\n /\\n /
:%s/^## \\(.*\\)/**\\1**/
:%s/^##+ \\(.*\\)/*\\1*/
:%s/<\\(http[^ ]+\\)>/\\1/
:%s/    /  /g")

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

(setq initial-major-mode 'org-mode)
(add-to-list 'auto-mode-alist '("/journal/" . org-mode))
(add-to-list 'auto-mode-alist '("\\.twee\\'" . twee-chapbook-mode))
(add-hook 'twee-chapbook-mode-hook 'twee-mode)
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

;;; Data Preservation

;; UNDO
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

(if (and org-directory (file-exists-p org-directory))
    (setq default-directory org-directory
          org-agenda-files (append (directory-files-recursively (expand-file-name "project/") "\\`[^.]*\\'" 't) (list (expand-file-name "inbox/")))))

(use-package! projectile
  :init
    (add-to-list 'projectile-ignored-projects (expand-file-name user-data-dir))
    (add-to-list 'projectile-ignored-projects user-data-dir)
    (projectile-add-known-project (expand-file-name "music/" user-data-dir))
    (after! org
      (projectile-add-known-project org-directory)
      (projectile-register-project-type 'org '(".orgids"))
      ;(setq projectile-project-search-path '((org-directory . 0) ((expand-file-name "1-projects" user-data-dir) . 3)))
      )
  )

;; Automatically add modified stamp - https://github.com/org-roam/org-roam/issues/1935#issuecomment-968047007
(setq time-stamp-format "[%Y-%m-%d]")
(use-package! time-stamp
  :init (setq time-stamp-start "modified:[       ]+\\\\?"
              time-stamp-end "$")
  :hook before-save)

;;;; ORG

(use-package! org
  :bind (:map org-mode-map
         ("C-c b" . org-cycle-list-bullet)
         ("C-c ." . org-time-stamp-inactive)
         ("C-c C-." . org-time-stamp)
         ("M-C-+" . org-timestamp-up)
         ("M-C--" . org-timestamp-down)
        )
  :config

  (map! :map org-mode-map
        :leader
        "j" 'org-insert-heading
        "\\" 'org-ctrl-c-ctrl-c
        :localleader
        "C" 'org-clock-in
        "j" 'org-insert-heading
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
        :desc "Set ID property" "lI" '(lambda () (interactive) (org-set-property "ID" nil))
        :desc "Set Roam Aliases" "la" '(lambda () (interactive) (org-set-property "ROAM_ALIASES" nil))
        :desc "Add tag" "mt" 'org-roam-tag-add
        :desc "Remove tag" "mT" 'org-roam-tag-remove
        :desc "Extract node to file" "me" 'org-roam-extract-subtree
        )

  ; TODO customize org-log-note-headings

  ;; Behavior
  (setq org-read-date-prefer-future nil)
  (defun my/org-attach-id-folder-format (id)
    "Translate any ID into a folder-path."
    (format "%s/%s"
            (substring id 0 2)
            (if (> (seq-length id) 2) (substring id 2) id))
    )
  (setq org-attach-id-dir (expand-file-name "3-resources/attach" user-data-dir)
        org-attach-method 'mv
        org-attach-preferred-new-method nil
        org-attach-id-to-path-function-list '(my/org-attach-id-folder-format)
        )

  (setq org-id-method 'org
        org-id-ts-format "%Y%m%dT%H%M%S")

  ;; Visuals
  ; https?[0-z.\/-]*\.(png|jpg)\?[^?]*
  (setq org-image-actual-width nil)
  (setq org-ellipsis "◀")

  ;; https://unicode-table.com/en/blocks/miscellaneous-symbols-and-arrows https://www.w3schools.com/colors/colors_names.asp
  ; (custom-reevaluate-setting 'org-fancy-priorities-list) (add-to-list 'org-fancy-priorities-list "☠" t)
  (setq org-priority-default 67
        org-priority-lowest 68
        org-priority-start-cycle-with-default nil
        org-fancy-priorities-list '("❗" "✯" "❖" "⬢" "■")
        org-priority-faces '((65 . error) (66 . "DarkGoldenRod") (67 . warning) (68 . "bisque") (69 . "grey")))

  ;; Org startup - https://orgmode.org/manual/In_002dbuffer-Settings.html
  (setq org-startup-folded 'show2levels
        org-display-remote-inline-images 'cache)

  ;; Fix xdg-open & pdfs - https://depp.brause.cc/dotemacs/#orgd97f08c
  (setq org-file-apps '((remote . emacs)
                        ("\\.pdf\\'" . default)
                        (auto-mode . emacs)
                        (directory . emacs)
                        (system . "setsid -w xdg-open %s")
                        (t . system)))

  ;; Automated logging for todos - https://stackoverflow.com/questions/12262220/add-created-date-property-to-todos-in-org-mode/52815573#52815573
  (setq org-log-done 'time
        org-log-into-drawer t
        org-treat-insert-todo-heading-as-state-change t)

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

)

(use-package! org-journal
  ;; Prompt after idleness - Focused? ETC? (Pragmatic Programmer)
  :init
    (setq org-journal-file-type 'monthly
          org-journal-file-format "%Y%m%d.org"
          org-journal-created-property-timestamp-format time-stamp-format
          org-journal-carryover-delete-empty-journal 'always
          )
  :config
    (defvar my/survey-mode-journal--timer nil)
    (defvar my/survey-mode-journal--timer-interval 300)

    (define-minor-mode my/survey-mode
      "New org-journal entry after long idleness"
      :group 'org-roam
      :global t
      (when my/survey-mode-journal--timer (cancel-timer my/survey-mode-journal--timer))
      (setq my/survey-mode-journal--timer
            (when my/survey-mode
              (run-with-idle-timer
               my/survey-mode-journal--timer-interval :repeat
               #'my/journal-survey))))

    (defun my/journal-survey ()
      "Open a new journal entry"
      (interactive)
      (unless (equal major-mode 'org-journal-mode) (call-interactively 'org-journal-new-entry)))

    (if (file-exists-p org-journal-dir) (my/survey-mode))
    ; TODO journal at start (call-interactively 'org-journal-new-entry)
  )

;; FIXME can I combine defer and after?
(use-package! org-roam
  ;:after org-mode
  :defer 3
  :config
    (require 'org-roam-protocol)

    (setq org-roam-db-update-on-save nil
          org-roam-extract-new-file-path "${slug}.org"
          +org-roam-open-buffer-on-find-file nil)
    (add-hook 'org-capture-after-finalize-hook (lambda () (if (org-roam-file-p) (org-roam-db-sync))))

    (setq my/org-roam-capture-props (concat ":properties:\n:id:       ${slug}\n:created:  %<" time-stamp-format ">\n:modified: <>\n:end:\n"))
    (setq my/org-roam-capture-title "\n#+title: ${title}")
    (setq org-roam-capture-templates
          `(("d" "default" plain "%?" :target
             (file+head ,org-roam-extract-new-file-path ,(concat my/org-roam-capture-props "#+filetags: :" my/org-roam-capture-title))
             :unnarrowed t)
            )
          )
    (cl-loop for item in '("health" "own" "list" "notes" "project" "entity:person" "tech:software:list" "faith" "inspiration")
      do (add-to-list 'org-roam-capture-templates
            `(,(substring item 0 1) ,(car (split-string item ":")) plain "%?" :target
             (file+head ,(concat (car (split-string item ":")) "/" org-roam-extract-new-file-path) ,(concat my/org-roam-capture-props "#+filetags: :" item ":" my/org-roam-capture-title))
             :unnarrowed t)
            )
      )

    (defvar my/auto-org-roam-db-sync--timer nil)
    (defvar my/auto-org-roam-db-sync--timer-interval 10)
    (define-minor-mode my/auto-org-roam-db-sync-mode
      "Toggle automatic `org-roam-db-sync' when Emacs is idle.
       Reference: `auto-save-visited-mode'"
      :group 'org-roam
      :global t
      (when my/auto-org-roam-db-sync--timer (cancel-timer my/auto-org-roam-db-sync--timer))
      (setq my/auto-org-roam-db-sync--timer
            (when my/auto-org-roam-db-sync-mode
              (run-with-idle-timer
               my/auto-org-roam-db-sync--timer-interval :repeat
               #'org-roam-db-sync))))

    ;; TODO kill opened buffers
    (defun my/org-roam-update ()
      "Update org-roam database and sync ids to orgids."
      (interactive)
      (org-roam-db-sync)
      (let ((org-display-remote-inline-images 'skip)) (org-roam-update-org-id-locations))
      (when (equal major-mode 'org-mode) (org-mode-restart)))

    (if (file-exists-p org-roam-directory) (my/auto-org-roam-db-sync-mode))
  )

(use-package! ox
  :config
    (map! :map org-mode-map
          :leader
          "e" 'org-export-dispatch-custom-date
          "E" 'org-export-repeat
          :desc "Save and Export" "be" '(lambda () (interactive) (basic-save-buffer) (org-export-repeat))
          :localleader
          "e" 'org-export-dispatch-custom-date
          "E" 'org-export-repeat
          )

    (defun org-export-repeat ()
      (interactive)
      (let ((current-prefix-arg '(4))) (call-interactively 'org-export-dispatch))
      )

    ;; TODO name file according to subtree headline
    (defun org-export-dispatch-custom-date ()
      (interactive)
      (let ((org-time-stamp-custom-formats
             '("<%d.%m.%Y>" . "<%d.%m.%Y>"))
            (org-display-custom-times 't))
        (org-export-dispatch))
      )

    ;(setq org-latex-to-pdf-process '("xelatex -interaction nonstopmode %f" "xelatex -interaction nonstopmode %f"))
    ;; Exporting - https://orgmode.org/manual/Export-Settings.html
    (setq org-export-with-tags nil
          org-export-with-tasks 'done
          org-export-with-todo-keywords nil
          org-export-with-toc nil
          org-export-with-section-numbers nil
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
  :disabled
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
          org-latex-listings 'minted
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
  ;; Make dired open certain file types externally when pressing RET on a file https://pastebin.com/8QWYpCA2
  ;; Alternative: https://www.emacswiki.org/emacs/OpenWith
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
      (if (or (string-prefix-p "audio" mime) (string-prefix-p "video" mime) (member mime unsupported-mime-types))
        (call-process "xdg-open" nil 0 nil file)
        (find-file file))))

  (map! :map dired-mode-map
        :n "RET" 'dired-find-file-dwim
        :n "l" 'dired-find-file-dwim
        :n "h" 'dired-up-directory
        :n "ö" 'evil-ex-search-forward
        :leader
        :desc "Dragon marked files" "d"
                (lambda () (interactive) (dragon (s-join " " (dired-get-marked-files))))
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
  :config
    (setq image-dired-external-viewer "gimp"
          image-dired-thumb-size 300
          image-dired-show-all-from-dir-max-files 300)
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
  :config (evil-set-register ?i "yiwjgriw")
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

;;; File modes

(use-package! plantuml-mode ; Diagrams
  :defer t
  :config
    (set-file-template! 'plantuml-mode :mode 'plantuml-mode)
    (setq plantuml-executable-path "nostderr"
          plantuml-executable-args '("plantuml" "-headless")
          plantuml-default-exec-mode 'jar
          plantuml-jar-path "/usr/share/java/plantuml/plantuml.jar"
          org-plantuml-jar-path plantuml-jar-path
          plantuml-java-args '("-Djava.awt.headless=true" "-jar")
          )
    (after! org
      (org-babel-do-load-languages 'org-babel-load-languages '((plantuml . t)))
      )
  )
(use-package! adoc-mode ; Asciidoc, a md alternative
  :mode "\\.adoc\\'"
  )
(use-package! nov
  :mode ("\\.epub\\'" . nov-mode)
  )

(use-package! chordpro-mode
  :mode "\\.cho"
  :config
    ;; TODO template
    (define-key chordpro-mode-map (kbd "C-c C-c") 'chordpro-insert-chord)
  )
(use-package! lilypond-mode
  :mode ("\\.ly\\'" . LilyPond-mode)
  :config
    ;; TODO template
    (setq LilyPond-pdf-command "xdg-open")
    (add-hook 'LilyPond-mode-hook 'turn-on-font-lock)
    (add-hook 'pdf-view-mode-hook 'auto-revert-mode)
    (setq auto-revert-interval 2)
  )

(after! json-mode
  (defconst json-mode-comments-re (rx (group "//" (zero-or-more nonl) line-end)))
  (push '(json-mode-comments-re 1 font-lock-comment-face) json-font-lock-keywords-1)
  )

;;; Misc package config

(setq pdf-misc-print-programm "/usr/bin/lpr")

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
  :init
    (setq magit-clone-set-remote.pushDefault 't
          magit-clone-default-directory (expand-file-name "4-incubator/dev" user-data-dir))
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
