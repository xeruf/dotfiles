;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Janek"
      user-mail-address "27jf@pm.me")

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
      scroll-margin 7
      hscroll-margin 20
      )

(setq evil-respect-visual-line-mode nil)
(add-hook 'visual-line-mode-hook (lambda () (setq line-move-visual nil)))

;;;; BINDINGS

(defun dragon ()
  ; Share file in current buffer via dragon
  (interactive)
  (shell-command (concat "dragon-drag-and-drop -x " (buffer-file-name)))
  )

;; rebing C-u - https://emacs.stackexchange.com/a/58320
(global-set-key (kbd "C-#") 'universal-argument)
(define-key universal-argument-map (kbd "C-#") 'universal-argument-more)
(global-set-key (kbd "C-*") 'universal-argument)
(define-key universal-argument-map (kbd "C-*") 'universal-argument-more)

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
(map! :leader
      "u"  'evil-prev-buffer
      "i"  'evil-next-buffer
      "bq" 'doom/save-and-kill-buffer
      "d"  'dragon
      "#"  'xah-open-in-external-app
      ;; Buffer-local font resizing
      :n
      "M-C-="   'text-scale-increase
      "M-C--"   'text-scale-decrease
      ;; Frame-local font resizing
      "C-="     'doom/increase-font-size
      "C--"     'doom/decrease-font-size
      )

;;;; GLOBAL SETUP

(setq confirm-kill-emacs nil)

(setq initial-major-mode 'org-mode)

(whitespace-mode 0)

(setq lazy-highlight-cleanup nil)

;; Undo
(setq evil-want-fine-undo t)
(setq amalgamating-undo-limit 5)

;; Backups & auto-saves
;; Doom defaults: /home/janek/.config/emacs/core/core-editor.el::89
(setq auto-save-default t
      auto-save-interval 40)

(setq make-backup-files t
      backup-directory-alist (list (cons "." (concat doom-cache-dir "backup/")))
      delete-old-versions t
      version-control t
      vc-make-backup-files t
      kept-new-versions 5
      kept-old-versions 3
      )

;; Data dirs

(defvar user-data-dir "~/data" "Location of the main user data")

(load! "./local.el" nil t)

(setq org-directory (expand-file-name "2-standards/notes" user-data-dir)
      default-directory org-directory
      org-roam-directory (expand-file-name "roam" org-directory)
      )

(setq org-journal-file-type 'weekly
      org-journal-file-format "%Y%m%d.org"
      org-journal-created-property-timestamp-format "[%Y-%m-%d]"
      org-journal-carryover-delete-empty-journal 'always
      )

(after! recentf
  (add-to-list 'recentf-exclude "writing\\/tug")
  (add-to-list 'recentf-exclude "\\.\\(sync\\|stversions\\|stfolder\\)")
  )

(after! ivy
  (ivy-define-key ivy-minibuffer-map (kbd "<S-return>") 'ivy-immediate-done)
  )

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

;;;; ORG
(after! org
  (setq org-agenda-files
    (apply 'append
           (mapcar
            (lambda (directory) (directory-files-recursively (expand-file-name directory user-data-dir) org-agenda-file-regexp))
            '("1-projects" "2-standards")
            )))

  (defun org-todo-or-insert (&optional arg)
    (interactive "P")
    (if (org-at-heading-p) (org-todo arg) (org-insert-todo-heading arg t)))

  (defun org-timestamp-up-week ()
    (interactive)
    (let ((current-prefix-arg '(7))) (call-interactively 'org-timestamp-up-day))
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

  ;; https://emacs.stackexchange.com/questions/38529/make-multiple-lines-todos-at-once-in-org-mode
  (defun org-change-todo-in-region ()
    (interactive)
    (let ((scope (if mark-active 'region 'tree))
          (state (org-fast-todo-selection))
          (org-enforce-todo-dependencies nil))
      (org-map-entries (lambda () (org-todo state)) nil scope)))

  (map! :map org-mode-map
        :leader
        "j" 'org-insert-heading
        "e" 'org-export-dispatch-custom-date
        "E" 'org-export-repeat
        "\\" 'org-ctrl-c-ctrl-c
        :localleader
        "j" 'org-insert-heading
        "k" 'org-latex-export-to-pdf
        "t" 'org-todo-or-insert
        "e" 'org-export-dispatch-custom-date
        "E" 'org-export-repeat
        "n" 'org-add-note
        "d=" 'org-timestamp-up-week
        "rt" 'org-change-todo-in-region
        "ra" 'org-change-tag-in-region
        "lk" 'counsel-org-link
        "lI" '(lambda () (interactive) (org-set-property "ID" nil))
        "gR" 'org-mode-restart
        )

  (define-key org-mode-map (kbd "C-c .") 'org-time-stamp-inactive)
  (define-key org-mode-map (kbd "M-C-+") 'org-timestamp-up)
  (define-key org-mode-map (kbd "M-C--") 'org-timestamp-down)

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
)

(after! ox
  (setq org-latex-toc-command "\\tableofcontents*\n\n")
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

  ;; Exporting - https://orgmode.org/manual/Export-Settings.html
  (setq org-latex-pdf-process '("latexmk -shell-escape -outdir=/tmp/latexmk -f -pdf %F; mv %f /tmp/latexmk; mv /tmp/latexmk/%b.pdf %o") ; https://emacs.stackexchange.com/a/48351
        org-latex-packages-alist '(("avoid-all" "widows-and-orphans") ("" "svg"))
        org-export-with-tags nil
        org-export-with-tasks 'done
        org-export-with-todo-keywords nil
        org-export-with-toc nil
        org-export-with-section-numbers nil
        )
  (add-to-list 'org-latex-classes
       '("shortreport" "\\documentclass[oneside]{memoir} \\chapterstyle{article}"
          ("\\chapter{%s}" . "\\chapter*{%s}")
          ("\\section{%s}" . "\\section*{%s}")
          ("\\subsection{%s}" . "\\subsection*{%s}")
          ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
          ("\\paragraph{%s}" . "\\paragraph*{%s}")
          ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))


  (require 'ox-extra)
  (ox-extras-activate '(ignore-headlines))
)

;; Behavior
(set-file-template! 'org-mode :ignore t)
(setq org-read-date-prefer-future nil)

;; Visuals
; https?[0-z.\/-]*\.(png|jpg)\?[^?]*
(setq org-image-actual-width nil)
(setq org-ellipsis "↴")

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
;; https://discord.com/channels/406534637242810369/406554085794381833/814175445004189706
;(after! org
;  (add-to-list 'org-file-apps '(system . "setsid -w xdg-open %s"))

;; Automated logging for todos - https://stackoverflow.com/questions/12262220/add-created-date-property-to-todos-in-org-mode/52815573#52815573
(setq org-log-done 'time
      org-log-into-drawer t
      org-treat-insert-todo-heading-as-state-change t)

;;;; PACKAGES

(setq eww-search-prefix "https://safe.duckduckgo.com/html/?q=")

(map! :map special-mode-map
      "<tab>" 'other-window
      "q"     'kill-this-buffer
      :map image-mode-map
      "<tab>" 'other-window
      :n "+"  'image-increase-size
      :n "-"  'image-decrease-size)

(after! dired
  ;; Make dired open certain file types externally when pressing RET on a file https://pastebin.com/8QWYpCA2
  ;; Alternative: https://www.emacswiki.org/emacs/OpenWith
  (defvar unsupported-mime-types
    '("image/x-xcf"))

  (load "subr-x")

  (defun get-mimetype (filepath)
    (string-trim
     (shell-command-to-string (concat "file -b --mime-type " filepath))))

  (defun dired-find-file-dwim ()
    (interactive)
    (let* ((file (dired-get-filename nil t))
           (mime (get-mimetype file)))
      (if (or (s-prefix? "video" mime) (member mime unsupported-mime-types))
        (call-process "xdg-open" nil 0 nil file)
        (find-file file))))

  (map! :map dired-mode-map
        :n "RET" 'dired-find-file-dwim
        :localleader
        :desc "Open dir in image-dired" "i" (lambda () (interactive) (image-dired buffer-file-name))
        :desc "Compress/Extract" "c" 'dired-do-compress
        )
  (map! :map wdired-mode-map
        :n "RET" 'dired-find-file-dwim
        )

  )

(after! spell-fu
  (remove-hook 'text-mode-hook #'spell-fu-mode)
  )

(use-package! tramp
  :defer t
  :config
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

(use-package! evil-replace-with-register ; gr
  :init
    (setq evil-replace-with-register-key (kbd "gr"))
    (evil-replace-with-register-install)
  :config
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

; (use-package evil-better-visual-line
;   :ensure t
;   :config
;   (evil-better-visual-line-on))

(use-package! direnv ; nix-shell stuffs
  :defer t
  :config
    (setq direnv-always-show-summary nil)
    (direnv-mode)
  )

(use-package! plantuml-mode ; Diagrams
  :defer t
  :config
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
(use-package! lilypond-mode
  :mode ("\\.ly\\'" . LilyPond-mode)
  :config
    (setq LilyPond-pdf-command "xdg-open")
    (add-hook 'LilyPond-mode-hook 'turn-on-font-lock)
    (add-hook 'pdf-view-mode-hook 'auto-revert-mode)
    (setq auto-revert-interval 2)
  )

(setq custom-emacs-data-dir (expand-file-name "data" doom-private-dir))
(setq ispell-personal-dictionary (expand-file-name "personal-dictionary" custom-emacs-data-dir))

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
