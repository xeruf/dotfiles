;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!

;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "xerus"
      user-mail-address "27jf@pm.me")

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
      doom-font (font-spec :family "Fira Code" :size 20 :weight 'semi-light)
      doom-variable-pitch-font (font-spec :family "sans" :size 20))

(setq display-line-numbers-type 'relative)

(defun dragon ()
  ; Share file in current buffer via dragon
  (interactive)
  (shell-command (concat "dragon-drag-and-drop -x " (buffer-file-name)))
  )

(map! :leader "u"  'evil-prev-buffer
      :leader "i"  'evil-next-buffer
      :leader "bq" 'doom/save-and-kill-buffer
      :leader "mj" 'org-insert-heading
      :leader "aa" 'annotate-annotate
      :leader "as" 'annotate-mode
      :leader "d"  'dragon
      )

;; Undo
(setq evil-want-fine-undo t)
(setq amalgamating-undo-limit 5)

;; Global config
(setq confirm-kill-emacs nil)

(setq initial-major-mode 'org-mode)

(whitespace-mode 0)

(setq eww-search-prefix "https://safe.duckduckgo.com/html/?q=")

;; Backups & auto-saves
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

;; ORG

(let ((default-directory user-data-dir))
  (setq org-directory (expand-file-name "1-projects"))
  (setq org-roam-directory (concat (file-name-as-directory (getenv "XDG_DATA_HOME")) "org-roam"))
  (require 'org)
  (setq org-agenda-files (apply 'append
			      (mapcar
			       (lambda (directory)
				 (directory-files-recursively
				  directory org-agenda-file-regexp))
			       '("1-projects" "2-standards" "3-resources")
                               )))
)

(set-file-template! 'org-mode :ignore t)
(setq default-directory org-directory)
(setq org-read-date-prefer-future nil)
(setq org-image-actual-width nil)

;; Exporting - https://orgmode.org/manual/Export-Settings.html
(setq org-latex-pdf-export "latexmk -outdir=/tmp/latexmk -f -pdf %F; mv %f /tmp/latexmk; mv /tmp/latexmk/%b.pdf %o")
(setq org-latex-packages-alist '(("margin=3cm" "geometry") ("avoid-all" "widows-and-orphans")))
(setq org-export-with-sub-superscripts nil)
(setq org-export-with-tags nil)
(setq org-export-with-tasks nil)

;; Org startup - https://orgmode.org/manual/In_002dbuffer-Settings.html
(setq org-startup-folded 'show2levels)
(setq org-startup-with-inline-images t)
(setq org-display-remote-inline-images 'cache)

;; Fix xdg-open & open PDF in Emacs - https://depp.brause.cc/dotemacs/#orgd97f08c
(setq org-file-apps '((remote . emacs)
                      (auto-mode . emacs)
                      (directory . emacs)
                      (system . "setsid -w xdg-open %s")
                      (t . system)))
;; https://discord.com/channels/406534637242810369/406554085794381833/814175445004189706
;(after! org
;  (add-to-list 'org-file-apps '(system . "setsid -w xdg-open %s"))

;; org toggle source blocks with C-c t
(defvar org-blocks-hidden nil)
(defun org-toggle-blocks ()
  "Toggle all source code blocks."
  (interactive)
  (if org-blocks-hidden
      (org-show-block-all)
    (org-hide-block-all))
  (setq-local org-blocks-hidden (not org-blocks-hidden)))
(define-key org-mode-map (kbd "C-c t") 'org-toggle-blocks)
(define-key org-mode-map (kbd "C-c .") 'org-time-stamp-inactive)

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

;; https://emacs.stackexchange.com/questions/16744/magit-help-popup-enabled-by-default
(defadvice magit-status (after my-magit-status-dispatch-popup)
  (call-interactively 'magit-dispatch))
(ad-activate 'magit-status)

(use-package! evil-replace-with-register ; gr
  :init
    (setq evil-replace-with-register-key (kbd "gr"))
    (evil-replace-with-register-install)
  :config
    (map! :nv "gR" #'+eval/line-or-region)
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

(use-package! direnv ; nix-shell stuffs
  :config
    (setq direnv-always-show-summary nil)
    (direnv-mode)
  )
(use-package! plantuml-mode ; Diagrams
  :config
    (setq   plantuml-executable-path "nostderr"
            plantuml-executable-args '("plantuml" "-headless")
            plantuml-default-exec-mode 'executable
            plantuml-jar-path "/usr/share/java/plantuml/plantuml.jar"
            org-plantuml-jar-path plantuml-jar-path
            plantuml-java-args '("-Djava.awt.headless=true" "-jar")
            )
    (add-to-list 'org-src-lang-modes '("plantuml" . plantuml))
    (with-eval-after-load 'org
      (org-babel-do-load-languages
        'org-babel-load-languages
        '(other Babel languages (plantuml . t))
        )
      )
  )
(use-package! adoc-mode ; Asciidoc, a md alternative
  :config
    (add-to-list 'auto-mode-alist (cons "\\.adoc\\'" 'adoc-mode))
  )
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
