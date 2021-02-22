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
(setq   doom-theme 'doom-one
        doom-font (font-spec :family "Fira Code" :size 30 :weight 'semi-light)
        doom-variable-pitch-font (font-spec :family "sans" :size 28))

(setq display-line-numbers-type 'relative)

(map!   :leader "u" 'evil-prev-buffer
        :leader "i" 'evil-next-buffer
        :leader "bq" 'doom/save-and-kill-buffer
        :leader "mj" 'org-insert-heading
        :leader "aa" 'annotate-annotate
        :leader "as" 'annotate-mode
        )

;; Undo
(setq   evil-want-fine-undo t)
(setq   amalgamating-undo-limit 5)

;; Global config
(setq   confirm-kill-emacs nil)

(setq   initial-major-mode 'org-mode)

(whitespace-mode 0)

;; Backups & auto-saves
(setq   auto-save-default t
        auto-save-interval 40)

(setq   backup-directory-alist `(("." . ,(expand-file-name "backups/" user-emacs-directory))))
(setq   delete-old-versions t
        kept-new-versions 6
        kept-old-versions 2
        version-control t
        vc-make-backup-files t
        )

;; Data dirs

(defvar user-data-dir "~/data" "Location of the main user data")

(load! "./local.el" nil t)

; ORG

;; Fix xdg-open - https://askubuntu.com/questions/646631/emacs-doesnot-work-with-xdg-open
(setq   process-connection-type nil)

(let ((default-directory user-data-dir))
  (setq org-directory (expand-file-name "1-projects"))
  (setq org-roam-directory (concat (file-name-as-directory (getenv "XDG_DATA_HOME")) "org-roam"))
  (require 'org)
  (setq org-agenda-files (apply 'append
			      (mapcar
			       (lambda (directory)
				 (directory-files-recursively
				  directory org-agenda-file-regexp))
			       '("1-projects" "2-standards" "3-resources"))))
)

(set-file-template! 'org-mode :ignore t)
(setq default-directory org-directory)
(setq org-read-date-prefer-future nil)
(setq org-image-actual-width nil)

; Exporting - https://orgmode.org/manual/Export-Settings.html
(setq org-latex-pdf-export "latexmk -outdir=/tmp/latexmk -f -pdf %F; mv %f /tmp/latexmk; mv /tmp/latexmk/%b.pdf %o")
(setq org-latex-packages-alist '(("margin=3cm" "geometry") ("avoid-all" "widows-and-orphans")))
(setq org-export-with-sub-superscripts nil)
(setq org-export-with-tags nil)
(setq org-export-with-tasks nil)

; Org startup - https://orgmode.org/manual/In_002dbuffer-Settings.html
(setq org-startup-folded 'show2levels)
(setq org-startup-with-inline-images t)

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

;; PLANTUML

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
 '(other Babel languages
   (plantuml . t)
   )))

;; OTHERS
(add-to-list 'auto-mode-alist (cons "\\.adoc\\'" 'adoc-mode))

;; https://emacs.stackexchange.com/questions/16744/magit-help-popup-enabled-by-default
(defadvice magit-status (after my-magit-status-dispatch-popup)
  (magit-dispatch-popup))
(ad-activate 'magit-status)

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
