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
(setq doom-theme 'doom-one)

(setq doom-font (font-spec :family "Fira Code" :size 22 :weight 'semi-light)
      doom-variable-pitch-font (font-spec :family "sans" :size 25))

(setq display-line-numbers-type 'relative)

(map! :leader "u" 'evil-prev-buffer
      :leader "i" 'evil-next-buffer
      :leader "bq" 'doom/save-and-kill-buffer
      :leader "mj" 'org-insert-heading
      :leader "aa" 'annotate-annotate
      :leader "as" 'annotate-mode
      )


;; Undo
(setq evil-want-fine-undo t)

(setq undo-tree-auto-save-history t)
(setq undo-tree-history-directory-alist `(("" . (expand-file-name "backups/undo" user-emacs-directory))))

(setq amalgamating-undo-limit 5)

; (advice-add 'undo-auto--last-boundary-amalgamating-number :override #'ignore)

;; Global config
(setq confirm-kill-emacs nil)

(setq initial-major-mode 'org-mode)

(desktop-save-mode 1)

;(global-whitespace-mode -1)
;(whitespace-mode -1)

;; Backups & auto-saves
(setq auto-save-default t)
(setq auto-save-interval 40)

(setq backup-directory-alist (expand-file-name "backups" user-emacs-directory))
(setq delete-old-versions t
  kept-new-versions 6
  kept-old-versions 2
  version-control t)
(setq vc-make-backup-files t)

;; Data dirs

(defvar user-data-dir "~/data" "Location of the main user data")

(load! "./local.el" nil t)

; ORG

;; Fix xdg-open - https://askubuntu.com/questions/646631/emacs-doesnot-work-with-xdg-open
(setq process-connection-type nil)

(setq org-image-actual-width 200)

(set-file-template! 'org-mode :ignore t)

(let ((default-directory user-data-dir))
  (setq org-directory (expand-file-name "1-projects"))
  (require 'org)
  (setq org-agenda-files (apply 'append
			      (mapcar
			       (lambda (directory)
				 (directory-files-recursively
				  directory org-agenda-file-regexp))
			       '("1-projects" "2-standards" "3-resources"))))
)
(setq org-roam-directory (concat (file-name-as-directory (getenv "XDG_DATA_HOME")) "org-roam"))

(setq default-directory org-directory)

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

(add-hook 'org-mode-hook 'org-toggle-blocks)
(add-hook 'org-mode-hook 'org-toggle-inline-images)
(add-hook 'org-mode-hook (apply-partially '+org/close-all-folds 2))

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

(load! "./togetherly.el")
