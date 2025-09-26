;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

;; To install a package with Doom you must declare them here and run 'doom sync'
;; on the command line, then restart Emacs for the changes to take effect -- or
;; use 'M-x doom/reload'.

;; If the package you are trying to install does not contain a PACKAGENAME.el
;; file, or is located in a subdirectory of the repo, you'll need to specify
;; `:files' in the `:recipe':
;(package! this-package
;  :recipe (:host github :repo "username/repo"
;           :files ("some-file.el" "src/lisp/*.el")))

;; If you'd like to disable a package included with Doom, you can do so here
;; with the `:disable' property:
;(package! builtin-package :disable t)

;; You can override the recipe of a built in package without having to specify
;; all the properties for `:recipe'. These will inherit the rest of its recipe
;; from Doom or MELPA/ELPA/Emacsmirror:
;(package! builtin-package :recipe (:nonrecursive t))
;(package! builtin-package-2 :recipe (:repo "myfork/package"))

;; Specify a `:branch' to install a package from a particular branch or tag.
;; This is required for some packages whose default branch isn't 'master' (which
;; our package manager can't deal with; see raxod502/straight.el#279)
;(package! builtin-package :recipe (:branch "develop"))

;; Use `:pin' to specify a particular commit to install.
;(package! builtin-package :pin "1a2b3c4d5e")

;; Doom's packages are pinned to a specific commit and updated from release to
;; release. The `unpin!' macro allows you to unpin single packages...
;(unpin! pinned-package)
;; ...or multiple packages
;(unpin! pinned-package another-pinned-package)
;; ...Or *all* packages (NOT RECOMMENDED; will likely break things)
;(unpin! t)

(setq straight-host-usernames '((github . "xeruf")))

;;; Multimedia & Integration
;(package! emms)
;(package! exiftool)
;(package! elpher)
(package! activity-watch-mode :recipe (:fork t)) ; eagle eyes

(package! kill-or-bury-alive) ; https://github.com/mrkkrp/kill-or-bury-alive
(package! recompile-on-save)

; https://blog.pancho.name/posts/enabling-pinentry-in-doom-emacs/
(package! pinentry)

;(package! vc-fossil)

;;; Prettification
(package! dired+)
(package! xterm-color)
(package! rainbow-mode) ;  Colorize color names in buffers 

;;; ORG
(package! websocket)
(package! org-roam-ui
  :recipe (:host github :repo "org-roam/org-roam-ui" :files ("*.el" "out")))

(package! ox-context
  :recipe (:host github :repo "Jason-S-Ross/ox-context" :branch "develop"))
(package! ox-bb
  :recipe (:host github :repo "mmitch/ox-bb"))

;;; Editing

(package! evil-tutor) ; dark vim tutor
(package! evil-replace-with-register) ; grrrrr
(package! visual-fill-column) ; Soft line wrapping at character count

; (package! togetherly) ; Collaborative editing

;; https://elpa.gnu.org/packages/auto-correct.html
(package! auto-correct)

;;; Viewers

(package! nov) ; epub reader
; (package! picpocket) ; image viewer - https://github.com/johanclaesson/picpocket
; now using dirvish

(package! dictcc)

;;; Programming
; (package! haml-mode)

(package! consult-dash)

;; https://www.emacswiki.org/emacs/CsvMode
(package! csv-mode)

(package! pkgbuild-mode) ; AUR PKGBUILDs
(package! adoc-mode) ; Asciidoc, a md alternative used in SoCha

(package! chordpro-mode
  :recipe (:host github :repo "toemacs/chordpro-mode")) ; chord sheets

(package! osm-mode
  :recipe (:host github :repo "ruediger/osm-mode"))

;(unpin! plantuml-mode)
;(package! plantuml-mode
;  :recipe (:host github :repo "xeruf/plantuml-mode"))

(package! twee-mode
  :recipe (:host github :repo "magoyette/twee-mode"))
(package! twee-chapbot-mode
  :recipe (:host github :repo "magoyette/twee-chapbook-mode"))
