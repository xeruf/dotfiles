;;; test.el -*- lexical-binding: t; -*-
(let ((hunspell-found-dicts
 (split-string
  (with-temp-buffer
    (ispell-call-process "hunspell"
			 null-device
			 t
			 nil
                               "-D"
                               ;; Use -a to prevent Hunspell from
                               ;; trying to initialize its
                               ;; curses/termcap UI, which causes it
                               ;; to crash or fail to start in some
                               ;; MS-Windows ports.
                               "-a"
                               ;; Hunspell 1.7.0 (and later?) won't
                               ;; show LOADED DICTIONARY unless
                               ;; there's at least one file argument
                               ;; on the command line.  So we feed
                               ;; it with the null device.
			 null-device)
    (buffer-string))
  "[\n\r]+"
  t))
hunspell-default-dict
hunspell-default-dict-entry
hunspell-multi-dict)
(message hunspell-found-dicts)
  )
