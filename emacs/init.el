;; All .el files are stored here
;;
(add-to-list 'load-path "~/.emacs.d/")

;; The whitespace feature allow you to see spaces and tabs. Use 'white-space-mode' to enable or disable.
;;
(require 'whitespace)

;; Graphically indicate the fill column when editing C code (emacs version >= 22)
;;
(unless (< emacs-major-version 22)
  (require 'fill-column-indicator)
  (add-hook 'c-mode-common-hook '(lambda () (fci-mode 1))))

;; Highlight matching parenthesis
;;
(show-paren-mode 1)

(custom-set-variables

	;; Set fill-column to 132
	;;
	'(fill-column 132)

	;; Show column-number in the mode line (column numbers are zero-based)
	;;
	'(column-number-mode t)

	;; Show line-number in the mode line (line numbers are one-based)
	;;
	'(line-number-mode t)

    ;; When we edit the file, replace TABs by spaces.
    ;; Note: you can still use C-q TAB to force a TAB to be inserted.
    ;;
	'(indent-tabs-mode nil)

	;; Tab width is 4 spaces.
    ;;
	'(tab-width 4)

    ;; C coding style is Kernighan and Ritchie
    ;;
    '(c-default-style "k&r")

    ;; C idendation is 4 spaces
    ;;
	'(c-basic-offset 4)

	;; TODO: What is this?
	;;
	'(font-lock-mode t nil (font-lock)))

(custom-set-faces

	;; Default font
	;;
	'(default ((t (:size "10pt" :family "Monospace"))) t))
