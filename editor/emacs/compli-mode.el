;;; compli-mode.el --- Simple major mode for editing compli files  -*- lexical-binding: t; -*-

;;; Commentary:
;; To use this major mode manually:
;; M-x eval-buffer, then go to a compli file and M-x compli-mode

;;; Code:

(defvar compli-tab-width 4 "Width of a tab for 'compli-mode'.")

(defun compli-types ()
  "Data types in the compli language."
  '("int" "bool" "float"))

(defun compli-keywords ()
  "Keywords in the compli language."
  '("def" "rec" "let" "in" "if" "then" "else" "true" "false"))

(defun compli-operators ()
  "Builtin functions in the compli language."
  '("trace" "cast_int" "cast_float" "sqrt" "input_int" "input_float"))

(defun compli-font-lock-defaults ()
  "Font lock defaults for the compli language."
  (list
   `(,(regexp-opt (compli-keywords) 'symbols) . font-lock-keyword-face)
   `(,(regexp-opt (compli-operators) 'symbols) . font-lock-builtin-face)
   `(,(regexp-opt (compli-types) 'symbols) . font-lock-type-face)))

(define-derived-mode compli-mode prog-mode "compli"
  "Simple major mode for editing compli files."

  (setq font-lock-defaults '(compli-font-lock-defaults))

  (when compli-tab-width
    (setq tab-width compli-tab-width))

  (setq comment-start "//")
  (setq comment-end "")

  (modify-syntax-entry ?/ ". 12b" compli-mode-syntax-table)
  (modify-syntax-entry ?\n "> b" compli-mode-syntax-table))

(provide 'compli-mode)
;;; compli-mode.el ends here
