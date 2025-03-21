(defvar compli-mode-syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?/ "< 12" table)
    (modify-syntax-entry ?\n ">" table)
    table))

(defvar compli-tab-width 4 "Width of a tab for compli-mode")

(defun compli-types ()
  '("int" "bool" "float"))

(defun compli-keywords ()
  '("def" "rec" "let" "in" "if" "then" "else" "true" "false"))

(defun compli-operators ()
  '("trace" "cast_int" "cast_float" "sqrt" "input_int" "input_float"))

(defun compli-font-lock-defaults ()
  (list
   `(,(regexp-opt (compli-keywords) 'symbols) . font-lock-keyword-face)
   `(,(regexp-opt (compli-operators) 'symbols) . font-lock-builtin-face)
   `(,(regexp-opt (compli-types) 'symbols) . font-lock-type-face)))

(define-derived-mode compli-mode prog-mode "compli"
  "Simple major mode for editing compli files."

  :syntax-table compli-mode-syntax-table

  (setq font-lock-defaults '(compli-font-lock-defaults))

  (when compli-tab-width
    (setq tab-width compli-tab-width))

  (setq comment-start "//")
  (setq comment-end "")

  (modify-syntax-entry ?/ "< 12" compli-mode-syntax-table)
  (modify-syntax-entry ?\n ">" compli-mode-syntax-table))

(provide 'compli-mode)
