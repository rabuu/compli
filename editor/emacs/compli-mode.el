(defun compli-types ()
  '("int" "bool" "float"))

(defun compli-keywords ()
  '("func" "let" "in" "if" "then" "else" "true" "false"))

(defun compli-font-lock-keywords ()
  (list
   `(,(regexp-opt (compli-keywords) 'symbols) . font-lock-keyword-face)
   `(,(regexp-opt (compli-types) 'symbols) . font-lock-type-face)))

(define-derived-mode compli-mode prog-mode "compli"
  "Simple major mode for editing compli files."
  (setq-local font-lock-defaults '(compli-font-lock-keywords)))

(provide 'compli-mode)
