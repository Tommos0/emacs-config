(define-minor-mode cross-eval-mode
  "Minor mode for evaluating Lisp in another buffer."
  :lighter "cross-eval"
  :keymap (let ((map (make-sparse-keymap)))
	    (define-key map (kbd "C-c C-e") 'cross-eval-last-sexp)
	    map))

(defvar cross-eval--eval-buffer nil
  "Buffer for evaluating Lisp as if in this buffer.")

(defvar cross-eval--original-buffer nil
  "Buffer from which `cross-eval' function was called.")

(defun cross-eval ()
  "Open the cross eval buffer.
Either switch to the buffer if it exists or create a new one."
  (interactive)
  (when (not (buffer-live-p cross-eval--eval-buffer))
    (setq-local cross-eval--eval-buffer (generate-new-buffer "cross-eval"))
    (let ((this-buffer (current-buffer)))
	(with-current-buffer cross-eval--eval-buffer
	    (lisp-interaction-mode)
	    (cross-eval-mode)
	    (setq-local cross-eval--original-buffer this-buffer))))
    (switch-to-buffer-other-window cross-eval--eval-buffer))

(defun cross-eval--eval-around (orig-fun &rest args)
  "Call the wrapped ORIG-FUN with ARGS in the original buffer."
  (with-current-buffer cross-eval--original-buffer
    (apply orig-fun args)
    (when (get-buffer-window)
	(set-window-point (get-buffer-window) (point)))))

(defun cross-eval-last-sexp (eval-last-sexp-arg-internal)
  "Eval last s-expression in `cross-eval--original-buffer'."
  (interactive "P")
  (when (buffer-live-p cross-eval--original-buffer)
      (unwind-protect (progn (advice-add 'eval :around #'cross-eval--eval-around)
			     (eval-last-sexp eval-last-sexp-arg-internal))
	(advice-remove 'eval #'cross-eval--eval-around))))

