;;; elfaq.el --- Emacs faq popup

;; Author: Matthias
;; Keywords: minor-mode faq popup

(defvar elfaq-map nil "Mapping from major modes to faq files.")
(defvar elfaq-buffer-name "*elfaq*" "elfaq buffer name.")
(defvar elfaq-buffer nil "Buffer used to displaz faq file contents.")

(setq elfaq-map '((emacs-lisp-mode . "~/.emacs.d/faq/elisp.txt")))

(defun elfaq-init ()
  "Initialize elfaq module."
  (interactive)
  (setq elfaq-buffer (generate-new-buffer elfaq-buffer-name))
  )

(defun popup-faq ()
  (interactive)
  (if (eq (current-buffer) elfaq-buffer)
      (delete-window)
    (let* ((faqfile (cdr (assoc major-mode elfaq-map)))
	   (faqwindow (split-window nil -15))
	   (faqfilend (file-name-nondirectory faqfile)))
      (progn
	(select-window faqwindow)
	(set-window-buffer nil elfaq-buffer)
	(set-buffer elfaq-buffer)
	(setq buffer-read-only nil)
	(erase-buffer)
	(insert-file faqfile)
	(setq buffer-read-only t)
	(view-mode)
	)
      )
    )
  t
  )

(define-minor-mode elfaq-mode
  "faq mode is a minor-mode that offers major mode specific help."
  :global t  
  :lighter " faq"
  :keymap (let ((map (make-sparse-keymap)))
	    (define-key map (kbd "C-h q") 'popup-faq)
	    map)
  (elfaq-init)
  )

(provide 'elfaq)
