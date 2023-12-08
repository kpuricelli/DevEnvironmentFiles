
(defvar real-auto-save-alist nil
  "List of buffers that will be auto saved truely.")

(defvar real-auto-save-interval 10
  "Time interval of real auto save.")

(defvar real-auto-save-p t
  "Toggle real auto save.")

(defvar real-auto-save-timer nil
  "real auto save timer.")

(defun real-auto-save()
  (interactive)
  (if real-auto-save-p
      (progn
	(save-excursion
	  (dolist (elem real-auto-save-alist)
	    (set-buffer elem)
	    (if (and (buffer-file-name) (buffer-modified-p))
		(progn
		  (write-file (buffer-file-name)))))))))

(defun turn-on-real-auto-save()
  (interactive)
  (if (buffer-file-name)
      (progn
	(unless real-auto-save-timer
	    (progn 
	      (setq real-auto-save-timer (timer-create))
	      (timer-set-time real-auto-save-timer (current-time) real-auto-save-interval)
	      (timer-set-function real-auto-save-timer 'real-auto-save)
	      (timer-activate real-auto-save-timer)))
	(add-to-list 'real-auto-save-alist (buffer-name)))))

(defun turn-off-real-auto-save ()
  (interactive)
  (when (buffer-file-name)
    (setq real-auto-save-alist (remove (buffer-name) real-auto-save-alist))))

(provide 'real-auto-save)
