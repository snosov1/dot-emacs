(require 'subr-x)

(defvar speed-freak--start-time nil)
(make-variable-buffer-local 'speed-freak--start-time)

(defvar speed-freak--orig-text nil)
(make-variable-buffer-local 'speed-freak--orig-text)

(defvar speed-freak--keymap nil)
(make-variable-buffer-local 'speed-freak--keymap)

(defun speed-freak--first-change ()
  "Start the timer."
  (when (not speed-freak--start-time)
    (setq speed-freak--start-time (float-time))))

(defun speed-freak--change (start end length)
  (let ((text (buffer-substring-no-properties (point-min) (point-max))))
    (when (string= (string-trim-right speed-freak--orig-text)
                   (string-trim-right text))
      (let* ((len (length speed-freak--orig-text))
             (time (speed-freak--elapsed-time))
             (stats (format "Time:\t\t%f\nWPM:\t\t%f\nCPM:\t\t%f\n\nGreat job!"
                            time
                            (/ (/ len 5.0) (/ time 60.0))
                            (/ len (/ time 60.0)))))
        (erase-buffer)
        (insert stats)
        (read-only-mode))
      (remove-hook 'first-change-hook 'speed-freak--first-change)
      (remove-hook 'after-change-functions 'speed-freak--change))))

(defun speed-freak--elapsed-time ()
  "Return float with the total time since start."
  (interactive)
  (let ((end-time (float-time)))
    (if (not speed-freak--start-time)
        0 (- end-time speed-freak--start-time))))

(defun speed-freak-buffer ()
  (interactive)
  (delete-trailing-whitespace)
  (let* ((text (buffer-substring (point-min) (point-max)))
         (raw-text (buffer-substring-no-properties (point-min) (point-max)))
         (buf (clone-buffer "speed-freak" t)))
    (switch-to-buffer buf)
    (delete-region (point-min) (point-max))
    (not-modified)
    (make-local-variable 'after-change-functions)
    (make-local-variable 'first-change-hook)
    (add-hook 'after-change-functions 'speed-freak--change)
    (add-hook 'first-change-hook 'speed-freak--first-change)
    (setq speed-freak--orig-text raw-text)
    (setq speed-freak--start-time nil)
    (setq speed-freak--keymap (copy-keymap (or (current-local-map) (make-sparse-keymap))))
    (use-local-map speed-freak--keymap)
    (define-key speed-freak--keymap (vector 'remap 'save-buffer) 'delete-trailing-whitespace)
    ))
