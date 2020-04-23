(defun scroll-up-half () (interactive) (scroll-up (window-half-height)))
(defun kill-all-buffers () (interactive)
  (mapc 'kill-buffer (buffer-list))
  (setq default-directory (concat (getenv "HOME") "/")))
(defun scroll-down-half () (interactive) (scroll-down (window-half-height)))
(defun window-half-height () (max 1 (/ (1- (window-height (selected-window))) 2)))

(defun backup-current-file ()
  (interactive)
  (let (($fname (buffer-file-name)) ($date-time-format "%Y-%m-%d_%H%M%S"))
    (if $fname
      (let 
        (($backup-name 
          (concat $fname "~" (format-time-string $date-time-format) "~")))
        (copy-file $fname $backup-name t)
        (message (concat "Backup saved at: " $backup-name)))
      (if (string-equal major-mode "dired-mode") (progn
        (mapc (lambda ($x) (let (($backup-name 
                 (concat $x "~" (format-time-string $date-time-format) "~")))
                (copy-file $x $backup-name t)))
            (dired-get-marked-files))
          (message "marked files backed up"))
        (user-error "buffer not file nor dired")))))

(defun switch-swap-buffer () (interactive) (switch-to-buffer (other-buffer
  (current-buffer) t)))

(defun eval-region-mark ()
  (interactive)
  (eval-region (region-beginning) (region-end)) (keyboard-escape-quit))

(defun kill-other-buffers ()
  (interactive)
  (mapc 'kill-buffer (delq (current-buffer) (buffer-list))))

(defun copy-file-path ()
  (interactive)
  (let ((filename (if (equal major-mode 'dired-mode) default-directory 
    (buffer-file-name))))
    (when filename (with-temp-buffer (insert filename) (clipboard-kill-region 
      (point-min) (point-max))) (message filename))))

(defun reset-undo () (interactive) (buffer-disable-undo) (buffer-enable-undo)
  (message "reset-undo done"))

(defun kill-and-delete-current-file ()
  (interactive)
  (let ((filename (buffer-file-name)))
    (when filename
      (if (vc-backend filename)
        (vc-delete-file filename)
        (progn
          (delete-file filename)
          (message "Deleted file %s" filename)
          (kill-buffer))))))

(defun my-count-lines ()
  (interactive)
  (message (concat "count-lines: " 
    (number-to-string (count-lines (point-min) (point-max))))))

(defun kill-dired-buffers ()
  (interactive)
  (mapc (lambda (buffer) 
    (when (eq 'dired-mode (buffer-local-value 'major-mode buffer)) 
      (kill-buffer buffer))) 
    (buffer-list)))

(defun search-selection (beg end)
  (interactive "r")
  (let ((selection (buffer-substring-no-properties beg end)))
    (deactivate-mark)
    (isearch-mode t nil nil nil)
    (isearch-yank-string selection)))

(defun highlight-symbol-at-point-toggle ()
  (interactive)
  (if (hi-lock--regexps-at-point)
    (unhighlight-regexp (concat "\\_<" (current-word) "\\_>"))
    (highlight-symbol-at-point)))

(defun highlight-selection (beg end)
  (interactive "r")
  (let ((selection (buffer-substring-no-properties beg end)))
    (deactivate-mark)
    (highlight-regexp selection)))

(provide 'mini-function)
