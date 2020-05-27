(defun imenu-complete ()
  (interactive)
  (minibuffer-with-setup-hook #'minibuffer-complete (call-interactively #'imenu)))

(defun switch-to-buffer-complete ()
  (interactive)
  (minibuffer-with-setup-hook #'minibuffer-complete (call-interactively #'switch-to-buffer)))

(defun bookmark-jump-complete ()
  (interactive)
  (minibuffer-with-setup-hook #'minibuffer-complete (call-interactively #'bookmark-jump)))

(defun find-file-home-dir ()
  (interactive)
  (let ((default-directory (concat (getenv "HOME") "/")))
    (minibuffer-with-setup-hook #'minibuffer-completion-help (call-interactively #'find-file))))

(defun find-file-root-dir ()
  (interactive)
  (let ((default-directory "/"))
    (minibuffer-with-setup-hook #'minibuffer-completion-help (call-interactively #'find-file))))

(defun find-file-current-dir ()
  (interactive)
  (minibuffer-with-setup-hook #'minibuffer-completion-help (call-interactively #'find-file)))

(defun minibuffer-set-key ()
  (defun minibuffer-alt-del () 
    (interactive)
    (backward-kill-word 1)
    (minibuffer-completion-help))
  (defun minibuffer-alt-backslash () 
    (interactive)
    (if (string= "Find file: " (substring (buffer-string) 0 11))
      (let* ((str (buffer-string)) (len (length str)))
        (setq s (file-name-directory (substring str 11 (- len 1))))
        (move-beginning-of-line 0)
        (kill-line)
        (insert s))
      (backward-kill-word 1))
    (minibuffer-completion-help))
  (local-set-key (kbd "TAB") (lambda () (interactive (progn (minibuffer-complete) (minibuffer-completion-help)))))
  (local-set-key (kbd "\\") 'minibuffer-complete)
  (local-set-key (kbd "M-DEL") 'minibuffer-alt-del)
  (local-set-key (kbd "<M-backspace>") 'minibuffer-alt-del)
  (local-set-key (kbd "M-\\") 'minibuffer-alt-backslash)
  (local-set-key [mouse-3] 'minibuffer-alt-backslash)
  (remove-hook 'minibuffer-setup-hook 'minibuffer-set-key nil))

(add-hook 'minibuffer-setup-hook 'minibuffer-set-key)

(global-set-key (kbd "C-x f") 'find-file-home-dir)
(global-set-key (kbd "C-x C-f") 'find-file-current-dir)
(global-set-key (kbd "C-x M-f") 'find-file-root-dir)
(global-set-key (kbd "C-x b") 'switch-to-buffer-complete)
(global-set-key (kbd "M-i") 'imenu-complete)
(global-set-key (kbd "C-x r b") 'bookmark-jump-complete)



(defun completion-list-mode-bind-key ()
  (interactive)
  (choose-completion)
  (let ((x (minibuffer-prompt)))
    (cond
      ((string= "Index item: " x) (shell-command "xdotool key ctrl+l && xdotool key ctrl+l"))
      ((string= "Find file: " x) (shell-command "xdotool key Tab"))
      ((string-match-p "Switch to buffer " x) (shell-command "xdotool key ctrl+x && xdotool key l")))))

(add-hook 'completion-list-mode-hook (lambda () 
  (local-set-key (kbd "RET") 'completion-list-mode-bind-key)
  (local-set-key [mouse-1] 'completion-list-mode-bind-key)))

(provide 'enhanced-minibuffer)
