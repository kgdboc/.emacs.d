(package-initialize)

(add-to-list 'load-path "~/.emacs.d/lisp/")

(require 'delete)
(require 'hi-lock)
(require 'mini-function)
(require 'enhanced-minibuffer)
(require 'sudo-save)
(require 'move-lines)
(require 'goto-last-change)
(require 'reopen-killed-file)

(global-set-key (kbd "<escape>") 'keyboard-escape-quit)
(global-set-key (kbd "<f1>") 'delete-other-windows)
(global-set-key (kbd "<f2>") 'delete-window)
(global-set-key (kbd "<f3>") 'other-window)
(global-set-key (kbd "<f4>") 'switch-swap-buffer)
(global-set-key (kbd "<f5>") 'reset-undo)
(global-set-key (kbd "<f6>") 'string-insert-rectangle)
(global-set-key (kbd "<f7>") 'erase-buffer)
(global-set-key (kbd "<f8>") 'highlight-regexp)
(global-set-key (kbd "<f9>") 'search-selection)
(global-set-key (kbd "<f10>") 'tmm-menubar)
(global-set-key (kbd "<f11>") 'erase-buffer)
(global-set-key (kbd "<f12>") 'kill-current-buffer)
(global-set-key (kbd "C-x k") 'kill-current-buffer)
(global-set-key (kbd "C-x l") 'my-count-lines)
(global-set-key (kbd "C-x p") 'find-file-at-point)
(global-set-key (kbd "C-x s") 'search-selection)
(global-set-key (kbd "C-x C-r") 'reopen-killed-file)
(global-set-key (kbd "C-x C-o") 'ff-find-other-file)
(global-set-key (kbd "C-x C-k") 'kill-all-buffers)
(global-set-key (kbd "C-x C-c") 'delete-frame)
(global-set-key (kbd "C-c h") 'highlight-selection)
(global-set-key (kbd "C-c b") 'backup-current-file)
(global-set-key (kbd "C-c m") 'man)
(global-set-key (kbd "C-c c") 'comment-or-uncomment-region)
(global-set-key (kbd "C-c t") 'create-scratch-buffer)
(global-set-key (kbd "C-c p") 'copy-file-path)
(global-set-key (kbd "C-c C-o") 'kill-other-buffers)
(global-set-key (kbd "C-c M-d") 'kill-and-delete-current-file)
(global-set-key (kbd "C-h C-f") 'find-function)
(global-set-key (kbd "C-h C-v") 'find-variable)
(global-set-key (kbd "C-v") 'scroll-up-half)
(global-set-key (kbd "M-v") 'scroll-down-half)
(global-set-key (kbd "M-e") 'eval-region-mark)
(global-set-key (kbd "<home>") 'beginning-of-buffer)
(global-set-key (kbd "<end>") 'end-of-buffer)
(global-set-key [mouse-2] 'keyboard-escape-quit)
(global-set-key [mouse-3] 'highlight-symbol-at-point-toggle)

(show-paren-mode)
(global-anzu-mode)
(which-function-mode)
(delete-selection-mode 1)
(menu-bar-mode 0)
(auto-save-mode 0)
(electric-indent-mode 0)
(global-font-lock-mode 0)

(defun init-on-gui (&optional frame)
  (fringe-mode 0)
  (tool-bar-mode 0)
  (scroll-bar-mode 0)
  (blink-cursor-mode 0)
  (set-frame-parameter nil 'undecorated t)
  (set-face-attribute 'region nil :background "#ffff00")
  (add-to-list 'default-frame-alist '(fullscreen . maximized)))
(if (boundp 'fringe-mode) (init-on-gui))

(fset 'yes-or-no-p 'y-or-n-p)
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'erase-buffer 'disabled nil)
(setenv "MANWIDTH" "193")
(add-to-list 'default-frame-alist '(font . "Source Code Pro-13"))

(setq column-number-mode t)
(setq make-backup-files nil)
(setq imenu-max-items 10000)
(setq auto-save-default nil)
(setq mouse-drag-copy-region t)
(setq inhibit-startup-message t)
(setq password-cache-expiry nil)
(setq initial-scratch-message "")
(setq ring-bell-function 'ignore)
(setq next-screen-context-lines 1)
(setq auto-save-list-file-prefix nil)
(setq enable-recursive-minibuffers t)
(setq mode-line-in-non-selected-windows nil)
(setq read-file-name-completion-ignore-case t)
(setq scroll-step 1 scroll-conservatively  10000)
(setq mouse-wheel-scroll-amount '(2 ((shift) . 1)))
(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/") ("melpa" .
                                               "https://melpa.org/packages/")))
(setq frame-title-format (list (getenv "USER") "@emacs:" '(buffer-file-name 
                                 "%f" (dired-directory dired-directory "%b"))))


(setq-default case-fold-search t)
(setq-default indent-tabs-mode t)
(setq-default tab-width 8)
(defvaralias 'c-basic-offset 'tab-width)
(setq-default mode-line-format (list '((buffer-file-name "%f" (dired-directory
          dired-directory "%b"))  " (%l,%c)" vc-mode " " mode-line-misc-info)))

(add-hook 'Man-mode-hook 'delete-window)
(add-hook 'find-file-hook (lambda () (my-count-lines)))
(add-hook 'after-make-frame-functions (lambda (frame) (when (display-graphic-p 
                           frame) (set-frame-parameter frame 'undecorated t))))
(add-hook 'completion-list-mode-hook '(lambda () (setq-local mouse-1-click-follows-link nil)))


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages '(ggtags expand-region w3m auto-complete anzu)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(anzu-mode-line ((t nil)))
 '(font-lock-function-name-face ((t nil)))
 '(minibuffer-prompt ((t nil))))

(setq ggtags-update-on-save nil)

(setq vc-follow-symlinks nil)

(add-hook 'ggtags-mode-hook (lambda ()
  (local-set-key [double-mouse-1] 'ggtags-find-tag-dwim)
  (local-set-key [mouse-2] 'xref-pop-marker-stack)
  (local-set-key [mouse-8] 'next-error)
  (local-set-key [mouse-9] 'previous-error)))

(add-hook 'minibuffer-setup-hook (lambda () (local-set-key [mouse-7] 'minibuffer-alt-backslash)))
(add-hook 'completion-list-mode-hook (lambda ()
  (local-set-key [mouse-2] 'keyboard-escape-quit)
  (local-set-key [mouse-7] 'minibuffer-alt-backslash)))

(defun mouse-imenu-complete () (interactive) (shell-command "xdotool key alt+i"))

(add-hook 'c-mode-common-hook (lambda ()
  (ggtags-mode)
  (local-set-key [mouse-7] 'mouse-imenu-complete)))
