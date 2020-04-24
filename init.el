(package-initialize)

(add-to-list 'load-path "~/.emacs.d/lisp/")

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
(global-set-key (kbd "<f3>") 'save-buffer)
(global-set-key (kbd "<f4>") 'switch-swap-buffer)
(global-set-key (kbd "<f5>") 'reset-undo)
(global-set-key (kbd "<f8>") 'highlight-symbol-at-point-toggle)
(global-set-key (kbd "<f9>") 'other-window)
(global-set-key (kbd "<f10>") 'tmm-menubar)
(global-set-key (kbd "<f11>") 'save-buffer)
(global-set-key (kbd "<f12>") 'kill-current-buffer)
(global-set-key (kbd "C-x k") 'kill-current-buffer)
(global-set-key (kbd "C-x l") 'my-count-lines)
(global-set-key (kbd "C-x p") 'find-file-at-point)
(global-set-key (kbd "C-x s") 'search-selection)
(global-set-key (kbd "C-x C-r") 'reopen-killed-file)
(global-set-key (kbd "C-x C-h") 'recentf-open-files)
(global-set-key (kbd "C-x C-o") 'kill-other-buffers)
(global-set-key (kbd "C-x C-k") 'kill-all-buffers)
(global-set-key (kbd "C-c o") 'ff-find-other-file)
(global-set-key (kbd "C-c h") 'highlight-selection)
(global-set-key (kbd "C-c s") 'shell)
(global-set-key (kbd "C-c b") 'backup-current-file)
(global-set-key (kbd "C-c m") 'man)
(global-set-key (kbd "C-c c") 'comment-or-uncomment-region)
(global-set-key (kbd "C-c t") 'create-scratch-buffer)
(global-set-key (kbd "C-c C-p") 'copy-file-path)
(global-set-key (kbd "C-c M-d") 'kill-and-delete-current-file)
(global-set-key (kbd "C-h C-f") 'find-function)
(global-set-key (kbd "C-h C-v") 'find-variable)
(global-set-key (kbd "C-v") 'scroll-up-half)
(global-set-key (kbd "M-v") 'scroll-down-half)
(global-set-key (kbd "M-c") 'shell-command)
(global-set-key (kbd "M-e") 'eval-region-mark)
(global-set-key (kbd "<M-left>") (lambda () (interactive) (progn 
                                   (universal-argument) (set-mark-command 0))))
(global-set-key (kbd "<home>") 'beginning-of-buffer)
(global-set-key (kbd "<end>") 'end-of-buffer)
(global-set-key [mouse-2] 'keyboard-escape-quit)
(global-set-key [mouse-3] 'highlight-symbol-at-point-toggle)
(global-set-key [C-mouse-4] 'previous-error)
(global-set-key [C-mouse-5] 'next-error)
(global-set-key (kbd "<XF86Favorites>") 'next-error)
(global-set-key (kbd "<s-XF86Mail>") '(lambda () (interactive) (shell-command
                                                    "xdotool key alt+period")))


(recentf-mode)
(savehist-mode)
(show-paren-mode)
(global-anzu-mode)
(which-function-mode)
(delete-selection-mode)
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
(setenv "MANWIDTH" "178")
(setenv "BASH_ENV" "~/.emacs.d/bashrc")
(add-to-list 'default-frame-alist '(font . "Source Code Pro-14"))
(add-to-list 'default-frame-alist '(tty-color-mode . -1))


(setq case-fold-search t)
(setq column-number-mode t)
(setq make-backup-files nil)
(setq imenu-max-items 10000)
(setq auto-save-default nil)
(setq shell-file-name "bash")
(setq mouse-drag-copy-region t)
(setq inhibit-startup-message t)
(setq shell-command-switch "-c")
(setq password-cache-expiry nil)
(setq recentf-max-menu-items 39)
(setq recentf-max-saved-items 39)
(setq initial-scratch-message "")
(setq ring-bell-function 'ignore)
(setq next-screen-context-lines 1)
(setq auto-save-list-file-prefix nil)
(setq enable-recursive-minibuffers t)
(setq mouse-1-click-follows-link nil)
(setq mode-line-in-non-selected-windows nil)
(setq read-file-name-completion-ignore-case t)
(setq scroll-step 1 scroll-conservatively  10000)
(setq mouse-wheel-scroll-amount '(2 ((shift) . 1)))
(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/") ("melpa" .
                                               "https://melpa.org/packages/")))
(setq frame-title-format (list (getenv "USER") "@emacs:" '(buffer-file-name 
                                 "%f" (dired-directory dired-directory "%b"))))


(setq-default case-fold-search t)
(setq-default indent-tabs-mode nil)
(setq-default mode-line-format (list '((buffer-file-name "%f" (dired-directory
          dired-directory "%b"))  " (%l,%c)" vc-mode " " mode-line-misc-info)))


(add-hook 'Man-mode-hook 'delete-window)
(add-hook 'c-mode-common-hook (lambda () (ggtags-mode)))
(add-hook 'find-file-hook (lambda () (my-count-lines)))
(add-hook 'completion-list-mode (lambda () (local-set-key (kbd "C-g")
(add-hook 'ggtags-global-mode-hook '(lambda () (toggle-truncate-lines)))
(add-hook 'c-mode-hook (lambda () (local-set-key (kbd "TAB") 
                           'self-insert-command))) 'delete-completion-window)))
(add-hook 'after-make-frame-functions (lambda (frame) (when (display-graphic-p 
                           frame) (set-frame-parameter frame 'undecorated t))))

(defun create-scratch-buffer nil
       "create a scratch buffer"
       (interactive)
       (switch-to-buffer (get-buffer-create "*scratch*"))
       (lisp-interaction-mode))  


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages '(expand-region w3m ggtags auto-complete anzu)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(anzu-mode-line ((t nil)))
 '(font-lock-function-name-face ((t nil)))
 '(minibuffer-prompt ((t nil))))
