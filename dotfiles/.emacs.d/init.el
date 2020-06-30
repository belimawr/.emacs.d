;;; .emacs.d --- My Emacs Config
;;; Commentary:

;; Copyright (C) 2017 Tiago Queiroz <contato@tiago.eti.br>
;; 
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;; 
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Code:

;; The first thing to do
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

;; Load custom first
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

;; Manually downloaded stuff
(add-to-list 'load-path "~/.emacs.d/downloaded/")

;; Turn off mouse interface early in startup to avoid momentary display
(if (functionp 'tool-bar-mode)(tool-bar-mode -1))
(if (functionp 'scroll-bar-mode) (scroll-bar-mode -1))
(menu-bar-mode -1)

;; Show  pairs of parentheses
(setq show-paren-delay 0)
(show-paren-mode)

;; No splash screen
(setq inhibit-startup-screen t)

;; Common LISP
(eval-when-compile (require 'cl))

;; Packages
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)

(package-initialize)

;; Install packages
(defvar my-packages '(
                      ;; Emacs packages
                      use-package

                      ;; Modes
                      arduino-mode
                      dockerfile-mode
                      go-mode
                      lsp-mode
                      markdown-mode+
                      markdown-toc
                      yaml-mode

                      ;; Others
                      base16-theme
                      company
                      company-lsp
                      exec-path-from-shell
                      fiplr
                      flymd
                      helm-ag
                      linum-relative
                      lsp-ui
                      magit
                      multiple-cursors
                      neotree
                      paredit
                      rainbow-delimiters
                      )
                      "Packages to install.")

(loop for pkg in my-packages
      unless (package-installed-p pkg) do (package-install pkg))
;; env
(when (memq window-system '(mac ns x))
  (setq-default exec-path-from-shell-check-startup-files nil)
  (setq-default exec-path-from-shell-variables '("PATH" "GOPATH" "GOROOT"))
  (exec-path-from-shell-initialize))

;; Linum relative
(require 'linum-relative )
(linum-relative-global-mode)
(setq linum-relative-current-symbol "")
(setq linum-relative-format "%3s ")
(global-linum-mode t)
(column-number-mode)
(setq linum-format "%4d \u2502 ")

;; Ido
(setq-default ido-use-filename-at-point 'guess)
(setq-default ido-create-new-buffer 'always)
(setq-default ido-enable-flex-matching t)
(setq-default ido-use-url-at-point nil)
(setq-default ido-everywhere t)
(setq-default ido-auto-merge-delay-time 5)
(ido-mode 1)

;; Fiplr
(setq-default fiplr-root-markers '(".git"
								   "project.clj"
								   "build.gradle"))

(setq-default fiplr-ignored-globs '((directories (".svn" ".git" ".hg" "CVS" "build" "target"))
									(files ("*.pyc" "*.pyo" "*.exe" "*.dll" "*.obj""*.o"
											"*.a" "*.lib" "*.so" "*.dylib" "*.ncb" "*.sdf"
											"*.suo" "*.pdb" "*.idb" ".DS_Store" "*.class"
											"*.psd" "*.db" "*.jpg" "*.jpeg" "*.png" "*.gif"
											"*.ttf" "*.tga" "*.dds" "*.ico" "*.eot" "*.pdf"
											"*.swf" "*.jar" "*.zip"))))
;; Hooks
(add-hook 'emacs-lisp-mode-hook #'paredit-mode)
(add-hook 'emacs-lisp-mode-hook #'rainbow-delimiters-mode)
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)
;;(add-hook 'before-save-hook #'gofmt-before-save)

(defun indent-buffer ()
  "Indent an entire buffer using the default intenting scheme."
  (interactive)
  (save-excursion
    (delete-trailing-whitespace)
    (indent-region (point-min) (point-max) nil)
    (untabify (point-min) (point-max))))

;; Better breaking line mode
(global-visual-line-mode 1)

;; Move between frames with Shift + arrow
(windmove-default-keybindings)

;; Disable backup/autosave files - I use Git
(setq backup-inhibited           t)
(setq make-backup-files        nil)
(setq auto-save-default        nil)
(setq auto-save-list-file-name nil)

(setq-default line-spacing 2)
(setq-default tab-width 4)
(setq-default indent-tabs-mode nil)

;; Color Theme
;;(load-theme 'dark-laptop)
;;(base16-theme-256-color-source 'terminal)
;;(load-theme 'base16-grayscale-dark t)
;;(load-theme 'base16-shell-colors t)

(use-package base16-theme
  :ensure t
  :config
  (load-theme 'base16-grayscale-dark t))

;; Custom Editor Font
(set-frame-font "Monospace-13")

;; Custom Keybindings
;; Multiple cursors
(require 'multiple-cursors)
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)

;; NEO Tree
(global-set-key [f8] 'neotree-toggle)

(global-set-key (kbd "M-g") 'goto-line)
(global-set-key (kbd "C-c f") 'fiplr-find-file)

(global-set-key (kbd "C-M-<left>") 'shrink-window-horizontally)
(global-set-key (kbd "C-M-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "C-M-<down>") 'shrink-window)
(global-set-key (kbd "C-M-<up>") 'enlarge-window)

;; Indent buffer
(global-set-key "\C-x\\" 'indent-buffer)

;; (custom-set-variables
;;  ;; custom-set-variables was added by Custom.
;;  ;; If you edit it by hand, you could mess it up, so be careful.
;;  ;; Your init file should contain only one such instance.
;;  ;; If there is more than one, they won't work right.
;;  '(package-selected-packages
;;    (quote
;;     (company-lsp company lsp-ui lsp-mode multiple-cursors magit linum-relative base16-theme use-package yaml-mode))))

;; (custom-set-faces
;;  ;; custom-set-faces was added by Custom.
;;  ;; If you edit it by hand, you could mess it up, so be careful.
;;  ;; Your init file should contain only one such instance.
;;  ;; If there is more than one, they won't work right.
;;  )

;; Flymd
(require 'flymd)
 (defun my-flymd-browser-function (url)
   (let ((browse-url-browser-function 'browse-url-firefox))
     (browse-url url)))
(setq flymd-browser-open-function 'my-flymd-browser-function)

;; (defun my-flymd-browser-function (url)
;;   (let ((process-environment (browse-url-process-environment)))
;;     (apply 'start-process
;;            (concat "firefox " url)
;;            nil
;;            "/usr/bin/open"
;;            (list "-a" "firefox" url))))
;; (setq flymd-browser-open-function 'my-flymd-browser-function)

;; Golang LSP
;; Alternative: https://gist.github.com/psanford/80d3268a666b2b11666313d452c054ed
(use-package lsp-mode
  :ensure t
  :commands (lsp lsp-deferred)
  :hook (go-mode . lsp-deferred))

;; Set up before-save hooks to format buffer and add/delete imports.
;; Make sure you don't have other gofmt/goimports hooks enabled.
(defun lsp-go-install-save-hooks ()
  (add-hook 'before-save-hook #'lsp-format-buffer t t)
  (add-hook 'before-save-hook #'lsp-organize-imports t t))
(add-hook 'go-mode-hook #'lsp-go-install-save-hooks)

;; Optional - provides fancier overlays.
(use-package lsp-ui
  :ensure t
  :commands lsp-ui-mode)

;; Company mode is a standard completion package that works well with lsp-mode.
(use-package company
  :ensure t
  :config
  ;; Optionally enable completion-as-you-type behavior.
  (setq company-idle-delay 0)
  (setq company-minimum-prefix-length 1)
  (setq company-tooltip-align-annotations t)
  )

;; company-lsp integrates company mode completion with lsp-mode.
;; completion-at-point also works out of the box but doesn't support snippets.
(use-package company-lsp
  :ensure t
  :commands company-lsp)

(when (not window-system)
    (set-face-attribute 'region nil :background "#b9b9b9"))

(provide 'init)
;;; init.el ends here
