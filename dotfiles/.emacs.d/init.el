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
(eval-when-compile (require 'cl-lib))

;; Packages
(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))
;; (add-to-list 'package-archives
;;              '("melpa" . "http://melpa.org/packages/") t)

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
                      markdown-mode
                      markdown-toc
                      yaml-mode

                      ;; Others
                      all-the-icons
                      base16-theme
                      company
                      doom-modeline
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
                      swiper
                      )
                      "Packages to install.")

(cl-loop for pkg in my-packages
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
(use-package base16-theme
  :ensure t
  :config
  (load-theme 'base16-grayscale-dark t))

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

;; Flymd
(require 'flymd)
 (defun my-flymd-browser-function (url)
   (let ((browse-url-browser-function 'browse-url-firefox))
     (browse-url url)))
(setq flymd-browser-open-function 'my-flymd-browser-function)

;; Go LSP
;; Alternative: https://gist.github.com/psanford/80d3268a666b2b11666313d452c054ed
(use-package lsp-mode
  :ensure t
  :commands (lsp lsp-deferred)
  :hook (go-mode . lsp-deferred))

(setq lsp-modeline-code-actions-segments '(name icon))
;;(global-set-key (kbd "M->") 'godef-jump-other-window)

;; (use-package flymake-diagnostic-at-point
;;   :after flymake
;;   :config
;;   (add-hook 'flymake-mode-hook #'flymake-diagnostic-at-point-mode))

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

(when (not window-system)
  (set-face-attribute 'region nil :background "#b9b9b9"))

;; Set up the visible bell
(setq visible-bell t)

;;(set-face-attribute 'default nil :font "Fira Code Retina" :height 250)
(set-face-attribute 'default nil :font "Fira Code Retina" :height 110)

;; Initialize use-package on non-Linux platforms
(unless (package-installed-p 'use-package)
   (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

;; Fuzzy search
(use-package swiper)

(use-package ivy
  :diminish
  :bind (("C-s" . swiper)
         :map ivy-minibuffer-map
         ("TAB" . ivy-alt-done) ;; TAB enters directories
	 ("RET" . ivy-alt-done) ;; ENTER enters directories
	 ("<C-return>" . ivy-alt-done) ;; Ctrl-Enter enters directories
         ("C-l" . ivy-alt-done)
         ("C-j" . ivy-next-line)
         ("C-k" . ivy-previous-line)
         :map ivy-switch-buffer-map
         ("C-k" . ivy-previous-line)
         ("C-l" . ivy-done)
         ("C-d" . ivy-switch-buffer-kill)
         :map ivy-reverse-i-search-map
         ("C-k" . ivy-previous-line)
         ("C-d" . ivy-reverse-i-search-kill)))
(ivy-mode 1)

(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1)
  :custom ((doom-modeline-height 15)))

(setq lsp-keymap-prefix "C-c C-l")

;; Update all packages
(use-package auto-package-update
  :ensure t
  :config
  ;; Yes, please delete the old version on updates.
  (setq auto-package-update-delete-old-versions t))

;; The package provides a function auto-package-update-now which I simply call from my shell script.

;; $ emacs --eval '(progn (sit-for 2) (auto-package-update-now))'
;; Don't ask me why I use --eval including the sit-for. I guess there were reasons but I simply cannot remember...

;; Well, you can also just use this simpler recipe.

;; $ emacs -f auto-package-update-now
;; That seems to work just as well. Try it out! If there's some case where that fails but the --eval '(progn (sit-for 2) (auto-package-update-now))' version works, please drop me a mail and remember me of the reasons I've had back then.



;; run: all-the-icons-install-fonts

(use-package org)


;; (setq
;;  lsp-go-env '((GOFLAGS . "-tags=linux,cgo,withjournald"))
;;  )

;; (setq
;;  lsp-go-env '((GOFLAGS . "-tags=integration"))
;;  )

;; (setq
;;  lsp-go-env '((GOFLAGS . "-tags=mage"))
;;  )

(provide 'init)
;;; init.el ends here

