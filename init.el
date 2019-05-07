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
                      ;; Modes
                      arduino-mode
                      dockerfile-mode
                      go-mode
                      kotlin-mode
                      lua-mode
                      markdown-mode+
                      markdown-toc
                      php-mode
                      rjsx-mode
                      web-mode
                      yaml-mode

                      ;; Golang
                      company-go
                      go-autocomplete
                      go-dlv
                      go-guru
                      go-rename

                      ;; Python
                      company-jedi
                      elpy
                      py-autopep8
                      pyvenv

                      ;; Others
                      auto-complete
                      base16-theme
                      color-theme-modern
                      company
                      exec-path-from-shell
                      fiplr
                      flycheck
                      flymd
                      helm-ag
                      hl-todo
                      linum-relative
                      magit
                      multiple-cursors
                      neotree
                      paredit
                      prettier-js
                      rainbow-delimiters
                      thrift)
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
(global-hl-todo-mode t)
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

;; Flycheck
(add-hook 'after-init-hook #'global-flycheck-mode)

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
(add-hook 'before-save-hook #'gofmt-before-save)

;; Auto complete
(add-hook 'after-init-hook 'global-company-mode)

(add-hook 'go-mode-hook (lambda ()
                          (set (make-local-variable 'company-backends) '(company-go))
                          (company-mode)
						  (go-guru-hl-identifier-mode)))

;; Python/Elpy
(elpy-enable)

(setq python-shell-interpreter "ipython"
            python-shell-interpreter-args "-i --simple-prompt")

(require 'pyvenv)

(when (require 'flycheck nil t)
  (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
  (add-hook 'elpy-mode-hook 'flycheck-mode))

(require 'py-autopep8)
(add-hook 'elpy-mode-hook 'py-autopep8-enable-on-save)

;; Python mode
(add-to-list 'company-backends '(company-jedi company-files))

(defun my-merge-imenu ()
  (interactive)
  (let ((mode-imenu (imenu-default-create-index-function))
        (custom-imenu (imenu--generic-function imenu-generic-expression)))
    (append mode-imenu custom-imenu)))

(defun my-python-hooks()
    (interactive)
    (setq tab-width     4
          python-indent 4
          python-shell-interpreter "ipython"
          python-shell-interpreter-args "-i")
    (if (string-match-p "rita" (or (buffer-file-name) ""))
        (setq indent-tabs-mode t)
      (setq indent-tabs-mode nil)
    )
    (add-to-list
        'imenu-generic-expression
        '("Sections" "^#### \\[ \\(.*\\) \\]$" 1))
    (setq imenu-create-index-function 'my-merge-imenu))

(add-hook 'python-mode-hook 'my-python-hooks)

(add-hook 'elpy-mode-hook
          (lambda ()
            (local-unset-key (kbd "M-."))
            (local-unset-key (kbd "M-,"))
            (local-unset-key (kbd "M-/"))
            (define-key elpy-mode-map (kbd "M-.") 'jedi:goto-definition)
            (define-key elpy-mode-map (kbd "M-,") 'jedi:goto-definition-pop-marker)
            (define-key elpy-mode-map (kbd "M-/") 'jedi:show-doc)))

;; React/JS
(add-hook 'rjsx-mode-hook 'prettier-js-mode)
(add-hook 'rjsx-mode-hook
          (lambda ()
            (setq js2-basic-offset 2)
            (setq tab-width 2)))

(setq prettier-js-args '(
                         "--single-quote"  "true"
                         "--jsx-bracket-same-line" "true"
                         "--bracket-spacing" "false"
                         ))

(autoload 'php-mode "php-mode" "Major mode for editing PHP code." t)

(defun indent-buffer ()
  "Indent an entire buffer using the default intenting scheme."
  (interactive)
  (save-excursion
    (delete-trailing-whitespace)
    (indent-region (point-min) (point-max) nil)
    (untabify (point-min) (point-max))))


(autoload 'apib-mode "apib-mode"
  "Major mode for editing API Blueprint files" t)

;; Disable backup/autosave files - I use Git
(setq backup-inhibited           t)
(setq make-backup-files        nil)
(setq auto-save-default        nil)
(setq auto-save-list-file-name nil)

;; filetype -> mode
(add-to-list 'auto-mode-alist '("\\.php$" . php-mode))
(add-to-list 'auto-mode-alist '("\\.inc$" . php-mode))
(add-to-list 'auto-mode-alist '("\\.apib\\'" . apib-mode))
(add-to-list 'auto-mode-alist '("\\.js\\'" . rjsx-mode))

(defun my-flymd-browser-function (url)
  (let ((process-environment (browse-url-process-environment)))
    (apply 'start-process
           (concat "firefox " url)
           nil
           "/usr/bin/open"
           (list "-a" "firefox" url))))
(setq flymd-browser-open-function 'my-flymd-browser-function)

;; Enable upcase and downcase region
;; C-x C-u and C-x C-l
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

(defun keybinds-go ()
  "For use in go-mode."
  (local-set-key (kbd "M-.") 'go-guru-definition)
  (local-set-key (kbd "M-,") 'pop-tag-mark))

;; add to hook
(add-hook 'go-mode-hook 'keybinds-go)

;; kOS - kerboscript
(require 'ks)
(setq ks-indent 4)

;; Better breaking line mode
(global-visual-line-mode 1)

;; Move between frames with Shift + arrow
(windmove-default-keybindings)

;; insert matching delimiters
(electric-pair-mode)

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

(global-set-key (kbd "M-TAB") #'company-complete)

;; Indent buffer
(global-set-key "\C-x\\" 'indent-buffer)

(setq-default line-spacing 2)
(setq-default tab-width 4)
(setq-default indent-tabs-mode nil)

;; Color Theme
;;(load-theme 'dark-laptop)
(load-theme 'base16-grayscale-dark t)

;; Custom Editor Font
(set-frame-font "Monospace-13")

(provide 'init)
;;; init.el ends here
