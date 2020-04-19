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
                      yaml-mode

                      ;; Emacs packages
                      use-package

                      ;; Others
                      base16-theme
                      linum-relative
                      magit
                      multiple-cursors)
                      "Packages to install.")

(loop for pkg in my-packages
      unless (package-installed-p pkg) do (package-install pkg))

;; Linum relative
(require 'linum-relative )
(linum-relative-global-mode)
(setq linum-relative-current-symbol "")
(setq linum-relative-format "%3s ")
(global-linum-mode t)
(column-number-mode)
(setq linum-format "%4d \u2502 ")

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
;;(load-theme 'base16-grayscale-dark t)

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

(provide 'init)
;;; init.el ends here
