;; Emacs config
;; Peter Popov

(setq work-directory (file-name-directory load-file-name))

;; ==============================================================================
;; Modes
(add-to-list 'load-path (concat work-directory "modes") )

;; ==============================================================================
;; Package managment
;; ==============================================================================
(require 'package)
(require 'cl-lib)
(package-initialize)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)

;; Auto load plugins on start
;; Taken from here: http://batsov.com/articles/2012/02/19/package-management-in-emacs-the-good-the-bad-and-the-ugly/
(defvar prelude-packages
  '(python yasnippet go-mode color-theme-solarized)
  "A list of packages to ensure are installed at launch.")

(defun prelude-packages-installed-p ()
   (cl-every 'package-installed-p prelude-packages))

(unless (prelude-packages-installed-p)
  ;; check for new packages (package versions)
  (message "%s" "Emacs Prelude is now refreshing its package database...")
  (package-refresh-contents)
  (message "%s" " done.")
  ;; install the missing packages
  (dolist (p prelude-packages)
    (when (not (package-installed-p p))
      (message "%s: %s" "Installing" p)
      (package-install p))))

(provide 'prelude-packages)


;; ==============================================================================
;; Plugins set up
;; ==============================================================================
;;
;;IDO
(require 'ido)
(ido-mode t)
(setq
 ido-ignore-buffers '("\\` " "^\*Mess" "^\*Back" ".*Completion" "^\*Ido" "^\*GTAGS")
 ido-enable-flex-matching t         ; enable fuzzy matching
 ido-max-prospects 6                ; don't spam my minibuffer
 ido-confirm-unique-completion t )  ; wait for RET, even with unique completion
;;
;; nxml mode
(add-to-list 'auto-mode-alist
             (cons (concat "\\." (regexp-opt '("xml" "xsd" "rng" "xslt" "svg" "rss") t) "\\'")
                   'nxml-mode))
(fset 'xml-mode 'nxml-mode)
(fset 'html-mode 'nxml-mode)
;;
;; yasnippet
(require 'yasnippet)
(yas-global-mode 1)

;; ==============================================================================
;; Appearance
;; ==============================================================================
;; Dont show the GNU splash screen
(setq inhibit-startup-message t)
(setq x-select-enable-clipboard t)
;; turn on visual bell
(setq visible-bell t)
;; get rid of the toolbar on top of the window
(tool-bar-mode 0)
(menu-bar-mode 0)
;; Show column number at bottom of screen
(column-number-mode 1)
(set-default 'truncate-lines t)
(setq font-lock-maximum-decoration t)
(setq next-line-add-newlines nil)
(setq initial-buffer-choice nil)
(setq initial-scratch-message nil)
(put 'scroll-left 'disabled nil)
;;
;; Color theme
(add-to-list 'custom-theme-load-path (concat work-directory "themes") )
(load-theme 'solarized-dark t)
(set-face-attribute 'default nil :height 140)

;; ==============================================================================
;; C & C++ mode settings
;; ==============================================================================
;; C mode
(add-hook 'c-mode-hook '(lambda() (setq indent-tabs-mode nil)))
(add-hook 'c-mode-hook '(lambda() (gtags-mode t)))

;; C++ mode
(add-hook 'c++-mode-hook '(lambda() (setq indent-tabs-mode nil)))
(add-hook 'c++-mode-hook '(lambda() (gtags-mode t)))
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))

;; Code formating
(setq c-default-style "k&r"
      c-basic-offset 4)
(setq-default indent-tabs-mode nil)
(setq-default show-trailing-whitespace t)

;;(toggle-show-tabs-show-ws)
;;(toggle-show-trailing-whitespace-show-ws)

;; Delete trailing whitespaces
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; No backup files
(setq make-backup-files nil)

;; Set M-1 as e key for goto line
(global-unset-key "\M-1")
(global-set-key "\M-1" 'goto-line)

;; Set C-tab to find other file
(global-unset-key [C-tab])
(global-set-key [C-tab] 'ff-find-other-file)
