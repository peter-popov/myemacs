;; Peter Popov

(add-to-list 'load-path (concat user-emacs-directory
        (convert-standard-filename "modules")))
;; ==============================================================================
;; Package managment
;; ==============================================================================
(require 'package)
(require 'cl-lib)
(package-initialize)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t )

;; Auto load packages on start(taken from emacs Prelude)
(defvar my-default-packages
  '(python
    yasnippet
    go-mode
    solarized-theme
    projectile
    helm
    popup
    smart-mode-line
    magit
    )
  "A list of packages to ensure are installed at launch.")

(defun my-packages-installed-p ()
   (cl-every 'package-installed-p my-default-packages))

(unless (my-packages-installed-p)
  ;; check for new packages (package versions)
  (message "%s" "Refreshing package database...")
  (package-refresh-contents)
  (message "%s" " done.")
  ;; install the missing packages
  (dolist (p my-default-packages)
    (when (not (package-installed-p p))
      (message "%s: %s" "Installing" p)
      (package-install p))))

(provide 'my-default-packages)


;; ==============================================================================
;; Plugins set up
;; ==============================================================================
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

;; Prjectile
;; Use only for some languages
(projectile-global-mode)
(setq projectile-indexing-method 'native)
(define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)

;; Helm
(helm-mode 1)
(helm-autoresize-mode 1)

;; Magit
(global-set-key (kbd "C-x g") 'magit-status)
(global-set-key (kbd "s-g") 'magit-dispatch-popup)

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
(load-theme 'solarized-dark t)
(set-face-attribute 'default nil :height 130)

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

;; Smart line with cow powers!
(setq sml/no-confirm-load-theme t)
(sml/setup)
(setq sml/theme 'respectful)

;; ==============================================================================
;; RTags
;; ==============================================================================
(require 'rtags)

(define-key c-mode-base-map (kbd "M-.") (function rtags-find-symbol-at-point))
(define-key c-mode-base-map (kbd "M-,") (function rtags-find-references-at-point))
(define-key c-mode-base-map (kbd "M-;") (function rtags-find-file))
(define-key c-mode-base-map (kbd "C-.") (function rtags-find-symbol))
(define-key c-mode-base-map (kbd "C-,") (function rtags-find-references))
