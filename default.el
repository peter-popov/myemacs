;; Emacs config
;; Peter Popov

;; ==============================================================================
;; Modes
(add-to-list 'load-path "~/emacs/modes")
(load "pig-mode.el") ; Apache PIG
(load "go-mode.el") ; Google go language


;; ==============================================================================
;; Package managment
(require 'package)
(package-initialize)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)

;; Auto load plugins on start
;; Taken from here: http://batsov.com/articles/2012/02/19/package-management-in-emacs-the-good-the-bad-and-the-ugly/
(defvar prelude-packages
  '(python ggtags yasnippet auto-complete member-function autopair )
  "A list of packages to ensure are installed at launch.")

(defun prelude-packages-installed-p ()
  (loop for p in prelude-packages
        when (not (package-installed-p p)) do (return nil)
        finally (return t)))

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
;;
;; auto complete mod
;; TURNED OFF BECAUSE OF ISSUE WITH YAS INTERGATION, see:
;;  http://www.kurup.org/blog/2012/10/15/emacs-autocomplete-stumbles-on-yasnippet/
;;; should be loaded after yasnippet so that they can work together
;;(require 'auto-complete-config)
;;(add-to-list 'ac-dictionary-directories "~/.emacs.d/ac-dict")
;;(delq 'ac-source-yasnippet ac-sources)
;;(ac-config-default)
;;; set the trigger key so that it can work together with yasnippet on tab key,
;;; if the word exists in yasnippet, pressing tab will cause yasnippet to
;;; activate, otherwise, auto-complete will
;;(ac-set-trigger-key "TAB")
;;(ac-set-trigger-key "<tab>")
;;
;; member function
(require 'member-function)
(setq mf--source-file-extension "cpp")



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

;; C mode
(add-hook 'c-mode-hook '(lambda()
                          (setq indent-tabs-mode nil)
                          )
)

;; C++ mode
(add-hook 'c++-mode-hook '(lambda()
                            (setq indent-tabs-mode nil)
                            )
)

(add-hook 'c-mode-hook '(lambda ()
                          (gtags-mode t)
                          )
)

(add-hook 'c++-mode-hook '(lambda ()
                            (gtags-mode t)
                            )
)

;; Code formating
(setq c-default-style "k&r"
      c-basic-offset 4)
(setq-default indent-tabs-mode nil)
(setq-default show-trailing-whitespace t)
;;(toggle-show-tabs-show-ws)
;;(toggle-show-trailing-whitespace-show-ws)

(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))

(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(column-number-mode t)
 '(initial-buffer-choice nil)
 '(initial-scratch-message nil)
 '(remote-shell-program "bash"))


;; Delete trailing whitespaces
(add-hook 'before-save-hook 'delete-trailing-whitespace)


(defun set-frame-size-according-to-resolution ()
  (interactive)
  (if window-system
  (progn
    ;; use 120 char wide window for largeish displays
    ;; and smaller 80 column windows for smaller displays
    ;; pick whatever numbers make sense for you
    (if (> (x-display-pixel-width) 1280)
        (add-to-list 'default-frame-alist (cons 'width 120))
      (add-to-list 'default-frame-alist (cons 'width 80)))
    ;; for the height, subtract a couple hundred pixels
    ;; from the screen height (for panels, menubars and
    ;; whatnot), then divide by the height of a char to
    ;; get the height we want
    (add-to-list 'default-frame-alist
                 (cons 'height (/ (+ (x-display-pixel-height) 160) (frame-char-height)))))))

(set-frame-size-according-to-resolution)

(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "white" :foreground "black" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 94 :width normal :foundry "unknown" :family "Liberation Mono"))))
 '(font-lock-comment-face ((((class color) (min-colors 88) (background light)) (:foreground "PaleGreen4"))))
 '(font-lock-string-face ((((class color) (min-colors 88) (background light)) (:foreground "skyblue3")))))

(put 'scroll-left 'disabled nil)


;; Set M-1 as e key for goto line
(global-unset-key "\M-1")
(global-set-key "\M-1" 'goto-line)

;; Set C-tab to find other file
(global-unset-key [C-tab])
(global-set-key [C-tab] 'ff-find-other-file)
