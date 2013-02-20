(setq x-select-enable-clipboard t)

;;load path set to folder ~/emacs for .el files
(add-to-list 'load-path "~/emacs/modes")
(add-to-list 'load-path "~/emacs/plugins")
(add-to-list 'load-path "~/tools/cedet-1.1/common")
;; Apache PIG mode
(load "pig-mode.el")

;; Google GO mode
(load "go-mode.el")

;; GTUGS/Global
(load "gtags.el")
(setq gtags-select-buffer-single t)

;; CEDET
(load "cedet.el")
(require 'cedet)
(require 'semantic)
(setq semanticdb-default-save-directory "~/.semantic")
(semantic-load-enable-code-helpers)      ; Enable prototype help and smart completion
(semantic-load-enable-excessive-code-helpers)
(global-srecode-minor-mode 1)            ; Enable template insertion menu
(global-semantic-tag-folding-mode 1)
(global-semantic-decoration-mode -1)
(setq-mode-local c++-mode
                 semanticdb-find-default-throttle
                 '(project unloaded recursive)) ;; Remove 'system from the throttle
(setq-mode-local c-mode
                 semanticdb-find-default-throttle
                 '(project unloaded recursive)) ;; Remove 'system from the throttle


;; Plugins
(require 'ido)
(ido-mode t)
(setq
 ido-ignore-buffers '("\\` " "^\*Mess" "^\*Back" ".*Completion" "^\*Ido" "^\*GTAGS")
 ido-enable-flex-matching t         ; enable fuzzy matching
 ido-max-prospects 6                ; don't spam my minibuffer
 ido-confirm-unique-completion t )  ; wait for RET, even with unique completion

;; Show whitespaces
(require 'show-wspace)

;;
;; nxml mode
;;
(add-to-list 'auto-mode-alist
             (cons (concat "\\." (regexp-opt '("xml" "xsd" "rng" "xslt" "svg" "rss") t) "\\'")
                   'nxml-mode))
(fset 'xml-mode 'nxml-mode)
(fset 'html-mode 'nxml-mode)


;; Dont show the GNU splash screen
(setq inhibit-startup-message t)

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
(toggle-show-tabs-show-ws)
(toggle-show-trailing-whitespace-show-ws)

(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))

(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(column-number-mode t)
 '(inhibit-startup-screen t)
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
