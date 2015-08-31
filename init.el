;; init.el
;; Initialise all in this file. This is the top of all configuration files.

;; initial package.el
(require 'package)
(add-to-list 'package-archives
	     '("melpa" . "http://melpa.org/packages/") t)
(package-initialize)


;; Load my custom configurations
(add-to-list 'load-path "~/.emacs.d/my_custom")

(require 'naive_configurations)
(require 'copy-paste-x)
(require 'copy-cut-one-line)
(require 'doc-mode)
(add-hook 'c-mode-common-hook 'doc-mode)
(add-hook 'c-mode-hook 'doc-mode)
(add-hook 'c++-mode-hook 'doc-mode)

;; Auto install all the packages which I mentioned.
(defconst demo-packages

  '(company
    ggtags
    helm
    helm-swoop
    yasnippet
    iedit
    helm-gtags
    comment-dwim-2
    smartparens
    zygospore
    undo-tree
    git-gutter
    column-marker
    jedi
    company-jedi
    markdown-mode
    powerline
    diminish
    sr-speedbar
    company-c-headers
    ))

(defun install-packages ()
  "Install all required packages."
  (interactive)
  (unless package-archive-contents
    (package-refresh-contents))
  (dolist (package demo-packages)
    (unless (package-installed-p package)
      (package-install package))))

(install-packages)


;; Load packages' custom configurations
(add-to-list 'load-path "~/.emacs.d/list")

(require 'config-helm)
(require 'config-helm-swoop)
(require 'config-helm-gtags)
(require 'config-cedet)
(require 'config-company)
(require 'config-git-gutter)


;; Powerline setup
(require 'powerline)
(powerline-default-theme)

(set-face-attribute 'mode-line nil
                    :foreground "Black"
                    ;;:background "DarkOrange"
                    :box nil)



;; Fix iedit bug in Mac
(define-key global-map (kbd "C-c ;") 'iedit-mode)


;; Speedbar
(require 'speedbar)
(define-key global-map (kbd "C-c o") 'sr-speedbar-toggle)

;; Package: yasnippet
(require 'yasnippet)
(yas-global-mode 1)


;; Enable comment-dwim-2
(global-set-key (kbd "M-;") 'comment-dwim-2)


;; Package: smartparens
(require 'smartparens-config)
(setq sp-base-key-bindings 'paredit)
(setq sp-autoskip-closing-pair 'always)
(setq sp-hybrid-kill-entire-symbol nil)
(sp-use-paredit-bindings)

(show-smartparens-global-mode +1)
(smartparens-global-mode 1)


;; Undo tree setup
(require 'undo-tree)
(global-undo-tree-mode 1)


;; Package zygospore
(global-set-key (kbd "C-x 1") 'zygospore-toggle-delete-other-windows)


;; Column-marker
(require 'column-marker)
(add-hook 'c++-mode-hook (lambda () (interactive) (column-marker-1 80)))
(add-hook 'c-mode-hook (lambda () (interactive) (column-marker-1 80)))
(global-set-key [?\C-c ?m] 'column-marker-1)


;; ------------------------------------------
;; *** Codes below are about Python env configurations
;; ------------------------------------------

;; Company-jedi & jedi
(add-hook 'python-mode-hook 'jedi:setup)
(defun my/python-mode-hook ()
  (add-to-list 'company-backends 'company-jedi))
(add-hook 'python-mode-hook 'my/python-mode-hook)

;; Initialize jedi:install-server
(setq dir "~/.emacs.d/.python-environments")
(defun funp(dir fn)
  (if (not (file-exists-p dir)) (funcall fn))
  )
(funp dir 'jedi:install-server)


;; ------------------------------------------
;; *** Codes below are about Markdown env configurations
;; ------------------------------------------
(autoload 'markdown-mode "markdown-mode"
  "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.text\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))




;; Hide some useless mode in mode-line
(require 'hide-minor-mode)




