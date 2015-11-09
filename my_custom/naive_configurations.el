;; naive_configuration.el
;; This file is used to configure emacs' basic features which are built in.


;; Unlike other Lisps, GNU Emacs Lisp does not call the garbage collector when the free list is empty.
;; Instead, it simply requests the operating system to allocate more storage,
;; and processing continues until gc-cons-threshold bytes have been used.
(setq gc-cons-threshold 100000000)


;; This variable, if non-nil, inhibits the startup screen.
;; In that case, Emacs typically displays the *scratch* buffer; but see initial-buffer-choice, below.
(setq inhibit-startup-message t)


;; Prevent type y from yes
(defalias 'yes-or-no-p 'y-or-n-p)


;; Available C style:
;; “gnu”: The default style for GNU projects
;; “k&r”: What Kernighan and Ritchie, the authors of C used in their book
;; “bsd”: What BSD developers use, aka “Allman style” after Eric Allman.
;; “whitesmith”: Popularized by the examples that came with Whitesmiths C, an early commercial C compiler.
;; “stroustrup”: What Stroustrup, the author of C++ used in his book
;; “ellemtel”: Popular C++ coding standards as defined by “Programming in C++, Rules and Recommendations,” Erik Nyquist and Mats Henricson, Ellemtel
;; “linux”: What the Linux developers use for kernel development
;; “python”: What Python developers use for extension modules
;; “java”: The default style for java-mode (see below)
;; “user”: When you want to define your own style
(setq
 c-default-style "linux" ;; set style to "linux"
)

(global-set-key (kbd "RET") 'newline-and-indent)  ; automatically indent when press RET

;; activate whitespace-mode to view all whitespace characters
(global-set-key (kbd "C-c w") 'whitespace-mode)

;; show unncessary whitespace that can mess up your diff
(add-hook 'prog-mode-hook (lambda () (interactive) (setq show-trailing-whitespace 1)))

;; enable line number
(global-linum-mode t)
(setq linum-format "%d ")

;; enable column number
(column-number-mode t)

;; display time
(display-time-mode t)
(setq display-time-24hr-format t)

;; Enable mouse
(require 'mouse)
(xterm-mouse-mode t)
(defun track-mouse (e)) 
(setq mouse-sel-mode t)

(unless window-system
  (xterm-mouse-mode 1)
  (global-set-key [mouse-4] (lambda ()
                              (interactive)
                              (scroll-down 1)))
  (global-set-key [mouse-5] (lambda ()
                              (interactive)
                              (scroll-up 1))))

;; moving between emacs windows
(progn
  (define-key input-decode-map "\e[1;2D" [S-left])
  (define-key input-decode-map (kbd "M-[ 1 ; 2 C") [S-right])
  (define-key input-decode-map (kbd "M-[ 1 ; 2 B")[S-down])
  (define-key input-decode-map "\e[1;2A" [S-up])
  (define-key input-decode-map "\e[1;6A" [S-C-up])
  (define-key input-decode-map "\e[1;6B" [S-C-down]))
(windmove-default-keybindings)

(provide 'naive_configurations)

;; Prevent emacs to create ~ files
(setq make-backup-files nil)
