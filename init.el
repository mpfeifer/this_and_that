;; [ header

;; Info  : Emacs initialization file
;; Author: Matthias
;; Date  : 20.06.2014
;; ]

;; [ personal information

(setq user-full-name "Matthias Pfeifer"
      user-mail-address "mpfeifer77@gmail.com")
;; ]

;; [ custom set variables
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-archives (quote (("melpa" . "http://melpa.org/packages/"))))
 '(scroll-conservatively 65000)
 '(tool-bar-mode nil)
 '(truncate-lines t))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(calendar-today ((t (:underline "red"))))
 '(speedbar-highlight-face ((t (:background "burlywood")))))
;; ]

;; [ load-path

(add-to-list 'load-path "~/.emacs.d/elisp/")
(add-to-list 'load-path "~/.emacs.d/")

;; ]

;; [ packaging

(require 'package)

(package-initialize)

(defun mp/install-package (package)
  "Install PACKAGE if it has not yet been installed."
  (interactive "SPackage:")
  (unless (package-installed-p package)
    (package-install package)))

(mp/install-package 'use-package)

(require 'use-package)

;; ]

;; [ General Emacs Behaviour

(setq delete-exited-processes t)
(setq yes-or-no-p 'y-or-n-p)

;; ]

;; [ appearence
(when window-system
  (tool-bar-mode -1)
  (menu-bar-mode 1)
  (scroll-bar-mode -1))

;; do not show startup screen
(setq inhibit-startup-screen t)

(require 'hl-line)
(setq global-hl-line-mode t)
(hl-line-mode)

;; [ auto compile

;; buffers are only auto-compiled if a corresponding elc file
;; already exists

(mp/install-package 'auto-compile)
(use-package auto-compile
  :config
  (setq auto-compile-display-buffer nil)
  (setq auto-compile-mode-line-counter t)
  (auto-compile-on-save-mode)
  (auto-compile-on-load-mode -1)
  )

;; ]

;; [ backups

(setq backup-directory-alist '(("." . "~/.emacs.d/backups")))
(setq delete-old-versions t)
(setq version-control t)
(setq vc-make-backup-files t)
(setq auto-save-file-name-transforms '((".*" "~/.emacs.d/auto-save-list/" t)))

;; ]

;; [ imenu

(setq imenu-auto-rescan t)
(setq imenu-max-items 35)
(setq imenu-use-popup-menu nil)
(setq imenu-space-replacement "-")

(defun mp/setup-imenu-for-dotemacs ()
  "This function applies the specific setup for imenu when editing init.el."
  (interactive)
  (setq imenu-generic-expression
	'(
	  (nil "^;; \\[ \\(.*\\)" 1)
	  )
	)
  )
;; ]

;; [ ibuffer

(require 'ibuffer)

(global-set-key (kbd "C-x C-b") 'ibuffer)

;; Do not show empty filter groups.
(setq ibuffer-show-empty-filter-groups nil)

(defun ibuffer-previous-line ()
  "Move point to last buffer when going before first buffer."
  (interactive)
  (previous-line)
  (if (<= (line-number-at-pos) 2)
      (goto-line (- (count-lines (point-min) (point-max)) 2))
    )
  )

(defun ibuffer-next-line ()
  "Wrap point to first buffer when goint after last buffer."
  (interactive)
  (next-line)
  (if (>= (line-number-at-pos) 
	  (- (count-lines (point-min) (point-max)) 1))
      (goto-line 3)
    )
  )

(add-hook 'ibuffer-mode-hook
	  '(lambda ()
	     (ibuffer-auto-mode 1)
	     (ibuffer-switch-to-saved-filter-groups "standard")
	     (define-key ibuffer-mode-map (kbd "p") 'ibuffer-previous-line)
	     (define-key ibuffer-mode-map (kbd "n") 'ibuffer-next-line)  
	     )
	  )
;; ]

;; [ emacs lisp mode
(defun mp/dotemacs-mode-hook ()
  (local-set-key (kbd "C-S-n") 'forward-paragraph)
  (local-set-key (kbd "C-S-p") 'backward-paragraph)
  (local-set-key (kbd "C-*") 'imenu)
  (make-local-variable 'paragraph-start)
  (setq paragraph-start ";; [")
  (make-local-variable 'paragraph-separate)
  (setq paragraph-separate ";; ]")
  )

(defun byte-compile-current-buffer ()
  (interactive)
  (byte-compile-file (buffer-file-name))
  )

(defun mp/emacs-lisp-mode-hook ()
  (eldoc-mode 1)
  (when (string= (buffer-name) "init.el")
    (mp/setup-imenu-for-dotemacs)
    (mp/dotemacs-mode-hook)
    (local-set-key (kbd "C-c C-c") 'byte-compile-current-buffer)
    )
  ) 

(add-hook 'emacs-lisp-mode-hook 'mp/emacs-lisp-mode-hook)
;; ]

;; [ emacs lisp slime navigation
(mp/install-package 'elisp-slime-nav)
(use-package elisp-slime-nav
  :config
  (add-hook 'emacs-lisp-mode-hook 'turn-on-elisp-slime-nav-mode)
  )
;; ]

;; [ save history
(setq savehist-file "~/.emacs.d/savehist")
(savehist-mode 1)
(setq history-length t)
(setq history-delete-duplicates t)
(setq savehist-save-minibuffer-history 1)
(setq savehist-additional-variables
      '(kill-ring
        search-ring
        regexp-search-ring))
;; ]

;; [ helm
;;
;; helm helps you steer into the right direction
(mp/install-package 'helm)
(mp/install-package 'helm-swoop)

(use-package helm
  :init
  (progn
    (require 'helm-config)
    (setq helm-candidate-number-limit 25)
    ;; From https://gist.github.com/antifuchs/9238468
    (setq helm-idle-delay 0.0 ; update fast sources immediately (doesn't).
	  helm-input-idle-delay 0.01  ; this actually updates things
	  ;; reeeelatively quickly.
	  helm-truncate-lines t
	  helm-quick-update t
	  helm-M-x-requires-pattern nil
	  helm-quick-update t
	  helm-split-window-default-side (quote right)
	  helm-swoop-split-direction 'split-window-horizontally
	  )
    ;; this one hooks into 'completing-read' and 'read-file-name'
    ;; to enable helm-style narrowing
    (helm-mode 1))
  :config
  (progn
    (define-key helm-map (kbd "C-S-n") 'helm-next-source)
    (define-key helm-map (kbd "C-S-p") 'helm-previous-source)
    ;;    (add-to-list 'helm-completing-read-handlers-alist '(find-file . ido))
    (global-set-key (kbd "C-s") 'helm-swoop)
    )
  
  :bind (("C-c h" . helm-mini))
  )
;; ]

;; [ expand region

(mp/install-package 'auto-compile)
(use-package expand-region
  :bind
  ("C-v" . er/expand-region)
  ("C-S-v" . er/contract-region)
  )
;; ]

;; [ highlight parenthesis
(mp/install-package 'highlight-parentheses)
(use-package highlight-parentheses
  :config
  (setq hl-paren-colors (list "red" "deep sky blue" "lawn green" "yellow"))
  (global-highlight-parentheses-mode 1)
  )
;; ]

;; [ yasnippet

(mp/install-package 'yasnippet)
(use-package yasnippet
  :config
  (setq yas-snippet-dirs '("~/.emacs.d/yasnippets/"))  
  (yas-global-mode 1)
  )

;; ]

;; [ auto complete
(mp/install-package 'auto-complete)
(use-package auto-complete
  :init
  (require 'auto-complete-config)
  :config
  (ac-config-default)
  (define-key ac-mode-map (kbd "C-M-s") 'ac-isearch)
  (setq ac-use-quick-help nil)
  (defun ac-emacs-lisp-mode-setup ()
    (setq ac-sources '(ac-source-features ;; collects 'require-able features from the file sytem
		       ac-source-functions
		       ac-source-yasnippet
		       ac-source-variables
		       ac-source-symbols
		       ac-source-words-in-same-mode-buffers)
	  )
    )
  )
;; ]

;; [ ace jump mode
(mp/install-package 'ace-jump-mode)
(use-package ace-jump-mode
  :bind ("C-S-j" . ace-jump-mode)
  )
;; ]

;; [ js2 mode

(mp/install-package 'js2-mode)
(use-package js2-mode
  :mode "\\.js\\'"
  )

;; ]

;; [ info browser

(defvar mp/info-index 0 "Index for info buffers. Used to make buffer names unique.")

(defun mp/open-info-browser(node)
  "Open a new frame and start a new info browser."
  (interactive (list nil))
  (select-frame (make-frame))
  (info node (format "Info<%d>" mp/info-index))
  (setq mp/info-index (+ 1 mp/info-index))
  )

(global-set-key (kbd "<f1>") 'mp/open-info-browser)

(defun mp/Info-mode-hook ()
  "Personal info mode hook."
  (define-key Info-mode-map (kbd "C-s") 'isearch-forward-regexp)
  )

(add-hook 'Info-mode-hook 'mp/Info-mode-hook)

;; ]

;; [ keybindings

(global-set-key (kbd "<f2>") 'make-frame)
(global-set-key (kbd "<f3>") 'delete-frame)

;; ]

;; [ calendar

(global-set-key (kbd "<f4>") 'calendar)
(setq calendar-mark-holidays-flag t)
(setq european-calendar-style t)
(setq calendar-latitude 50.8) ;; 50,840440
(setq calendar-longitude 6.1) ;;  6,116797
(setq calendar-location-name "Bardenberg")

;; german weekdays
(setq calendar-day-name-array
      ["Sonntag" "Montag" "Dienstag" "Mittwoch"
       "Donnerstag" "Freitag" "Samstag"])

;; german months
(setq calendar-month-name-array
      ["Januar" "Februar" "März" "April" "Mai" "Juni"
       "Juli" "August" "September" "Oktober" "November" "Dezember"])

;; german seasons
(setq solar-n-hemi-seasons
      '("Frühlingsanfang" "Sommeranfang" "Herbstanfang" "Winteranfang"))

;; german holidays
(setq holiday-general-holidays
      '((holiday-fixed 1 1 "Neujahr")
	(holiday-fixed 5 1 "1. Mai")
	(holiday-fixed 10 3 "Tag der Deutschen Einheit")))

(setq holiday-christian-holidays
      '((holiday-float 12 0 -4 "1. Advent")
	(holiday-float 12 0 -3 "2. Advent")
	(holiday-float 12 0 -2 "3. Advent")
	(holiday-float 12 0 -1 "4. Advent")
	(holiday-fixed 12 25 "1. Weihnachtstag")
	(holiday-fixed 12 26 "2. Weihnachtstag")
	(holiday-fixed 1 6 "Heilige Drei Könige")
	;; Date of Easter calculation taken from holidays.el.
	(let* ((century (1+ (/ displayed-year 100)))
	       (shifted-epact (% (+ 14 (* 11 (% displayed-year 19))
				    (- (/ (* 3 century) 4))
				    (/ (+ 5 (* 8 century)) 25)
				    (* 30 century))
				 30))
	       (adjusted-epact (if (or (= shifted-epact 0)
				       (and (= shifted-epact 1)
					    (< 10 (% displayed-year 19))))
				   (1+ shifted-epact)
				 shifted-epact))
	       (paschal-moon (- (calendar-absolute-from-gregorian
				 (list 4 19 displayed-year))
				adjusted-epact))
	       (easter (calendar-dayname-on-or-before 0 (+ paschal-moon 7))))
	  (filter-visible-calendar-holidays
	   (mapcar
	    (lambda (l)
	      (list (calendar-gregorian-from-absolute (+ easter (car l)))
		    (nth 1 l)))
	    '(
	      ;;(-48 "Rosenmontag")
	      ( -2 "Karfreitag")
	      ( 0 "Ostersonntag")
	      ( +1 "Ostermontag")
	      (+39 "Christi Himmelfahrt")
	      (+49 "Pfingstsonntag")
	      (+50 "Pfingstmontag")
	      (+60 "Fronleichnam")))))
	(holiday-fixed 8 15 "Mariä Himmelfahrt")
	(holiday-fixed 11 1 "Allerheiligen")
	(holiday-float 11 3 1 "Buß- und Bettag" 16)
	(holiday-float 11 0 1 "Totensonntag" 20)))

;;(setq calendar-holidays
;;      (append general-holidays local-holidays other-holidays
;;	      christian-holidays solar-holidays))

;; ]

;; [ session management

;; save and restore open buffers
(desktop-save-mode nil)

;; [ org mode
(add-hook 'org-mode-hook 'auto-fill-mode)
;; ]

;; [ prodigy
(use-package prodigy
  :config
  (prodigy-define-service
    :name "Echo Server"
    :command "mvn"
    :args '("exec:java")
    :cwd "/home/matthias/java/NetClients/"
    )
  (global-set-key (kbd "<f5>") 'prodigy)
  )
;; ]

;; [ speedbar

(mp/install-package 'sr-speedbar)

(use-package sr-speedbar
  :config
  (global-set-key (kbd "<f6>") 'sr-speedbar-toggle)
  )

;; ]

;; [ tags

(setq tags-revert-without-query t)

;; ]

;; [ perl mode

(fset 'perl-mode 'cperl-mode)

(add-hook 'cperl-mode-hook '(lambda ()
			      (interactive)
			      (local-set-key (kbd "C-h f") 'cperl-perldoc-at-point)))

;; ]

;; [ hungry delete mode
;;
;; This mode makes backspace and C-d erase all consecutive whitespace
;; instead of one.

(mp/install-package 'hungry-delete)
(require 'hungry-delete)
(global-hungry-delete-mode)

;; ]

;; [ ansi-term

(defconst ansi-term-window-height -15 "Height of ansi term window")
(defconst ansi-term-buffer-name "*ansi-term*")
(defconst ansi-term-shell-command "/bin/bash")

(defun start-bash-or-select-existing ()
  "If a buffer named *ansi-term* exists make it current.
Otherwise just call (ansi-term \"/bin/bash\")"
  (interactive)
  (let ((termbuffer (get-buffer ansi-term-buffer-name)))
    (if (bufferp termbuffer)
	(set-window-buffer nil termbuffer)
      (ansi-term ansi-term-shell-command))))

(defun start-bash-in-ansi-term ()
  "Run bash from emacs ansi-term with some context sensitivity.
If the current windows buffer is called *ansi-term* delete the window.
If it's the only window in the frame, also close the frame.
If the current windows buffer is not called *ansi-term* and the current 
window shows exactly one window then split the window and either show
existing *ansi-term* buffer or execute a new shell in ansi-term. If the
current frame shows more then one window open a new frame and open an
existing *ansi-term* there (or execute a new shell in ansi-term)."
  (interactive)
  (let ((numWindows (length (window-list))))
    (if (string= (buffer-name) ansi-term-buffer-name)
	(if (eq 1 numWindows)
	    (delete-frame)
	  (delete-window))
      (if (eq 1 numWindows)
	  (let ((newwindow (split-window nil ansi-term-window-height)))
	    (select-window newwindow)
	    (start-bash-or-select-existing)	
	    )
	(progn
	  (select-frame (make-frame))
	  (start-bash-or-select-existing))))))

(global-set-key (kbd "<f7>") 'start-bash-in-ansi-term)

;; ]
