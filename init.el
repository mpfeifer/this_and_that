;;;; header

;; Info  : Emacs initialization file
;; Author: Matthias
;; Date  : 20.06.2014
;; ---------------------------------------------------------

;;;; personal information
(setq user-full-name "Matthias Pfeifer"
      user-mail-address "mpfeifer77@gmail.com")
;; ---------------------------------------------------------

;;;; load-path
(add-to-list 'load-path "~/.emacs.d/elisp/")
(add-to-list 'load-path "~/.emacs.d/")
;; ---------------------------------------------------------

;;;; packaging
(require 'package)

(package-initialize)

(defun mp/install-package (package &optional repository)
  "Install PACKAGE if it has not yet been installed. If REPOSITORY is specified, use that."
  (interactive "SPackage:")
  (unless (package-installed-p package)
    (let ((package-archives (if repository
                                (list (assoc repository package-archives))
                              package-archives)))
      (package-install package)
      )
    )
  )

(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)

(mp/install-package 'use-package)
(require 'use-package)
;; ---------------------------------------------------------

;;;; General Emacs Behaviour

(setq delete-exited-processes t)
(setq yes-or-no-p 'y-or-n-p)

;; ---------------------------------------------------------

;;;; appearence
(when window-system
  (tool-bar-mode -1)
  (menu-bar-mode 1)
  (scroll-bar-mode -1))

(require 'toggle-theme)

;; do not show startup screen
(setq inhibit-startup-screen t)

;; handy keybinding
(global-set-key [f12] 'mp/theme-toggle) 

;; highlight current line
(require 'hl-line)
(setq global-hl-line-mode t)
(hl-line-mode)

;;;; backups
(setq backup-directory-alist '(("." . "~/.emacs.d/backups")))
(setq delete-old-versions t)
(setq version-control t)
(setq vc-make-backup-files t)
(setq auto-save-file-name-transforms '((".*" "~/.emacs.d/auto-save-list/" t)))
;; ---------------------------------------------------------

;;;; imenu
(setq imenu-auto-rescan t)
(setq imenu-max-items 35)
(setq imenu-use-popup-menu nil)
(setq imenu-space-replacement "-")

(defun mp/setup-imenu-for-dotemacs ()
  "This function applies the specific setup for imenu when editing init.el."
  (interactive)
  (setq imenu-generic-expression
	'(
	  (nil "^;;;; \\(.*\\)" 1)
	  )
	)
  )
;; ---------------------------------------------------------

;;;; ibuffer
(require 'ibuffer)

(global-set-key (kbd "C-x C-b") 'ibuffer)

;; Filter group
(setq ibuffer-saved-filter-groups
      '(("standard"
	 ("Projekt - Memorie" (filename . "/memory/"))
	 ("Project - Java" ( filename . "/matthias/java/"))
	 ("Dotemacs" ( filename . "/.emacs.d/" ))
	 ("Helm" (name . "helm"))
	 ("Help" (or (mode . help-mode)
		     (mode . apropos-mode)
		     (mode . Info-mode)))
	 ("Markup" (or (mode . html-mode)
		       (mode . css-mode)
		       (mode . xml-mode)))
	 ("Org" (mode . org-mode))
	 ("Perl" (mode . perl-mode))
	 ("*Internal*" (name . "^\*"))
	 )
	)
      )

;; Do not show empty filter groups.
(setq ibuffer-show-empty-filter-groups nil)

(setq mp/ibuffer-collapsed-groups (list "Helm" "*Internal*"))

(defadvice ibuffer (after collapse-helm)
  (dolist (group mp/ibuffer-collapsed-groups)
    (progn
      (goto-char 1)
      (when (search-forward (concat "[ " group " ]") (point-max) t)
	(progn
	  (move-beginning-of-line nil)
	  (ibuffer-toggle-filter-group)
	  )
	)
      )
    )
  (goto-char 1)
  (search-forward "[ " (point-max) t)
  )

(ad-activate 'ibuffer) 

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
	     (define-key ibuffer-mode-map (kbd "<up>") 'ibuffer-previous-line)
	     (define-key ibuffer-mode-map (kbd "C-p") 'ibuffer-previous-line)
	     (define-key ibuffer-mode-map (kbd "<down>") 'ibuffer-next-line)
	     (define-key ibuffer-mode-map (kbd "C-n") 'ibuffer-next-line)  
	     )
	  )
;; ---------------------------------------------------------

;;;; emacs lisp mode
(defun mp/dotemacs-mode-hook ()
  (local-set-key (kbd "C-S-n") 'forward-paragraph)
  (local-set-key (kbd "C-S-p") 'backward-paragraph)
  (local-set-key (kbd "C-*") 'imenu)
  (make-local-variable 'paragraph-start)
  (setq paragraph-start ";;;;")
  (make-local-variable 'paragraph-separate)
  (setq paragraph-separate ";; ---------------------------------------------------------")
  )

(defun byte-compile-current-buffer ()
  (interactive)
  (byte-compile-file (buffer-file-name))
  ;;  (select-window (
  )

(defun mp/emacs-lisp-mode-hook ()
  (eldoc-mode 1)
  (when (string= (buffer-name) "init.el")
    (mp/setup-imenu-for-dotemacs)
    (mp/dotemacs-mode-hook)
    (local-set-key (kbd "C-c C-c") 'byte-compile-current-buffer)
    (diminish 'elisp-slime-nav-mode)
    (diminish 'eldoc-mode)
    (diminish 'helm-mode)
    (diminish 'auto-complete-mode)
    (diminish 'yas-minor-mode)
    (diminish 'highlight-parentheses-mode)
    )
  ) 

(add-hook 'emacs-lisp-mode-hook 'mp/emacs-lisp-mode-hook)
;; ---------------------------------------------------------

;;;; emacs lisp slime navigation
(mp/install-package 'elisp-slime-nav)
(use-package elisp-slime-nav
  :config
  (add-hook 'emacs-lisp-mode-hook 'turn-on-elisp-slime-nav-mode)
  )
;; ---------------------------------------------------------

;;;; save history
(setq savehist-file "~/.emacs.d/savehist")
(savehist-mode 1)
(setq history-length t)
(setq history-delete-duplicates t)
(setq savehist-save-minibuffer-history 1)
(setq savehist-additional-variables
      '(kill-ring
        search-ring
        regexp-search-ring))
;; ---------------------------------------------------------

;;;; helm
;;
;; helm helps you steer into the right direction
(mp/install-package 'helm)
(use-package helm
  :init
  (progn
    (require 'helm-config)
    (require 'helm-projectile)
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
    (global-set-key (kbd "C-S-s") 'helm-projectile)    
    )
  
  :bind (("C-c h" . helm-mini))
  )
;; ---------------------------------------------------------

;;;; custom set variables
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes (quote ("1e7e097ec8cb1f8c3a912d7e1e0331caeed49fef6cff220be63bd2a6ba4cc365" "09f27115fd67d30db7ae52b645c8ae9c60159f2f95327ca8610328e46445c6b1" "146d24de1bb61ddfa64062c29b5ff57065552a7c4019bee5d869e938782dfc2a" "6c9ddb5e2ac58afb32358def7c68b6211f30dec8a92e44d2b9552141f76891b3" "cc0dbb53a10215b696d391a90de635ba1699072745bf653b53774706999208e3" "255de8fde9647c6a0fbd0c9877ee02b612f8ed3cf5c78162b83ee68dfef05952" "7ed6913f96c43796aa524e9ae506b0a3a50bfca061eed73b66766d14adfa86d1" "86f4407f65d848ccdbbbf7384de75ba320d26ccecd719d50239f2c36bec18628" "7dd515d883520286fc8936ce32381fb01b978d0d7cfb6fe56f7f55d8accbf63a" "fc2782b33667eb932e4ffe9dac475f898bf7c656f8ba60e2276704fabb7fa63b" "769bb56fb9fd7e73459dcdbbfbae1f13e734cdde3cf82f06a067439568cdaa95" "96b023d1a6e796bab61b472f4379656bcac67b3af4e565d9fb1b6b7989356610" "297063d0000ca904abb446944398843edaa7ef2c659b7f9087d724bf6f8c1d1f" "bc89fda3d232a3daa4eb3a9f87d6ffe1272fea46e4cf86686d9e8078e4209e2c" "97a2b10275e3e5c67f46ddaac0ec7969aeb35068c03ec4157cf4887c401e74b1" "c4e6fe8f5728a5d5fd0e92538f68c3b4e8b218bcfb5e07d8afff8731cc5f3df0" "d8a4e35ee1b219ccb8a8c15cdfed687fcc9d467c9c8b9b93bd25229b026e4703" "f0ea6118d1414b24c2e4babdc8e252707727e7b4ff2e791129f240a2b3093e32" "0744f61189c62ed6d1f8fa69f6883d5772fe8577310b09e623c62c040f208cd4" "0795e2c85394140788d72d34969be4acb305e4a54149e7237787d9df27832fbb" "0d19ff470ad7029d2e1528b3472ca2d58d0182e279b9ab8acd65e2508845d2b6" "50edb7914e8d369bc03820d2dcde7e74b7efe2af5a39511d3a130508e2f6ac8f" "8b231ba3e5f61c2bb1bc3a2d84cbd16ea17ca13395653566d4dfbb11feaf8567" "0369b452b9045611cb159268acab0aac6b0319b591d455b60b3b82c95929ffb0" "f5486211de3961667b809a4c25086cb096b1c6b82a0e50a0bd28eead29ce7cc7" "c739f435660ca9d9e77312cbb878d5d7fd31e386a7758c982fa54a49ffd47f6e" "2affb26fb9a1b9325f05f4233d08ccbba7ec6e0c99c64681895219f964aac7af" "65ae93029a583d69a3781b26044601e85e2d32be8f525988e196ba2cb644ce6a" "2b5aa66b7d5be41b18cc67f3286ae664134b95ccc4a86c9339c886dfd736132d" "7a9f392481b6e2fb027ab9d8053ab36c0f23bf5cc1271206982339370d894c74" "b9183de9666c3a16a7ffa7faaa8e9941b8d0ab50f9aaba1ca49f2f3aec7e3be9" default)))
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
;; ---------------------------------------------------------

;;;; auto compile
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
;; ---------------------------------------------------------

;;;; expand region
(mp/install-package 'auto-compile)
(use-package expand-region
  :bind
  ("C-v" . er/expand-region)
  ("C-S-v" . er/contract-region)
  )
;; ---------------------------------------------------------

;;;; highlight parenthesis
(mp/install-package 'highlight-parentheses)
(use-package highlight-parentheses
  :config
  (setq hl-paren-colors (list "red" "deep sky blue" "lawn green" "yellow"))
  (global-highlight-parentheses-mode 1)
  )
;; ---------------------------------------------------------

;;;; yasnippet
(mp/install-package 'yasnippet)
(use-package yasnippet
  :config
  (setq yas-snippet-dirs '("~/.emacs.d/yasnippets/"))  
  :idle
  (yas-global-mode 1)
  )
;; ---------------------------------------------------------

;;;; auto complete
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
;; ---------------------------------------------------------

;;;; ace jump mode
(mp/install-package 'ace-jump-mode)
(use-package ace-jump-mode
  :bind ("C-S-j" . ace-jump-mode)
  )
;; ---------------------------------------------------------

;;;; rainbow blocks
;; (mp/install-package 'rainbow-blocks)
;; (use-package rainbow-blocks
;;   :config
;;   (global-rainbow-blocks-mode 1)
;;   )
;; ---------------------------------------------------------

;;;; js2 mode
(mp/install-package 'js2-mode)
(use-package js2-mode
  :mode "\\.js\\'"
  )
;; ---------------------------------------------------------

;;;; info browser
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
;; ---------------------------------------------------------

;;;; keybindings
(global-set-key (kbd "<f2>") 'make-frame)
(global-set-key (kbd "<f3>") 'delete-frame)
;; ---------------------------------------------------------

;;;; calendar
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

;; ---------------------------------------------------------

;;;; session management

;; save and restore open buffers
(desktop-save-mode nil)

;; (use-package elscreen
;;   :config
;;   (elscreen-start))
;; ---------------------------------------------------------

;;;; org mode
;; not much to do here... 
(add-hook 'org-mode-hook 'auto-fill-mode)
;; ---------------------------------------------------------


;;;; projectile

;; C-c p f    projectile-find-file
;; C-u C-c pf purge cache and find file
;; C-c p z    add current file to projectile
(mp/install-package 'projectile)
(use-package projectile
  :config
  (setq projectile-enable-caching t)
  (dolist (hook (list 'java-mode-hook))
    (add-hook hook 'projectile-on)
    )
  )
;; ---------------------------------------------------------

;;;; prodigy
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
;; ---------------------------------------------------------

;;;; dirtree
(mp/install-package 'sr-speedbar)
(use-package sr-speedbar
  :config
  (global-set-key (kbd "<f6>") 'sr-speedbar-toggle)
  )

;; ---------------------------------------------------------

;;;; tags

(setq tags-revert-without-query t)

(require 'tagger)
(tagger/start)

;; ---------------------------------------------------------

;;;; dwim-compile

;; Is there a pom.xml or Makefile in cwd? If so invoke mvn or make with a predefined lifecycle/target
;; Is there a pom.xml in up to five parent directories? If so find the target directory and update the current buffers' file in the target directory.

;; ---------------------------------------------------------

;;;; perl mode
(fset 'perl-mode 'cperl-mode) ;; cperl mode looks more promising on the first sight.

(add-hook 'cperl-mode-hook '(lambda ()
			      (interactive)
			      (local-set-key (kbd "C-h f") 'cperl-perldoc-at-point)))

;; ---------------------------------------------------------

;;;; hungry delete mode
;;
;; This mode makes backspace and C-d erase all consecutive whitespace
;; instead of one.
;;
(mp/install-package 'hungry-delete)
(require 'hungry-delete)
(global-hungry-delete-mode)

;; ---------------------------------------------------------

;;;; ansi-term

(defun start-bash-in-ansi-term ()
  (interactive)
  ;; TODO
  ;; if single window in frame -> start term in n-line height window in same frame
  ;; if more then one window in current frame -> start term in new frame
  ;;  (if (length (window-list 
  (ansi-term "/bin/bash")
  )

(global-set-key (kbd "<f7>") 'start-bash-in-ansi-term)
  
;; ---------------------------------------------------------
