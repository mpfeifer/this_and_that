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
 '(auto-insert-alist (quote ((("pom.xml//'" . "Maven Project Object Model") . ["template-pom.xml" mp/yas-preprocessor]) (("\\.html\\'" . "Hypertext Markup Language 4.01 strict") . ["template.html" mp/yas-preprocessor]) (("\\.css\\'" . "Cascading Stylesheets File") . ["template.css" mp/yas-preprocessor]) (("\\.js\\'" . "Javascript Sourcecode") . ["template.js" mp/yas-preprocessor]) (("\\.[hH]\\(pp\\|PP\\)?" . "C/C++ Header") . ["template.h" mp/yas-preprocessor]))))
 '(auto-insert-directory "~/.emacs.d/autoinsert/")
 '(auto-insert-query nil)
 '(calendar-latitude 50.8)
 '(calendar-location-name "Bardenberg")
 '(calendar-longitude 6.1)
 '(custom-safe-themes (quote ("9dae95cdbed1505d45322ef8b5aa90ccb6cb59e0ff26fef0b8f411dfc416c552" "1934bf7e1713bf706a9cb36cc6a002741773aa42910ca429df194d007ee05c67" "f0ea6118d1414b24c2e4babdc8e252707727e7b4ff2e791129f240a2b3093e32" default)))
 '(ediff-diff-options "")
 '(ediff-split-window-function (quote split-window-horizontally))
 '(ediff-window-setup-function (quote ediff-setup-windows-default))
 '(helm-candidate-number-limit 75)
 '(helm-ff-file-name-history-use-recentf t)
 '(helm-move-to-line-cycle-in-source t)
 '(helm-quick-update t)
 '(helm-scroll-amount 1)
 '(help-window-select t)
 '(package-archives (quote (("melpa" . "http://melpa.org/packages/"))))
 '(scroll-conservatively 65000)
 '(tool-bar-mode nil)
 '(truncate-lines t)
 '(view-read-only t))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(calendar-today ((t (:underline "red"))))
 '(speedbar-highlight-face ((t (:background "burlywood"))) t))

;; ]

;; [ Load-path

(add-to-list 'load-path "~/.emacs.d/lib/")
(add-to-list 'load-path "~/.emacs.d/pman/")


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

;; [ s

(use-package s
  :ensure t)

;;]

;; [ auto insert

(defun mp/yas-preprocessor()
  "Replace all yasnippets in buffer. Snippets must be marked with $(yas ....)."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "$(yas \\([a-zA-Z0-9_]+\\))" (point-max) t)
      (let ((mb (match-beginning 0))
            (ms (match-string 1))
            (me (match-end 0)))
        (goto-char (match-beginning 0))
        (delete-char 6)
        (forward-char (- me mb 6))
        (delete-char -1)
        (yas-expand)))))

(auto-insert-mode)

;; ]

;; [ General Emacs Behaviour

(setq delete-exited-processes t)
(defalias 'yes-or-no-p 'y-or-n-p)
(defalias 'symbol-to-string 'symbol-name)
(defalias 'string-to-symbol 'intern)

;; ]

;; [ appearence

(when window-system
  (tool-bar-mode -1)
  (menu-bar-mode 1)
  (scroll-bar-mode -1))

;; do not show startup screen
(setq inhibit-startup-screen t)

(require 'hl-line)
(global-hl-line-mode nil)

(use-package theme-changer
  :ensure t
  :config
  (change-theme 'solarized-light 'solarized-dark))

;; [ auto compile

;; buffers are only auto-compiled if a corresponding elc file
;; already exists

(use-package auto-compile
  :ensure t
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

(use-package ibuffer
  :bind ("C-x C-b" . ibuffer)
  :config
  (progn
    (setq ibuffer-show-empty-filter-groups nil)
    (add-hook 'ibuffer-mode-hook
	      '(lambda ()
		 (ibuffer-auto-mode 1)
		 (ibuffer-switch-to-saved-filter-groups "standard")
		 (define-key ibuffer-mode-map (kbd "p") 'ibuffer-previous-line)
		 (define-key ibuffer-mode-map (kbd "n") 'ibuffer-next-line)))))
;; ]

;; [ emacs lisp mode

(defun mp/dotemacs-mode-hook ()
  (local-set-key (kbd "C-S-n") 'forward-paragraph)
  (local-set-key (kbd "C-S-p") 'backward-paragraph)
  (local-set-key (kbd "C-*") 'imenu)
  (make-local-variable 'paragraph-start)
  (setq paragraph-start "^;; \\[")
  (make-local-variable 'paragraph-separate)
  (setq paragraph-separate "^;; ]$")
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

(use-package elisp-slime-nav
  :ensure t
  :init
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
;; helm helps you steer into the right direction and is handy to have when
;; - M-x'ing
;; - swooping (helm-swoop)
;; - imenu'ing
;;
;; C-Spc       to mark candidate
;; M-a         mark all candidates
;; C-c C-i     insert marked candidates in current buffer
;; C-t         toggle between horizontal and vertical window
;; ( C-u ) C-c h /  call helm-find
;; C-c h b     resume current helm session
;; C-c h C-c SPC helm-all-mark-rings
;; helm-top    to view processes

(mp/install-package 'helm-swoop)

(use-package helm
  :ensure t
  :config
  (progn
					;    (require 'helm-config)
    (define-key helm-map (kbd "C-i") 'helm-execute-persistent-action) ; make TAB works in terminal
    (define-key helm-map (kbd "C-S-n") 'helm-next-source)
    (define-key helm-map (kbd "C-S-p") 'helm-previous-source)
    (define-key helm-map (kbd "C-z")  'helm-select-action) ; list actions using C-z
    (define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; rebind tab to run persistent action
    (global-set-key (kbd "C-M-s") 'helm-swoop)
    (global-set-key (kbd "M-x") 'helm-M-x)
    (global-set-key (kbd "M-y") 'helm-show-kill-ring)

    (global-unset-key (kbd "C-x c"))

    (when (executable-find "curl")
      (setq helm-google-suggest-use-curl-p t))

    ;; From https://gist.github.com/antifuchs/9238468
    (setq helm-buffers-fuzzy-matching           t ; fuzzy matching buffer names when non--nil
	  helm-ff-file-name-history-use-recentf t
	  helm-ff-search-library-in-sexp        t ; search for library in `require' and `declare-function' sexp.
	  helm-idle-delay                       0.0 ; update fast sources immediately (doesn't).
	  helm-input-idle-delay                 0.01  ; this actually updates things
	  helm-move-to-line-cycle-in-source     t ; move to end or beginning of source when reaching top or bottom of source.
	  helm-M-x-requires-pattern             nil
	  helm-scroll-amount                    8 ; scroll 8 lines other window using M-<next>/M-<prior>
	  helm-split-window-in-side-p           t ; open helm buffer inside current window, not occupy whole other window
	  helm-truncate-lines                   t)

    (add-to-list 'helm-sources-using-default-as-input 'helm-source-man-pages)
    
    ;; this one hooks into 'completing-read' and 'read-file-name'
    ;; to enable helm-style narrowing
    (helm-mode 1)))

;; ]

;; [ expand region

;; Very handy package. Sets er/try-expand-list on a per mode basis to list of
;; defuns. Each defun marks part of the buffer. Incrementally largens the part
;; of the buffer a defun operats on. The next larger marked part is then set
;; to the region.
;; To customize add defun to er/try-expand-list in any mode hook.

(use-package expand-region
  :ensure t
  :bind
  ("C-v" . er/expand-region)
  ("C-S-v" . er/contract-region)
  )

;; ]

;; [ highlight parenthesis

(use-package highlight-parentheses
  :ensure t
  :config
  (setq hl-paren-colors (list "red" "deep sky blue" "lawn green" "yellow"))
  (global-highlight-parentheses-mode 1)
  )

;; ]

;; [ yasnippet

(use-package yasnippet
  :ensure t
  :config
  (setq yas-snippet-dirs '("~/.emacs.d/yasnippets/"))
  (yas-global-mode 1)
  (define-key yas-minor-mode-map (kbd "<SPC>") 'yas-expand))

;; ]

;; [ auto complete

(use-package auto-complete
  :ensure t
  :config
  (require 'auto-complete-config)  
  (ac-config-default)
  (define-key ac-mode-map (kbd "C-M-s") 'ac-isearch)
  (setq ac-use-quick-help nil)

  (add-hook 'html-mode-hook '(lambda ()
			       "Enable html auto-complete for html-mode."
			       (auto-complete-mode)
			       (require 'ac-html)
			       (add-to-list 'ac-sources 'ac-source-html-attribute-value)
			       (add-to-list 'ac-sources 'ac-source-html-tag)
			       (add-to-list 'ac-sources 'ac-source-html-attribute)))

  (defun ac-emacs-lisp-mode-setup ()
    (setq ac-sources '(ac-source-features ;; collects 'require-able features from the file sytem
		       ac-source-functions
		       ac-source-variables
		       ac-source-symbols
		       ac-source-words-in-same-mode-buffers))))

;; ]

;; [ ace jump mode

(use-package ace-jump-mode
  :ensure t
  :bind ("C-S-j" . ace-jump-mode)
  )

;; ]

;; [ js2 mode

(use-package js2-mode
  :ensure t
  :mode "\\.js\\'"
  )

;; ]

;; [ info browser

(defadvice info (before
		 info-browser-own-frame (&optional file-or-node buffer)
		 activate)
  "Open new frame for info browser."
  (let ((next-index 0))
    (while (get-buffer (format "info[%d]" next-index))
      (setq next-index (+ 1 next-index)))
    (setq buffer (format "info[%d]" next-index)))
  (select-frame (make-frame)))

(defun mp/Info-mode-hook ()
  "Personal info mode hook."
  (define-key Info-mode-map (kbd "C-s") 'isearch-forward-regexp)
  )

(add-hook 'Info-mode-hook 'mp/Info-mode-hook)

;; ]

;; [ calendar

(global-set-key (kbd "<f4>") 'calendar)
(setq calendar-mark-holidays-flag t)
(setq calendar-date-style 'european)

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
	      (-48 "Rosenmontag")
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
(desktop-save-mode)

;; [ org mode

(add-hook 'org-mode-hook 'auto-fill-mode)

;; ]

;; [ prodigy service manager

(use-package prodigy
  :ensure t
  :bind ("<f5>" . prodigy)
  :config
  (prodigy-define-service
    :name "Echo Server"
    :command "mvn"
    :args '("exec:java -DmainClass=\"EchoServer-1.0.jar\"")
    :cwd "/home/user/java/EchoServer/"
    )
  )

;; ]

;; [ speedbar

(use-package sr-speedbar
  :ensure t
  :bind ("<f6>" . sr-speedbar-toggle)
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
;; This mode makes backspace erase all consecutive whitespace (instead of just a single one).

(use-package hungry-delete
  :ensure t
  :config
  (require 'hungry-delete)
  (global-hungry-delete-mode))

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
frame hosts exactly one window then split the window and either show
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

;; [ C/C++

(defun mp/c-mode-hook ()
  (local-set-key (kbd "C-c C-c") 'compile))

(add-hook 'c-mode-hook 'mp/c-mode-hook)

;; ]

;; [ web development

(defgroup mp nil "All things related to my customization [mp].")

(defgroup development nil "All things related to development [mp]." :group 'mp)

(defgroup web nil "All things related to web development [mp]." :group 'development)

(defcustom web-project-root "~/www/" "New web projects are stored in this directory." :group 'web)

(defun mp/start-web-project (name)
  (interactive "MProjectname?")
  (let ((projectroot (concat web-project-root name)))
    (unless (file-exists-p projectroot)
      (mkdir projectroot))
    (select-frame (make-frame))
    (split-window-vertically)
    (find-file (concat projectroot "/" name ".html"))
    (other-window 1)
    (find-file (concat projectroot "/" name ".js"))
    (split-window-horizontally)
    (find-file (concat projectroot "/" name ".css"))
    (other-window -1)))

;; ]

;; [ elnode

(use-package elnode
  :ensure t
  :bind
  ("<f12> s" . mp/elnode-webserver-start)
  ("<f12> e" . mp/elnode-webserver-stop)
  :config

  (defconst mp/elnode-webserver-port 80 "Port number for the elnode webserver.")

  (defconst mp/elnode-webserver-ip "127.0.0.1" "Ip4 address for the elnode webserver.")

  (defconst mp/elnode-webserver-request-handler 'elnode-webserver "Request handler for the elnode webserver.")

  (setq elnode-log-files-directory (concat elnode-config-directory "log/"))
  
  
  (defun mp/elnode-webserver-start ()
    "Start elnode webserver with default settings."
    (interactive)
    (elnode-start mp/elnode-webserver-request-handler
		  :port mp/elnode-webserver-port
		  :host mp/elnode-webserver-ip))

  (defun mp/elnode-webserver-stop ()
    "Stop elnode default webserver with default settings."
    (interactive)
    (elnode-stop  mp/elnode-webserver-port))
  
  )

;; ]

;; [ frame+window handling

(defgroup frames+windows nil "All things related to windows and frames [mp]." :group 'mp)

(defcustom mp/frame-configuration-file "~/.emacs.d/frame-configuration.el" "New web projects are stored in this directory." :group 'famres+windows)

(winner-mode)

(add-hook 'kill-emacs-hook #'(lambda ()
			       "Store window configuration in filesystem."
			       (interactive)
			       (frame-configuration-to-register ?w)
			       (with-temp-buffer
				 (insert (prin1-to-string (get-register ?w)))
				 (write-region (point-min) (point-max) mp/frame-configuration-file))))

(defun mp/detach-window ()
  "Iff current frame hosts at least two windows, close current window
and display corresponding buffer in new frame."
  (interactive)
  (if (not (one-window-p))
      (let ((buffer (current-buffer)))
	(delete-window)
	(display-buffer-pop-up-frame buffer nil))
    (message "Refusing to detach window when one-window-p is true.")))

(global-set-key (kbd "<f1>") 'mp/detach-window)
(global-set-key (kbd "<f2>") 'make-frame)
(global-set-key (kbd "<f3>") 'delete-frame)

;; ]

;; [ ediff

(add-hook 'ediff-after-quit-hook-internal 'winner-undo)

;; ]

;; [ whitespace mode

(global-set-key (kbd "<f12> w") 'whitespace-mode)

;; ]

;; [ Man mode

(defsubst scroll-up-one-line ()
  (interactive)
  (scroll-up 1))

(defsubst scroll-down-one-line ()
  (interactive)  
  (scroll-down 1))

(defun mp/man-mode-hook ()
  (define-key Man-mode-map (kbd "C-n") 'scroll-up-one-line)
  (define-key Man-mode-map (kbd "C-p") 'scroll-down-one-line))

(add-hook 'Man-mode-hook 'mp/man-mode-hook)

;; ]

;; [ emacs server

(server-start)

;; ]

;; [ java mode

(use-package pman
  :init
  (require 'pman))

(defun mp/java-mode-hook ()
  "Personal java mode hook [mp]."
  (pman-minor-mode))
  
(add-hook 'java-mode-hook 'mp/java-mode-hook)

;; ]

;; [ dired

;; make dired use find-alternate-file instead of standard dired-find-file 
;; this way the one single dired buffer is reused when new directories are
;; found
;;
(put 'dired-find-alternate-file 'disabled nil)

;; prevent new buffer creation when going up in the directory tree
;;
(add-hook 'dired-mode-hook
 (lambda ()
  (define-key dired-mode-map (kbd "^")
    (lambda () (interactive) (find-alternate-file "..")))
  ; was dired-up-directory
 ))

;; ]

