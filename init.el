(menu-bar-mode -1)
(toggle-scroll-bar -1)
(tool-bar-mode -1)
(save-place-mode 1)

(setq pgtk-wait-for-event-timeout nil)

(setq custom-file (concat user-emacs-directory "custom.el"))
(push '("\\.\\(vcf\\|gpg\\)$" . sensitive-minor-mode) auto-mode-alist)
(setq
 backup-by-copying t										; don't clobber symlinks
 my/backup-dir (concat user-emacs-directory "backups/")
 backup-directory-alist `(("." . ,my/backup-dir)) ; don't litter my fs tree
 delete-old-versions t
 kept-new-versions 10
 kept-old-versions 0
 version-control t
 vc-make-backup-files t)       ; use versioned backups
(dolist (i (list custom-file))
	(unless (file-exists-p i)
		(write-region "" nil i)))
(dolist (i (list my/backup-dir))
	(unless (file-exists-p i)
		(make-directory i)))

(defun map-files (fun file-list)
	"do fun with the buffer as each file in file-list"
	(dolist (file file-list)
		(let ((buf (find-file file)))
			(goto-char (point-min))						; in case file is open
			(funcall fun)
			(save-buffer)
			(kill-buffer buf))))

(defmacro dofiles (fspec &rest body)
	"map-files body across fspec where fspec is (directory regexp) or a list of such forms"
	(declare (indent 1))
	(require 'find-lisp)
	(when (stringp (car fspec)) (setq fspec (list fspec)))
	`(map-files (lambda () ,@body)
							(mapcan (lambda (e) (apply 'find-lisp-find-files e)) ',fspec)))

(defmacro measure-time (&rest body)
	(declare (indent 0))
	"Measure the time it takes to evaluate BODY."
	(let ((time (gensym)) (result (gensym)))
		`(let* ((,time (current-time))
						(,result (progn ,@body)))
			 (message "%.06f" (float-time (time-since ,time)))
			 ,result)))

(add-to-list 'default-frame-alist '(font . "Iosevka 9"))
(set-face-attribute 'default t :font "Iosevka")
(global-display-line-numbers-mode 1)
(setq-default display-line-numbers t
							display-line-numbers-widen t
							display-line-numbers-type 'relative)
(setq display-line-numbers-type 'relative)

(defmacro undo-group (&rest body)
	"do the arguments as one undo section"
	(declare (indent 0))
	(let ((marker (gensym)))
		`(let ((,marker (prepare-change-group)))
			 (unwind-protect (atomic-change-group ,@body)
				 (undo-amalgamate-change-group ,marker)))))

(setq show-paren-delay 0)
(show-paren-mode)
(electric-pair-mode)

(defmacro translate (key states event &rest bindings)
	"translate (kbd key) to (kbd event) in states (quoted as in evil-define-key but not nil)"
	(declare (indent 0))
	`(progn (define-key key-translation-map
						(kbd ,key) (lambda (_)
												 (pcase evil-state
													 (,(if (symbolp (cadr states))
																 states
															 (cons 'or (mapcar (lambda (a) `',a) (cadr states))))
														(kbd ,event))
													 (_ (kbd ,key)))))
					,(if bindings `(translate ,@bindings))))
(defun send-keys (keys)
	(setq prefix-arg current-prefix-arg)
	(setq unread-command-events
				(nconc (mapcar (lambda (i) (cons t i)) (kbd keys))
							 unread-command-events)))
(defmacro prefix-translate (key states event &rest bindings)
	"translate but only for keys that appear at the start of chords"
	(declare (indent 0))
	`(with-eval-after-load 'evil
		 (evil-define-key ,states 'global
			 (kbd ,key) (lambda () (interactive) (send-keys ,event)))
		 ,@(if bindings (cddr (macroexpand-1 `(prefix-translate ,@bindings))))))
(prefix-translate
	"SPC" '(normal visual) "<leader>"
	"\\" '(normal visual) "<global-leader>"
	"M-;" 'insert "<leader>"
	"M-:" 'insert "<global-leader>")

(setq-default tab-width 2)
(setq-default evil-shift-width tab-width)
(setq-default rust-indent-offset tab-width)
(setq backward-delete-char-untabify-method 'all)

(defmacro interactive-chain (&rest args)
	(declare (indent 0))
	`(lambda () (interactive) ,@(mapcar #'cdr args)))

(defvar bootstrap-version)
(defvar all-the-icons-fonts-installed? t)

(let ((bootstrap-file
			 (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
			(bootstrap-version 5))
	(unless (file-exists-p bootstrap-file)
		(setq all-the-icons-fonts-installed? nil)
		(with-current-buffer
				(url-retrieve-synchronously
				 "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
				 'silent 'inhibit-cookies)
			(goto-char (point-max))
			(eval-print-last-sexp)))
	(load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)
(defmacro pkg (name &rest args)
	(declare (indent 1))
	`(use-package ,name :straight t ,@args))
(defmacro pkg-github (name repo &rest args)
	(declare (indent 2))
	`(use-package ,name
		 :straight (,name
								:type git
								:host github
								:repo ,repo)
		 ,@args))

(pkg gruvbox-theme
	:config
	(load-theme 'gruvbox-dark-hard t))

(pkg dashboard
	:after (projectile)
	:init
	(setq dashboard-projects-backend 'projectile)
	:config
	(setq dashboard-items '((recents . 5) (bookmarks . 5) (agenda . 5) (projects . 5)))
	(dashboard-setup-startup-hook))

(pkg keyfreq
	:config
	(keyfreq-mode 1)
	(keyfreq-autosave-mode 1)
	(require 'keyfreq)
	(setq keyfreq-excluded-commands '(self-insert-command)))

(pkg auto-package-update
	:init
	(setq auto-package-update-delete-old-versions t
				auto-package-update-hide-results t)
	:config
	(auto-package-update-maybe))

(pkg diminish)
(pkg goto-chg)

(pkg undo-tree
	:diminish undo-tree-mode
	:init
	(setq undo-tree-visualizer-timestamps t
				undo-tree-visualizer-lazy-drawing nil
				undo-tree-auto-save-history t)
	;; this is broken, the after save hook below fixes it, but it still needs to be here
	(let ((undo-dir (expand-file-name "undo" user-emacs-directory)))
		(setq undo-tree-history-directory-alist (list (cons "." undo-dir))))
	:config
	(add-hook 'after-save-hook '(lambda () (when undo-tree-mode (undo-tree-save-history nil t))))
	(global-undo-tree-mode))

(pkg evil
	:init
	;; (add-hook 'pre-command-hook 'evil-ex-nohighlight) ;; hack for sticky search (setq evil-want-integration t ;; This is optional since it's already set to t by default.
	(setq
	 evil-want-keybinding nil
	 evil-cross-lines t
	 evil-search-module 'evil-search
	 evil-undo-system 'undo-tree
	 evil-ex-substitute-global t
	 evil-want-C-u-scroll t
	 evil-want-C-i-jump t
	 evil-want-visual-char-semi-exclusive t
	 evil-want-Y-yank-to-eol t
	 evil-ex-search-vim-style-regexp t
	 evil-ex-substitute-global t
	 evil-ex-visual-char-range t ;; column range for ex commands this doesn't work
	 ;; more vim-like behavior
	 evil-symbol-word-search t
	 evil-want-change-word-to-end nil ;; ce and cw are now different
	 ;; don't activate mark on shift-click
	 shift-select-mode nil)
	:config
	(setq evil-emacs-state-cursor 'box
				evil-normal-state-cursor 'box
				evil-visual-state-cursor 'box
				evil-insert-state-cursor 'bar
				evil-replace-state-cursor 'hbar
				evil-operator-state-cursor 'hollow)
	(setq evil-extra-operator-eval-modes-alist
				'((lisp-mode slime-eval-region)
					(scheme-mode geiser-eval-region)
					(clojure-mode cider-eval-region)
					(ruby-mode ruby-send-region)
					(enh-ruby-mode ruby-send-region)
					(python-mode python-shell-send-region)
					(julia-mode julia-shell-run-region)))
	(evil-define-key 'motion 'global
		(kbd "M-e") 'evil-backward-word-end
		(kbd "M-E") 'evil-backward-WORD-end)
	(evil-define-key '(normal visual) 'global
		(kbd "<leader>;") 'execute-extended-command
		"ge" (evil-define-operator evil-eval (beg end)
					 "Evil operator for evaluating code."
					 :move-point nil
					 (interactive "<r>")
					 (let* ((ele (assoc major-mode evil-extra-operator-eval-modes-alist))
									(f-a (cdr-safe ele))
									(func (car-safe f-a))
									(args (cdr-safe f-a)))
						 (if (fboundp func)
								 (apply func beg end args)
							 (eval-region beg end t))))
		"gE" (evil-define-operator evil-eval-elisp-replace (beg end)
					 "Evil operator for evaluating code."
					 :move-point nil
					 (interactive "<r>")
					 (let ((result (eval (car (read-from-string (buffer-substring-no-properties beg end))))))
						 (evil-delete beg end nil ?_)
						 (message "%S" result)
						 (insert (prin1-to-string result))))
		"gc" (evil-define-operator evil-comment (beg end)
					 "Evil operator for evaluating code."
					 (interactive "<r>")
					 (comment-or-uncomment-region beg end))
		"gs" (evil-define-operator evil-replace-with-reg (beg end type register)
					 "Evil operator for evaluating code."
					 (interactive "<R><x>")
					 (evil-delete beg end type ?_)
					 (evil-paste-before 1 register)))
	(evil-define-key 'normal evil-ex-search-keymap
		"j" 'next-line-or-history-element
		"k" 'previous-line-or-history-element)
	(evil-define-key 'normal 'global
		"U" 'evil-redo
		(kbd "<escape>") 'evil-ex-nohighlight
		(kbd "<global-leader>s") (lambda () (interactive) (switch-to-buffer "*scratch*"))
		(kbd "<global-leader>b") 'bookmark-jump
		(kbd "<global-leader>B") 'bookmark-set
		"S" (lambda () (interactive) (evil-ex "%s/"))
		"gb" 'switch-to-buffer
		"gB" 'ibuffer)
	(evil-define-key nil 'global
		(kbd "C-h") 'evil-window-left
		(kbd "C-j") 'evil-window-down
		(kbd "C-k") 'evil-window-up
		(kbd "C-l") 'evil-window-right
		(kbd "C-q") 'image-kill-buffer
		(kbd "C-S-q") (interactive-chain 'save-buffer 'kill-buffer)
		(kbd "M-RET") (lambda () (interactive)
										(split-window-horizontally)
										(evil-window-right 1)
										(call-interactively #'switch-to-buffer))
		(kbd "M-DEL") (lambda () (interactive)
										(split-window-vertically)
										(evil-window-down 1)
										(call-interactively #'switch-to-buffer)))
	(evil-mode 1))

(pkg evil-surround
	:config
	(global-evil-surround-mode 1))

(pkg-github targets "noctuid/targets.el"
	:config
	(targets-setup t))

(pkg evil-exchange
	:config (evil-exchange-install))

(pkg centered-cursor-mode
	:diminish centered-cursor-mode
	:preface
	(setq-default require-final-newline nil)
	(setq mode-require-final-newline nil)
	:config
	(global-centered-cursor-mode 1))

(pkg evil-collection
	:diminish evil-collection-unimpaired-mode
	:after evil
	:init
	(setq evil-collection-setup-minibuffer t)
	:config
	(setq evil-collection-mode-list (delete 'lispy evil-collection-mode-list))
	(evil-collection-init)
	(dolist (i evil-collection-minibuffer-maps)
		(evil-define-key 'normal (eval i)
			"cc" (lambda () (interactive) (evil-change (line-beginning-position) (line-end-position)))
			"j" 'previous-complete-history-element
			"k" 'next-complete-history-element)))

(pkg lispy
	:defer t
	:diminish lispy-mode
	:after evil-collection)

(pkg lispyville
	:defer t
	:after (targets lispy)
	:diminish lispyville-mode
	:preface
	(add-hook 'emacs-lisp-mode-hook 'lispyville-mode)
	(add-hook 'common-lisp-mode-hook 'lispyville-mode)
	(add-hook 'scheme-mode-hook 'lispyville-mode)
	(add-hook 'lisp-mode-hook 'lispyville-mode)
	:init
	(add-hook 'lispyville-mode-hook
						(cl-macrolet ((defto (name key)
														`(targets-define-to ,name ',name nil object :bind t :keys ,key)))
							(lambda ()
								(defto lispyville-comment "c")
								(defto lispyville-atom "a")
								(defto lispyville-list "f")
								(defto lispyville-sexp "x")
								(defto lispyville-function "d")
								(defto lispyville-string "s"))))
	:config
	(lispyville-set-key-theme '(operators
															c-w
															prettify
															(atom-movement t)
															additional-movement
															commentary
															slurp/barf-cp
															(escape insert)))
	(defmacro surround-paren-insert (object at-end)
		"surround object and instert at the given end (either start or end)"
		`(lambda () (interactive)
			 (evil-start-undo-step)
			 (apply 'evil-surround-region
							(append (let* ((obj (,object))
														 (start (car obj)))
												(if (eq (char-after start) ?')
														(cons (+ 1 start) (cdr obj))
													obj))
											'(?\))))
			 ,@(if (eq at-end 'end)
						 '((lispyville-up-list)
							 (insert " ")
							 (evil-insert 1))
					 '((forward-char)
						 (insert " ")
						 (backward-char 1)
						 (evil-insert 1)))))
	;; TODO make these work for visual
	(evil-define-key '(visual normal) lispyville-mode-map
		(kbd "<leader>(") 'lispy-wrap-round
		(kbd "<leader>{") 'lispy-wrap-braces
		(kbd "<leader>[") 'lispy-wrap-brackets
		(kbd "<leader>)") 'lispyville-wrap-with-round
		(kbd "<leader>}") 'lispyville-wrap-with-braces
		(kbd "<leader>]") 'lispyville-wrap-with-brackets
		(kbd "M-j") 'lispyville-drag-forward
		(kbd "M-k") 'lispyville-drag-backward
		(kbd "<leader>@") 'lispy-splice
		(kbd "<leader>w") (surround-paren-insert targets-inner-lispyville-sexp start)
		(kbd "<leader>W") (surround-paren-insert targets-inner-lispyville-sexp end)
		(kbd "<leader>i") (surround-paren-insert targets-a-lispyville-list start)
		(kbd "<leader>I") (surround-paren-insert targets-a-lispyville-list end)
		(kbd "<leader>s") 'lispy-split
		(kbd "<leader>j") 'lispy-join
		(kbd "<leader>r") 'lispy-raise
		(kbd "<leader>R") 'lispyville-raise-list
		(kbd "<leader>h") (evil-define-command my/lispyville-insert-at-beginnging-of-list (count) (interactive "<c>")
												(lispyville-insert-at-beginning-of-list count)
												(insert " ")
												(backward-char))
		(kbd "<leader>l") 'lispyville-insert-at-end-of-list
		(kbd "<leader>o") 'lispyville-open-below-list
		(kbd "<leader>O") 'lispyville-open-above-list))

(pkg org
	:defer t
	:ensure nil
	:preface
	(evil-define-key 'normal 'global
		(kbd "<global-leader>a") 'org-agenda
		(kbd "<global-leader>A") (lambda () (interactive)
															 (require 'org-roam)
															 (org-roam-node-visit (org-roam-node-from-title-or-alias "Agenda"))
															 (goto-char (point-max))))
	:init
	(add-hook 'org-mode-hook 'org-indent-mode)
	(setq org-todo-keywords
				'((sequence "TODO" "IN-PROGRESS" "DONE")))
	:config
	(custom-set-faces
	 '(org-level-1 ((t (:inherit outline-1 :height 1.5))))
	 '(org-level-2 ((t (:inherit outline-2 :height 1.4))))
	 '(org-level-3 ((t (:inherit outline-3 :height 1.3))))
	 '(org-level-4 ((t (:inherit outline-4 :height 1.2))))
	 '(org-level-5 ((t (:inherit outline-5 :height 1.1)))))
	(setq org-cycle-level-faces nil))

(pkg org-superstar
	:defer t
	:after (org)
	:preface
	(add-hook 'org-mode-hook 'evil-org-mode)
	:init
	(setq org-superstar-leading-bullet "·")
	:config
	(cl-delete-if (lambda (elt) (eq (car elt) ?+)) org-superstar-item-bullet-alist))

(pkg org-roam
	:defer t
	:after (org)
	:preface
	(add-hook 'org-mode-hook (lambda () (require 'org-roam)))
	(setq org-roam-v2-ack t
				org-roam-completion-everywhere t
				org-roam-directory (file-truename "~/org"))
	(evil-define-key 'normal 'global
		(kbd "<global-leader>n") 'org-roam-node-find)
	:config
	(setq org-roam-capture-templates
				'(("d" "default" plain "\n%?"
					 :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n")
					 :unnarrowed t)
					("c" "computer science A-level" plain "\n%?"
					 :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n#+filetags: :CSAL:\n")
					 :unnarrowed t)
					("p" "physics A-level" plain "\n%?"
					 :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n#+filetags: :PAL:\n")
					 :unnarrowed t)
					("m" "maths A-level" plain "\n%?"
					 :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n#+filetags: :MAL:\n")
					 :unnarrowed t)
					("f" "further maths A-level" plain "\n%?"
					 :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n#+filetags: :FMAL:\n")
					 :unnarrowed t)))
	(evil-define-key 'insert org-mode-map
		(kbd "<leader>n") 'org-roam-node-insert)
	(evil-define-key 'normal org-mode-map
		(kbd "<leader>nb") 'org-roam-buffer-toggle
		(kbd "<leader>ng") 'org-roam-graph
		(kbd "<leader>ni") 'org-roam-node-insert
		(kbd "<leader>nc") 'org-roam-capture
		(kbd "<leader>nn") 'org-id-get-create
		(kbd "<leader>nt") 'org-roam-tag-add
		(kbd "<leader>nT") 'org-roam-tag-remove
		(kbd "<leader>nd") 'org-roam-dailies-capture-today
		(kbd "<leader>na") 'org-roam-alias-add)
	(org-roam-db-autosync-mode)
	;; If using org-roam-protocol
	;; (require 'org-roam-protocol)
	)

(pkg evil-org
	:defer t
	:after (evil org)
	:preface
	(add-hook 'org-mode-hook 'evil-org-mode)
	:config
	(require 'evil-org-agenda)
	(evil-org-agenda-set-keys)
	(evil-define-key 'insert org-mode-map
		(kbd "M-h") 'org-metaleft
		(kbd "M-l") 'org-metaright)
	(evil-define-key 'normal org-capture-mode-map
		(leader "k") 'org-capture-kill
		(leader "c") 'org-capture-finalize)
	(evil-define-key '(normal insert) org-mode-map
		(kbd "<leader>.") 'org-time-stamp
		(kbd "<leader>l") 'org-insert-link)
	(evil-define-key 'normal org-mode-map
		(kbd "<leader>a") 'org-agenda-file-to-front
		(kbd "<leader>r") 'org-remove-file
		(kbd "<leader>c") 'org-ctrl-c-ctrl-c
		(kbd "<leader>l") 'org-insert-link
		(kbd "<leader>d") 'org-deadline
		(kbd "<leader>s") 'org-schedule
		(kbd "<leader>p") 'org-priority
		(kbd "<leader>RET") 'org-open-at-point
		(kbd "<leader>t") 'org-shiftright
		(kbd "<leader>T") 'org-shiftleft))

(pkg which-key
	:diminish which-key-mode
	:config (which-key-mode))

(pkg selectrum
	:config
	(evil-define-key '(insert normal) selectrum-minibuffer-map
		(kbd "M-RET") 'selectrum-submit-exact-input
		(kbd "M-TAB") 'selectrum-insert-current-candidate
		(kbd "TAB") 'selectrum-next-candidate
		(kbd "<backtab>") 'selectrum-previous-candidate)
	(selectrum-mode))

(pkg marginalia
	:init
	(marginalia-mode))

(pkg prescient
	:config (prescient-persist-mode))

(pkg selectrum-prescient
	:after (selectrum prescient)
	:config (selectrum-prescient-mode))

(pkg eldoc
	:defer t
	:ensure nil
	:diminish eldoc-mode
	:config
	(setq eldoc-idle-delay 0))

(pkg company
	:diminish company-mode
	:after (evil evil-collection)
	:init
	(setq company-idle-delay 0
				company-minimum-prefix-length 1
				company-selection-wrap-around t)
	:config
	(add-hook 'company-mode-hook 'company-tng-mode)
	(evil-define-key 'insert company-mode-map
		(kbd "TAB") 'company-complete)
	(evil-define-key nil company-active-map
		(kbd "<tab>") (interactive-chain 'company-complete-common 'company-select-next)
		;; (lambda () (interactive)
		;; 	(company-complete-common)
		;; 	;; hack to get quickhelp to play nice with tng
		;; 	(when (not (null company-quickhelp--timer))
		;; 		(company-quickhelp--cancel-timer))
		;; 	(company-select-next)
		;; 	(company-quickhelp--set-timer)
		;; 	(message "eset"))
		(kbd "TAB") 'company-select-next
		(kbd "<backtab>") 'company-select-previous
		(kbd "M-TAB") 'company-complete-common
		(kbd "M-q") (interactive-chain 'company-select-first 'company-select-previous)
		(kbd "<next>") 'company-next-page
		(kbd "<prior>") 'company-previous-page
		(kbd "<return>") nil
		(kbd "RET") nil)
	(global-company-mode))

(pkg all-the-icons
	:defer t
	:config
	(unless all-the-icons-fonts-installed?
		(all-the-icons-install-fonts t)))

(pkg-github company-box "jack-faller/company-box"
	:defer t
	:after (company all-the-icons)
	:diminish company-box-mode
	:preface
	(add-hook 'company-tng-mode-hook 'company-box-mode)
	:config
	(setq company-box-doc-delay 0.13
				company-box-icons-all-the-icons
				`((Unknown . ,(all-the-icons-material "find_in_page" :height 0.8 :face 'all-the-icons-purple))
					(Text . ,(all-the-icons-material "text_fields" :height 0.8 :face 'all-the-icons-green))
					(Method . ,(all-the-icons-material "functions" :height 0.8 :face 'all-the-icons-red))
					(Function . ,(all-the-icons-material "functions" :height 0.8 :face 'all-the-icons-red))
					(Constructor . ,(all-the-icons-material "functions" :height 0.8 :face 'all-the-icons-red))
					(Field . ,(all-the-icons-material "functions" :height 0.8 :face 'all-the-icons-red))
					(Variable . ,(all-the-icons-material "adjust" :height 0.8 :face 'all-the-icons-blue))
					(Class . ,(all-the-icons-material "class" :height 0.8 :face 'all-the-icons-red))
					(Interface . ,(all-the-icons-material "settings_input_component" :height 0.8 :face 'all-the-icons-red))
					(Module . ,(all-the-icons-material "view_module" :height 0.8 :face 'all-the-icons-red))
					(Property . ,(all-the-icons-material "settings" :height 0.8 :face 'all-the-icons-red))
					(Unit . ,(all-the-icons-material "straighten" :height 0.8 :face 'all-the-icons-red))
					(Value . ,(all-the-icons-material "filter_1" :height 0.8 :face 'all-the-icons-red))
					(Enum . ,(all-the-icons-material "plus_one" :height 0.8 :face 'all-the-icons-red))
					(Keyword . ,(all-the-icons-material "filter_center_focus" :height 0.8 :face 'all-the-icons-red))
					(Snippet . ,(all-the-icons-material "short_text" :height 0.8 :face 'all-the-icons-red))
					(Color . ,(all-the-icons-material "color_lens" :height 0.8 :face 'all-the-icons-red))
					(File . ,(all-the-icons-material "insert_drive_file" :height 0.8 :face 'all-the-icons-red))
					(Reference . ,(all-the-icons-material "collections_bookmark" :height 0.8 :face 'all-the-icons-red))
					(Folder . ,(all-the-icons-material "folder" :height 0.8 :face 'all-the-icons-red))
					(EnumMember . ,(all-the-icons-material "people" :height 0.8 :face 'all-the-icons-red))
					(Constant . ,(all-the-icons-material "pause_circle_filled" :height 0.8 :face 'all-the-icons-red))
					(Struct . ,(all-the-icons-material "streetview" :height 0.8 :face 'all-the-icons-red))
					(Event . ,(all-the-icons-material "event" :height 0.8 :face 'all-the-icons-red))
					(Operator . ,(all-the-icons-material "control_point" :height 0.8 :face 'all-the-icons-red))
					(TypeParameter . ,(all-the-icons-material "class" :height 0.8 :face 'all-the-icons-red))
					;; (Template   . ,(company-box-icons-image "Template.png"))))
					(Yasnippet . ,(all-the-icons-material "short_text" :height 0.8 :face 'all-the-icons-green))
					(ElispFunction . ,(all-the-icons-material "functions" :height 0.8 :face 'all-the-icons-red))
					(ElispVariable . ,(all-the-icons-material "check_circle" :height 0.8 :face 'all-the-icons-blue))
					(ElispFeature . ,(all-the-icons-material "stars" :height 0.8 :face 'all-the-icons-orange))
					(ElispFace . ,(all-the-icons-material "format_paint" :height 0.8 :face 'all-the-icons-pink)))
				company-box-icons-alist 'company-box-icons-all-the-icons))

(pkg company-prescient
	:after (company prescient)
	:config (company-prescient-mode))

(pkg rustic
	:defer t
	:after (lsp lsp-ui)
	:preface
	(add-hook 'rust-mode-hook
						'rustic-mode)
	(setq lsp-rust-server 'rust-analyzer)
	(require 'mode-local)
	(setq-mode-local rustic-mode
									 lsp-ui-sideline-show-hover nil
									 lsp-rust-analyzer-cargo-watch-command "clippy")
	(setq rustic-indent-offset 2))
(pkg flycheck-rust
	:defer t
	:after (flycheck)
	:preface
	(add-hook 'rust-mode-hook 'flycheck-rust-setup))

(pkg projectile
	:config
	(projectile-mode)
	(setq compilation-scroll-output t)
	(evil-define-key '(insert normal) projectile-mode-map
		(kbd "<f5>") 'projectile-run-project)
	(dolist (map evil-collection-compile-maps)
		(evil-define-key 'normal map
			"q" (interactive-chain 'kill-compilation 'quit-window)))
	(setq projectile-project-search-path '("~/code/")))

(pkg flyspell
	:preface
	(add-hook 'prog-mode-hook 'flyspell-prog-mode)
	(add-hook 'text-mode-hook 'flyspell-mode)
	:init
	(setq flyspell-issue-message-flag nil)
	:config
	(evil-define-key 'normal flyspell-mode-map
		"[s" 'evil-prev-flyspell-error
		"]s" 'evil-next-flyspell-error))

(pkg flycheck
	:defer t
	:preface
	(add-hook 'emacs-lisp-mode-hook (lambda () (flycheck-mode -1)))
	:config
	(evil-define-key 'normal flycheck-mode-map
		(kbd "<leader>e") 'list-flycheck-errors
		"]]" 'flycheck-next-error
		"[[" 'flycheck-previous-error)
	(global-flycheck-mode))
(pkg flycheck-inline
	:defer t
	:after (flycheck)
	:preface
	(add-hook 'flycheck-mode-hook 'flycheck-inline-mode)
	(setq flycheck-display-errors-delay 0.2))

(pkg lsp-mode
	:defer t
	:after (company flycheck)
	:init
	(setq lsp-eldoc-enable-hover nil
				lsp-signature-render-documentation nil)
	:config
	(add-hook 'lsp-mode-hook 'evil-normal-state)
	(evil-define-key 'normal lsp-mode-map
		(kbd "<leader>=") 'lsp-format-buffer
		(kbd "<leader>gd") 'lsp-find-definition
		(kbd "<leader>gD") 'lsp-find-declaration
		(kbd "<leader>gr") 'lsp-find-references
		(kbd "<leader>gi") 'lsp-find-implementation
		(kbd "<leader>gt") 'lsp-find-type-definition
		;; (kbd "<leader>gh") 'hierarchy
		(kbd "<leader>ga") 'xref-find-apropos
		(kbd "<leader>o") 'lsp-organize-imports
		(kbd "<leader>r") 'lsp-rename
		(kbd "<leader>te") (lambda () (interactive) (setq lsp-eldoc-enable-hover (not lsp-eldoc-enable-hover)))
		(kbd "<leader>a") 'lsp-execute-code-action
		"K" 'lsp-ui-doc-show
		"gK" 'lsp-describe-thing-at-point))

(pkg lsp-ui
	:defer t
	:preface
	(setq lsp-ui-doc-enable t
				lsp-ui-doc-delay most-positive-fixnum
				lsp-ui-doc-position 'top
				lsp-ui-sideline-show-hover t
				lsp-ui-sideline-show-symbol t
				lsp-ui-sideline-show-diagnostics t
				lsp-ui-sideline-show-code-actions t))

(pkg page-break-lines
	:config
	(global-page-break-lines-mode))

(pkg highlight-indent-guides
	:diminish highlight-indent-guides-mode
	:config
	(add-hook 'prog-mode-hook 'highlight-indent-guides-mode)
	(setq highlight-indent-guides-method 'character))

(pkg magit
	:after (evil-collection)
	:defer t
	:preface
	(evil-define-key 'normal 'global
		(kbd "<global-leader>m") 'magit)
	(evil-define-key 'normal magit-mode-map
		(kbd "M-h") 'magit-section-up
		(kbd "M-j") 'magit-section-forward-sibling
		(kbd "M-k") 'magit-section-backward-sibling))

(pkg treemacs
	:defer t
	:preface
	(evil-define-key 'normal 'global
		"gt" 'treemacs)
	(pkg treemacs-evil
		:after (treemacs evil))
	(pkg treemacs-all-the-icons
		:after (treemacs all-the-icons)
		:config
		(treemacs-load-theme 'all-the-icons))
	(pkg treemacs-projectile
		:after (treemacs projectile))
	(pkg treemacs-icons-dired
		:after (treemacs))
	(pkg treemacs-magit
		:after (treemacs magit)))
