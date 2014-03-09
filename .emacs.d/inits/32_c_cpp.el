;; -----------------------------------------------------------------------
;; Name     : CEDET
;; History  : 2014/02/04 add 
;; Install  : http://www.logilab.org/blogentry/173886
;; Function : 統合開発環境
;;            先頭のほうに配置する必要がある
;; ------------------------------------------------------------------------
;; Load CEDET.
;; See cedet/common/cedet.info for configuration details.
;; IMPORTANT: Tou must place this *before* any CEDET component (including
;; EIEIO) gets activated by another package (Gnus, auth-source, ...).
;;(load-file "/home/tsu-nera/.emacs.d/cedet-bzr/trunk/cedet-devel-load.el")

;;(semantic-mode 1)  ;; Enable Semantic
;; (global-ede-mode 1);; Enable EDE (Project Management) features
;;(semantic-load-enable-code-helpers)   Enable prototype help and smart completion

;;(setq semantic-default-submodes
;;      '(
;;	global-semantic-idle-scheduler-mode
;;	global-semantic-idle-completions-mode
;;	global-semanticdb-minor-mode
;;	global-semantic-decoration-mode
;;	global-semantic-highlight-func-mode
;;	global-semantic-stickyfunc-mode
;;	global-semantic-mru-bookmark-mode
;;	))

;; -----------------------------------------------------------------------
;; Name     : Emacs Code Browser
;; Function : 
;; History  : 2014/02/05
;; Install  : github
;;            git clone https://github.com/emacsmirror/ecb
;; ------------------------------------------------------------------------
;;(require 'ecb)
;;(require 'ecb-autoloads)
