;; -----------------------------------------------------------------------
;; Name     : webkit
;; Install  : el-get
;; Function : web browser
;; ------------------------------------------------------------------------
;;(require 'webkit)

;; -----------------------------------------------------------------------
;; Name     : wanderlust
;; Install  :el-get
;; Function : emacsのメーラ
;; ------------------------------------------------------------------------
(setq ssl-certificate-verification-policy 1) ; この行がないとimapサーバに繋がらない
(autoload 'wl "wl" "Wanderlust" t)
(autoload 'wl-other-frame "wl" "Wanderlust on new frame." t)
(autoload 'wl-draft "wl-draft" "Write draft with Wanderlust." t)

;; -----------------------------------------------------------------------
;; Name     : esup
;; Function : Emacs 起動時のプロファイラ
;; ------------------------------------------------------------------------
(autoload 'esup "esup" "Emacs Start Up Profiler." nil)
