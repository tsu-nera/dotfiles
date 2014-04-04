;; -----------------------------------------------------------------------
;; Name     :  Oz
;; Function :  multi-paradium language
;; ------------------------------------------------------------------------
(add-hook 'oz-mode-hook
	  (lambda ()
	    (define-key oz-mode-map "\C-c\C-b" 'oz-feed-buffer)
	    (define-key oz-mode-map "\C-c\C-l" 'oz-feed-line)
	    (define-key oz-mode-map "\C-c\C-r" 'oz-feed-region)))
;; -----------------------------------------------------------------------
;; Name     :  Cool
;; ------------------------------------------------------------------------
(autoload 'cool-mode "cool-mode" "Major mode for editing COOL programs" t)
(setq auto-mode-alist
            (append '(("\\.cl\\'" . cool-mode)) auto-mode-alist))

;; -----------------------------------------------------------------------
;; Name     :  VBA
;; ------------------------------------------------------------------------
(require 'vbasense)

(setq vbasense-popup-help-key "C-:")
(setq vbasense-jump-to-definition-key "C->")

;; (customize-group "vbasense")

;; use recommmended setting
(vbasense-config-default)(require 'vbasense)

;; visual-basic-mode
(autoload 'visual-basic-mode "visual-basic-mode" "Visual Basic mode" t)
(add-to-list 'auto-mode-alist '("\\.\\(vbs\\|vb\\|wsf\\|frm\\|bas\\|cls\\)$" . visual-basic-mode))
