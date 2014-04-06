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
(autoload 'vbasense "vbasense" "Visual Basic Mode" t)
(add-to-list 'auto-mode-alist '("\\.\\(vbs\\|vb\\|wsf\\|frm\\|bas\\|cls\\)$" .
				vbasense))
(setq vbasense-popup-help-key "C-:")
(setq vbasense-jump-to-definition-key "C->")

(customize-group "vbasense")

;; use recommmended setting
(vbasense-config-default)

;;(custom-set-variables '(vbasense-tli-files "C:/Program Files (x86)/Common Files/microsoft shared/VBA/VBA7/VBE7.DLL"))

;; (custom-set-variables '(vbasense-tli-files "C:/Program Files/Microsoft Office/OFFICE11/EXCEL.EXE"))
				;; "c:/Program Files/Common Files/Microsoft Shared/VBA/VBA6/VBE6.DLL"
				;; "c:/Program Files/Common Files/Microsoft Shared/VBA/VBA6/VBE6EXT.OLB"
				;; "c:/Program Files/Common Files/Microsoft Shared/OFFICE11/MSO.DLL"
				;; "c:/WINDOWS/system32/stdole2.tlb")
				;; 	   )
;; visual-basic-mode
(autoload 'visual-basic-mode "visual-basic-mode" "Visual Basic mode" t)
(add-to-list 'auto-mode-alist '("\\.\\(vbs\\|vb\\|wsf\\|frm\\|bas\\|cls\\)$" .
				visual-basic-mode))
