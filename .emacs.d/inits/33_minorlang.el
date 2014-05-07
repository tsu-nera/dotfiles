;; -----------------------------------------------------------------------
;; Name     :  Oz
;; Function :  multi-paradium language
;; ------------------------------------------------------------------------
;; http://www.eecs.ucf.edu/~leavens/ComS541Fall06/running_oz.shtml#
;;; Oz program mode with Mozart/Oz.
;;; You have to arrange for the Oz-related emacs lisp (.el) files
;;; to be in emacs's load-path, for its bin directory to be in the PATH,
;;; and for the OZHOME environment variable to be set properly.
(when linux-p
(or (getenv "OZHOME")
    (setenv "OZHOME" 
            "/mnt/win/opt/mozart"))   ; or wherever Mozart is installed
(setenv "PATH" (concat (getenv "OZHOME") "/bin:" (getenv "PATH")))
)

(add-to-list 'auto-mode-alist '("\\.oz\\'" . oz-mode))
(add-to-list 'auto-mode-alist '("\\.ozg\\'" . oz-gump-mode))
(autoload 'run-oz "oz" "" t)
(autoload 'oz-mode "oz" "" t)
(autoload 'oz-gump-mode "oz" "" t)
(autoload 'oz-new-buffer "oz" "" t)

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
(when windows-p
(autoload 'vbasense "vbasense" "Visual Basic Mode" t)
(add-to-list 'auto-mode-alist '("\\.\\(vbs\\|vb\\|wsf\\|frm\\|bas\\|cls\\)$" .
				vbasense))
(setq vbasense-popup-help-key "C-:")
(setq vbasense-jump-to-definition-key "C->")

(customize-group "vbasense")

;; use recommmended setting
(vbasense-config-default)

;; (custom-set-variables
;; '(vbasense-tli-files "C:/Program Files/Microsoft Office/OFFICE11/EXCEL.EXE"))
;; "c:/Program Files/Common Files/Microsoft Shared/VBA/VBA6/VBE6.DLL"
;; "c:/Program Files/Common Files/Microsoft Shared/VBA/VBA6/VBE6EXT.OLB"
;; "c:/Program Files/Common Files/Microsoft Shared/OFFICE11/MSO.DLL"
;; "c:/WINDOWS/system32/stdole2.tlb")
;; 	   )

;; visual-basic-mode
(autoload 'visual-basic-mode "visual-basic-mode" "Visual Basic mode" t)
(add-to-list 'auto-mode-alist '("\\.\\(vbs\\|vb\\|wsf\\|frm\\|bas\\|cls\\)$" .
				visual-basic-mode))
)
;; ------------------------------------------------------------------------
;; Rst-mode (for Sphinx)
;; ------------------------------------------------------------------------
;;; RSTモードを見やすくする。
(setq frame-background-mode 'dark)

(autoload 'rst "rst-mode" "Rst mode for Sphinx" t)
;;; *.rst, *.restファイルをrst-modeでOpen
(setq auto-mode-alist
      (append '(("\\.rst$" . rst-mode)
		("\\.rest$" . rst-mode)
		) auto-mode-alist))

;;; 全部スペースでインデントしましょう
(add-hook 'rst-mode-hook '(lambda() (setq indent-tabs-mode nil)))

;; エラーするので封印
;; (add-hook 'rst-mode-hook
;; 	  (lambda ()
;; 	    (setq rst-slides-program "open -a Firefox")
;; 	    ))

;; ------------------------------------------------------------------------
;; Name     : Markdown Mode
;; Function : Use Markdown
;; History  : 2014.1.11 Add
;; Install  : http://jblevins.org/projects/markdown-mode/markdown-mode.el
;; ------------------------------------------------------------------------
(autoload
  'markdown-mode "markdown-mode.el" "Major mode for editing Markdown files" t)
;; associate .md file to markdown-mode
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

;; -----------------------------------------------------------------------
;; Name     : plantuml
;; Install  : 
;; ------------------------------------------------------------------------
(autoload
  'plantuml-mode "plantuml-mode" "Major mode for editing PlantUML" t)
(add-to-list 'auto-mode-alist '("\\.puml$" . plantuml-mode))
(add-to-list 'auto-mode-alist '("\\.plantuml$" . plantuml-mode))
