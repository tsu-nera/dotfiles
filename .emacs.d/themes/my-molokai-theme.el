;;; molokai-theme.el --- Yet another molokai theme for Emacs 24

;; Copyright (C) 2013 Huang Bin

;; Author: Huang Bin <embrace.hbin@gmail.com>
;; URL: https://github.com/hbin/molokai-theme
;; Version: 0.8

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; This is another molokai dark theme for Emacs 24.
;; Equiped with my favorites.

;;; Requirements:
;;
;; Emacs 24

;;; Code:
(deftheme my-molokai "The molokai color theme for Emacs 24")

(let ((class '((class color) (min-colors 89)))
      ;; molokai palette
      (my-molokai-white          "#ffffff")
      (my-molokai-fg             "#f8f8f0")
      (my-molokai-red            "#ff0000")
      (my-molokai-pink           "#f92672")
      (my-molokai-orange+5       "#ef5939")
      (my-molokai-orange         "#fd971f")
      (my-molokai-yellow         "#ffff00")
      (my-molokai-darkgoldenrod  "#e6db74")
      (my-molokai-wheat          "#c4be89")
      (my-molokai-olive          "#808000")
      (my-molokai-chartreuse     "#a6e22e")
      (my-molokai-lime           "#00ff00")
      (my-molokai-green          "#008000")
      (my-molokai-darkwine       "#1e0010")
      (my-molokai-maroon         "#800000")
      (my-molokai-wine           "#960050")
      (my-molokai-teal           "#008080")
      (my-molokai-aqua           "#00ffff")
      (my-molokai-blue           "#66d9ef")
      (my-molokai-slateblue      "#7070f0")
      (my-molokai-purple         "#ae81ff")
      (my-molokai-palevioletred  "#d33682")
      (my-molokai-grey-2         "#bcbcbc")
      (my-molokai-grey-1         "#8f8f8f")
      (my-molokai-grey           "#808080")
      (my-molokai-grey+2         "#403d3d")
      (my-molokai-grey+3         "#4c4745")
      (my-molokai-grey+5         "#232526")
      (my-molokai-bg             "#1b1d1e")
      (my-molokai-grey+10        "#080808")
      (my-molokai-dark           "#000000")
      (my-molokai-base01         "#465457")
      (my-molokai-base02         "#455354")
      (my-molokai-base03         "#293739")
      (my-molokai-dodgerblue     "#13354a"))
  (custom-theme-set-faces
   'my-molokai

   ;; base
   ;;   `(default ((t (:background ,my-molokai-bg :foreground ,my-molokai-fg))))
   `(default ((t (:foreground ,my-molokai-fg))))
   `(cursor ((t (:background ,my-molokai-fg :foreground ,my-molokai-bg))))
   `(fringe ((t (:foreground ,my-molokai-base02 :background ,my-molokai-bg))))
   `(highlight ((t (:background ,my-molokai-grey))))
   `(region ((t (:background  ,my-molokai-grey+2))
             (t :inverse-video t)))
   `(warning ((t (:foreground ,my-molokai-palevioletred :weight bold))))

   ;; font lock
   `(font-lock-builtin-face ((t (:foreground ,my-molokai-chartreuse))))
;;   `(font-lock-comment-face ((t (:foreground ,my-molokai-base01))))
;;   `(font-lock-comment-delimiter-face ((t (:foreground ,my-molokai-base01))))
   `(font-lock-comment-face ((t (:foreground ,my-molokai-grey-1))))
   `(font-lock-comment-delimiter-face ((t (:foreground ,my-molokai-grey-1))))
   `(font-lock-constant-face ((t (:foreground ,my-molokai-purple))))
   `(font-lock-doc-string-face ((t (:foreground ,my-molokai-darkgoldenrod))))
   `(font-lock-function-name-face ((t (:foreground ,my-molokai-chartreuse))))
   `(font-lock-keyword-face ((t (:foreground ,my-molokai-pink :weight bold))))
   `(font-lock-negation-char-face ((t (:foreground ,my-molokai-wine))))
   `(font-lock-preprocessor-face ((t (:inherit (font-lock-builtin-face)))))
   `(font-lock-regexp-grouping-backslash ((t (:inherit (bold)))))
   `(font-lock-regexp-grouping-construct ((t (:inherit (bold)))))
   `(font-lock-string-face ((t (:foreground ,my-molokai-darkgoldenrod))))
   `(font-lock-type-face ((t (:foreground ,my-molokai-blue :weight bold))))
   `(font-lock-variable-name-face ((t (:foreground ,my-molokai-orange))))
   `(font-lock-warning-face ((t (:foreground ,my-molokai-palevioletred :weight bold))))

   ;; mode line
   `(mode-line ((t (:foreground ,my-molokai-fg
                                :background ,my-molokai-base03
                                :box nil))))
   `(mode-line-buffer-id ((t (:weight bold))))
   `(mode-line-inactive ((t (:foreground ,my-molokai-fg
                                         :background ,my-molokai-base02
                                         :box nil))))

   ;; search
   `(isearch ((t (:foreground ,my-molokai-dark :background ,my-molokai-wheat :weight bold))))
   `(isearch-fail ((t (:foreground ,my-molokai-wine :background ,my-molokai-darkwine))))

   ;; linum-mode
   `(linum ((t (:foreground ,my-molokai-grey-2 :background ,my-molokai-grey+5))))

   ;; hl-line-mode
   `(hl-line-face ((,class (:background ,my-molokai-grey+5)) (t :weight bold)))
   `(hl-line ((,class (:background ,my-molokai-grey+5)) (t :weight bold)))

   ;; TODO
   ;; ido-mode
   ;; flycheck
   ;; show-paren
   ;; rainbow-delimiters
   ;; highlight-symbols
   ))

(defcustom molokai-theme-kit nil
  "Non-nil means load molokai-theme-kit UI component"
  :type 'boolean
  :group 'molokai-theme)

(defcustom molokai-theme-kit-file
  (concat (file-name-directory
           (or (buffer-file-name) load-file-name))
          "molokai-theme-kit.el")
  "molokai-theme-kit-file"
  :type 'string
  :group 'molokai-theme)

(if (and molokai-theme-kit
         (file-exists-p molokai-theme-kit-file))
    (load-file molokai-theme-kit-file))

;;;###autoload
(and load-file-name
     (boundp 'custom-theme-load-path)
     (add-to-list 'custom-theme-load-path
                  (file-name-as-directory
                   (file-name-directory load-file-name))))

(provide-theme 'my-molokai)

;; Local Variables:
;; no-byte-compile: t
;; End:

;;; molokai-theme.el ends here
