;; -*- lexical-binding: t; -*-

(TeX-add-style-hook
 "grammar"
 (lambda ()
   (TeX-add-to-alist 'LaTeX-provided-class-options
                     '(("article" "")))
   (TeX-add-to-alist 'LaTeX-provided-package-options
                     '(("booktabs" "") ("fontspec" "") ("geometry" "a4paper" "margin=2.5cm") ("plex-serif" "") ("syntax" "") ("backnaur" "")))
   (TeX-run-style-hooks
    "latex2e"
    "article"
    "art10"
    "booktabs"
    "fontspec"
    "geometry"
    "plex-serif"
    "syntax"
    "backnaur"))
 :latex)

