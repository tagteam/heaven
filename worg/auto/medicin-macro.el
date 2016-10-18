(TeX-add-style-hook
 "medicin-macro"
 (lambda ()
   (TeX-add-to-alist 'LaTeX-provided-package-options
                     '(("fontenc" "T1") ("inputenc" "utf8") ("ulem" "normalem")))
   (add-to-list 'LaTeX-verbatim-environments-local "lstlisting")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "lstinline")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "path")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "url")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "nolinkurl")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "hyperbaseurl")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "hyperimage")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "hyperref")
   (add-to-list 'LaTeX-verbatim-macros-with-delims-local "lstinline")
   (add-to-list 'LaTeX-verbatim-macros-with-delims-local "path")
   (TeX-run-style-hooks
    "latex2e"
    "article"
    "art10"
    "listings"
    "color"
    "amsmath"
    "array"
    "fontenc"
    "natbib"
    "authblk"
    "inputenc"
    "fixltx2e"
    "graphicx"
    "grffile"
    "longtable"
    "wrapfig"
    "rotating"
    "ulem"
    "textcomp"
    "amssymb"
    "capt-of"
    "hyperref")
   (TeX-add-symbols
    "EE"
    "one"
    "VV"
    "PP"
    "norm"
    "lag"
    "str"
    "smin"
    "smax"
    "styp"
    "period"
    "periodK"
    "K"
    "kk"
    "D"
    "B"
    "E"
    "XX"
    "LL"
    "QQ"
    "Ru"
    "GG"
    "T"
    "st"
    "Nn"
    "A"
    "C"
    "uu"
    "vv"
    "zz"
    "ww"
    "M"
    "I"
    "RR")
   (LaTeX-add-labels
    "sec:orgheadline1"
    "sec:orgheadline2"
    "fig:1"
    "sec:orgheadline3"
    "sec:orgheadline7"
    "sec:orgheadline4"
    "sec:orgheadline5"
    "sec:orgheadline6"
    "sec:orgheadline9"
    "sec:orgheadline8")
   (LaTeX-add-bibliographies
    "heaven"))
 :latex)

