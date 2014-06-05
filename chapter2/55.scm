;;; Footnote 34 tells us that quote serves the same purpose as the quotation mark. Thus you could
;;; type (quote a) instead of 'a, (quote (a b c)) instead of '(a b c), and likewise
;;; (quote (quote abracadabra)) instead of ''abracadabra. So then, the expression (car ''abracadabra)
;;; yields quote naturally.