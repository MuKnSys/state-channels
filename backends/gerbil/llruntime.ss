; llruntime[Gerbil].ss
;
;  Copyright (C) 2022, MUKN
;
;  Authors: Henri Lesourd (August 2022)
;
;    This is free software: you can redistribute it and/or modify it under
;    the terms of the Apache 2.0 License or (at your option) any later version.
;

(import :std/srfi/13)

;; =>
(defsyntax (=> LS)
  (let ((L (syntax->list LS)))
    `(lambda ,(cadr L) . ,(cddr L))))

(defsyntax (while LS)
  (let ((L (syntax->list LS)))
    `(do ()
         ((not ,(cadr L)))
         .
         ,(cddr L))))
