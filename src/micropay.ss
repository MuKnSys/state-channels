; micropay.ss
;
;  Copyright (C) 2023, MUKN
;
;  Authors: Henri Lesourd (April 2023)
;
;    This is free software: you can redistribute it and/or modify it under
;    the terms of the Apache 2.0 License or (at your option) any later version.
;

(export #t)
(import ./runtime)
(import ../examples/mp1)
(import ../examples/mp2)

(set! tsc_micropay (type `("sc_micropay" ,tstatech) ;; State channel for micropayments
                         '()))
