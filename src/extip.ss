(import ./basics)
(import ./json)

(define REC (json-parse (string-join (sh-cmd "curl -s http://ipinfo.io/json") "")))
(outraw* (: REC 'ip) "\n")
