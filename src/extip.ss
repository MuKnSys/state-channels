(import ./rexpr)
(import ./json)

(define REC (json-parse (string-join (sh-cmd "curl -s http://ipinfo.io/json") ""))) ;; FIXME: dont use that s$%t in production
(outraw* (: REC 'ip) "\n")
