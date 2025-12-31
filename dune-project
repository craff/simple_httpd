(lang dune 3.0)
(name tiny_httpd)
(generate_opam_files true)

(authors Christophe Raffalli)
(maintainers Christophe Raffalli)
(source (github craff/simple_httpd))
(homepage https://raffalli.eu/simple_httpd/)
(documentation https://raffalli.eu/simple_httpd/simple_httpd)
(bug_reports https://github.com/craff/simple_httpd/issues)
(license MIT)

(package
  (name simple_httpd)
  (synopsis "Simple HTTP server using ocaml 5 domains")
  (tags (http thread server simple_httpd http_of_dir))
  (depends
    (pacomb (>= 1.4.0))
    (ssl (>= 0.7.0))
    (polly (>= 0.4.0))
    (magic-mime (<= 1.3.0))
    (ocaml (>= 5.3.0))
    (camlzip (>= 1.11))
    digestif base64
    (cppo :build)
    (odoc :with-doc)
    (qtest (and (>= 2.9) :with-test))
    (conf-libcurl :with-test)
    (qcheck (and :with-test (>= 0.9)))
    (csv :with-test)))

(package
  (name simple_httpd_chaml)
  (synopsis "Chaml language for simple_httpd (like php but in OCaml)")
  (depends
    (pacomb (>= 1.4.0))
    (yojson :build)))

(package
  (name simple_httpd_caqti)
  (synopsis "DB interface for simple_httpd using caqti")
  (depends
	(caqti (>= 2.0.0))
	ipaddr
  	(simple_httpd (= :version))
  	(logs (>= 0.10.0))
        (caqti-driver-postgresql :with-test)))
