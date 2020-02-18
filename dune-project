(lang dune 1.11)
(name opam-mirror)

(generate_opam_files true)

(source (github holrock/opam-mirror))
(maintainers "Anil Madhavapeddy <anil@recoil.org>" "holrock")
(authors "Anil Madhavapeddy <anil@recoil.org>" "holrock")
(license ISC)

(package
 (name opam-mirror)
 (synopsis "Mirror upstream distribution files for an OPAM repository")
 (description "Mirror upstream distribution files for an OPAM repositor")
 (depends
  (dune (> 1.5))
  (cmdliner (>= 1.0.4))
  (uri (>= 3.1.0))
  (opam-repository (>= 2.0.6))
  (cohttp-lwt-unix (>= 2.0.2))
  (lwt (>= 5.1.1))
  (lwt_ssl (>= 1.1.3))
  (lambda-term (>= 2.0.3))
  (sha (>= 1.12))))