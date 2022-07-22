(lang dune 3.3)
(name ordinal)
(version 1.1)

(generate_opam_files true)

(source
 (github RainbowAsteroids/ordinal))

(authors "Rainbow Asteroids")
(maintainers "rainbow@portal2d.xyz")

(license MIT)

(package
 (name ordinal)
 (synopsis "A language interpreter based on the Forth language")
 (description "A language interpreter based on the Forth language")
 (depends ocaml dune unix))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
