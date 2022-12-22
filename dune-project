(lang dune 2.9)

(name ezjs_fetch)

(license LGPL-2.1-or-later)

(authors "OCamlPro <contact@ocamlpro.com>")

(maintainers "OCamlPro <contact@ocamlpro.com>")

(source
 (github ocamlpro/ezjs_fetch))

(generate_opam_files true)

(package
 (name ezjs_fetch)
 (synopsis "Bindings for Fetch")
 (description "Bindings for Fetch")
 (depends (ezjs_min (>= 0.3)))
 (depopts lwt))
