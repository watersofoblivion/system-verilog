(lang dune 3.1)
(using menhir 2.0)

(name systemVerilog)

(generate_opam_files true)

(source
 (github watersofoblivion/system-verilog))

(authors "Jonathan Bryant")

(maintainers "Jonathan Bryant")

(license MIT)

(documentation https://github.com/watersofoblivion/system-verilog)

(package
 (name systemVerilog)
 (synopsis "System Verilog")
 (description "An implementation of System Verilog (IEEE 1800-2012)")
 (depends
   ocaml
   dune
   menhir
   llvm
   (alt-ergo (= 2.2.0))
   odoc
   cmdliner
   ANSITerminal
   logs
   ounit2
   bisect_ppx
   sedlex))
