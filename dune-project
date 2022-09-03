(lang dune 3.1)
(using menhir 2.0)

(name cfn++)

(generate_opam_files true)

(source
 (github watersofoblivion/cfn))

(authors "Jonathan Bryant")

(maintainers "Jonathan Bryant")

(license MIT)

(documentation https://github.com/watersofoblivion/cfn)

(package
 (name cfn++)
 (synopsis "A native-code compiler for AWS::Serverless")
 (description "CFN++ is a cloud-native language targeted at AWS::Serverless.  It provides an object-oriented syntax for CloudFormation and native-code compiler for AWS Lambda, combined into one unified language.")
 (depends
   ocaml
   dune
   menhir
   odoc
   atdgen
   yojson
   cmdliner
   ounit2
   llvm
   zarith
   simple-diff
   ANSITerminal
   bisect_ppx
   ctypes-foreign
   uri
   uutf
   uunf
   sedlex))
