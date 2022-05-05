(* Compilation Units *)

type t =
  | CompilationUnit of { files: File.file list }

let compilation_unit files = CompilationUnit { files }

let pp _ _ = failwith "TODO"
