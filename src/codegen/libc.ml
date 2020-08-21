open Llvm

type t = {
  int_ty:      lltype;
  word_ty:     lltype;
  size_ty:     lltype;
  void_ty:     lltype;
  void_ptr_ty: lltype;

  malloc: llvalue
}

let int_ty libc = libc.int_ty
let word_ty libc = libc.word_ty
let size_ty libc = libc.size_ty
let void_ty libc = libc.void_ty
let void_ptr_ty libc = libc.void_ptr_ty

let malloc libc = libc.malloc

let generate md =
  let ctx = module_context md in

  let int_ty = i32_type ctx in
  let word_ty = i64_type ctx in
  let size_ty = i64_type ctx in
  let void_ty = void_type ctx in
  let void_ptr_ty = pointer_type (i8_type ctx) in

  let malloc =
    let ty = function_type void_ptr_ty [|size_ty|] in
    declare_function "malloc" ty md
  in

  { int_ty      = int_ty;
    word_ty     = word_ty;
    size_ty     = size_ty;
    void_ty     = void_ty;
    void_ptr_ty = void_ptr_ty;

    malloc = malloc }
