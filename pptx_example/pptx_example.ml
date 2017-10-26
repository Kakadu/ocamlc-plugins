
(* Print all fully qualified names in expressions *)

open Asttypes
open Longident
open Typedtree


open Tast_mapper

let tast_mapper =
  { default with
    expr = (fun mapper expr ->
      match expr with
      | { exp_desc = Texp_ident (path, _lid, _valdesc) }
        ->
        Printf.printf "Ident %S\n%!" (Path.name path);
          expr
      | {exp_desc = Texp_constant (Const_int n) } -> {expr  with exp_desc = Texp_constant (Const_int (2*n)) }
      | other -> default.expr mapper other); 
    value_binding = fun _ vb ->
      {vb with vb_attributes =  [ (Location.mknoloc "asdf", PStr []) ] }
 }

let () =
  Typemod.ImplementationHooks.add_hook "pptx_example"
    (fun
      (hook_info : Misc.hook_info)
      ((ast, coercion) : Typedtree.structure * Typedtree.module_coercion) ->
        let ast = tast_mapper.structure tast_mapper ast in
        (ast, coercion)
    );
  Typemod.InterfaceHooks.add_hook "pptx_example"
    (fun (hook_info : Misc.hook_info)
      (ast : Typedtree.signature) ->

        ast);
  ()
