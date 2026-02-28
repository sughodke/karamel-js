(* Copyright (c) INRIA and Microsoft Corporation. All rights reserved. *)
(* Licensed under the Apache 2.0 and MIT Licenses. *)

(** Converting from C* to JavaScript. *)

open PPrint
open CStar
open PrintCommon

module K = Constant
module GN = GlobalNames

(* --- Name helpers --- *)

let to_js_name m lid =
  GN.to_c_name m (lid, GN.Other)

(* --- Operator precedence (JS follows C closely) --- *)

let prec_of_op2 = function
  | K.Comma -> 0
  | K.Assign -> 1
  | K.Or -> 2
  | K.Xor -> 3
  | K.And -> 4
  | K.BOr -> 5
  | K.BXor -> 6
  | K.BAnd -> 7
  | K.Eq | K.Neq -> 8
  | K.Lt | K.Lte | K.Gt | K.Gte -> 9
  | K.BShiftL | K.BShiftR -> 10
  | K.Add | K.AddW | K.Sub | K.SubW -> 11
  | K.Mult | K.MultW | K.Div | K.DivW | K.Mod -> 12
  | _ -> 13

let prec_of_op1 = function
  | K.Not | K.BNot | K.Neg -> 14
  | K.PreIncr | K.PreDecr -> 14
  | K.PostIncr | K.PostDecr -> 15
  | _ -> 13

let paren_if ctx_prec doc prec =
  if prec >= ctx_prec then doc
  else lparen ^^ doc ^^ rparen

(* --- Typed array selection --- *)

let typed_array_of_width = function
  | K.UInt8 -> "Uint8Array"
  | K.Int8 -> "Int8Array"
  | K.UInt16 -> "Uint16Array"
  | K.Int16 -> "Int16Array"
  | K.UInt32 -> "Uint32Array"
  | K.Int32 -> "Int32Array"
  | K.UInt64 -> "BigUint64Array"
  | K.Int64 -> "BigInt64Array"
  | K.Bool -> "Uint8Array"
  | _ -> "Array"

let typed_array_of_typ = function
  | Pointer (Int w) | Array (Int w, _) -> typed_array_of_width w
  | _ -> "Array"

(* --- Constant formatting --- *)

let p_constant (w, c) =
  (* Strip width suffixes -- JS doesn't need them *)
  ignore w;
  string c

(* --- Expression printer --- *)

let rec p_expr (m: GN.mapping) ?(ctx_prec=0) (e: expr): document =
  match e with
  | InlineComment (_, e, _) ->
      p_expr m ~ctx_prec e

  | Call (Op o, [ e1 ]) ->
      let prec = prec_of_op1 o in
      let doc = match o with
        | K.PreIncr | K.PreDecr ->
            print_op o ^^ p_expr m ~ctx_prec:prec e1
        | K.PostIncr | K.PostDecr ->
            p_expr m ~ctx_prec:prec e1 ^^ print_op o
        | _ ->
            print_op o ^^ p_expr m ~ctx_prec:prec e1
      in
      paren_if ctx_prec doc prec

  | Call (Op o, [ e1; e2 ]) ->
      let prec = prec_of_op2 o in
      let doc =
        p_expr m ~ctx_prec:prec e1 ^^ space ^^
        print_op o ^^ space ^^
        p_expr m ~ctx_prec:(prec + 1) e2
      in
      paren_if ctx_prec doc prec

  | Call (Qualified ([ "LowStar"; "Ignore" ], "ignore"), [ arg; _; _ ]) ->
      p_expr m ~ctx_prec arg

  | Call (e, es) ->
      p_expr m ~ctx_prec:15 e ^^
      parens_with_nesting (separate_map (comma ^^ break1) (p_expr m ~ctx_prec:0) es)

  | Var ident ->
      string ident

  | Qualified ident ->
      string (to_js_name m ident)

  | Macro ident ->
      string (to_js_name m ident)

  | Constant c ->
      p_constant c

  | BufCreate _ | BufCreateL _ ->
      failwith "[p_expr]: BufCreate should only appear under Decl"

  | BufRead (e1, e2) ->
      p_expr m ~ctx_prec:15 e1 ^^ lbracket ^^ p_expr m ~ctx_prec:0 e2 ^^ rbracket

  | BufSub (e1, Constant (_, "0")) ->
      p_expr m ~ctx_prec e1

  | BufSub (e1, e2) ->
      p_expr m ~ctx_prec:15 e1 ^^ dot ^^ string "subarray" ^^
      parens_with_nesting (p_expr m ~ctx_prec:0 e2)

  | BufNull ->
      string "null"

  | Op _ ->
      failwith "[p_expr]: Op should be under Call"

  | Cast (e, Int K.Int32) ->
      (* i32 coercion *)
      lparen ^^ p_expr m ~ctx_prec:0 e ^^ space ^^ string "| 0" ^^ rparen

  | Cast (e, Int K.UInt32) ->
      (* u32 coercion *)
      lparen ^^ p_expr m ~ctx_prec:0 e ^^ space ^^ string ">>> 0" ^^ rparen

  | Cast (e, Int K.UInt8) ->
      lparen ^^ p_expr m ~ctx_prec:0 e ^^ space ^^ string "& 0xFF" ^^ rparen

  | Cast (e, Int K.UInt16) ->
      lparen ^^ p_expr m ~ctx_prec:0 e ^^ space ^^ string "& 0xFFFF" ^^ rparen

  | Cast (e, Int K.Int8) ->
      lparen ^^ p_expr m ~ctx_prec:0 e ^^ space ^^ string "<< 24 >> 24" ^^ rparen

  | Cast (e, Int K.Int16) ->
      lparen ^^ p_expr m ~ctx_prec:0 e ^^ space ^^ string "<< 16 >> 16" ^^ rparen

  | Cast (e, _) ->
      (* Other casts: just emit the expression (JS is dynamically typed) *)
      p_expr m ~ctx_prec e

  | Bool b ->
      string (if b then "true" else "false")

  | Struct (_, fields) ->
      braces_with_nesting (
        separate_map (comma ^^ break1) (fun (name, e) ->
          match name with
          | Some n -> string n ^^ colon ^^ space ^^ p_expr m ~ctx_prec:0 e
          | None -> p_expr m ~ctx_prec:0 e
        ) fields
      )

  | Field (BufRead (e, Constant (_, "0")), field) ->
      (* Pointer dereference + field access → just field access in JS *)
      p_expr m ~ctx_prec:15 e ^^ dot ^^ string field

  | Field (e, field) ->
      p_expr m ~ctx_prec:15 e ^^ dot ^^ string field

  | Comma (e1, e2) ->
      let doc = p_expr m ~ctx_prec:0 e1 ^^ comma ^^ space ^^ p_expr m ~ctx_prec:0 e2 in
      paren_if ctx_prec doc 0

  | StringLiteral s ->
      dquote ^^ string s ^^ dquote

  | Any ->
      string "undefined"

  | AddrOf e ->
      (* JS arrays/objects are already references *)
      p_expr m ~ctx_prec e

  | EAbort (_, s) ->
      string "(() => { throw new Error(" ^^ dquote ^^ string s ^^ dquote ^^ string "); })()"

  | Stmt _ ->
      failwith "[p_expr]: Stmt in expr not supported in JS"

  | Type _ ->
      empty


(* --- Statement printer --- *)

and p_stmts m stmts =
  separate_map hardline (p_stmt m) stmts

and p_stmt (m: GN.mapping) (stmt: stmt): document =
  match stmt with
  | Comment s ->
      string "// " ^^ string s

  | Return (Some e) ->
      string "return" ^^ space ^^ p_expr m e ^^ semi

  | Return None ->
      string "return" ^^ semi

  | Break ->
      string "break" ^^ semi

  | Continue ->
      string "continue" ^^ semi

  | Ignore e ->
      p_expr m e ^^ semi

  | Abort s ->
      string "throw new Error(" ^^ dquote ^^ string s ^^ dquote ^^ string ")" ^^ semi

  | Decl (binder, BufCreate (_, init, size)) ->
      let arr_type = typed_array_of_typ binder.typ in
      string "let" ^^ space ^^ string binder.name ^^ space ^^ equals ^^ space ^^
      string "new " ^^ string arr_type ^^
      parens_with_nesting (p_expr m size) ^^ semi ^^
      (* Initialize if non-zero *)
      begin match init with
      | Constant (_, "0") | Any -> empty
      | _ ->
          hardline ^^
          string binder.name ^^ dot ^^ string "fill" ^^
          parens_with_nesting (p_expr m init) ^^ semi
      end

  | Decl (binder, BufCreateL (_, inits)) ->
      let arr_type = typed_array_of_typ binder.typ in
      string "let" ^^ space ^^ string binder.name ^^ space ^^ equals ^^ space ^^
      string "new " ^^ string arr_type ^^
      parens_with_nesting (
        lbracket ^^
        separate_map (comma ^^ space) (p_expr m ~ctx_prec:0) inits ^^
        rbracket
      ) ^^ semi

  | Decl (binder, e) ->
      string "let" ^^ space ^^ string binder.name ^^ space ^^ equals ^^ space ^^
      p_expr m e ^^ semi

  | IfThenElse (_, cond, then_, []) ->
      string "if" ^^ space ^^
      parens_with_nesting (p_expr m cond) ^^ space ^^
      p_block m then_

  | IfThenElse (_, cond, then_, else_) ->
      string "if" ^^ space ^^
      parens_with_nesting (p_expr m cond) ^^ space ^^
      p_block m then_ ^^ space ^^
      string "else" ^^ space ^^
      p_block m else_

  | While (cond, body) ->
      string "while" ^^ space ^^
      parens_with_nesting (p_expr m cond) ^^ space ^^
      p_block m body

  | For (`Decl (binder, init), cond, iter, body) ->
      string "for" ^^ space ^^ lparen ^^
      string "let" ^^ space ^^ string binder.name ^^ space ^^ equals ^^ space ^^ p_expr m init ^^
      semi ^^ space ^^ p_expr m cond ^^
      semi ^^ space ^^ p_iter_stmt m iter ^^
      rparen ^^ space ^^ p_block m body

  | For (`Stmt s, cond, iter, body) ->
      string "for" ^^ space ^^ lparen ^^
      p_iter_stmt m s ^^
      semi ^^ space ^^ p_expr m cond ^^
      semi ^^ space ^^ p_iter_stmt m iter ^^
      rparen ^^ space ^^ p_block m body

  | For (`Skip, cond, iter, body) ->
      string "for" ^^ space ^^ lparen ^^
      semi ^^ space ^^ p_expr m cond ^^
      semi ^^ space ^^ p_iter_stmt m iter ^^
      rparen ^^ space ^^ p_block m body

  | Assign (lhs, _, rhs) ->
      p_expr m lhs ^^ space ^^ equals ^^ space ^^ p_expr m rhs ^^ semi

  | BufWrite (e1, e2, e3) ->
      p_expr m ~ctx_prec:15 e1 ^^ lbracket ^^ p_expr m e2 ^^ rbracket ^^
      space ^^ equals ^^ space ^^ p_expr m e3 ^^ semi

  | BufBlit (_, dst, dst_idx, src, src_idx, len) ->
      p_expr m dst ^^ dot ^^ string "set" ^^
      parens_with_nesting (
        p_expr m src ^^ dot ^^ string "subarray" ^^
        parens_with_nesting (
          p_expr m src_idx ^^ comma ^^ space ^^
          p_expr m src_idx ^^ space ^^ string "+" ^^ space ^^ p_expr m len
        ) ^^ comma ^^ space ^^ p_expr m dst_idx
      ) ^^ semi

  | BufFill (_, dst, v, len) ->
      p_expr m dst ^^ dot ^^ string "fill" ^^
      parens_with_nesting (
        p_expr m v ^^ comma ^^ space ^^ string "0" ^^ comma ^^ space ^^ p_expr m len
      ) ^^ semi

  | BufFree _ ->
      (* GC handles deallocation in JS *)
      string "/* free */"

  | Block stmts ->
      p_block m stmts

  | Switch (e, cases, default) ->
      string "switch" ^^ space ^^ parens_with_nesting (p_expr m e) ^^ space ^^
      braces_with_nesting (
        separate_map hardline (fun (label, body) ->
          (match label with
           | `Ident lid -> string "case " ^^ string (to_js_name m lid)
           | `Int c -> string "case " ^^ p_constant c) ^^
          colon ^^ jump (p_stmts m body ^^ hardline ^^ string "break" ^^ semi)
        ) cases ^^
        (match default with
         | Some body ->
             hardline ^^ string "default" ^^ colon ^^
             jump (p_stmts m body)
         | None -> empty)
      )

and p_block m stmts =
  braces_with_nesting (p_stmts m stmts)

and p_iter_stmt m = function
  | Ignore e -> p_expr m e
  | Assign (lhs, _, rhs) -> p_expr m lhs ^^ space ^^ equals ^^ space ^^ p_expr m rhs
  | _ -> empty


(* --- Top-level declarations --- *)

let p_decl (m: GN.mapping) (d: decl): document list =
  let dominated_by_private = List.mem Common.Private (flags_of_decl d) in
  match d with
  | Function (_, _, _, name, params, body) ->
      let js_name = to_js_name m name in
      let export = if dominated_by_private then empty else string "export " in
      let doc =
        export ^^
        string "function" ^^ space ^^ string js_name ^^
        parens_with_nesting (
          separate_map (comma ^^ break1) (fun b -> string b.name) params
        ) ^^ space ^^
        p_block m body
      in
      [ doc ]

  | Global (name, _is_macro, _, _typ, expr) ->
      let js_name = to_js_name m name in
      let export = if dominated_by_private then empty else string "export " in
      let doc =
        export ^^
        string "let" ^^ space ^^ string js_name ^^ space ^^ equals ^^ space ^^
        p_expr m expr ^^ semi
      in
      [ doc ]

  | Type _ | TypeForward _ ->
      (* Type definitions become nothing in JS — structs are plain objects *)
      []

  | External _ ->
      (* External declarations are not emitted *)
      []


(* --- Cross-module import collection --- *)

(* Collect all Qualified lidents referenced in an expression *)
let rec collect_refs_expr acc (e: expr) = match e with
  | Qualified lid -> lid :: acc
  | Call (e, es) -> List.fold_left collect_refs_expr (collect_refs_expr acc e) es
  | BufRead (e1, e2) | BufSub (e1, e2) | Comma (e1, e2) ->
      collect_refs_expr (collect_refs_expr acc e1) e2
  | Cast (e, _) | Field (e, _) | AddrOf e | InlineComment (_, e, _) ->
      collect_refs_expr acc e
  | Struct (_, fields) ->
      List.fold_left (fun a (_, e) -> collect_refs_expr a e) acc fields
  | BufCreate (_, e1, e2) ->
      collect_refs_expr (collect_refs_expr acc e1) e2
  | BufCreateL (_, es) ->
      List.fold_left collect_refs_expr acc es
  | Var _ | Macro _ | Constant _ | BufNull | Op _ | Bool _ | StringLiteral _
  | Any | EAbort _ | Stmt _ | Type _ -> acc

let rec collect_refs_stmt acc = function
  | Return (Some e) -> collect_refs_expr acc e
  | Ignore e -> collect_refs_expr acc e
  | Decl (_, e) -> collect_refs_expr acc e
  | Assign (e1, _, e2) -> collect_refs_expr (collect_refs_expr acc e1) e2
  | BufWrite (e1, e2, e3) ->
      collect_refs_expr (collect_refs_expr (collect_refs_expr acc e1) e2) e3
  | BufBlit (_, e1, e2, e3, e4, e5) ->
      List.fold_left collect_refs_expr acc [e1; e2; e3; e4; e5]
  | BufFill (_, e1, e2, e3) ->
      List.fold_left collect_refs_expr acc [e1; e2; e3]
  | BufFree (_, e) -> collect_refs_expr acc e
  | IfThenElse (_, cond, t, f) ->
      let acc = collect_refs_expr acc cond in
      let acc = collect_refs_block acc t in
      collect_refs_block acc f
  | While (cond, body) ->
      collect_refs_block (collect_refs_expr acc cond) body
  | For (init, cond, iter, body) ->
      let acc = match init with
        | `Decl (_, e) -> collect_refs_expr acc e
        | `Stmt s -> collect_refs_stmt acc s
        | `Skip -> acc in
      let acc = collect_refs_expr acc cond in
      let acc = collect_refs_stmt acc iter in
      collect_refs_block acc body
  | Switch (e, cases, default) ->
      let acc = collect_refs_expr acc e in
      let acc = List.fold_left (fun a (_, body) -> collect_refs_block a body) acc cases in
      (match default with Some body -> collect_refs_block acc body | None -> acc)
  | Block stmts -> collect_refs_block acc stmts
  | Return None | Break | Continue | Comment _ -> acc
  | Abort _ -> acc

and collect_refs_block acc stmts =
  List.fold_left collect_refs_stmt acc stmts

let collect_refs_decl acc (d: decl) = match d with
  | Function (_, _, _, _, _, body) -> collect_refs_block acc body
  | Global (_, _, _, _, e) -> collect_refs_expr acc e
  | Type _ | TypeForward _ | External _ -> acc

(* Given a module name and its decls, compute import statements for cross-module refs *)
let mk_imports (m: GN.mapping) (this_module: string) (decls: decl list) : document =
  (* Collect all qualified refs *)
  let refs = List.fold_left collect_refs_decl [] decls in
  (* Get names defined in this module *)
  let defined = List.filter_map (fun d ->
    let (mods, _) = lid_of_decl d in
    Some (String.concat "_" (mods))
  ) decls in
  ignore defined;
  (* Group by source module, excluding this module *)
  let module_map = Hashtbl.create 16 in
  List.iter (fun ((mods, _) as lid) ->
    let src_module = String.concat "_" mods in
    if src_module <> this_module && src_module <> "" then begin
      let js_name = to_js_name m lid in
      let existing = try Hashtbl.find module_map src_module with Not_found -> [] in
      if not (List.mem js_name existing) then
        Hashtbl.replace module_map src_module (js_name :: existing)
    end
  ) refs;
  (* Generate import statements *)
  if Hashtbl.length module_map = 0 then empty
  else
    let imports = Hashtbl.fold (fun src_mod names acc ->
      let doc =
        string "import" ^^ space ^^
        braces_with_nesting (
          separate_map (comma ^^ break1) string (List.sort String.compare names)
        ) ^^ space ^^
        string "from" ^^ space ^^
        squote ^^ string ("./" ^ src_mod ^ ".js") ^^ squote ^^ semi
      in
      doc :: acc
    ) module_map [] in
    separate hardline (List.sort compare imports) ^^ hardline ^^ hardline


(* --- File-level entry point --- *)

let mk_file (m: GN.mapping) (name, decls) : (string * document) option =
  let docs = List.concat_map (p_decl m) decls in
  if docs = [] then None
  else
    let header =
      string "// This file was generated by KaRaMeL <https://github.com/FStarLang/karamel>" ^^
      hardline ^^
      string ("// F* module: " ^ name) ^^
      hardline ^^ hardline
    in
    let imports = mk_imports m name decls in
    let body = separate (hardline ^^ hardline) docs ^^ hardline in
    Some (name, header ^^ imports ^^ body)

let mk_files (m: GN.mapping) (files: (string * CStar.decl list) list): (string * document) list =
  List.filter_map (mk_file m) files
