(**
 * Copyright (c) Facebook, Inc. and its affiliates.
 * Copyright (c) Ziqin Wang <wangzq2017@mail.sustech.edu.cn>
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Ty

let as_json_obj li = Hh_json.JSON_Object(li)
let as_json_arr li = Hh_json.JSON_Array(li)
let as_json_str it = Hh_json.JSON_String(it)
let as_json_bool it = Hh_json.JSON_Bool(it)
let as_json_int it = Hh_json.int_ it


let string_of_ctor = function
  | TVar(RVar _, _)     -> "RecVar"
  | Bound(_)            -> "Bound"
  | Generic(_)          -> "Generic"
  | Any Annotated       -> "Explicit Any"
  | Any(_)              -> "Implicit Any"
  | Top                 -> "Top"
  | Bot(_)              -> "Bot"
  | Void                -> "Void"
  | Null                -> "Null"
  | Symbol              -> "Symbol"
  | Num(_)              -> "Num"
  | Str(_)              -> "Str"
  | Bool(_)             -> "Bool"
  | NumLit(_)           -> "NumLit"
  | StrLit(_)           -> "StrLit"
  | BoolLit(_)          -> "BoolLit"
  | Fun(_)              -> "Fun"
  | Obj(_)              -> "Obj"
  | Arr(_)              -> "Arr"
  | Tup(_)              -> "Tup"
  | Union(_)            -> "Union"
  | Inter(_)            -> "Inter"
  | TypeAlias(_)        -> "TypeAlias"
  | InlineInterface(_)  -> "InlineInterface"
  | TypeOf(_)           -> "Typeof"
  | ClassDecl(_)        -> "ClassDecl"
  | InterfaceDecl(_)    -> "InterfaceDecl"
  | EnumDecl(_)         -> "EnumDecl"
  | Utility(_)          -> "Utility"
  | Module(_)           -> "Module"
  | Mu(_)               -> "Mu"

let builtin_value = function
  | FunProto            -> "Function.prototype"
  | ObjProto            -> "Object.prototype"
  | FunProtoApply       -> "Function.prototype.apply"
  | FunProtoBind        -> "Function.prototype.bind"
  | FunProtoCall        -> "Function.prototype.call"

let string_of_polarity = function
  | Negative            -> "Negative"
  | Neutral             -> "Neutral"
  | Positive            -> "Positive"

let json_of_provenance loc p =
  [
    "kind",
      p
      |> Ty.debug_string_of_provenance_ctor
      |> as_json_str;
    "loc",
      loc
      |> Reason.string_of_aloc ~strip_root:None
      |> as_json_str;
  ]
  |> as_json_obj

let json_of_symbol { provenance; def_loc; name; _ } =
  [
    "provenance", json_of_provenance def_loc provenance;
    "name", name |> as_json_str;
  ]
  |> as_json_obj

let rec json_of_t (t: Ty.t) =
  (("kind", t |> string_of_ctor |> as_json_str)
  ::
  match t with
  | TVar(v, ts)       -> json_of_tvar v @ json_of_targs ts
  | Bound(_, name)    -> ["bound", name |> as_json_str]
  | Generic(g)        -> g |> json_of_generic
  | Any Annotated     -> ["any", "explicit" |> as_json_str]
  | Any(_)            -> ["any", "implicit" |> as_json_str]
  | Top
  | Bot(_) 
  | Void
  | Null
  | Symbol
  | Num(_)
  | Str(_)
  | Bool(_)           -> []
  | NumLit(s)
  | StrLit(s)         -> ["literal", s |> as_json_str]
  | BoolLit(b)        -> ["literal", b |> as_json_bool]
  | Fun(f)            -> f |> json_of_fun_t
  | Obj(o)            -> o |> json_of_obj_t
  | Arr({ arr_readonly; arr_literal; arr_elt_t }) ->
    [
      "readonly", arr_readonly |> as_json_bool;
      "literal", arr_literal |> as_json_bool;
      "type", arr_elt_t |> json_of_t;
    ]
  | Tup(ts)           -> ["types", ts |> List.map json_of_t |> as_json_arr]
  | Union(t0, t1, ts) -> ["types", (t0 :: t1 :: ts) |> List.map json_of_t |> as_json_arr]
  | Inter(t0, t1, ts) -> ["types", (t0 :: t1 :: ts) |> List.map json_of_t |> as_json_arr]
  | TypeAlias({ ta_name; ta_tparams; ta_type }) ->
    [
      "name", ta_name |> json_of_symbol;
      "typeParams", ta_tparams |> json_of_type_params;
      "body", ta_type |> Option.value_map ~f:json_of_t ~default:Hh_json.JSON_Null;
    ]
  | InlineInterface({ if_extends; if_body }) ->
    [
      "extends",
        if_extends
        |> List.map json_of_generic
        |> List.map as_json_obj
        |> as_json_arr;
      "body",
        if_body
        |> json_of_obj_t
        |> as_json_obj;
    ]
  | TypeOf(b)         -> ["name", b |> builtin_value |> as_json_str]
  | Module(name, _)   -> ["name", name |> Option.value_map ~f:json_of_symbol ~default:Hh_json.JSON_Null]
  | ClassDecl(name, tparams) ->
    [
      "name", name |> json_of_symbol;
      "typeParams", tparams |> json_of_type_params;
    ]
  | InterfaceDecl(name, tparams) ->
    [
      "name", name |> json_of_symbol;
      "typeParams", tparams |> json_of_type_params;
    ]
  | EnumDecl(name)    -> ["name", name |> json_of_symbol]
  | Utility(u)        -> u |> json_of_utility
  | Mu(i, t)          ->
    [
      "mu_var", i |> as_json_int;
      "type", t |> json_of_t;
    ])
  |> as_json_obj

and json_of_tvar (RVar i) = ["id", i |> as_json_int]

and json_of_generic (s, k, targs_opt) =
  json_of_targs targs_opt
  @
  [
    "type", s |> json_of_symbol;
    "genericKind", k |> Ty.debug_string_of_generic_kind |> as_json_str;
  ]

and json_of_fun_t({ fun_params; fun_rest_param; fun_return; fun_type_params }) =
  [
    "typeParams", fun_type_params |> json_of_type_params;
    "paramTypes",
      fun_params
      |> List.map (fun (_, t, _) -> t |> json_of_t)
      |> as_json_arr;
    "paramNames",
      fun_params
      |> List.rev_map (function
        | Some(n), _, _ -> n |> as_json_str
        | None, _, _ -> "_" |> as_json_str)
      |> as_json_arr;
    "restParam",
      (match fun_rest_param with
      | None -> Hh_json.JSON_Null
      | Some(name, t) ->
          ("restParamType", t |> json_of_t)
          ::
          (match name with
          | None -> []
          | Some(name) -> ["restParamName", name |> as_json_str])
          |> as_json_obj);
    "returnType", fun_return |> json_of_t;
  ]

and json_of_obj_t { obj_exact; obj_props; obj_literal; obj_frozen } =
  [
    "exact", obj_exact |> as_json_bool;
    "frozen", obj_frozen |> as_json_bool;
    "literal", obj_literal |> as_json_bool;
    "props", obj_props |> List.map json_of_prop |> as_json_arr;
  ]

and json_of_type_params ps =
  match ps with
  | None -> Hh_json.JSON_Null
  | Some(tparams) -> tparams |> List.map json_of_typeparam |> as_json_arr;

and json_of_targs = function
  | Some(targs) -> ["typeArgs", targs |> List.map json_of_t |> as_json_arr]
  | None -> []

and json_of_typeparam { tp_name: string; tp_bound: t option; tp_polarity: polarity; tp_default: t option } =
  [
    "name", tp_name |> as_json_str;
    "bound", tp_bound |> Option.value_map ~f:json_of_t ~default:Hh_json.JSON_Null;
    "polarity", tp_polarity |> json_of_polarity;
  ]
  @
  (match tp_default with
    | Some(t) -> ["default", t |> json_of_t]
    | None -> [])
  |> as_json_obj

and json_of_polarity polarity = polarity |> string_of_polarity |> as_json_str

and json_of_prop prop =
  (match prop with
  | NamedProp(name, p) ->
    [
      "kind", "NamedProp" |> as_json_str;
      "prop",
        [
          "name", name |> as_json_str;
          "prop", p |> json_of_named_prop;
        ]
        |> as_json_obj;
    ]
  | IndexProp d ->
    [
      "kind", "IndexProp" |> as_json_str;
      "prop", d |> json_of_dict
    ]
  | CallProp ft ->
    [
      "kind", "NamedProp" |> as_json_str;
      "prop", ft |> json_of_fun_t |> as_json_obj
    ]
  | SpreadProp t ->
    [
      "kind", "SpreadProp" |> as_json_str;
      "prop", t |> json_of_t
    ])
  |> as_json_obj

and json_of_dict { dict_polarity; dict_name; dict_key; dict_value } =
  [
    "polarity", dict_polarity |> json_of_polarity;
    "name", dict_name |> Option.value ~default:"_" |> as_json_str;
    "key", dict_key |> json_of_t;
    "value", dict_value |> json_of_t;
  ]
  |> as_json_obj

and json_of_named_prop p =
  (match p with
  | Field(t, { fld_polarity; fld_optional }) ->
    [
      "kind",     "field" |> as_json_str;
      "type",     t |> json_of_t;
      "polarity", fld_polarity |> json_of_polarity;
      "optional", fld_optional |> as_json_bool;
    ]
  | Method(t) ->
    [
      "kind",     "Method" |> as_json_str;
      "funtype",  t |> json_of_fun_t |> as_json_obj;
    ]
  | Get(t) ->
    [
      "kind", "Get" |> as_json_str;
      "type", t |> json_of_t;
    ]
  | Set(t) ->
    [
      "kind", "Set" |> as_json_str;
      "type", t |> json_of_t;
    ])
  |> as_json_obj

and json_of_utility u =
  ("kind", u |> Ty.string_of_utility_ctor |> as_json_str)
  ::
  (u |> Ty.types_of_utility |> json_of_targs)



(* Simplified *)

let kind_of = function
  | Void      -> "undefined"
  | Null      -> "null"
  | Num _     -> "number"
  | Str _     -> "string"
  | Bool _    -> "boolean"
  | Fun _     -> "function"
  | Obj _     -> "object"
  | Arr _     -> "array"
  | Generic _ -> "generic"
  | Union _   -> "union"
  | Inter _   -> "intersection"
  | Top       -> "mixed"
  | Bot _     -> "empty"
  | ClassDecl _ -> "class"
  | Module _  -> "module"
  | Symbol    -> "symbol"
  | Any _     -> "any"
  (* literal types are actually proprietary in Flow *)
  | NumLit _  -> "number"
  | StrLit _   -> "string"
  | BoolLit _  -> "boolean"
  | Mu _
  | Tup _
  | InlineInterface _
  | EnumDecl _
  | Utility _
  | InterfaceDecl _
  | TVar _
  | Bound _
  | TypeAlias _
  | TypeOf _
    -> "proprietary"

let rec dump (t: Ty.t) =
  (("kind", t |> kind_of |> as_json_str)
  ::
  details_of t)
  |> as_json_obj

and details_of (t: Ty.t) = match t with
  | Void
  | Null
  | Num _
  | Str _
  | Bool _
  | Top
  | Bot _     
  | Any _     -> []
  | Fun f     -> details_of_function f
  | Obj o     -> details_of_object o
  | Arr a     -> details_of_array a
  | Generic g -> details_of_generic g
  | Union (t0, t1, ts) -> ["types", (t0 :: t1 :: ts) |> List.map dump |> as_json_arr]
  | Inter (t0, t1, ts) -> ["types", (t0 :: t1 :: ts) |> List.map dump |> as_json_arr]
  | Tup ts    -> ["types", ts |> List.map dump |> as_json_arr]
  | _ -> ["flowtype", json_of_t t]

and details_of_function ({ fun_params; fun_return; _ }) =
  let details_of_param ty name =
    [
      "kind", dump ty;
      "name", name |> Option.value ~default:"_" |> as_json_str
    ]
    |> as_json_obj
  in
  [
    "params",
      fun_params
      |> List.map (fun (name, ty, _) -> details_of_param ty name)
      |> as_json_arr;
    "returnType", fun_return |> dump;
  ]

and details_of_object { obj_exact; obj_props; obj_literal; obj_frozen } =
  let details_of_prop prop = match prop with
    | NamedProp(name, p) ->
      [
        "propType", "named" |> as_json_str;
        "name", name |> as_json_str;
      ]
      @
      (match p with
        | Field (ty, { fld_optional; _ }) ->
          [
            "kind", "field" |> as_json_str;
            "type", dump ty;
            "optional", fld_optional |> as_json_bool;
          ]
        | Method ty ->
          [
            "kind", "method" |> as_json_str;
            "type", ty |> details_of_function |> as_json_obj;
          ]
        | Get ty ->
          [
            "kind", "get" |> as_json_str;
            "type", dump ty;
          ]
        | Set ty ->
          [
            "kind", "set" |> as_json_str;
            "type", dump ty;
          ])
    | _ ->
      [
        "propType", "other" |> as_json_str;
        "flowtype", json_of_prop prop
      ]
  in
  [
    "exact", obj_exact |> as_json_bool;
    "frozen", obj_frozen |> as_json_bool;
    "literal", obj_literal |> as_json_bool;
    "props", obj_props |> List.map details_of_prop |> List.map as_json_obj |> as_json_arr;
  ]
and details_of_array ({ arr_readonly; arr_literal; arr_elt_t }) =
  [
    "readonly", arr_readonly |> as_json_bool;
    "literal", arr_literal |> as_json_bool;
    "type", dump arr_elt_t;
  ]
and details_of_generic (s, k, targs_opt) =
  (match targs_opt with
  | Some(targs) -> ["typeArgs", targs |> List.map dump |> as_json_arr]
  | None -> [])
  @
  [
    "type", s |> json_of_symbol;
    "genericKind", k |> Ty.debug_string_of_generic_kind |> as_json_str;
  ]
