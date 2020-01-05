(**
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(***********************************************************************)
(* flow export-types command *)
(***********************************************************************)

open CommandUtils


let spec =
  {
    CommandSpec.name = "export-types";
    doc = "Outputs list of all types in the file";
    usage =
      Printf.sprintf
        "Usage: %s export-types [OPTION]... [FILE]\n\ne.g. %s export-types foo.js\nor   %s export-types < foo.js\n"
        CommandUtils.exe_name
        CommandUtils.exe_name
        CommandUtils.exe_name;
    args =
      CommandSpec.ArgSpec.(
        empty
        |> base_flags
        |> connect_and_json_flags
        |> root_flag
        |> strip_root_flag
        |> from_flag
        |> path_flag
        |> wait_for_recheck_flag
        |> flag "--expand-type-aliases" no_arg ~doc:"Replace type aliases with their bodies"
        |> flag
            "--evaluate-type-destructors"
            no_arg
            ~doc:"Use the result of type destructor evaluation if available"
        |> anon "file" (optional string));
  }


let types_to_json ~file_content types ~strip_root =
  let open Hh_json in
  let offset_table =
    Option.map file_content ~f:(Offset_utils.make ~kind:Offset_utils.Utf8)
  in
  let type_with_location (loc, t) = JSON_Object [
    "type", Ty_export.json_of_t t;
    "loc", Reason.json_of_loc ~strip_root ~offset_table loc
  ]
  in
  JSON_Array (types |> List.map type_with_location)


let handle_response types ~file_content ~pretty ~strip_root =
  types
  |> types_to_json ~file_content ~strip_root
  |> Hh_json.print_json_endline ~pretty


let handle_error err ~file_content ~pretty ~strip_root =
  let open Hh_json in
  JSON_Object [("error", JSON_String err)] |> prerr_json_endline ~pretty;

  (* also output an empty array on stdout, for JSON parsers *)
  handle_response [] ~file_content ~pretty ~strip_root


let main
    base_flags
    option_values
    _json
    pretty
    root
    strip_root
    path
    wait_for_recheck
    expand_aliases
    evaluate_type_destructors
    filename
    () =
  let file = get_file_from_filename_or_stdin ~cmd:CommandSpec.(spec.name) path filename in
  let file_content = file |> File_input.content_of_file_input |> Base.Result.ok in
  let flowconfig_name = base_flags.Base_flags.flowconfig_name in
  let root =
    guess_root
      flowconfig_name
      (match root with
      | Some root -> Some root
      | None -> File_input.path_of_file_input file)
  in
  let strip_root =
    if strip_root then
      Some root
    else
      None
  in
  let request =
    ServerProt.Request.DUMP_TYPES
      { input = file; expand_aliases; evaluate_type_destructors; wait_for_recheck }
  in
  match connect_and_make_request flowconfig_name option_values root request with
  | ServerProt.Response.DUMP_TYPES (Error err) -> handle_error err ~file_content ~pretty ~strip_root
  | ServerProt.Response.DUMP_TYPES (Ok resp) -> handle_response resp ~file_content ~pretty ~strip_root
  | response -> failwith_bad_response ~request ~response

let command = CommandSpec.command spec main
