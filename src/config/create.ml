let () = if Array.length Sys.argv <> 3 then assert false

let ( // ) = Filename.concat

let run_one_line_process exe args =
  let ic = Unix.open_process_args_in exe (Array.of_list (exe :: args)) in
  let output = try Stdlib.input_line ic with Stdlib.End_of_file -> "" in
  match Unix.close_process_in ic with
  | Unix.WEXITED 0 -> Some output
  | Unix.WEXITED _ | Unix.WSIGNALED _ | Unix.WSTOPPED _ -> None

let version = Sys.argv.(1)
let project_root = Sys.argv.(2)

let git_root = project_root // ".." // ".." // ".git"

let git_version =
  match Sys.is_directory git_root with
  | true ->
      begin match run_one_line_process "git" ["-C"; git_root; "rev-parse"; "--is-inside-git-dir"] with
      | Some "true" ->
          begin match run_one_line_process "git" ["describe"; "--always"; "--dirty"; "--abbrev=0"] with
          | Some hash -> hash
          | None -> failwith "git describe should never fail"
          end
      | Some _ | None -> failwith "git rev-parse should never fail"
      end
  | false -> failwith ".git should be a directory"
  | exception Stdlib.Sys_error _ -> "(unknown)"

let file_content = Printf.sprintf {|
let version = "%s"
let git_version = "%s"

type kind = Env | Dune_site | Custom

let (<|>) x f = match x with
  | None, _ -> f ()
  | Some x, k -> (x, k)

let (//) = Filename.concat

let parse_dir dir =
  if Sys.file_exists dir && Sys.is_directory dir then
    Ok dir
  else
    Error (`Msg (Printf.sprintf "'%%s' is not a valid directory" dir))

let get_libdir () =
  (Sys.getenv_opt "LABRYS_LIBDIR", Env) <|> fun () ->
  (List.head_opt Labrys_libdir.Sites.labrys, Dune_site) <|> fun () ->
  (* TODO: Is dune-site even necessary now? *)
  (* see: https://github.com/ocaml/dune/issues/4198 *)
  let exe = Sys.argv.(0) in
  let exe_dir = Filename.dirname exe in
  if not (String.equal "bin" (Filename.basename exe_dir)) then begin
    prerr_endline "Error: Cannot detect LABRYS_LIBDIR.";
    prerr_endline "Please set it manually using the LABRYS_LIBDIR environment variable or --lib-dir";
    exit 1
  end;
  (Filename.dirname exe_dir // "lib" // "labrys", Custom)

let lib = lazy begin
  let libdir, kind = get_libdir () in
  match parse_dir libdir with
  | Ok libdir -> libdir
  | Error (`Msg msg) ->
      begin match kind with
      | Env -> prerr_endline "Error: Environment variable LABRYS_LIBDIR is not set properly."
      | Dune_site -> prerr_endline "Error: Something is wrong in your use of labrys. Cannot detect LABRYS_LIBDIR."
      | Custom -> prerr_endline "Error: Something is wrong in your installation of labrys. Cannot detect LABRYS_LIBDIR."
      end;
      prerr_endline ("Reason: "^msg);
      exit 1
end
|} (String.escaped version) (String.escaped git_version)

let () = print_endline file_content
