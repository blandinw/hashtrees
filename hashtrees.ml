open Printf

module StringSet = Set.Make (struct
  type t = string

  let compare = Stdlib.compare
end)

let report typ fmt =
  printf "%-7s | " typ;
  printf fmt

let rec diff_aux n_same_ref a b =
  (* TODO catch Sys_error *)
  let a_files = StringSet.of_list (Array.to_list (Sys.readdir a)) in
  let b_files = StringSet.of_list (Array.to_list (Sys.readdir b)) in
  let both = StringSet.inter a_files b_files in
  let a_only = StringSet.diff a_files b_files in
  let b_only = StringSet.diff b_files a_files in

  StringSet.iter (fun x -> report "a only" "%s\n" (Filename.concat a x)) a_only;
  StringSet.iter (fun x -> report "b only" "%s\n" (Filename.concat b x)) b_only;

  (* case:
       if only one is directory, print X is a directory while Y is a file
       if both file, compare md5
       if both directories, recurse
  *)
  StringSet.iter
    (fun x ->
      let is_dir x = try Sys.is_directory x with _ -> false in
      let ax = Filename.concat a x in
      let bx = Filename.concat b x in
      if is_dir ax && not (is_dir bx) then
        report "bad type" "%s (d) %s (f)\n" ax bx
      else if (not (is_dir ax)) && is_dir bx then
        report "bad type" "%s (f) %s (d)\n" ax bx
      else if (not (is_dir ax)) && not (is_dir bx) then
        let digest path =
          try Digest.file path
          with Sys_error msg ->
            eprintf "warning: could not compute file digest. %s\n" msg;
            ""
        in
        if not (Digest.equal (digest ax) (digest bx)) then
          report "bad md5" "%s %s\n" ax bx
        else
          (* files are the same *)
          (* report "ok" "%s %s\n" ax bx; *)
          n_same_ref := !n_same_ref + 1
      else (* both are directories *)
        diff_aux n_same_ref ax bx)
    both

let diff a b =
  let n_same = ref 0 in
  diff_aux n_same a b;
  eprintf "Found %d identical files.\n" !n_same

(* Command-line interface *)

open Cmdliner

let source =
  let doc = "Source directory" in
  let docv = "SOURCE" in
  Arg.(required & pos 0 (some string) None & info [] ~doc ~docv)

let dest =
  let doc = "Destination directory" in
  let docv = "DEST" in
  Arg.(required & pos 1 (some string) None & info [] ~doc ~docv)

let diff_t = Term.(const diff $ source $ dest)

let diff_cmd =
  let doc =
    "Recursively compare two directories and print out the differences (missing files/directories, different MD5 sums, etc.)"
  in
  (diff_t, Term.info "diff" ~doc)

let main _ = Term.exit @@ Term.eval diff_cmd

;;
main ()
