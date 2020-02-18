let rewrite_uri new_uri repo_dir =
  let opam_dir = OpamFilename.Dir.of_string repo_dir in
  let repo = OpamRepositoryBackend.local opam_dir in
  OpamPackage.Map.iter
    (fun pkg prefix ->
      let name = OpamPackage.(Name.to_string (name pkg)) in
      let opam_file = OpamRepositoryPath.opam opam_dir prefix pkg in
      if OpamFile.exists opam_file then
        let file = OpamFile.OPAM.read opam_file in
        match OpamFile.OPAM.url file with
        | Some u ->
            let fname =
              OpamFile.URL.url u |> OpamUrl.to_string |> Filename.basename
            in
            let new_uri =
              Printf.sprintf "%s/%s/%s.%s/%s" new_uri name name
                OpamPackage.(Version.to_string (version pkg))
                fname
              |> OpamUrl.parse
            in
            let uri =
              OpamFile.URL.create ~checksum:(OpamFile.URL.checksum u) new_uri
            in
            let replaced = OpamFile.OPAM.with_url uri file in
            OpamFile.OPAM.write opam_file replaced
        | None -> ()
      else ())
    (OpamRepository.packages_with_prefixes repo)

open Cmdliner

let repodir =
  let doc = "Input directory." in
  Arg.(value & opt string "." & info [ "i" ] ~docv:"INPUT_DIR" ~doc)

let uri =
  Arg.(
    required
    & pos 0 (some string) None
    & info [] ~docv:"URI" ~doc:"URI for rewrite to.")

let cmd =
  let doc = "rewrite OPAM uri" in
  ( Term.(pure rewrite_uri $ uri $ repodir),
    Term.info "opam_mirror_rewrite_urls" ~version:"1.0.0" ~doc )

let () = match Term.eval cmd with `Error _ -> exit 1 | _ -> exit 0
