(* SPDX-License-Identifier: MIT *)

type errors = [
  | `Api_error of Http_lwt_client.response * string option
  | `Malformed_json of string
  | `Msg of string
  | `No_corresponding_arch_found
  | `No_corresponding_os_found
]

let fmt = Printf.sprintf

let hurl ~meth ~headers url =
  match%lwt
    Http_lwt_client.one_request
      ~config:(`HTTP_1_1 Httpaf.Config.default) (* TODO: Remove this when https://github.com/roburio/http-lwt-client/issues/7 is fixed *)
      ~meth
      ~headers
      url
  with
  | Ok ({Http_lwt_client.status = `OK; _}, Some body) -> Lwt.return (Ok body)
  | Ok (resp, body) -> Lwt.return (Error (`Api_error (resp, body)))
  | Error e -> Lwt.return (Error e)

let json_get field = function
  | `Assoc l -> List.find_map (fun (k, v) -> if String.equal k field then Some v else None) l
  | _ -> None

let get_token ~repo =
  match%lwt
    hurl ~meth:`GET
      ~headers:[]
      (fmt "https://auth.docker.io/token?service=registry.docker.io&scope=repository:%s:pull" repo)
  with
  | Ok token_json ->
      let token_json = Yojson.Safe.from_string token_json in
      begin match json_get "token" token_json with
      | Some (`String token) -> Lwt.return (Ok token)
      | _ -> Lwt.return (Error (`Malformed_json "token"))
      end
  | Error e -> Lwt.return (Error e)

let rec find_manifest ~os ~arch = function
  | manifest::manifests ->
      begin match json_get "platform" manifest with
      | Some platform ->
          begin match json_get "os" platform with
          | Some (`String os_) when String.equal os os_ ->
              begin match json_get "architecture" platform with
              | Some (`String arch_) when String.equal arch arch_ -> Ok manifest
              | Some (`String _arch) -> find_manifest ~os ~arch manifests
              | _ -> Error `No_corresponding_arch_found
              end
          | Some (`String _os) -> find_manifest ~os ~arch manifests
          | _ -> Error `No_corresponding_os_found
          end
      | None -> Error (`Malformed_json "platform")
      end
  | [] -> Error (`Malformed_json "empty manifests")

let fetch_digest ~os ~arch ~repo ~tag =
  let tag = Option.value tag ~default:"latest" in
  match%lwt get_token ~repo with
  | Ok token ->
      begin match%lwt
        let api = "application/vnd.docker.distribution.manifest.v2+json" in
        let apil = "application/vnd.docker.distribution.manifest.list.v2+json" in
        hurl ~meth:`GET
          ~headers:[("Accept", api); ("Accept", apil); ("Authorization", fmt "Bearer %s" token)]
          (fmt "https://registry-1.docker.io/v2/%s/manifests/%s" repo tag)
      with
      | Ok manifests_json ->
          let manifests_json = Yojson.Safe.from_string manifests_json in
          begin match json_get "manifests" manifests_json with
          | Some (`List manifests) ->
              begin match find_manifest ~os ~arch manifests with
              | Ok manifest ->
                  begin match json_get "digest" manifest with
                  | Some (`String digest) -> Lwt.return (Ok digest)
                  | _ -> Lwt.return (Error (`Malformed_json "digest"))
                  end
              | Error e -> Lwt.return (Error e)
              end
          | _ -> Lwt.return (Error (`Malformed_json "no manifests"))
          end
      | Error e -> Lwt.return (Error e)
      end
  | Error e -> Lwt.return (Error e)
