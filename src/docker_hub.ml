(* SPDX-License-Identifier: MIT *)

type fetch_errors = [
  | `Api_error of Http_lwt_client.response * string option
  | `Malformed_json of string
  | `Msg of string
]

type digest_errors = [
  | `Malformed_json of string
  | `No_corresponding_arch_found
  | `No_corresponding_os_found
]

type t = Yojson.Safe.t list

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

(* OCaml translation of a shell script found here: https://stackoverflow.com/a/37759182 *)
let fetch_manifests ~repo ~tag =
  (* TODO: Check the repo and tag arguments for invalid characters *)
  (* nameComponentRegexp restricts registry path component names to start
     with at least one letter or number, with following parts able to be
     separated by one period, one or two underscore and multiple dashes.
     Source: https://stackoverflow.com/questions/43091075/docker-restrictions-regarding-naming-image *)
  let repo = match String.split_on_char '/' repo with
    | [_; _] -> repo
    | [_] -> "library/"^repo
    | _ -> raise (Invalid_argument "Docker_hub.fetch_manifests ~repo")
  in
  let tag = Option.value tag ~default:"latest" in
  match%lwt get_token ~repo with
  | Ok token ->
      let api = "application/vnd.docker.distribution.manifest.v2+json" in
      let apil = "application/vnd.docker.distribution.manifest.list.v2+json" in
      begin match%lwt
        hurl ~meth:`GET
          ~headers:[("Accept", api); ("Accept", apil); ("Authorization", fmt "Bearer %s" token)]
          (fmt "https://registry-1.docker.io/v2/%s/manifests/%s" repo tag)
      with
      | Ok json ->
          begin match Yojson.Safe.from_string json with
          | json ->
              begin match json_get "mediaType" json with
              | Some (`String media_type) ->
                  if String.equal media_type api then
                    Lwt.return (Ok [json])
                  else if String.equal media_type apil then
                    match json_get "manifests" json with
                    | Some (`List manifests) -> Lwt.return (Ok manifests)
                    | _ -> Lwt.return (Error (`Malformed_json "no manifests"))
                  else
                    Lwt.return (Error (`Malformed_json "mediaType does not match request"))
              | _ -> Lwt.return (Error (`Malformed_json "no mediaType"))
              end
          | exception (Yojson.Json_error msg) -> Lwt.return (Error (`Malformed_json msg))
          end
      | Error e -> Lwt.return (Error e)
      end
  | Error e -> Lwt.return (Error e)

let pp fmt manifests =
  Yojson.Safe.pretty_print fmt (`List manifests)

let digest ~os ~arch manifests =
  match find_manifest ~os ~arch manifests with
  | Ok manifest ->
      begin match json_get "digest" manifest with
      | Some (`String digest) -> Ok digest
      | _ -> Error (`Malformed_json "digest")
      end
  | Error e -> Error e
