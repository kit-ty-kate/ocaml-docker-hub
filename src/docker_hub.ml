(* SPDX-License-Identifier: MIT *)

(* OCaml translation of a shell script found here: https://stackoverflow.com/a/37759182 *)
(* More complete script also available here: https://github.com/moby/moby/blob/924edb948c2731df3b77697a8fcc85da3f6eef57/contrib/download-frozen-image-v2.sh *)

type fetch_errors = [
  | `Api_error of Http_lwt_client.response * string option
  | `Malformed_json of string
  | `Msg of string
]

let fmt = Printf.sprintf
let ( >>= ) = Result.bind

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

module Json : sig
  type t

  val parse : string -> (t, [> `Malformed_json of string]) result

  val get : string -> t -> (t, [> `Malformed_json of string]) result
  val get_string : string -> t -> (string, [> `Malformed_json of string]) result
  val get_list : string -> t -> (t list, [> `Malformed_json of string]) result
  val get_string_list : string -> t -> (string list, [> `Malformed_json of string]) result

  val get_string_opt : string -> t -> string option

  val map :
    (t -> ('a, [> `Malformed_json of string] as 'b) result) ->
    t list ->
    ('a list, [> `Malformed_json of string] as 'b) result

  val pp : Format.formatter -> t -> unit
end = struct
  type t = Yojson.Safe.t

  let parse json =
    match Yojson.Safe.from_string json with
    | json -> Ok json
    | exception (Yojson.Json_error msg) -> Error (`Malformed_json msg)

  let get field = function
    | `Assoc l ->
        begin match List.find_opt (fun (k, _) -> String.equal k field) l with
        | Some (_, v) -> Ok v
        | None -> Error (`Malformed_json field)
        end
    | _ -> Error (`Malformed_json field)

  let get_opt field json =
    match get field json with
    | Ok x -> Some x
    | Error _ -> None

  let get_string field json =
    get field json >>= function
    | `String str -> Ok str
    | _ -> Error (`Malformed_json field)

  let get_string_opt field json =
    match get_opt field json with
    | Some (`String str) -> Some str
    | _ -> None

  let get_list field json =
    get field json >>= function
    | `List l -> Ok l
    | _ -> Error (`Malformed_json field)

  let map f l =
    List.fold_left (fun acc x ->
      acc >>= fun acc ->
      f x >>= fun x ->
      Ok (acc @ [x])
    ) (Ok []) l

  let get_string_list field json =
    get field json >>= function
    | `List l ->
        map (function
          | `String x -> Ok x
          | _ -> Error (`Malformed_json "not a list of string")
        ) l
    | _ -> Error (`Malformed_json field)

  let pp fmt json =
    Yojson.Safe.pretty_print fmt json
end

module Platform = struct
  type t = {
    os : string;
    arch : string;
    variant : string option;
  }
end

module Image = struct
  type name = {name : string}
  type tag = {tag : string}
  type digest = {digest : string}

  let parse ~func_name image =
    (* TODO: Check the image argument for invalid characters *)
    (* nameComponentRegexp restricts registry path component names to start
       with at least one letter or number, with following parts able to be
       separated by one period, one or two underscore and multiple dashes.
       Source: https://stackoverflow.com/questions/43091075/docker-restrictions-regarding-naming-image *)
    let fail () = raise (Invalid_argument func_name) in
    let parse_name_and_tag image =
      let parse_name image =
        match String.split_on_char '/' image with
        | [_; _] -> {name = image}
        | [_] -> {name = "library/"^image}
        | _ -> fail ()
      in
      match String.split_on_char ':' image with
      | [image; tag] -> (parse_name image, {tag})
      | [image] -> (parse_name image, {tag = "latest"})
      | _ -> fail ()
    in
    match String.split_on_char '@' image with
    | [image; digest] -> (parse_name_and_tag image, Some {digest})
    | [image] -> (parse_name_and_tag image, None)
    | _ -> fail ()

  let with_digest image =
    let func_name = "Docker_hub.Image.with_digest" in
    match parse ~func_name image with
    | (_, None) -> raise (Invalid_argument func_name)
    | ((name, tag), Some digest) -> (name, tag, digest)

  let without_digest image =
    let func_name = "Docker_hub.Image.without_digest" in
    match parse ~func_name image with
    | (_, Some _) -> raise (Invalid_argument func_name)
    | ((name, tag), None) -> (name, tag)

  let ignore_digest image =
    let func_name = "Docker_hub.Image.ignore_digest" in
    Stdlib.fst (parse ~func_name image)
end

module Token = struct
  type t = {
    json : Json.t;
    token : string;
    name : string;
  }

  let fetch {Image.name} =
    match%lwt
      hurl ~meth:`GET
        ~headers:[]
        (fmt "https://auth.docker.io/token?service=registry.docker.io&scope=repository:%s:pull" name)
    with
    | Ok json ->
        Lwt.return begin
          Json.parse json >>= fun json ->
          Json.get_string "token" json >>= fun token ->
          Ok {json; token; name}
        end
    | Error e -> Lwt.return (Error e)

  let pp fmt {json; token = _; name = _} =
    Json.pp fmt json
end

let check_media_type media_type media_type' =
  if String.equal media_type media_type' then
    Ok ()
  else
    Error (`Malformed_json (fmt "mediaType: %s != %s" media_type media_type'))

module Manifest = struct
  type t = {
    json : Json.t;
    config_digest : string;
    rootfs_digest : string;
  }

  let media_type = "application/vnd.docker.distribution.manifest.v2+json"
  let config_media_type = "application/vnd.docker.container.image.v1+json"
  let rootfs_media_type = "application/vnd.docker.image.rootfs.diff.tar.gzip"

  let fetch {Image.digest} {Token.token; name; _} =
    match%lwt
      hurl ~meth:`GET
        ~headers:[("Accept", media_type); ("Authorization", fmt "Bearer %s" token)]
        (fmt "https://registry-1.docker.io/v2/%s/manifests/%s" name digest)
    with
    | Ok json ->
        Lwt.return begin
          Json.parse json >>= fun json ->
          begin
            Json.get "config" json >>= fun config ->
            Json.get_string "mediaType" config >>= fun config_media_type' ->
            check_media_type config_media_type config_media_type' >>= fun () ->
            Json.get_string "digest" config
          end >>= fun config_digest ->
          begin
            Json.get_list "layers" json >>= function
            | [] | _::_::_ -> Error (`Msg "Does not support multiple layers yet") (* TODO *)
            | [layer] ->
                Json.get_string "mediaType" layer >>= fun rootfs_media_type' ->
                check_media_type rootfs_media_type rootfs_media_type' >>= fun () ->
                Json.get_string "digest" layer
          end >>= fun rootfs_digest ->
          Ok {json; config_digest; rootfs_digest}
        end
    | Error e -> Lwt.return (Error e)

  let pp fmt {json; config_digest = _; rootfs_digest = _} =
    Json.pp fmt json
end

module Manifests = struct
  type elt = {
    platform : Platform.t;
    digest : Image.digest;
  }

  type t = {
    json : Json.t;
    elements : elt list;
  }

  let get_elt json =
    Json.get_string "digest" json >>= fun digest ->
    Json.get "platform" json >>= fun platform ->
    Json.get_string "os" platform >>= fun os ->
    Json.get_string "architecture" platform >>= fun arch ->
    let variant = Json.get_string_opt "variant" platform in
    let platform = {Platform.os; arch; variant} in
    Ok {platform; digest = {Image.digest}}

  let media_type = "application/vnd.docker.distribution.manifest.list.v2+json"

  let fetch {Image.tag} {Token.token; name; _} =
    match%lwt
      hurl ~meth:`GET
        ~headers:[("Accept", media_type); ("Authorization", fmt "Bearer %s" token)]
        (fmt "https://registry-1.docker.io/v2/%s/manifests/%s" name tag)
    with
    | Ok json ->
        Lwt.return begin
          Json.parse json >>= fun json ->
          Json.get_list "manifests" json >>= fun elements ->
          Json.map get_elt elements >>= fun elements ->
          Ok {json; elements}
        end
    | Error e -> Lwt.return (Error e)

  let elements {elements; _} = elements

  let pp fmt {json; elements = _} =
    Json.pp fmt json
end

module Config = struct
  type t = {
    json : Json.t;
    env : string list;
    (* TODO: parse .architecture .os .variant *)
  }

  let fetch {Manifest.config_digest; _} {Token.token; name; _} =
    match%lwt
      hurl ~meth:`GET
        ~headers:[("Accept", Manifest.config_media_type); ("Authorization", fmt "Bearer %s" token)]
        (fmt "https://registry-1.docker.io/v2/%s/blobs/%s" name config_digest)
    with
  | Ok json ->
      Lwt.return begin
        Json.parse json >>= fun json ->
        (Json.get "config" json >>= Json.get_string_list "Env") >>= fun env ->
        Ok {json; env}
      end
  | Error e -> Lwt.return (Error e)

  let env {env; _} = env

  let pp fmt {json; env = _} =
    Json.pp fmt json
end

let fetch_rootfs ~output_dir {Manifest.rootfs_digest; _} {Token.token; name; _} =
  match%lwt
    hurl ~meth:`GET
      ~headers:[("Accept", Manifest.rootfs_media_type); ("Authorization", fmt "Bearer %s" token)]
      (fmt "https://registry-1.docker.io/v2/%s/blobs/%s" name rootfs_digest)
  with
  | Ok x ->
      Lwt_io.with_file ~mode:Lwt_io.Output (Fpath.to_string output_dir) begin fun ch ->
        let%lwt () = Lwt_io.write ch x in
        Lwt.return (Ok ())
      end
  | Error e -> Lwt.return (Error e)
