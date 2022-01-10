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

type t

val fetch_manifests :
  repo:string ->
  tag:string option ->
  (t, [> fetch_errors]) result Lwt.t

val digest :
  os:string ->
  arch:string ->
  t ->
  (string, [> digest_errors]) result
