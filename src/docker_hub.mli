(* SPDX-License-Identifier: MIT *)

type errors = [
  | `Api_error of Http_lwt_client.response * string option
  | `Malformed_json of string
  | `Msg of string
  | `No_corresponding_arch_found
  | `No_corresponding_os_found
]

val fetch_digest :
  os:string ->
  arch:string ->
  repo:string ->
  tag:string option ->
  (string, [> errors]) result Lwt.t
