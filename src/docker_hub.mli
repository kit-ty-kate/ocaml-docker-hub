(* SPDX-License-Identifier: MIT *)

type fetch_errors = [
  | `Api_error of Http_lwt_client.response * string option
  | `Malformed_json of string
  | `Msg of string
]

module Platform : sig
  type t = {
    os : string;
    arch : string;
    variant : string option;
  }
end

module Image : sig
  type name
  type tag
  type digest

  val with_digest : string -> name * tag * digest
  val without_digest : string -> name * tag
  val ignore_digest : string -> name * tag
end

module Token : sig
  type t

  val fetch : Image.name -> (t, [> fetch_errors]) result Lwt.t

  val pp : Format.formatter -> t -> unit
end

module Manifest : sig
  type t

  val fetch :
    Image.digest ->
    Token.t ->
    (t, [> fetch_errors]) result Lwt.t

  val pp : Format.formatter -> t -> unit
end

module Manifests : sig
  type t

  type elt = {
    platform : Platform.t;
    digest : Image.digest;
  }

  val fetch :
    Image.tag ->
    Token.t ->
    (t, [> fetch_errors]) result Lwt.t

  val elements : t -> elt list

  val pp : Format.formatter -> t -> unit
end

module Config : sig
  type t

  val fetch :
    Manifest.t ->
    Token.t ->
    (t, [> fetch_errors]) result Lwt.t

  val env : t -> string list

  val pp : Format.formatter -> t -> unit
end

val fetch_rootfs :
  output_dir:Fpath.t ->
  Manifest.t ->
  Token.t ->
  (unit, [> fetch_errors]) result Lwt.t
