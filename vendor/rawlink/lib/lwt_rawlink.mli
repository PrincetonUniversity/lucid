(*
 * Copyright (c) 2015-2022 Christiano F. Haesbaert <haesbaert@haesbaert.org>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *)

(** A portable API to send and receive raw packets.

    There are times when one needs to construct the full ethernet frame to
    send/receive. Most unixes support BPF (BSD Packet Filter) to achieve it, but
    linux provides the same functionality via AF_SOCKET. This API works with
    either a BPF or AF_SOCKET backend, so it should work on every usable UNIX,
    as well as linux out there. The module is designed to work with Lwt threads.
*)

type t

val open_link : ?filter:string -> ?promisc:bool -> string -> t
(** [open_link ~filter ~promisc interface]. Creates a rawlink on the
    specified [interface], a BPF program [filter] can be passed to
    filter out incoming packets. If [promisc] is true, sets [interface]
    to promiscuous mode, defaults to false. *)

val close_link : t -> unit Lwt.t
(** [close_link]. Closes a rawlink. *)

val read_packet : t -> Cstruct.t Lwt.t
(** [read_packet t]. Reads a full packet, may raise Unix.Unix_error. *)

val send_packet : t -> Cstruct.t -> unit Lwt.t
(** [send_packet t]. Sends a full packet, may raise Unix.Unix_error. *)

val dhcp_server_filter : unit -> string
(** [dhcp_server_filter]. Returns a BPF program suitable to be passed in
    [open_link ~filter], it accepts UDP packets destined to
    port 67 (DHCP client). *)

val dhcp_client_filter : unit -> string
(** [dhcp_client_filter]. Returns a BPF program suitable to be passed in
    [open_link ~filter], it accepts UDP packets destined to
    port 68 (DHCP server). *)
