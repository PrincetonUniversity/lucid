let a_mac = Macaddr.of_string_exn "18:18:18:18:18:18"
let a_ip = Ipaddr.V4.of_string_exn "18.18.18.18"
let b_mac = Macaddr.of_string_exn "29:29:29:29:29:29"
let b_ip = Ipaddr.V4.of_string_exn "29.29.29.29"

let a_arp_request =
  let source_mac = a_mac in
  let source_ip = a_ip in
  let target_mac = Macaddr.of_string_exn "00:00:00:00:00:00" in
  let target_ip = b_ip in
  let arp = Arp_packet.{ operation = Request; source_mac; source_ip; target_mac; target_ip } in
  let eth = Ethernet.Packet.{ source = source_mac; destination = Macaddr.broadcast; ethertype = `ARP } in
  Cstruct.append (Ethernet.Packet.make_cstruct eth) (Arp_packet.encode arp)

let b_arp_reply =
  let source_mac = b_mac in
  let source_ip = b_ip in
  let target_mac = b_mac in
  let target_ip = a_ip in
  let arp = Arp_packet.{ operation = Reply; source_mac; source_ip; target_mac; target_ip } in
  let eth = Ethernet.Packet.{ source = source_mac; destination = Macaddr.broadcast; ethertype = `ARP } in
  Cstruct.append (Ethernet.Packet.make_cstruct eth) (Arp_packet.encode arp)

let pp hdr buf =
  print_string hdr;
  Cstruct.hexdump buf;
  print_newline ()

let send link hdr pkt =
  Eio_rawlink.send_packet link pkt;
  pp hdr pkt

let recv link hdr =
  let pkt = Eio_rawlink.read_packet link in
  pp hdr pkt;
  pkt

let rec wait_pkt link hdr pkt =
  let pkt' = recv link hdr in
  if not (Cstruct.equal pkt pkt') then
    wait_pkt link hdr pkt

let () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 3.0 @@ fun () ->

  let a_link = Eio_rawlink.open_link ~sw ~promisc:true "lo" in
  let b_link = Eio_rawlink.open_link ~sw ~promisc:true "lo" in

  send a_link "A sent:" a_arp_request;
  wait_pkt b_link "B got:" a_arp_request;
  Printf.printf "request match !\n%!";

  send b_link "B sent:" b_arp_reply;
  wait_pkt a_link "A got:" b_arp_reply;
  Printf.printf "reply match !\n%!"
