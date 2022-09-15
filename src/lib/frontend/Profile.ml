let time_profile msg (f : unit -> 'a) : 'a =
  let start_time = Sys.time () in
  let res = f () in
  let finish_time = Sys.time () in
  Printf.printf "%s took: %f secs to complete\n%!" msg (finish_time -. start_time);
  res
;;

let time_profile_absolute msg (f : unit -> 'a) : 'a =
  let start_time = Unix.gettimeofday () in
  let res = f () in
  let finish_time = Unix.gettimeofday () in
  Printf.printf "%s took: %f secs to complete\n%!" msg (finish_time -. start_time);
  res
;;
