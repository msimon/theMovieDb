open Lwt
open Ocsigen_http_frame
open Ocsigen_stream
open Ocsigen_lib

open TheMovieDbConfig

exception Incorrect_response

let generate_headers headers =
  List.fold_left (
    fun headers (label, value) ->
      let name = Http_headers.name label in
      Http_headers.add name value headers
  ) (Http_headers.empty) headers

let fragment_url (url : string) =
  let (https, host, port, uri, _, _, _) = Url.parse (Obj.magic url) in
  let host = match host with
    | None -> raise Not_found
    | Some h -> h in
  (https, host, port, "/" ^ uri)

let read_stream_full = function
  | None -> Lwt.return ""
  | Some content ->
    let rec read_stream acc = function
      | Ocsigen_stream.Finished (None) -> Lwt.return acc
      | Ocsigen_stream.Finished (Some stream) -> Ocsigen_stream.next stream >>= read_stream acc
      | Ocsigen_stream.Cont (s, next_stream) -> Ocsigen_stream.next next_stream >>= read_stream (acc ^ s)
    in
    let st = Ocsigen_stream.get content in
    lwt s =
      lwt step = Ocsigen_stream.next st in
      read_stream "" step
    in
    lwt _ = Ocsigen_stream.finalize content `Success in
    Lwt.return s

let read_stream_bound ?(max=10000) stream =
  match stream with
    | None -> Lwt.return ""
    | Some stream ->
      let content = Ocsigen_stream.get stream in
      try_lwt
        lwt s = Ocsigen_stream.string_of_stream max content in
        lwt _ = Ocsigen_stream.finalize stream `Success in
        Lwt.return s
      with _ ->
        lwt _ = Ocsigen_stream.finalize stream `Success in
        Lwt.return ""

let read_response ?max frame =
  lwt s = read_stream_bound ?max frame.frame_content in
  Lwt.return (s,frame.frame_header)

let rec do_safe_call ?max f =
  try_lwt
    f () >>= (read_response ?max)
  with
    | Ocsigen_http_com.Keepalive_timeout ->
      lwt _ = Lwt_unix.sleep 1. in
      do_safe_call ?max f
    | exc -> fail exc

(***** GET REQUEST *****)
let get ?max ?https ?port ?(headers=[]) ~host ~uri () =
  let headers = generate_headers headers in
  do_safe_call ?max (Ocsigen_http_client.get ~headers ?https ?port ~host ~uri)

let rec get_safe ?(headers=[]) ~host ~uri () =
  try_lwt
    get ~headers ~host ~uri ()
  with _ -> Lwt_unix.sleep 1.0 >>= get_safe ~headers ~host ~uri


let get_url ?max ?headers url () =
  let (https, host, port, uri) = fragment_url url in
  get ?max ?https ?port ?headers ~host ~uri ()

(***** POST REQUEST *****)
let post_string ?https ?port ?(headers=[]) ~host ~uri ~content ?(content_type=("application","x-www-form-urlencoded")) () =
  let headers = generate_headers headers in
  let content = Netencoding.Url.mk_url_encoded_parameters content in
  do_safe_call (Ocsigen_http_client.post_string ?https ?port ~headers ~host ~uri ~content ~content_type)

let post_string_raw ?https ?port ?(headers=[]) ~host ~uri ~content ?(content_type=("application","x-www-form-urlencoded")) () =
  let headers = generate_headers headers in
  do_safe_call (Ocsigen_http_client.post_string ?https ?port ~headers ~host ~uri ~content ~content_type)

let post_string_url ?headers ?content_type ~content url () =
  let (https, host, port, uri) = fragment_url url in
  post_string ?https ?port ?headers ~host ~uri ~content ?content_type ()


let build_url =
  let call_nb = ref 1 in
  let first_time = ref None in
  let retry_after = ref 0 in

  let rec build_url_in ?(params=[]) config ~uri =
    incr(call_nb);
    let call_nb = !call_nb in

    lwt _ =
      match !first_time with
        | None ->
          first_time := Some (Unix.gettimeofday ());
          Lwt.return_unit
        | Some t ->
          let t_ = Unix.gettimeofday () in

          let n = (float_of_int call_nb) /. config.request_per_second in
          let waiting_time = n +. (float_of_int !retry_after) in

          if (t_ -. t < waiting_time) then begin
            Lwt_unix.sleep (waiting_time -. (t_ -. t))
          end else Lwt.return_unit
    in

    let url = Printf.sprintf "%s%s?api_key=%s" config.api_url uri config.api_key in
    let headers = [
      "Accept", "application/json"
    ] in

    let url =
      List.fold_left (
        fun s (k,v) ->
          s ^ (Printf.sprintf "&%s=%s" k v)
      ) url params
    in

    lwt s,h = get_url ~headers url () in

    lwt s =
      Ocsigen_http_frame.Http_header.(
        match h.mode with
          | Answer i when i = 200 || i = 304 ->
            if (config.debug.display_correct_response) then
              Printf.printf "cr: %d: %s\n%!" call_nb s;
            Lwt.return s
          | Answer i when i = 429 ->
            let n = get_headers_value h (Http_headers.name "Retry-After") in

            if (config.debug.display_retry_after) then
              Printf.printf "retry-after: %d: %s\n%!" call_nb n;

            lwt _ = Lwt_unix.sleep (float_of_string n) in
            lwt s = build_url_in ~params config ~uri in
            retry_after := !retry_after + (((int_of_string n) + 1) / config.max_concurrent_connection);
            Lwt.return s
          | Answer i when i = 404 ->
            raise Not_found
          | _ ->
            if (config.debug.display_incorrect_response) then
              Printf.printf "ir: %d: %s\n%!" call_nb s;
            raise Incorrect_response
      )
    in

    Lwt.return s
  in
  build_url_in
