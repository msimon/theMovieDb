open OUnit2

(* Api key verification *)
let api_key = ""
let api_key_opt = Conf.make_string_opt "key" None "Your MovieDb Api Key"
let all_test = Conf.make_bool "all" false "Apply all test"

let get_api_key test_ctxt =
  match api_key_opt test_ctxt with
    | Some key -> key
    | None -> api_key

(** Bracket **)
let setup test_ctxt =
  TheMovieDbConfig.create (get_api_key test_ctxt)

let teardown _ _ = ()

(** TESTS **)

let api_key_provided test_ctxt =
  assert_equal ~cmp:(fun a b -> a <> b) ~msg:"No Api Key provided" "" (get_api_key test_ctxt)

let fetch_moviedb_configuration test_ctxt =
  let config = bracket setup teardown test_ctxt in

  ignore (Lwt_main.run (TheMovieDb.fetch_moviedb_configuration config))

let fetch_last_movie test_ctxt =
  let config = bracket setup teardown test_ctxt in
  ignore (Lwt_main.run (TheMovieDb.fetch_last_movie config))

let fetch_movie test_ctxt =
  let config = bracket setup teardown test_ctxt in

  let run _ =
    let uid = 550 in
    lwt m = TheMovieDb.fetch_movie config uid in

    assert_equal ~msg:"Uid does not match" uid m.TheMovieDb.Movie.id;
    assert_equal ~msg:"Title does not match" "Fight Club" m.TheMovieDb.Movie.title ;
    assert_equal ~msg:"Original title does not match" "Fight Club" m.TheMovieDb.Movie.original_title;

    Lwt.return ();
  in

  ignore (Lwt_main.run (run ()))


let fetch_genres test_ctxt =
  let config = bracket setup teardown test_ctxt in
  ignore (Lwt_main.run (TheMovieDb.fetch_genres config))


let fetch_movie_changes test_ctxt =
  let config = bracket setup teardown test_ctxt in
  ignore (Lwt_main.run (TheMovieDb.fetch_movie_changes config))


let fetch_all_movie_changes test_ctxt =
  let config = bracket setup teardown test_ctxt in

  let run () =
    lwt c = TheMovieDb.fetch_movie_changes config in
    lwt all_changes = TheMovieDb.fetch_all_movie_changes config in

    assert_equal
      ~msg:"Wrong number of element"
      c.TheMovieDb.Change.total_results
      (List.length all_changes);

    Lwt.return_unit
  in

  ignore (Lwt_main.run (run ()))


let fetch_movie_change test_ctxt =
  let config = bracket setup teardown test_ctxt in

  let run () =
    lwt _ = TheMovieDb.fetch_movie_change config 262254 in

    Lwt.return_unit
  in

  ignore (Lwt_main.run (run ()))

let apply_all_movie_changes test_ctxt =
  let config = bracket setup teardown test_ctxt in

  let apply_change =
    function
      | TheMovieDb.Change.Created m -> Lwt.return_unit
      | TheMovieDb.Change.Updated m -> Lwt.return_unit
      | TheMovieDb.Change.Deleted uid -> Lwt.return_unit
      | TheMovieDb.Change.Not_found uid -> Lwt.return_unit
      | TheMovieDb.Change.Error uid -> Lwt.return_unit
  in

  let run () =
    lwt c = TheMovieDb.fetch_movie_changes config in
    lwt all_changes = TheMovieDb.apply_all_movie_changes ~f:apply_change config in

    assert_equal
      ~msg:"Wrong number of element"
      c.TheMovieDb.Change.total_results
      (List.length all_changes);

    Lwt.return_unit
  in

  if all_test test_ctxt then
    ignore (Lwt_main.run (run ()))


let suite = "Test MovieDb" >:::
            [
              "APi key provied" >:: api_key_provided;
              "Fetch movieDb configuration" >:: fetch_moviedb_configuration ;
              "Fetch last movie" >:: fetch_last_movie ;
              "Fetch movie 550" >:: fetch_movie ;
              "Fetch genre" >:: fetch_genres ;
              "Fetch movie changes" >:: fetch_movie_changes;
              "Fetch all movie changes" >:: fetch_all_movie_changes;
              "Apply all movie changes" >:: apply_all_movie_changes;
            ]

let () = run_test_tt_main suite
