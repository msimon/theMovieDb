module Configuration = struct
  type image = {
    base_url : string;
    secure_base_url : string ;
    poster_sizes : string list ;
    backdrop_sizes : string list ;
    profile_sizes : string list ;
    logo_sizes : string list ;
  } deriving (Yojson)

  type t = {
    images : image ;
    change_keys : string list ;
  } deriving (Yojson)

  type error = {
    status_code : string ;
    status_message: string ;
  } deriving (Yojson)
end
module Genre = struct
  type t = {
    id: int ;
    name: string ;
  } deriving (Yojson)
end

module Movie = struct
  module ProductionCompanies = struct
    type t = {
      id: int ;
      name: string ;
    } deriving (Yojson)
  end

  module ProductionCountries = struct
    type t = {
      iso_3166_1: string ;
      name: string ;
    } deriving (Yojson)
  end

  module SpokenLanguages = struct
    type t = {
      iso_639_1: string;
      name: string ;
    } deriving (Yojson)
  end

  type t = {
    id: int ;
    adult: bool ;
    backdrop_path: string option ;
    budget: int ;
    genres: Genre.t list ;
    homepage: string option ;
    imdb_id: string option ;
    original_title: string ;
    overview: string option ;
    popularity: float ;
    poster_path: string option ;
    production_companies: ProductionCompanies.t list ;
    production_countries: ProductionCountries.t list ;
    release_date: string ;
    revenue: int;
    runtime: int option;
    spoken_languages: SpokenLanguages.t list ;
    status: string;
    tagline: string option;
    title: string;
    vote_average: float;
    vote_count: int;
  } deriving (Yojson)

end

module Change = struct
  type result = {
    id: int;
    adult: bool option; (* adult can be null *)
  } deriving (Yojson)

  type t = {
    results: result list ;
    page : int;
    total_pages: int;
    total_results: int;
  } deriving (Yojson)

  type c =
    | Created of Movie.t
    | Updated of Movie.t
    | Deleted of int
    | Not_found of int
    | Error of (string * int)
          deriving (Yojson)

end


module Http = TheMovieDbHttp
open TheMovieDbConfig

let fetch_moviedb_configuration config =
  lwt s = Http.build_url config ~uri:"/3/configuration" in
  Lwt.return (Configuration.Yojson_t.from_string s)

let fetch_last_movie config =
  lwt s = Http.build_url config ~uri:"/3/movie/latest" in
  Lwt.return (Movie.Yojson_t.from_string s)

let fetch_movie config uid =
  lwt s = Http.build_url config ~uri:(Printf.sprintf "/3/movie/%d" uid) in
  Lwt.return (Movie.Yojson_t.from_string s)

let fetch_genres config =
  lwt s = Http.build_url config ~uri:"/3/genre/list" in
  match Yojson.Safe.from_string s with
    | `Assoc l -> begin
        try
          let g = List.assoc "genres" l in
          let g = (let module M = Deriving_Yojson.Yojson_list(Genre.Yojson_t) in
                   M.from_json) g
          in
          Lwt.return g
        with _ ->
          failwith "Wrong json value"
      end
    | _ -> failwith "Wrong json value"

(* fetch movie change + if created/update fetch new movie value *)
let fetch_movie_change config uid=
  let get_change_type j =
    let action_of_string =
      function
        | `String "created" -> `Created
        | `String "updated" -> `Updated
        | `String "deleted" -> `Deleted
        | `String "destroyed" -> `Deleted
        | _ ->  failwith "Wrong json value 1"
    in

    let rec get_action =
      function
        | (`Assoc h)::t ->
          if List.assoc "key" h = `String "general" then begin
            match List.assoc "items" h with
              | `List l ->
                begin match List.nth l 0 with
                  | `Assoc v ->
                    action_of_string (List.assoc "action" v)
                  | _ ->
                    failwith "Wrong json value 2"
                end
              | _ -> failwith "Wrong json value 2"
          end else
            get_action t
        | _ ->
          (* if the general key is not given then its an update *)
          `Updated
    in

    match j with
      | `Assoc l ->
        begin try
            let j = List.assoc "changes" l in
            match j with
              | `List js ->
                get_action js
              | _ ->
                failwith "Wrong json value 3"
          with e ->
            failwith (Printf.sprintf "Wrong json value 4, %s , %d" (Printexc.to_string e) uid)
        end
      | _ -> failwith "Wrong json value 5"
  in

  lwt s = Http.build_url config ~uri:(Printf.sprintf "/3/movie/%d/changes" uid) in

  let json = Yojson.Basic.from_string s in

  let fetch_movie _ =
    try_lwt
      fetch_movie config uid
    with Not_found ->
      raise Not_found
  in

  match get_change_type json with
    | `Created ->
      lwt m = fetch_movie () in
      Lwt.return (Change.Created m)
    | `Updated ->
      lwt m = fetch_movie () in
      Lwt.return (Change.Updated m)
    | `Deleted ->
      Lwt.return (Change.Deleted uid)


let fetch_movie_changes ?start_date ?end_date ?page config =
  let string_of_date f =
    let t = Unix.localtime f in
    Printf.sprintf "%d-%d-%d" (t.Unix.tm_year + 1900) (t.Unix.tm_mon + 1) t.Unix.tm_mday
  in

  let start_date =
    match start_date with
      | Some d -> Some (string_of_date d)
      | None -> None
  in
  let end_date =
    match end_date with
      | Some d -> Some (string_of_date d)
      | None -> None
  in
  let page =
    match page with
      | Some page -> Some (string_of_int page)
      | None -> None
  in

  let params =
    List.fold_left (
      fun acc (k,v) ->
        match v with
          | Some v -> (k,v)::acc
          | None -> acc
    ) [] [ ("start_date", start_date); ("end_date", end_date); ("page", page) ]
  in

  lwt s = Http.build_url ~params config ~uri:"/3/movie/changes" in
  Lwt.return (Change.Yojson_t.from_string s)


let fetch_all_movie_changes ?start_date ?end_date config : 'a list Lwt.t =
  lwt c = fetch_movie_changes ?start_date ?end_date ~page:1 config in

  let page_to_fetch = c.Change.total_pages in

  let rec fetch acc page =
    if page <= 1 then acc
    else begin
      let c = fetch_movie_changes ?start_date ?end_date ~page config in
      Lwt.async (fun _ ->
          lwt c = c in
          Lwt.return_unit;
        );
        fetch (c::acc) (page - 1)
    end
  in

  let l = fetch [] page_to_fetch in

  let changes = ref c.Change.results in
  lwt _ = Lwt_list.iter_p (
      fun c ->
        lwt c = c in
        changes := List.append c.Change.results !changes;
        Lwt.return_unit
    ) l
  in

  Lwt.return !changes


let apply_all_movie_changes ?f ?start_date ?end_date config =
  lwt changes = fetch_all_movie_changes ?start_date ?end_date config in

  let acc = ref [] in

  let rec fetch_change n =
    try_lwt
      let c = List.nth changes n in

      lwt m =
        try_lwt
          fetch_movie_change config c.Change.id
        with
          | Not_found -> Lwt.return (Change.Not_found c.Change.id)
          | exn -> Lwt.return (Change.Error ((Printexc.to_string exn), c.Change.id))
      in

      lwt _ = match f with
        | Some f -> f m
        | None -> Lwt.return_unit
      in

      acc := m::!acc;
      fetch_change (n + config.max_concurrent_connection)
    with
      | Failure "nth" ->
        Lwt.return_unit
      | exn ->
        Lwt.return_unit
  in

  let rec generate_thread n acc =
    if n >= config.max_concurrent_connection then acc
    else
      generate_thread (n + 1) ((fetch_change n)::acc)
  in

  lwt _ = Lwt.join (generate_thread 0 []) in

  Lwt.return !acc
