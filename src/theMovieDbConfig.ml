type debug = {
  display_correct_response: bool;
  display_retry_after: bool;
  display_incorrect_response: bool;
}

type t = {
  api_url: string;
  api_key: string;
  request_per_second: float ;
  max_concurrent_connection: int;
  debug: debug;
}


let create
    ?(api_url="http://api.themoviedb.org")
    ?(request_per_second=3.0)
    ?(max_concurrent_connection=5)
    ?(debug_display_correct_response=false)
    ?(debug_display_retry_after=false)
    ?(debug_display_incorrect_response=false)
    api_key =

    {
      api_url = api_url;
      api_key = api_key;
      request_per_second = request_per_second;
      max_concurrent_connection = max_concurrent_connection;
      debug = {
        display_correct_response = debug_display_correct_response;
        display_retry_after = debug_display_retry_after;
        display_incorrect_response = debug_display_incorrect_response;
      }
    }
