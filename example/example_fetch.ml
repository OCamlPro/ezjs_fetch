open Ezjs_min

class type test = object
  method hash : js_string t prop
end

let test_lwt () =
  let open Ezjs_fetch_lwt in
  Lwt.async @@ fun () ->
  Lwt.bind (fetch "https://ithaca.tz.functori.com/chains/main/blocks/head" to_js) @@
  function
  | Error e ->
    Lwt.return @@ Firebug.console##log e
  | Ok (r : test t response) ->
    Lwt.return @@ Firebug.console##log r.body##.hash

let test_stream () =
  let open Ezjs_fetch in
  let i = ref 0 in
  let fold () s =
    incr i;
    Firebug.console##log (string s);
    if !i > 2 then Error None else Ok () in
  fetch "http://ithaca.tz.functori.com/monitor/heads/main" (to_str_stream fold ()) @@
  function
  | Error e -> Firebug.console##log e
  | Ok _ -> Firebug.console##log (string "ok")


let () =
  export "test_fetch" @@ object%js
    method lwt = test_lwt ()
    method stream = test_stream ()
  end
