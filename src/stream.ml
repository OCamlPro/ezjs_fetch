open Ezjs_min

class type ['a] promise = ['a] Promise.promise

class type abort_signal = object
  inherit Dom_html.eventTarget
  method aborted : bool t readonly_prop
  method abort : Dom_html.event t prop
end

class type options = object
  method preventClose: bool t optdef readonly_prop
  method preventAbort: bool t optdef readonly_prop
  method preventCancel: bool t optdef readonly_prop
  method signal: abort_signal t optdef readonly_prop
end

class type ['chunk] read_result = object
  method value: 'chunk optdef readonly_prop
  method done_: bool t readonly_prop
end

class type reader = object
  method closed: unit promise t readonly_prop
  method cancel: js_string t optdef -> js_string t optdef promise t meth
  method read: 'chunk t read_result t promise t meth
  method releaseLock: unit meth
end

class type writer = object
  method closed: unit promise t readonly_prop
  method desiredSize: int readonly_prop
  method ready: unit promise t readonly_prop
  method abort: js_string t optdef -> js_string t optdef promise t meth
  method close: unit meth
  method releaseLock: unit meth
  method write: 'chunk t -> unit promise t meth
end

class type rstream = object
  method locked: bool t readonly_prop
  method cancel: js_string t optdef -> js_string t optdef promise t meth
  method getReader: js_string t optdef -> reader t meth
  method pipeThrough: tstream t -> options t optdef -> rstream t meth
  method pipeTo: wstream t -> options t optdef -> unit promise t meth
  method tee: rstream t js_array t meth
end

and tstream = object
  method readable: rstream t readonly_prop
  method writable: wstream t readonly_prop
end

and wstream = object
  method locked: bool t readonly_prop
  method abort: js_string t optdef -> js_string t optdef promise t meth
  method close: unit meth
  method getWriter: writer t meth
end

class type controller = object
  method desiredSize: int readonly_prop
  method close: unit meth
  method enqueue: 'chunk t -> unit meth
  method error: 'error t -> unit meth
end

class type transformer = object
  method start: (controller t -> unit) callback readonly_prop
  method transform: ('chunk t -> controller t -> unit) callback readonly_prop
  method flush: (controller t -> unit) callback readonly_prop
end

class type strategy = object
  method highWaterMark: int prop
  method size: 'chunk t -> int meth
end

type tstream_cs = (transformer t optdef -> strategy t optdef -> strategy t optdef -> tstream t) constr

class type underlying_sink = object
  method start: (controller t -> unit) callback optdef readonly_prop
  method write: ('chunk t -> controller t -> unit) callback optdef readonly_prop
  method close: (controller t -> unit) callback optdef readonly_prop
  method abort: (js_string t optdef -> unit) callback optdef readonly_prop
end

type wstream_cs = (underlying_sink t optdef -> strategy t optdef -> wstream t) constr

class type underlying_source = object
  method start: (controller t -> unit) callback optdef readonly_prop
  method pull: (controller t -> unit) callback optdef readonly_prop
  method cancel: (js_string t optdef -> unit) callback optdef readonly_prop
  method type_: js_string t optdef readonly_prop
  method autoAllocateChunkSize: int optdef readonly_prop
end

type rstream_cs = (underlying_source t optdef -> strategy t optdef -> rstream t) constr

let get_reader ?mode (s: rstream t) : reader t = s##getReader (optdef string mode)

let read ~source ~fold acc cb =
  let reader = match source with
    | `reader r -> r
    | `stream (s, mode) -> get_reader ?mode s in
  let rec aux acc =
    Promise.rthen reader##read @@ function
    | Error e -> cb (Error e)
    | Ok v ->
      match to_bool v##.done_, Optdef.to_option v##.value with
      | true, None -> cb (Ok acc)
      | true, Some v ->
        begin match fold acc v with
          | Ok acc -> cb (Ok acc)
          | Error reason ->
            Promise.jthen (reader##cancel (optdef string reason))
              (fun x -> cb (Ok acc))
        end
      | false, Some v ->
        begin match fold acc v with
          | Ok acc -> aux acc
          | Error reason ->
            Promise.jthen (reader##cancel (optdef string reason))
              (fun x -> cb (Ok acc))
        end
      | _ -> cb (Error (error_of_string "undefined read result")) in
  aux acc
