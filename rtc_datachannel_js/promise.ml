(*
MIT License

Copyright (c) 2019 Alexander Yanin

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
*)

open Js_of_ocaml

type ('a, 'b) t

type 'a resolve = 'a -> unit

type 'a reject = 'a -> unit

let promise_global = Js.Unsafe.global##._Promise

let is_supported () = Js.Optdef.test promise_global

let make f =
  Js.Unsafe.new_obj promise_global [|Js.Unsafe.inject f|]

let resolve value =
  Js.Unsafe.fun_call promise_global##.resolve [|Js.Unsafe.inject value|]

let reject value =
  Js.Unsafe.fun_call promise_global##.reject [|Js.Unsafe.inject value|]

let js_of_opt = function
  | Some value -> Js.Unsafe.inject value
  | None       -> Js.Unsafe.inject Js.undefined

let then_bind ~on_fulfilled ?on_rejected promise =
  Js.Unsafe.meth_call promise "then"
    [|Js.Unsafe.inject on_fulfilled; js_of_opt on_rejected|]

let then_map ~on_fulfilled ?on_rejected promise =
  Js.Unsafe.meth_call promise "then"
    [|Js.Unsafe.inject on_fulfilled; js_of_opt on_rejected|]

let catch_bind ~on_rejected promise =
  Js.Unsafe.meth_call promise "catch" [|Js.Unsafe.inject on_rejected|]

let catch_map ~on_rejected promise =
  Js.Unsafe.meth_call promise "catch" [|Js.Unsafe.inject on_rejected|]

let then_final ~on_fulfilled ~on_rejected promise : unit =
  Js.Unsafe.meth_call promise "then"
    [|Js.Unsafe.inject on_fulfilled; Js.Unsafe.inject on_rejected|]

let to_lwt (p : ('a, exn) t) : 'a Lwt.t =
  let t, w = Lwt.task () in
  let on_fulfilled =
    Js.wrap_callback
    @@ (fun x -> Lwt.wakeup w x) in
  let on_rejected =
    Js.wrap_callback
    @@ (fun x -> Lwt.wakeup_exn w x) in
  then_final ~on_fulfilled ~on_rejected p;
  t

let to_lwt_result (p : ('a, 'b) t) : ('a, 'b) result Lwt.t =
  let t, w = Lwt.task () in
  let on_fulfilled =
    Js.wrap_callback
    @@ (fun x -> Lwt.wakeup w (Ok x)) in
  let on_rejected =
    Js.wrap_callback
    @@ (fun x -> Lwt.wakeup w (Error x)) in
  then_final ~on_fulfilled ~on_rejected p;
  t

let all promises =
  let intermediate_promise =
    Js.Unsafe.fun_call promise_global##.all
      [|Js.Unsafe.inject (Js.array promises)|]
  in
  then_map
    ~on_fulfilled:(fun js_array -> Js.to_array js_array) intermediate_promise

let race promises =
  Js.Unsafe.fun_call promise_global##.race
    [|Js.Unsafe.inject (Js.array promises)|]

module Infix = struct
  let (>>=) promise on_fulfilled = then_bind ~on_fulfilled promise
  let (>|=) promise on_fulfilled = then_map  ~on_fulfilled promise

  let (>>~) promise on_rejected = catch_bind ~on_rejected promise
  let (>|~) promise on_rejected = catch_map  ~on_rejected promise

  let (>||) promise (on_fulfilled, on_rejected) =
    then_final ~on_fulfilled ~on_rejected promise
end
