open! Base
open! Expect_test_helpers_base
open Basement

module%test [@name "single-domain behavior tests"] _ = struct
  module Operation = struct
    type t =
      | Get
      | Set_root of int
      | With_temporarily of int * t list

    let perform ts ~init =
      let dynamic = Dynamic.make init in
      let rec loop = function
        | Get -> [%message "get" ~_:(Dynamic.get dynamic : int)]
        | Set_root to_ ->
          Dynamic.set_root dynamic to_;
          [%message "set_root" ~_:(to_ : int)]
        | With_temporarily (to_, ts) ->
          let body =
            Dynamic.with_temporarily dynamic to_ ~f:(fun () -> List.map ts ~f:loop)
          in
          [%message "with_temporarily" ~_:(to_ : int) ~_:(body : Sexp.t list)]
      in
      List.map ts ~f:loop |> List.iter ~f:print_s
    ;;
  end

  module Fiber = struct
    type _ Stdlib.Effect.t += Suspend : unit Stdlib.Effect.t

    type t =
      | Finished
      | Suspended of (unit, t) Stdlib.Effect.Deep.continuation

    let run f =
      Stdlib_shim.Effect.Deep.Safe.try_with
        (fun h () ->
          f h;
          Finished)
        ()
        { effc =
            (fun (type a) (effect : a Stdlib.Effect.t) ->
              match effect with
              | Suspend ->
                Some
                  (fun (continuation : (a, t) Stdlib.Effect.Deep.continuation) ->
                    Suspended continuation)
              | _ -> None)
        }
    ;;

    let suspend h = Stdlib_shim.Effect.Safe.perform h Suspend

    let resume = function
      | Finished -> failwith "Cannot resume a finished fiber"
      | Suspended continuation -> Stdlib.Effect.Deep.continue continuation ()
    ;;
  end

  let%expect_test "initial value" =
    Operation.perform ~init:0 [ Get ];
    [%expect {| (get 0) |}]
  ;;

  let%expect_test "[set_root] and [get]" =
    Operation.perform ~init:0 [ Set_root 1; Get; Set_root 2; Get ];
    [%expect
      {|
      (set_root 1)
      (get 1)
      (set_root 2)
      (get 2)
      |}]
  ;;

  let%expect_test "[with_temporarily]" =
    Operation.perform
      ~init:0
      [ Get
      ; With_temporarily (1, [ Get ])
      ; Get
      ; With_temporarily (2, [ Get; Set_root 3; Get ])
      ; Get
      ; With_temporarily
          ( 4
          , [ Get
            ; With_temporarily (5, [ Get ])
            ; Get
            ; With_temporarily (6, [ Get ])
            ; Get
            ] )
      ; Get
      ; With_temporarily (7, [ Get; With_temporarily (8, [ Get; Set_root 9; Get ]); Get ])
      ; Get
      ];
    [%expect
      {|
      (get 0)
      (with_temporarily 1 ((get 1)))
      (get 0)
      (with_temporarily 2 (
        (get      2)
        (set_root 3)
        (get      2)))
      (get 3)
      (with_temporarily 4 (
        (get 4)
        (with_temporarily 5 ((get 5)))
        (get 4)
        (with_temporarily 6 ((get 6)))
        (get 4)))
      (get 3)
      (with_temporarily 7 (
        (get 7)
        (with_temporarily 8 (
          (get      8)
          (set_root 9)
          (get      8)))
        (get 7)))
      (get 9)
      |}]
  ;;

  let%expect_test ("a suspended fiber escapes [with_temporarily]" [@tags "no-js"]) =
    let dynamic = Dynamic.make "root" in
    let print_value message =
      print_s [%sexp ((message, Dynamic.get dynamic) : string * string)]
    in
    let fiber =
      Fiber.run (fun h ->
        Dynamic.with_temporarily dynamic "fiber" ~f:(fun () ->
          print_value "before suspending";
          Fiber.suspend h;
          print_value "after resuming")
        [@nontail])
    in
    print_value "while suspended";
    (match Fiber.resume fiber with
     | Finished -> print_value "after finishing"
     | Suspended _ -> failwith "Fiber suspended more than once");
    [%expect
      {|
      ("before suspending" fiber)
      ("while suspended" root)
      ("after resuming" fiber)
      ("after finishing" root)
      |}]
  ;;

  let%expect_test ("a fiber is resumed inside [with_temporarily]" [@tags "no-js"]) =
    let dynamic = Dynamic.make "root" in
    let print_value message =
      print_s [%sexp ((message, Dynamic.get dynamic) : string * string)]
    in
    let fiber =
      Fiber.run (fun h ->
        Dynamic.with_temporarily dynamic "fiber" ~f:(fun () ->
          Fiber.suspend h;
          print_value "resumed fiber")
        [@nontail])
    in
    Dynamic.with_temporarily dynamic "resumer" ~f:(fun () ->
      print_value "before resuming";
      (match Fiber.resume fiber with
       | Finished -> ()
       | Suspended _ -> failwith "Fiber suspended more than once");
      print_value "after resuming");
    print_value "after [with_temporarily]";
    [%expect
      {|
      ("before resuming" resumer)
      ("resumed fiber" fiber)
      ("after resuming" resumer)
      ("after [with_temporarily]" root)
      |}]
  ;;

  let%expect_test ("a fiber is created and resumed in different [with_temporarily] calls"
    [@tags "no-js"])
    =
    let dynamic = Dynamic.make "root" in
    let print_value message =
      print_s [%sexp ((message, Dynamic.get dynamic) : string * string)]
    in
    let fiber =
      Dynamic.with_temporarily dynamic "creator" ~f:(fun () ->
        print_value "before creating";
        let fiber = Stdlib.Effect.Shallow.fiber (fun () -> print_value "running fiber") in
        print_value "after creating";
        fiber)
    in
    print_value "after creator";
    Dynamic.with_temporarily dynamic "resumer" ~f:(fun () ->
      print_value "before resuming";
      Stdlib.Effect.Shallow.continue_with
        fiber
        ()
        { retc = (fun () -> ()); exnc = raise; effc = (fun _ -> None) };
      print_value "after resuming");
    print_value "after resumer";
    [%expect
      {|
      ("before creating" creator)
      ("after creating" creator)
      ("after creator" root)
      ("before resuming" resumer)
      ("running fiber" resumer)
      ("after resuming" resumer)
      ("after resumer" root)
      |}]
  ;;
end
