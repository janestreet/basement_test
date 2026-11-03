open Basement
open! Base
open Expect_test_helpers_base

module Test_lazy (Lazy : sig
    type +'a t

    val from_val : 'a. 'a -> 'a t
    val from_fun : 'a. (unit -> 'a) -> 'a t
    val from_fun_fixed : 'a. ('a t -> 'a) -> 'a t
    val force : 'a. 'a t -> 'a
    val is_val : 'a. 'a t -> bool
    val peek : 'a. 'a t -> 'a Or_null_shim.t
    val peek_opt : 'a. 'a t -> 'a option
    val globalize : 'a 'b. 'b -> 'a t -> 'a t
  end) =
struct
  let%expect_test "from_val" =
    let t = Lazy.from_val 1 in
    print_s [%message (Lazy.is_val t : bool)];
    print_s [%message (Lazy.force t : int)];
    print_s [%message (Lazy.is_val t : bool)];
    [%expect
      {|
      ("Lazy.is_val t" true)
      ("Lazy.force t" 1)
      ("Lazy.is_val t" true)
      |}]
  ;;

  let%expect_test "from_fun" =
    let t = Lazy.from_fun (fun () -> 2 + 2) in
    print_s [%message (Lazy.is_val t : bool)];
    print_s [%message (Lazy.force t : int)];
    print_s [%message (Lazy.is_val t : bool)];
    [%expect
      {|
      ("Lazy.is_val t" false)
      ("Lazy.force t" 4)
      ("Lazy.is_val t" true)
      |}]
  ;;

  let%expect_test "from_fun_fixed" =
    let t =
      Lazy.from_fun_fixed (fun lz ->
        print_endline "forced";
        fun n ->
          if n = 0
          then print_endline "finished"
          else (
            print_endline "called";
            (Lazy.force lz) (n - 1)))
    in
    let f = Lazy.force t in
    f 5;
    [%expect
      {|
      forced
      called
      called
      called
      called
      called
      finished
      |}];
    f 3;
    [%expect
      {|
      called
      called
      called
      finished
      |}]
  ;;

  let%expect_test "force always returns the same value" =
    let t = Lazy.from_fun (fun () -> Portable_atomic.make 0) in
    let force1 = Lazy.force t in
    let force2 = Lazy.force t in
    require (phys_equal force1 force2);
    [%expect {| |}]
  ;;

  let%expect_test "a raising thunk runs once and raises on every force" =
    let test (t : int Lazy.t) =
      require_does_raise (fun () -> Lazy.force t);
      require_does_raise (fun () -> Lazy.force t);
      print_s [%message (Lazy.is_val t : bool)]
    in
    test
      (Lazy.from_fun (fun () ->
         print_endline "ran";
         failwith "from_fun"));
    [%expect
      {|
      ran
      (Failure from_fun)
      (Failure from_fun)
      ("Lazy.is_val t" false)
      |}];
    test
      (Lazy.from_fun_fixed (fun _ ->
         print_endline "ran";
         failwith "from_fun_fixed"));
    [%expect
      {|
      ran
      (Failure from_fun_fixed)
      (Failure from_fun_fixed)
      ("Lazy.is_val t" false)
      |}]
  ;;

  let%expect_test "peek and peek_opt" =
    let t = Lazy.from_fun (fun () -> 1) in
    let result = Lazy.peek t in
    let result_opt = Lazy.peek_opt t in
    print_s [%message "" ~peek:(result : int or_null)];
    print_s [%message "" ~peek_opt:(result_opt : int option)];
    print_s [%message "" ~force:(Lazy.force t : int)];
    let result = Lazy.peek t in
    let result_opt = Lazy.peek_opt t in
    print_s [%message "" ~peek:(result : int or_null)];
    print_s [%message "" ~peek_opt:(result_opt : int option)];
    [%expect
      {|
      (peek ())
      (peek_opt ())
      (force 1)
      (peek (1))
      (peek_opt (1))
      |}]
  ;;

  let%expect_test "globalize" =
    let t = Lazy.from_fun (fun () -> 1) in
    print_s [%message (Lazy.is_val t : bool)];
    print_s [%message "globalizing"];
    let t_copy = Lazy.globalize () t in
    print_s [%message (Lazy.is_val t : bool)];
    print_s [%message (Lazy.is_val t_copy : bool)];
    print_s [%message (Lazy.force t_copy : int)];
    print_s [%message (Lazy.is_val t : bool)];
    print_s [%message (Lazy.is_val t_copy : bool)];
    [%expect
      {|
      ("Lazy.is_val t" false)
      globalizing
      ("Lazy.is_val t" false)
      ("Lazy.is_val t_copy" false)
      ("Lazy.force t_copy" 1)
      ("Lazy.is_val t" true)
      ("Lazy.is_val t_copy" true)
      |}]
  ;;

  let%expect_test "relaxed value restriction applies (because it's covariant)" =
    let t : type a. a list Lazy.t = Lazy.from_fun (fun () -> []) in
    let result : int list = Lazy.force t in
    print_s [%message "" ~force:(result : _ list)];
    let result : bool list = Lazy.force t in
    print_s [%message "" ~force:(result : _ list)];
    [%expect
      {|
      (force ())
      (force ())
      |}]
  ;;
end

module _ = Test_lazy (Atomic_lazy)

module _ = Test_lazy (struct
    open Atomic_lazy

    type nonrec 'a t = 'a t

    let from_val = from_val
    let from_fun = from_fun
    let from_fun_fixed = from_fun_fixed
    let force = force
    let is_val = is_val
    let peek = peek
    let peek_opt = peek_opt
    let globalize = globalize
  end)

(* Example of [Atomic_lazy] being used for mode-polymorphism; the main novel result is
   that [of_lazy] and [fixed_point] below can be defined mode-polymorphically (rather than
   ad-hoc). *)

module Portable = struct
  let wrap = Fn.id
  let unwrap = Fn.id
end

module Generator : sig
  type 'a t

  val run : 'a t -> int -> 'a
  val inspect : int t
  val return : 'a. 'a -> 'a t
  val map : 'a t -> f:('a -> 'b) -> 'b t
  val bind : 'a t -> f:('a -> 'b t) -> 'b t
  val recursive_union : base:'a t -> recur:('a t -> 'a t) -> 'a t
end = struct
  type 'a t = int -> 'a

  let run t n = t n
  let inspect : _ = fun n -> n
  let shrink t : _ = fun n -> run t (n / 2)
  let return x : _ = fun _ -> x
  let map t ~f : _ = fun n -> f (run t n)
  let bind t ~f : _ = fun n -> run (f (run t n)) n
  let of_lazy lz : _ = fun n -> run (Atomic_lazy.force lz) n

  let fixed_point of_generator =
    let lz =
      Atomic_lazy.from_fun_fixed (fun lz ->
        let lz' =
          Atomic_lazy.from_fun (fun () ->
            Atomic_lazy.force lz |> Modes.Contended.cross |> Portable.unwrap)
        in
        Portable.wrap (of_generator (of_lazy lz')))
    in
    require (not (Atomic_lazy.is_val lz));
    let result = Atomic_lazy.force lz |> Portable.unwrap in
    require (Atomic_lazy.is_val lz);
    result
  ;;

  let recursive_union ~base ~recur =
    fixed_point (fun gen ->
      bind inspect ~f:(fun n -> if n = 0 then base else recur (shrink gen)))
  ;;
end

let%expect_test "lazy generators work" =
  let open Generator in
  let gen = return 1 in
  print_s [%message (run gen 999 : int)];
  [%expect {| ("run gen 999" 1) |}];
  let gen =
    recursive_union
      ~base:(return ([] : int list))
      ~recur:(fun t -> bind inspect ~f:(fun n -> map t ~f:(fun l -> n :: l)))
  in
  print_s [%message (Generator.run gen 999 : int list)];
  [%expect {| ("Generator.run gen 999" (999 499 249 124 62 31 15 7 3 1)) |}]
;;
