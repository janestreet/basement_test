(* [Portable_lazy] without the [%identity] retyping of the caller's function: [from_fun]
   and [from_fun_fixed] wrap it in a closure that adds [portended] to the argument and
   result instead. Compiling this module against [Portable_lazy_intf.S] is the mode-safety
   argument for that retyping. *)

open Basement

type ('a : value_or_null) portended = { portended : 'a @@ contended portable } [@@unboxed]
type ('a : value_or_null) t = 'a portended Atomic_lazy.t portended

let from_val value = { portended = Atomic_lazy.from_val__portable { portended = value } }

let from_fun func =
  { portended = Atomic_lazy.from_fun__portable (fun () -> { portended = func () }) }
;;

let from_fun_fixed func =
  { portended =
      Atomic_lazy.from_fun_fixed__portable (fun lz ->
        { portended = func { portended = lz } })
  }
;;

let force { portended = t } = (Atomic_lazy.force__contended t).portended
let map t ~f = from_fun (fun () -> f (force t))
let bind t ~f = from_fun (fun () -> force (f (force t)))
let compare compare_a t1 t2 = compare_a (force t1) (force t2)
let compare__local compare_a t1 t2 = compare_a (force t1) (force t2)
let equal equal_a t1 t2 = equal_a (force t1) (force t2)
let equal__local equal_a t1 t2 = equal_a (force t1) (force t2)
let globalize ga { portended = t } = { portended = Atomic_lazy.globalize__contended ga t }
let is_val { portended = t } = Atomic_lazy.is_val__contended t

let peek { portended = t } : _ Or_null_shim.t =
  match Atomic_lazy.peek__contended t with
  | This value -> This value.portended
  | Null -> Null
;;

let peek_opt { portended = t } =
  match Atomic_lazy.peek_opt__contended t with
  | Some value -> Some value.portended
  | None -> None
;;
