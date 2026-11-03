(* [Portable_lazy] without the [%identity] retyping of the caller's function: [from_fun]
   and [from_fun_fixed] wrap it in a closure that adds [portended] to the argument and
   result instead. Compiling this module against [Portable_lazy_intf.S] is the mode-safety
   argument for that retyping. *)

open Basement

type 'a portended = { portended : 'a } [@@unboxed]
type 'a t = 'a portended Atomic_lazy.t portended

let from_val value = { portended = Atomic_lazy.from_val { portended = value } }

let from_fun func =
  { portended = Atomic_lazy.from_fun (fun () -> { portended = func () }) }
;;

let from_fun_fixed func =
  { portended =
      Atomic_lazy.from_fun_fixed (fun lz -> { portended = func { portended = lz } })
  }
;;

let force { portended = t } = (Atomic_lazy.force t).portended
let map t ~f = from_fun (fun () -> f (force t))
let bind t ~f = from_fun (fun () -> force (f (force t)))
let compare compare_a t1 t2 = compare_a (force t1) (force t2)
let equal equal_a t1 t2 = equal_a (force t1) (force t2)
let globalize ga { portended = t } = { portended = Atomic_lazy.globalize ga t }
let is_val { portended = t } = Atomic_lazy.is_val t

let peek { portended = t } : _ Or_null_shim.t =
  match Atomic_lazy.peek t with
  | This value -> This value.portended
  | Null -> Null
;;

let peek_opt { portended = t } =
  match Atomic_lazy.peek_opt t with
  | Some value -> Some value.portended
  | None -> None
;;
