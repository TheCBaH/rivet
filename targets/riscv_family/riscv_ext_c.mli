(** The compressed extensions. [c] is an implication-only group: Zca, plus Zcd when D is present and
    Zcf when F is present on RV32. Zcb and Zcmop require Zca. Zcmp and Zcmt conflict with Zcd and
    are off by default; the opt-in recipes are [-c,-zcd,+zcmp] and [-c,-zcd,+zcmt]. *)

val zca : Target_component.t
val zcd : Target_component.t
val zcf : Target_component.t
val zcb : Target_component.t
val zcmp : Target_component.t
val zcmt : Target_component.t
val zcmop : Target_component.t
val c : Target_component.t
