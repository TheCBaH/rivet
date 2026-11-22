(** The compressed extensions. [c] is an implication-only group: Zca, plus Zcd when D is present and
    Zcf when F is present on RV32. Zcb and Zcmop require Zca. Zcmp and Zcmt conflict with Zcd and
    are off by default; the opt-in recipes are [-c,-zcd,+zcmp] and [-c,-zcd,+zcmt]. *)

val zca : Rivet_target_features.Component.t
val zcd : Rivet_target_features.Component.t
val zcf : Rivet_target_features.Component.t
val zcb : Rivet_target_features.Component.t
val zcmp : Rivet_target_features.Component.t
val zcmt : Rivet_target_features.Component.t
val zcmop : Rivet_target_features.Component.t
val c : Rivet_target_features.Component.t
