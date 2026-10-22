(** RISC-V ISA strings: [-march] in, [Tag_RISCV_arch] out.

    A configuration is spelled in {!Target_config}'s syntax; every other tool spells it as an ISA
    string such as [rv32imf_zve32f_zvfbfwma]. {!of_march} reads one into a spec and {!to_string}
    writes a resolved configuration back as one, so a profile can be named once. *)

type isa_spec = [ `V2_2 | `V20191213 ]
(** The unprivileged ISA specification edition, which sets the versions of I, M, A, F, D and Q and
    whether Zicsr and Zifencei are part of I ([`V2_2]) or separate. GNU as defaults to
    [`V20191213], LLVM knows only it. *)

type march = {
  xlen : int;
  spec : Target_config.spec;
      (** starts from [none]; the privileged instructions GNU as accepts under any [-march]
          ([system] and [s]) are enabled *)
  vlen : int option;  (** the largest [zvl<n>b] named, which no feature models *)
}

val of_march : ?isa_spec:isa_spec -> string -> (march, string) result
(** Parse [rv32]/[rv64], the base [i] or [g], single-letter extensions and [_]-separated
    multi-letter ones, each with an optional version that is ignored ([2p1], or [2] after a single letter). Names are not
    validated here: {!Target_config.resolve} rejects unknown ones. *)

val to_string :
  ?isa_spec:isa_spec -> ?vlen:int -> xlen:int -> Target_config.t -> (string, string) result
(** The canonical ISA string of a resolved configuration: the base, the single letters in
    canonical order, then the Z, S and X extensions, each with its ratified version. The set is
    the configuration's own closure plus the [zvl<n>b] its vector profile and [vlen] imply;
    [system] and [s] have no name and are left out. A feature with no known version is an error. *)
