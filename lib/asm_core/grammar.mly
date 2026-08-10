(* The common statement and expression grammar (.ai/asm_plan.md §4.7).

   Menhir owns this and only this. What it recognizes is labels, assignments,
   directives and instruction *lines* - not operands: an instruction's operands
   leave here as token slices and are completed by a hand-written target parser.
   That boundary is deliberate. There is no honest universal grammar covering
   x86 addressing, ARM register lists and AArch64 shift/extend operands at once,
   and individual instruction forms must never become Menhir productions.

   Every terminal carries its whole Token.t rather than a payload plus a
   position. A slice is then literally the tokens that spelled it, so the target
   parser and any diagnostic see the same spans the lexer produced, and there is
   no second representation of a token to keep in step. *)

%token <Token.t> IDENT DIRECTIVE INT STRING REGISTER LOCAL
%token <Token.t> COLON COMMA SEMI IMMSIGIL
%token <Token.t> LPAREN RPAREN LBRACKET RBRACKET LBRACE RBRACE
%token <Token.t> PLUS MINUS STAR SLASH PERCENT AMP PIPE CARET TILDE BANG EQUALS
%token <Token.t> LSHIFT RSHIFT DOT AT
%token <Token.t> EOL EOF

(* GAS's documented precedence, which is not C's: the multiplicative and shift
   operators bind tightest, then the bitwise ones, then addition. Writing C's
   table here would assemble [1 + 2 | 3] differently from every other assembler
   and the difference would only show up in a value, never in a diagnostic. *)
%left PLUS MINUS
%left PIPE AMP CARET
%left STAR SLASH PERCENT LSHIFT RSHIFT
%nonassoc UNARY

%start <Statement.line list> program
%start <Expr.t> expression_only

%%

program:
  | lines EOF { $1 }

lines:
  | { [] }
  | line lines { $1 :: $2 }

(* A line is any number of labels followed by at most one statement. Labels are
   their own items rather than a field of the statement, because [foo:] alone on
   a line and [foo: ret] must produce the same sequence.

   The recursion is written as [label line] rather than as [labels statement],
   and that is not a stylistic choice: with a separate [labels] list the parser
   must decide at an IDENT whether a label list is starting or ending, which
   needs the *following* token and is therefore not LR(1). Written this way the
   decision happens after IDENT has been shifted, where COLON distinguishes a
   label from a mnemonic with one token of lookahead. *)
line:
  | statement terminator { { Statement.labels = []; statement = $1 } }
  | label line { { $2 with Statement.labels = $1 :: $2.Statement.labels } }

terminator:
  | EOL { () }
  | SEMI { () }

label:
  | IDENT COLON { Statement.Named (Token.span $1, match Token.kind $1 with Token.Ident s -> s | _ -> "") }
  | INT COLON
    { Statement.Numeric
        (Token.span $1,
         match Token.kind $1 with
         | Token.Int v -> (match Foundation.Bigint.to_int_opt v with Some n -> n | None -> -1)
         | _ -> -1) }

statement:
  | { Statement.Empty }
  | IDENT slices
    { Statement.Instruction
        { mnemonic = (match Token.kind $1 with Token.Ident s -> s | _ -> "");
          operands = $2;
          span = Token.span $1 } }
  | IDENT EQUALS expr
    { Statement.Assignment
        { name = (match Token.kind $1 with Token.Ident s -> s | _ -> "");
          value = $3;
          span = Token.span $1 } }
  | DIRECTIVE slices
    { Statement.Directive
        { name = (match Token.kind $1 with Token.Directive s -> s | _ -> "");
          arguments = $2;
          span = Token.span $1 } }

slices:
  | { [] }
  | slice { [ $1 ] }
  | slice COMMA slices { $1 :: $3 }

slice:
  | atom { [ $1 ] }
  | atom slice { $1 :: $2 }

(* Every terminal that may appear inside an operand or a directive argument.
   COMMA separates slices, COLON introduces a label and EQUALS an assignment, so
   none of the three is an atom; everything else is, because what an operand
   means is not this grammar's business. *)
atom:
  | IDENT { $1 } | DIRECTIVE { $1 } | INT { $1 } | STRING { $1 } | REGISTER { $1 } | LOCAL { $1 }
  | IMMSIGIL { $1 }
  | LPAREN { $1 } | RPAREN { $1 } | LBRACKET { $1 } | RBRACKET { $1 } | LBRACE { $1 } | RBRACE { $1 }
  | PLUS { $1 } | MINUS { $1 } | STAR { $1 } | SLASH { $1 } | PERCENT { $1 }
  | AMP { $1 } | PIPE { $1 } | CARET { $1 } | TILDE { $1 } | BANG { $1 }
  | LSHIFT { $1 } | RSHIFT { $1 } | DOT { $1 } | AT { $1 }

(* The expression entry point. Directive handlers and target operand parsers
   call it on a slice; it is not reachable from [program], because an operand's
   structure is not decided here. *)
expression_only:
  | expr EOF { $1 }

expr:
  | INT { Expr.Const (match Token.kind $1 with Token.Int v -> v | _ -> Foundation.Bigint.zero) }
  | IDENT { Expr.Symbol (match Token.kind $1 with Token.Ident s -> s | _ -> "") }
  | DOT { Expr.Current_location }
  | LOCAL
    { match Token.kind $1 with
      | Token.Local_label (n, d) -> Expr.Local_ref (n, d)
      | _ -> Expr.Const Foundation.Bigint.zero }
  | LPAREN expr RPAREN { $2 }
  | MINUS expr %prec UNARY { Expr.Unary (Expr.Neg, $2) }
  | TILDE expr %prec UNARY { Expr.Unary (Expr.Lognot, $2) }
  | expr PLUS expr { Expr.Binary (Expr.Add, $1, $3) }
  | expr MINUS expr { Expr.Binary (Expr.Sub, $1, $3) }
  | expr STAR expr { Expr.Binary (Expr.Mul, $1, $3) }
  | expr SLASH expr { Expr.Binary (Expr.Div, $1, $3) }
  | expr PERCENT expr { Expr.Binary (Expr.Mod, $1, $3) }
  | expr AMP expr { Expr.Binary (Expr.And, $1, $3) }
  | expr PIPE expr { Expr.Binary (Expr.Or, $1, $3) }
  | expr CARET expr { Expr.Binary (Expr.Xor, $1, $3) }
  | expr LSHIFT expr { Expr.Binary (Expr.Shl, $1, $3) }
  | expr RSHIFT expr { Expr.Binary (Expr.Shr, $1, $3) }
