let () =
  Js_of_ocaml.Js.export "AsmSmoke"
    (object%js
       method run = Js_of_ocaml.Js.string (Smoke.run ())
    end)
