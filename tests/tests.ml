open OUnit2

let () =
  run_test_tt_main
    ("Compiler tests" >:::
     [ "Simply typed λ-calculus" >::
       (fun ctxt ->
          let has_output = ref false in
          let foutput stream =
            try Stream.empty stream with
            | Stream.Failure ->
                has_output := true;
          in
          assert_command
            ~exit_code:(Unix.WEXITED 0)
            ~foutput
            ~use_stderr:true
            ~ctxt
            "./main.native"
            ["tests/lambda.sfw"];
          assert_equal !has_output false;
       )
     ]
    )
