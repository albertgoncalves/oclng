open Compile

let () : unit =
  let buffer : Buffer.t =
    compile
      [
        {
          label = "_entry_";
          args = [];
          body =
            [
              ExprCall
                (
                  CallIntrin IntrinPrintf,
                  [
                    ExprStr "%ld\n";
                    ExprCall
                      (CallLabel "fib", [ExprInt 50; ExprInt 0; ExprInt 1]);
                  ]
                );
              ExprAssign
                (
                  "x",
                  ExprCall
                    (
                      CallIntrin IntrinPack,
                      [ExprInt 1; ExprStr "%s\n"; ExprStr "Here!"]
                    )
                );
              ExprUnpack
                (
                  ExprVar "x",
                  [
                    (
                      [],
                      [ExprCall (CallIntrin IntrinPrintf, [ExprStr "!\n"])]
                    );
                    (
                      ["a"; "b"],
                      [
                        ExprCall
                          (
                            CallIntrin IntrinPrintf,
                            [ExprVar "a"; ExprVar "b"]
                          )
                      ]
                    );
                  ]
                );
              ExprInt 0;
            ];
        };
        {
          label = "fib";
          args = ["n"; "a"; "b"];
          body =
            [
              ExprIfThen
                (
                  (ExprBinOp (BinOpEq, ExprVar "n", ExprInt 0)),
                  [ExprVar "a"],
                  [
                    ExprCall
                      (
                        CallLabel "fib",
                        [
                          ExprBinOp (BinOpSub, ExprVar "n", ExprInt 1);
                          ExprVar "b";
                          ExprBinOp (BinOpAdd, ExprVar "a", ExprVar "b");
                        ]
                      )
                  ]
                );
            ];
        };
      ] in
  let file : out_channel = open_out Sys.argv.(2) in
  Buffer.output_buffer file buffer;
  close_out file
