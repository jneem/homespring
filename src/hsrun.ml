open Homespring;;

class io =
    object
    method print s = print_string s
    method ready =
        try
            match Unix.select [Unix.stdin] [] [] 0.0 with
                (x::xs, [], []) -> true
              | _ -> false
        with
            _ -> false
    method get = read_line ()
end;;

if Array.length Sys.argv = 1 then
    print_string "error: you must supply a filename\n"
else
    let run n =
        while not n#isFinished do
            n#snowTick;
            n#waterTick;
            n#powerTick;
            n#fishTick;
            n#miscTick;
            n#inputTick;
            flush Pervasives.stdout;
        done
    in
    match build_tree Sys.argv.(1) (new io) with
        None -> print_string "In Homespring, the null program is not a quine.\n"
      | Some n -> n#printTree 0; run n
