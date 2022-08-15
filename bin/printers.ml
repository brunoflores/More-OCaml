let pp_int ppf n = Printf.fprintf ppf "%d" n

let pp_option printer ppf = function
  | None -> Printf.fprintf ppf "None"
  | Some v -> Printf.fprintf ppf "Some(%a)" printer v

let () =
  Printf.printf "Integer: %a\n" pp_int 42;
  Printf.printf "Maybe Integer: %a\n" (pp_option pp_int) (Some 42)
