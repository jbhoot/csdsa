let rec present_in x arr =
match arr with
| [] -> false
| hd :: [] -> x == hd
| hd :: tl -> if x == hd then true else present_in x tl

let rec intersection a b =
match a with
| [] -> []
| hd :: tl -> if present_in hd b then hd :: intersection tl b else intersection tl b

let rec make_res_print res =
match res with
| [] -> ""
| hd :: [] -> Printf.sprintf "%d" hd
| hd :: tl -> Printf.sprintf "%d; " hd ^ make_res_print tl

let print_res res =
Printf.printf "[%s]" (make_res_print res)

let () = print_res (intersection [3; 5; 4; 2] [4; 5; 3; 6])
