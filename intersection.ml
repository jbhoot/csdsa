let rec present_in x arr =
match arr with
| [] -> false
| hd :: [] -> x == hd
| hd :: tl -> if x == hd then true else present_in x tl

let rec intersection a b =
match a with
| [] -> []
| hd :: tl -> if present_in hd b then hd :: intersection tl b else intersection tl b
