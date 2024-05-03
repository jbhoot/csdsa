module Intersection = struct
  let rec present_in x list =
    match list with
    | [] -> false
    | hd :: [] -> x == hd
    | hd :: tl -> if x == hd then true else present_in x tl

  let rec intersection a b =
    match a with
    | [] -> []
    | hd :: tl ->
        if present_in hd b then hd :: intersection tl b else intersection tl b
end

let () =
  print_endline ("Intersection: " ^ Printers.list_to_string (Intersection.intersection [ 3; 5; 4; 2 ] [ 4; 5; 3; 6 ]));
