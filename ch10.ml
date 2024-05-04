let rec countdown num =
  match num with
  | 0 -> print_endline "0"
  | v ->
      print_endline (string_of_int v);
      countdown (v - 1)

let rec traverse_dir dir_path =
  let sub_dirs =
    dir_path |> Sys.readdir |> Array.to_list
    |> List.map (fun fname -> dir_path ^ "/" ^ fname)
    |> List.filter Sys.is_directory
  in
  match sub_dirs with
  | [] -> print_endline dir_path
  | lst -> List.iter traverse_dir lst

module NumbArray = struct
  (*
   * Here is an array containing both numbers as well as other arrays, which in turn contain numbers and arrays.
   * Write a recursive function that prints all the numbers.
   *)

  type t = Number of int | Array of int array | NumbArray of t array

  let rec print_only_numbers t =
    match t with
    | Number n -> print_endline (string_of_int n)
    | Array arr -> Array.iter (fun n -> print_endline (string_of_int n)) arr
    | NumbArray t_arr -> Array.iter print_only_numbers t_arr

  let test () =
    let test_data =
      NumbArray
        [|
          Number 1;
          Number 2;
          Number 3;
          Array [| 4; 5; 6 |];
          Number 7;
          NumbArray
            [|
              Number 8;
              NumbArray
                [| Number 9; Number 10; Number 11; Array [| 12; 13; 14 |] |];
            |];
        |]
    in
    print_only_numbers test_data
end

let () =
  countdown 10;
  traverse_dir "/Users/jb/Movies";
  NumbArray.test ()
