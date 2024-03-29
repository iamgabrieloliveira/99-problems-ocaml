open OUnit2

let rec last list =
  match list with [] -> None | [ x ] -> Some x | _ :: t -> last t

let rec last_two list =
  match list with
  | [] -> None
  | [ _ ] -> None
  | [ x; y ] -> Some (x, y)
  | _ :: t -> last_two t

let rec at list idx =
  match list with
  | [] -> None
  | h :: t -> if idx = 0 then Some h else at t (idx - 1)

let length list =
  let rec aux list' count =
    match list' with [] -> count | _ :: t -> aux t (count + 1)
  in

  aux list 0

let rev list =
  let rec aux list' acc =
    match list' with [] -> acc | h :: t -> aux t (h :: acc)
  in

  aux list []

let is_palindrome list list' = rev list = list'

type 'a node = One of 'a | Many of 'a node list

let flatten list =
  let rec aux list' acc =
    match list' with
    | [] -> acc
    | One h :: t -> aux t (h :: acc)
    | Many h :: t -> aux t (aux h acc)
  in

  List.rev @@ aux list []

let rec compress = function
  | h :: (h' :: _ as t) -> if h = h' then compress t else h :: compress t
  | l -> l

let pack list =
  let rec aux list' acc current =
    match list' with
    | [] -> []
    | [ x ] -> (x :: current) :: acc
    | h :: (next :: _ as t) ->
        if h = next then aux t acc (h :: current)
        else aux t ((h :: current) :: acc) []
  in
  List.rev @@ aux list [] []

let run_length list =
  let rec aux list' acc count =
    match list' with
    | [] -> acc
    | [ x ] -> (count + 1, x) :: acc
    | h :: (next :: _ as t) ->
        if h = next then aux t acc (count + 1)
        else aux t ((count + 1, h) :: acc) 0
  in
  List.rev @@ aux list [] 0

let assert_none value = assert_equal None value
let assert_false value = assert_equal false value
let assert_true value = assert_equal true value

let tests =
  "90 Problems Ocaml"
  >::: [
         ( "Problem 1 - Last element of a list" >:: fun _ ->
           assert_equal (Some 3) (last [ 1; 2; 3 ]) );
         ( "Problem 1 - Last element of an empty list" >:: fun _ ->
           assert_none @@ last [] );
         ( "Problem 2 - Last two elements of a list" >:: fun _ ->
           assert_equal (Some (2, 3)) (last_two [ 1; 2; 3 ]) );
         ( "Problem 2 - Last two elements of a list with 1 element" >:: fun _ ->
           assert_none @@ last_two [ 1 ] );
         ( "Problem 2 - Last two elements of an empty list" >:: fun _ ->
           assert_none @@ last_two [] );
         ( "Problem 3 - Getting elemenet from a list by index" >:: fun _ ->
           assert_equal (Some 2) (at [ 1; 2; 3 ] 1) );
         ( "Problem 3 - Getting element by index out of range" >:: fun _ ->
           assert_none @@ at [ 1; 2; 3 ] 99 );
         ( "Problem 4 - Getting length of a list" >:: fun _ ->
           assert_equal 3 (length [ 1; 2; 3 ]) );
         ( "Problem 5 - Reversing a list" >:: fun _ ->
           assert_equal [ 3; 2; 1 ] (rev [ 1; 2; 3 ]) );
         ( "Problem 6 - Checking if a list is palindrome - is" >:: fun _ ->
           assert_true @@ is_palindrome [ 1; 2; 3 ] [ 3; 2; 1 ] );
         ( "Problem 6 - Checking if a list is palindrome - isn't" >:: fun _ ->
           assert_false @@ is_palindrome [ 1; 2; 3 ] [ 3; 9; 1 ] );
         ( "Problem 7 - Flatten an array" >:: fun _ ->
           assert_equal [ 1; 2; 3; 4; 5; 6 ]
             (flatten [ One 1; One 2; One 3; Many [ One 4; One 5; One 6 ] ]) );
         ( "Problem 8 - Removing consecutives duplicates" >:: fun _ ->
           assert_equal [ 1; 2; 3; 4 ] (compress [ 1; 2; 2; 3; 3; 4; 4 ]) );
         ( "Problem 9 - Group consecutives duplicates" >:: fun _ ->
           assert_equal
             [ [ 1 ]; [ 2; 2 ]; [ 3; 3 ]; [ 4; 4 ] ]
             (pack [ 1; 2; 2; 3; 3; 4; 4 ]) );
         ( "Problem 10 - Run Length" >:: fun _ ->
           assert_equal
             [ (1, 1); (2, 2); (3, 3); (2, 4) ]
             (run_length [ 1; 2; 2; 3; 3; 3; 4; 4 ]) );
       ]

let _ = run_test_tt_main tests
