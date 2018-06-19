let rec last = function
  | [] -> None
  | [x] -> Some x
  | x :: xs -> last xs;;

assert (last ["a"; "b"; "c"; "d"] = Some "d");;
assert (last [] = None)

let rec last_two = function
  | [] -> None
  | [x] -> None
  | [x; y] -> Some (x, y)
  | x :: xs -> last_two xs;;

assert (last_two ["a"; "b"; "c"; "d"] = Some ("c", "d"));;
assert (last_two ["a"] = None)

let rec at k l = match k with
  | 1 -> begin match l with
      | [] -> None
      | x :: xs -> Some x
    end
  | _ -> begin match l with
      | [] -> None
      | x :: xs -> at (k - 1) xs
    end;;

assert (at 3 ["a"; "b"; "c"; "d"; "e"] = Some "c");;
assert (at 3 ["a"] = None)

let rec length = function
  | [] -> 0
  | _ :: t -> 1 + length t;;

assert (length ["a"; "b"; "c"] = 3);;
assert (length [] = 0)

let rec rev = function
  | [] -> []
  | h :: t -> rev t @ [h];;

assert (rev ["a"; "b"; "c"] = ["c"; "b"; "a"])

let is_palindrome l = l = List.rev l;;

assert (is_palindrome ["x"; "a"; "m"; "a"; "x"]);;
assert (is_palindrome ["a"; "b"] = false)

type 'a node =
  | One of 'a
  | Many of 'a node list;;

let rec flatten = function
  | [] -> []
  | One x :: t -> [x] @ flatten t
  | Many x :: t -> flatten x @ flatten t;;

assert (flatten [One "a"; Many [One "b"; Many [One "c"; One "d"]; One "e"]] =
        ["a"; "b"; "c"; "d"; "e"])

let rec compress = function
  | [] -> []
  | [x] -> [x]
  | x :: y :: t -> if x = y then compress (x :: t) else x :: compress (y :: t);;

assert (compress ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"e";"e";"e";"e"] =
        ["a"; "b"; "c"; "a"; "d"; "e"])

let pack input =
  let rec _pack output = function
    | [] -> output
    | h :: t -> _pack (if output = [] || h <> List.hd (List.hd output)
                       then [h] :: output
                       else (h :: List.hd output) :: List.tl output) t in
  List.rev (_pack [] input);;

assert (pack ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"d";"e";"e";"e";"e"] =
        [["a"; "a"; "a"; "a"]; ["b"]; ["c"; "c"]; ["a"; "a"]; ["d"; "d"];
         ["e"; "e"; "e"; "e"]])

(*let encode input =
  let rec _encode output = function
    | [] -> output
    | h :: t -> _encode (if output = [] || h <> snd (List.hd output)
                         then (1, h) :: output
                         else let (i, c) = List.hd output in
                           (i + 1, c) :: List.tl output) t in
  List.rev (_encode [] input);;

assert (encode ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"e";"e";"e";"e"] =
        [(4, "a"); (1, "b"); (2, "c"); (2, "a"); (1, "d"); (4, "e")])*)

type 'a rle =
  | One of 'a
  | Many of int * 'a;;

let encode input =
  let rec _encode output = function
    | [] -> output
    | h :: t -> _encode (if output = [] then One h :: output
                         else match List.hd output with
                           | One g when g = h -> Many (2, h) :: List.tl output
                           | Many (n, g) when g = h ->
                             Many ((n + 1), h) :: List.tl output
                           | _ -> One h :: output) t in
  List.rev (_encode [] input);;

assert (encode ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"e";"e";"e";"e"] =
        [Many (4, "a"); One "b"; Many (2, "c"); Many (2, "a"); One "d";
         Many (4, "e")])

let rec decode = function
  | [] -> []
  | One c :: t -> c :: decode t
  | Many (2, c) :: t -> c :: decode (One c :: t)
  | Many (n, c) :: t -> c :: decode (Many (n - 1, c) :: t);;

assert (decode [Many (4,"a"); One "b"; Many (2,"c"); Many (2,"a"); One "d";
                Many (4,"e")] =
        ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e"])

let rec duplicate = function
  | [] -> []
  | h :: t -> h :: h :: duplicate t;;

assert (duplicate ["a";"b";"c";"c";"d"] =
        ["a"; "a"; "b"; "b"; "c"; "c"; "c"; "c"; "d"; "d"])

let rec replicate l n =
  let rec _replicate l' n' =
    if l' = [] then []
    else if n' = 0 then _replicate (List.tl l') n
    else List.hd l' :: _replicate l' (n' - 1) in
  _replicate l n;;

assert (replicate ["a"; "b"; "c"] 3 =
        ["a"; "a"; "a"; "b"; "b"; "b"; "c"; "c"; "c"])

let rec drop l n =
  let rec _drop l' n' =
    if l' = [] then []
    else if n' = 1 then drop (List.tl l') n
    else List.hd l' :: _drop (List.tl l') (n' - 1) in
  _drop l n;;

assert (drop ["a";"b";"c";"d";"e";"f";"g";"h";"i";"j"] 3 =
        ["a"; "b"; "d"; "e"; "g"; "h"; "j"])

let rec split l = function
  | 0 -> ([], l)
  | n -> if l = [] then (l, [])
    else let (f, s) = split (List.tl l) (n - 1) in
      (List.hd l :: f, s);;

assert (split ["a";"b";"c";"d";"e";"f";"g";"h";"i";"j"] 3 =
        (["a"; "b"; "c"], ["d"; "e"; "f"; "g"; "h"; "i"; "j"]));;
assert (split ["a";"b";"c";"d"] 5 = (["a"; "b"; "c"; "d"], []))

let rec slice l i k = match l, i, k with
  | [], _, _
  | _, _, -1 -> []
  | h :: t, 0, k -> h :: slice t 0 (k - 1)
  | _ :: t, i, k -> slice t (i - 1) (k - 1);;

assert (slice ["a";"b";"c";"d";"e";"f";"g";"h";"i";"j"] 2 6 =
        ["c"; "d"; "e"; "f"; "g"])

let rec rotate l n =
  if n = 0 then l
  else if n > 0 then rotate (List.tl l @ [List.hd l]) (n - 1)
  else rotate l (List.length l + n);;

assert (rotate ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"] 3 =
        ["d"; "e"; "f"; "g"; "h"; "a"; "b"; "c"]);;
assert (rotate ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"] (-2) =
        ["g"; "h"; "a"; "b"; "c"; "d"; "e"; "f"])

let rec remove_at n l = match n, l with
  | _, [] -> []
  | 0, _ :: t -> t
  | n, h :: t -> h :: remove_at (n - 1) t;;

assert (remove_at 1 ["a";"b";"c";"d"] = ["a"; "c"; "d"])

let rec insert_at e n l = match n, l with
  | _, [] -> [e]
  | 0, l -> e :: l
  | n, h :: t -> h :: insert_at e (n - 1) t;;

assert (insert_at "alfa" 1 ["a";"b";"c";"d"] = ["a"; "alfa"; "b"; "c"; "d"]);;
assert (insert_at "alfa" 3 ["a";"b";"c";"d"] = ["a"; "b"; "c"; "alfa"; "d"]);;
assert (insert_at "alfa" 4 ["a";"b";"c";"d"] = ["a"; "b"; "c"; "d"; "alfa"])

let rec range a b =
  if a = b then [a]
  else a :: range (if a < b then a + 1 else a - 1) b;;

assert (range 4 9 = [4; 5; 6; 7; 8; 9]);;
assert (range 9 4 = [9; 8; 7; 6; 5; 4])

let rec rand_select l n =
  let rec take n l = match n, l with
    | _, [] -> failwith "empty list"
    | 0, h :: _ -> h
    | n, _ :: t -> take (n - 1) t in
  match n with
  | 0 -> []
  | n -> take (Random.int (List.length l)) l :: rand_select l (n - 1)

let rec lotto_select n m = match n with
  | 0 -> []
  | n -> Random.int m :: lotto_select (n - 1) m

let permutation l =
  let rec take n l = match n, l with
    | _, [] -> failwith "empty list"
    | 0, h :: t -> h, t
    | n, h :: t ->
      let (e, r) = take (n - 1) t in
      e, h :: r
  in
  let rec loop n l = match n, l with
    | _, []
    | 0, _ -> []
    | n, l ->
      let (e, r) = take (Random.int (List.length l)) l in
      e :: loop (n - 1) r
  in
  loop (List.length l) l

let rec extract n l = match n, l with
  | _, [] -> []
  | 1, l -> List.map (fun e -> [e]) l
  | n, h :: t -> (List.map (List.cons h) (extract (n - 1) t)) @ extract n t;;

assert (extract 2 ["a";"b";"c";"d"] = [["a"; "b"]; ["a"; "c"]; ["a"; "d"];
                                       ["b"; "c"]; ["b"; "d"]; ["c"; "d"]])

let rec group l n =
  let rec extract n l = match n, l with
    | 0, l -> [[], l]
    | _, [] -> []
    | n, h :: t -> (List.map (fun (used, unused) -> h :: used, unused)
                      (extract (n - 1) t)) @
                   List.map (fun (used, unused) -> used, h :: unused)
                     (extract n t)
  in
  match n with
  | [] -> [[]]
  | h :: t ->
    List.flatten (List.map (fun (used, unused) -> List.map (List.cons used)
                               (group unused t)) (extract h l))

(*assert (group ["a";"b";"c";"d"] [2;1] =
        [[["a"; "b"]; ["c"]]; [["a"; "c"]; ["b"]]; [["b"; "c"]; ["a"]];
         [["a"; "b"]; ["d"]]; [["a"; "c"]; ["d"]]; [["b"; "c"]; ["d"]];
         [["a"; "d"]; ["b"]]; [["b"; "d"]; ["a"]]; [["a"; "d"]; ["c"]];
         [["b"; "d"]; ["c"]]; [["c"; "d"]; ["a"]]; [["c"; "d"]; ["b"]]])*)

let length_sort l =
  List.sort (fun a b -> List.length a - List.length b) l;;

assert (length_sort [ ["a";"b";"c"]; ["d";"e"]; ["f";"g";"h"]; ["d";"e"];
                      ["i";"j";"k";"l"]; ["m";"n"]; ["o"] ] =
        [["o"]; ["d"; "e"]; ["d"; "e"]; ["m"; "n"]; ["a"; "b"; "c"];
         ["f"; "g"; "h"]; ["i"; "j"; "k"; "l"]])

let frequency_sort l =
  let rec frequency n = function
    | [] -> 0
    | h :: t when List.length h = n -> 1 + frequency n t
    | _ :: t -> frequency n t
  in
  List.sort (fun a b -> frequency (List.length a) l -
                        frequency (List.length b) l) l;;

assert (frequency_sort [ ["a";"b";"c"]; ["d";"e"]; ["f";"g";"h"]; ["d";"e"];
                         ["i";"j";"k";"l"]; ["m";"n"]; ["o"] ] =
        [["i"; "j"; "k"; "l"]; ["o"]; ["a"; "b"; "c"]; ["f"; "g"; "h"];
         ["d"; "e"]; ["d"; "e"]; ["m"; "n"]])

let is_prime n =
  let rec _div n k =
    if k >= n then false else
    if n mod k = 0 then true
    else _div n (k + 1)
  in
  if n = 1 then false
  else not (_div n 2);;

assert (not(is_prime 1));;
assert (is_prime 7);;
assert (not (is_prime 12))

let rec gcd a b =
  if b = 0 then a else
  if b > a then gcd b a
  else gcd b (a mod b);;

assert (gcd 13 27 = 1);;
assert (gcd 20536 7826 = 2)

let coprime a b =
  gcd a b = 1;;

assert (coprime 13 27);;
assert (not (coprime 20536 7826))

let phi m =
  let rec _phi m r =
    if r = m then 0 else
    if coprime m r then 1 + _phi m (r + 1)
    else _phi m (r + 1)
  in
  _phi m 1;;

assert (phi 10 = 4);;
assert (phi 13 = 12)

(*let factors n =
  let rec _factors n k =
    if k > n then [] else
    if is_prime k && n mod k = 0 then k :: _factors (n / k) k
    else _factors n (k + 1)
  in
  _factors n 2;;

  assert (factors 315 = [3; 3; 5; 7])*)

let factors number =
  let rec _factors number potential_divisor divisor_list =
    if potential_divisor > number then divisor_list else
    if is_prime potential_divisor && number mod potential_divisor = 0 then
      _factors (number / potential_divisor) potential_divisor (
        match divisor_list with
        | [] -> [potential_divisor, 1]
        | ((divisor, power) :: tail) ->
          if potential_divisor = divisor then (divisor, power + 1) :: tail
          else (potential_divisor, 1) :: divisor_list)
    else _factors number (potential_divisor + 1) divisor_list
  in
  List.rev (_factors number 2 []);;

assert (factors 315 = [(3, 2); (5, 1); (7, 1)])

let phi_improved m =
  let rec multiply = function
    | [] -> 1
    | (factor, multiplicity) :: tail ->
      (factor - 1) * int_of_float (float_of_int (factor) **
                                   float_of_int (multiplicity - 1)) *
      multiply tail
  in
  multiply (factors m);;

assert (phi_improved 10 = 4);;
assert (phi_improved 13 = 12)

let timeit f x =
  let t = Sys.time() in
  let _ = f x in
  Sys.time() -. t

let rec all_primes a b =
  if a >= b then [] else
  if is_prime a then a :: all_primes (a + 1) b
  else all_primes (a + 1) b;;

assert (List.length (all_primes 2 7920) = 1000)

let goldbach n =
  let rec _goldbach n = function
    | [] -> failwith "publish this!"
    | p1 :: t ->
      let p2 = n - p1 in
      if is_prime p2 then p1, p2
      else _goldbach n t
  in
  _goldbach n (all_primes 2 n);;

assert (goldbach 28 = (5, 23))

let rec goldbach_list a b =
  if a mod 2 = 1 then goldbach_list (a + 1) b else
  if a > b then []
  else (a, goldbach a) :: goldbach_list (a + 2) b;;

assert (goldbach_list 9 20 = [(10, (3, 7)); (12, (5, 7)); (14, (3, 11));
                              (16, (3, 13)); (18, (5, 13)); (20, (3, 17))])

let rec goldbach_limit a b l =
  let rec _goldbach n = function
    | [] -> None
    | p1 :: t ->
      let p2 = n - p1 in
      if is_prime p2 then Some (p1, p2)
      else _goldbach n t
  in
  if a mod 2 = 1 then goldbach_limit (a + 1) b l else
  if a > b then []
  else let result = _goldbach a (all_primes l (a - l)) in
    match result with
    | None -> goldbach_limit (a + 2) b l
    | Some r -> (a, r) :: goldbach_limit (a + 2) b l

(*assert (goldbach_limit 1 2000 50 = [(992, (73, 919)); (1382, (61, 1321));
                                    (1856, (67, 1789)); (1928, (61, 1867))])*)

type bool_expr =
  | Var of string
  | Not of bool_expr
  | And of bool_expr * bool_expr
  | Or of bool_expr * bool_expr

let table2 a b expression =
  let rec evaluate a_value b_value = function
    | Var name when name = a -> a_value
    | Var name -> b_value
    | Not expr -> not (evaluate a_value b_value expr)
    | And (expr1, expr2) -> (evaluate a_value b_value expr1) &&
                            (evaluate a_value b_value expr2)
    | Or (expr1, expr2) -> (evaluate a_value b_value expr1) ||
                           (evaluate a_value b_value expr2)
  in
  List.map (fun (a, b) -> a, b, evaluate a b expression)
    [(true, true); (true, false); (false, true); (false, false)];;

assert (table2 "a" "b" (And(Var "a", Or(Var "a", Var "b"))) =
        [(true, true, true); (true, false, true);
         (false, true, false); (false, false, false)])

let table variables expr =
  let rec evaluate values = function
    | Var name -> snd (List.find (fun (n, _) -> n = name) values)
    | Not expr -> not (evaluate values expr)
    | And (expr1, expr2) -> (evaluate values expr1) &&
                            (evaluate values expr2)
    | Or (expr1, expr2) -> (evaluate values expr1) ||
                           (evaluate values expr2)
  and generate_values = function
    | [] -> [[]]
    | variable :: tail ->
      let others = generate_values tail in
      (List.map (fun values -> (variable, true) :: values) others) @
      (List.map (fun values -> (variable, false) :: values) others)
  in
  List.map (fun values -> values, evaluate values expr)
    (generate_values variables);;


assert (table ["a"; "b"] (And(Var "a", Or(Var "a", Var "b"))) =
        [([("a", true); ("b", true)], true);
         ([("a", true); ("b", false)], true);
         ([("a", false); ("b", true)], false);
         ([("a", false); ("b", false)], false)]);;

assert (let a = Var "a"
        and b = Var "b"
        and c = Var "c" in
        table ["a"; "b"; "c"] (Or(And(a, Or(b,c)), Or(And(a,b), And(a,c)))) =
        [([("a", true); ("b", true); ("c", true)], true);
         ([("a", true); ("b", true); ("c", false)], true);
         ([("a", true); ("b", false); ("c", true)], true);
         ([("a", true); ("b", false); ("c", false)], false);
         ([("a", false); ("b", true); ("c", true)], false);
         ([("a", false); ("b", true); ("c", false)], false);
         ([("a", false); ("b", false); ("c", true)], false);
         ([("a", false); ("b", false); ("c", false)], false)])

let rec gray = function
  | 1 -> ["0"; "1"]
  | n ->
    let original = gray (n - 1) in
    let reversed = List.rev original in
    (List.map (fun x -> "0" ^ x) original) @
    (List.map (fun x -> "1" ^ x) reversed);;

assert (gray 1 = ["0"; "1"]);;
assert (gray 2 = ["00"; "01"; "11"; "10"]);;
assert (gray 3 = ["000"; "001"; "011"; "010"; "110"; "111"; "101"; "100"])

type 'a binary_tree =
  | Empty
  | Node of 'a * 'a binary_tree * 'a binary_tree

(* from the official manual *)
module PrioQueue = struct
  type priority = int
  type 'a queue = Empty | Node of priority * 'a * 'a queue * 'a queue
  let empty = Empty
  let rec insert queue prio elt =
    match queue with
      Empty -> Node(prio, elt, Empty, Empty)
    | Node(p, e, left, right) ->
      if prio <= p
      then Node(prio, elt, insert right p e, left)
      else Node(p, e, insert right prio elt, left)
  exception Queue_is_empty
  let rec remove_top = function
      Empty -> raise Queue_is_empty
    | Node(prio, elt, left, Empty) -> left
    | Node(prio, elt, Empty, right) -> right
    | Node(prio, elt, (Node(lprio, lelt, _, _) as left),
           (Node(rprio, relt, _, _) as right)) ->
      if lprio <= rprio
      then Node(lprio, lelt, remove_top left, right)
      else Node(rprio, relt, left, remove_top right)
  let extract = function
      Empty -> raise Queue_is_empty
    | Node(prio, elt, _, _) as queue -> (prio, elt, remove_top queue)
end

let huffman frequencies =
  let rec build_tree queue =
    let frequency1, node1, queue = PrioQueue.extract queue in
    try
      let frequency2, node2, queue = PrioQueue.extract queue in
      let new_node = Node ("", node1, node2) in
      let new_queue = PrioQueue.insert queue (frequency1 + frequency2)
          new_node in
      build_tree new_queue
    with PrioQueue.Queue_is_empty -> node1
  in
  let rec generate_coding code = function
    | Empty -> []
    | Node (character, Empty, Empty) -> [(character, code)]
    | Node (_, left, right) ->
      generate_coding (code ^ "0") left @ generate_coding (code ^ "1") right
  in
  let queue = List.fold_left
      (fun q (c, f) -> PrioQueue.insert q f (Node (c, Empty, Empty)))
      PrioQueue.empty frequencies in
  let tree = build_tree queue in
  generate_coding "" tree;;

let fs = [ ("a", 45); ("b", 13); ("c", 12); ("d", 16); ("e", 9); ("f", 5) ];;
assert (huffman fs = [("a", "0"); ("c", "100"); ("b", "101");
                      ("f", "1100"); ("e", "1101"); ("d", "111")]);;
assert (huffman ["a", 10;  "b", 15;  "c", 30;  "d", 16;  "e", 29] =
        [("d", "00"); ("a", "010"); ("b", "011"); ("e", "10"); ("c", "11")])

let example_tree = Node('a', Node('b', Node('d', Empty, Empty),
                                  Node('e', Empty, Empty)),
                        Node('c', Empty, Node('f', Node('g', Empty, Empty),
                                              Empty)))
let example_int_tree = Node(1, Node(2, Node(4, Empty, Empty),
                                    Node(5, Empty, Empty)),
                            Node(3, Empty, Node(6, Node(7, Empty, Empty),
                                                Empty)))

let rec cbal_tree n =
  let rec _cbal_tree left_trees right_trees =
    List.flatten (List.map (fun left_tree ->
        List.map (fun right_tree -> Node ('x', left_tree, right_tree))
          right_trees) left_trees)
  in
  match n with
  | 0 -> [Empty]
  | n when n mod 2 = 1 ->
    let subtrees = cbal_tree ((n - 1) / 2) in
    _cbal_tree subtrees subtrees
  | n ->
    let subtrees1 = cbal_tree (n / 2 - 1)
    and subtrees2 = cbal_tree (n / 2) in
    _cbal_tree subtrees2 subtrees1 @ _cbal_tree subtrees1 subtrees2

(*assert (cbal_tree 4 = [Node ('x', Node ('x', Empty, Empty),
                             Node ('x', Node ('x', Empty, Empty), Empty));
                       Node ('x', Node ('x', Empty, Empty),
                             Node ('x', Empty, Node ('x', Empty, Empty)));
                       Node ('x', Node ('x', Node ('x', Empty, Empty), Empty),
                             Node ('x', Empty, Empty));
                       Node ('x', Node ('x', Empty, Node ('x', Empty, Empty)),
                             Node ('x', Empty, Empty))]);;
  assert (List.length (cbal_tree 40) = 524288)*)

let rec is_mirror t1 t2 = match t1, t2 with
  | Empty, Empty -> true
  | Empty, _
  | _, Empty -> false
  | Node (_, l1, r1), Node (_, l2, r2) -> is_mirror l1 r2 && is_mirror l2 r1

let is_symmetric = function
  | Empty -> true
  | Node (_, l, r) -> is_mirror l r

let rec construct = function
  | [] -> Empty
  | h :: t as l -> Node (h, (construct (List.filter ((>) h) l)),
                         (construct (List.filter ((<) h) l)));;

assert (construct [3;2;5;7;1] =
        Node (3, Node (2, Node (1, Empty, Empty), Empty),
              Node (5, Empty, Node (7, Empty, Empty))));;
assert (is_symmetric(construct [5;3;18;1;4;12;21]));;
assert (not(is_symmetric(construct [3;2;5;7;4])))

let sym_cbal_trees n =
  List.filter is_symmetric (cbal_tree n);;

assert (sym_cbal_trees 5 =
        [Node ('x', Node ('x', Node ('x', Empty, Empty), Empty),
               Node ('x', Empty, Node ('x', Empty, Empty)));
         Node ('x', Node ('x', Empty, Node ('x', Empty, Empty)),
               Node ('x', Node ('x', Empty, Empty), Empty))]);;
assert (List.length (sym_cbal_trees 57) = 256);;
assert (List.map (fun n -> n, List.length(sym_cbal_trees n)) (range 10 20) =
        [(10, 0); (11, 4); (12, 0); (13, 4); (14, 0); (15, 1);
         (16, 0); (17, 8); (18, 0); (19, 16); (20, 0)])

let rec hbal_tree height =
  let combine trees1 trees2 =
    List.fold_left (fun list tree1 ->
        (List.map (fun tree2 -> Node ('x', tree1, tree2)) trees2) @ list)
      [] trees1
  in
  if height = 0 then [Empty] else
  if height = 1 then [Node ('x', Empty, Empty)]
  else
    let subtrees1 = hbal_tree (height - 1)
    and subtrees2 = hbal_tree (height - 2) in
    combine subtrees1 subtrees1 @ combine subtrees1 subtrees2 @
    combine subtrees2 subtrees1;;

let t = hbal_tree 3;;
let x = 'x';;
assert (List.mem (Node(x, Node(x, Node(x, Empty, Empty), Node(x, Empty, Empty)),
                       Node(x, Node(x, Empty, Empty),
                            Node(x, Empty, Empty)))) t);;
assert (List.mem (Node(x, Node(x, Node(x, Empty, Empty), Node(x, Empty, Empty)),
                       Node(x, Node(x, Empty, Empty), Empty))) t);;
assert (List.length t = 15)

let max_nodes h = 1 lsl h - 1

let rec min_nodes = function
  | 0 -> 0
  | 1 -> 1
  | h -> 1 + min_nodes (h - 1) + min_nodes (h - 2)

let min_height n = int_of_float (ceil (log (float_of_int (n + 1)) /. log 2.0))

let max_height n =
  let rec maximise_height h =
    if min_nodes (h + 1) > n then h
    else maximise_height (h + 1)
  in
  maximise_height 0

let hbal_tree_nodes num_nodes =
  let max_tree_height = max_height num_nodes in
  let rec count_nodes = function
    | Empty -> 0
    | Node (_, l, r) -> 1 + (count_nodes l) + (count_nodes r)
  in
  let two_subtrees_to_tree subtrees1 subtrees2 =
    List.fold_left (fun list subtree1 ->
        let remaining_nodes = num_nodes - (count_nodes subtree1) - 1 in
        let remaining_right_subtrees = List.filter
            (fun tree -> count_nodes tree = remaining_nodes)
            subtrees2 in
        (List.map (fun subtree2 -> Node ('x', subtree1, subtree2))
           remaining_right_subtrees) @ list) [] subtrees1
  in
  let rec construct_trees = function
    | 0 -> [Empty]
    | 1 -> [Node ('x', Empty, Empty)]
    | height ->
      if height > max_tree_height then []
      else
        let subtrees1 = hbal_tree (height - 1)
        and subtrees2 = hbal_tree (height - 2) in
        two_subtrees_to_tree subtrees1 subtrees1 @
        two_subtrees_to_tree subtrees1 subtrees2 @
        two_subtrees_to_tree subtrees2 subtrees1 @ construct_trees (height + 1)
  in
  construct_trees (min_height num_nodes);;

assert (List.length (hbal_tree_nodes 15) = 1553);;
assert (List.map hbal_tree_nodes [0; 1; 2; 3] =
        [[Empty]; [Node ('x', Empty, Empty)];
         [Node ('x', Node ('x', Empty, Empty), Empty);
          Node ('x', Empty, Node ('x', Empty, Empty))];
         [Node ('x', Node ('x', Empty, Empty), Node ('x', Empty, Empty))]])

let rec count_leaves = function
  | Empty -> 0
  | Node (_, Empty, Empty) -> 1
  | Node (_, l, r) -> count_leaves l + count_leaves r;;

assert (count_leaves Empty = 0);;
assert (count_leaves example_tree = 3)

let rec leaves = function
  | Empty -> []
  | Node (e, Empty, Empty) -> [e]
  | Node (_, l, r) -> leaves l @ leaves r;;

assert (leaves Empty = []);;
assert (leaves example_tree = ['d'; 'e'; 'g'])

let rec internals = function
  | Empty -> []
  | Node (_, Empty, Empty) -> []
  | Node (e, l, r) -> internals l @ [e] @ internals r;;

assert (internals (Node('a', Empty, Empty)) = []);;
assert (internals example_tree = ['b'; 'a'; 'c'; 'f'])

let rec at_level t l = match t, l with
  | Empty, _ -> []
  | Node (e, left, right), l ->
    if l = 1 then [e]
    else at_level left (l - 1) @ at_level right (l - 1);;

assert (at_level example_tree 2 = ['b'; 'c']);;
assert (at_level example_tree 5 = [])

let complete_binary_tree nodes =
  let length = List.length nodes in
  let rec _complete_binary_tree index =
    if index > length then Empty
    else Node (List.nth nodes (index - 1), _complete_binary_tree (2 * index),
               _complete_binary_tree (2 * index + 1))
  in
  _complete_binary_tree 1;;

assert (complete_binary_tree [1;2;3;4;5;6] =
        Node (1, Node (2, Node (4, Empty, Empty), Node (5, Empty, Empty)),
              Node (3, Node (6, Empty, Empty), Empty)))

let layout_binary_tree_1 tree =
  let rec _layout_binary_tree first_x y = function
    | Empty -> Empty, first_x
    | Node (e, l, r) ->
      let left_subtree, x = _layout_binary_tree first_x (y + 1) l in
      let right_subtree, next_x = _layout_binary_tree (x + 1) (y + 1) r in
      Node ((e, x, y), left_subtree, right_subtree), next_x
  in
  fst (_layout_binary_tree 1 1 tree);;

let example_layout_tree =
    let leaf x = Node (x, Empty, Empty) in
    Node('n', Node('k', Node('c', leaf 'a',
                             Node('h', Node('g', leaf 'e',Empty), Empty)),
                   leaf 'm'),
         Node('u', Node('p', Empty, Node('s', leaf 'q', Empty)), Empty));;
assert (layout_binary_tree_1 example_layout_tree =
        Node (('n', 8, 1),
              Node (('k', 6, 2),
                    Node (('c', 2, 3),
                          Node (('a', 1, 4), Empty, Empty),
                          Node (('h', 5, 4),
                                Node (('g', 4, 5),
                                      Node (('e', 3, 6), Empty, Empty), Empty),
                                Empty)), Node (('m', 7, 3), Empty, Empty)),
              Node (('u', 12, 2),
                    Node (('p', 9, 3), Empty,
                          Node (('s', 11, 4),
                                Node (('q', 10, 5), Empty, Empty), Empty)),
                    Empty)))

let layout_binary_tree_2 tree =
  let rec find_height = function
    | Empty -> 0
    | Node (_, l, r) -> 1 + max (find_height l) (find_height r)
  in
  let height = find_height tree in
  let rec layout x depth = function
    | Empty -> Empty, 1
    | Node (e, l, r) ->
      let x_difference = 1 lsl (height - depth) in
      let left_subtree, left_x = layout (x - x_difference / 2) (depth + 1) l in
      let x = if x > 0 then x else left_x in
      let right_subtree, _ = layout (x + x_difference / 2) (depth + 1) r in
      Node ((e, x, depth), left_subtree, right_subtree), x + x_difference
  in
  fst (layout 0 1 tree)

let example_layout_tree =
  let leaf x = Node (x,Empty,Empty) in
  Node('n', Node('k', Node('c', leaf 'a', Node('e', leaf 'd', leaf 'g')),
                 leaf 'm'), Node('u', Node('p', Empty, leaf 'q'), Empty));;
assert (layout_binary_tree_2 example_layout_tree =
        Node (('n', 15, 1),
              Node (('k', 7, 2),
                    Node (('c', 3, 3),
                          Node (('a', 1, 4), Empty, Empty),
                          Node (('e', 5, 4),
                                Node (('d', 4, 5), Empty, Empty),
                                Node (('g', 6, 5), Empty, Empty))),
                    Node (('m', 11, 3), Empty, Empty)),
              Node (('u', 23, 2),
                    Node (('p', 19, 3), Empty,
                          Node (('q', 21, 4), Empty, Empty)), Empty)))
let example2_layout_tree =
  let leaf x = Node (x,Empty,Empty) in
  Node('n', Empty, Node('u', Node('p', Empty, leaf 'q'), Empty));;
assert (layout_binary_tree_2 example2_layout_tree =
        Node (('n', 1, 1), Empty,
              Node (('u', 5, 2),
                    Node (('p', 3, 3), Empty,
                          Node (('q', 4, 4), Empty, Empty)), Empty)))

let layout_binary_tree_3 tree =
  let rec update_protrusions main auxiliary = match (main, auxiliary) with
    | _, [] -> List.map ((+) 1) main
    | [], _ -> List.map (fun x -> x - 1) auxiliary
    | l :: ls, r :: rs -> max (l + 1) (r - 1) :: update_protrusions ls rs
  in
  let rec calculate_distance left_protrusions right_protrusions =
    if left_protrusions = [] || right_protrusions = [] then 1
    else max (let t = List.hd left_protrusions + List.hd right_protrusions + 1 in
             if t mod 2 = 0 then t / 2 else t / 2 + 1)
        (calculate_distance (List.tl left_protrusions)
           (List.tl right_protrusions))
  in
  let rec assign_distances leftmost depth = function
    | Empty -> Empty, [], []
    | Node (e, l, r) ->
      let left_node, left_protrusions_to_left, left_protrusions_to_right =
        assign_distances leftmost (depth + 1) l
      and right_node, right_protrusions_to_left, right_protrusions_to_right =
        assign_distances false (depth + 1) r in
      let distance = calculate_distance left_protrusions_to_right
          right_protrusions_to_left in
      Node ((e, 0, depth, distance), left_node, right_node),
      0 :: update_protrusions left_protrusions_to_left right_protrusions_to_left,
      0 :: update_protrusions right_protrusions_to_right
        left_protrusions_to_right
  in
  let rec distances_to_x_coords x_from_parent = function
    | Empty -> Empty
    | Node ((e, _, y, d), l, r) ->
      let left_child = distances_to_x_coords
          (if x_from_parent = 0 then 0 else x_from_parent - d) l in
      let x =
        if x_from_parent <> 0 then x_from_parent
        else
          match left_child with
          | Empty -> 1
          | Node ((_, x, _), _, _) -> x + d
      in
      let right_child = distances_to_x_coords (x + d) r in
      Node ((e, x, y), left_child, right_child)
  in
  let updated_tree, _, _ = assign_distances true 1 tree in
  distances_to_x_coords 0 updated_tree;;

assert (layout_binary_tree_3 example_layout_tree =
        Node (('n', 5, 1),
              Node (('k', 3, 2),
                    Node (('c', 2, 3),
                          Node (('a', 1, 4), Empty, Empty),
                          Node (('e', 3, 4),
                                Node (('d', 2, 5), Empty, Empty),
                                Node (('g', 4, 5), Empty, Empty))),
                    Node (('m', 4, 3), Empty, Empty)),
              Node (('u', 7, 2),
                    Node (('p', 6, 3), Empty,
                          Node (('q', 7, 4), Empty, Empty)), Empty)))
let example3_layout_tree =
  Node('a',
       Node('b', Empty,
            Node('e', Empty, Node('f', Empty, Empty))),
       Node('c', Empty, Node('d', Node('g', Empty, Empty), Empty)));;
assert (layout_binary_tree_3 example3_layout_tree =
        Node (('a', 3, 1),
              Node (('b', 1, 2), Empty,
                    Node (('e', 2, 3), Empty, Node (('f', 3, 4), Empty, Empty))),
              Node (('c', 5, 2), Empty,
                    Node (('d', 6, 3),
                          Node (('g', 5, 4), Empty, Empty), Empty))))

let rec string_of_tree = function
  | Empty -> ""
  | Node (e, Empty, Empty) -> Char.escaped e
  | Node (e, l, r) -> Char.escaped e ^ "(" ^ string_of_tree l ^ "," ^
                      string_of_tree r ^ ")"

let tree_of_string s =
  let rec _tree_of_string s =
    let length = String.length s in
    if length = 0 then Empty, "" else
    if s.[0] = ',' || s.[0] = ')' then Empty, s else
    if length = 1 || s.[1] <> '(' then
      Node (s.[0], Empty, Empty), String.sub s 1 (length - 1)
    else
      let element = s.[0] in
      let left_side, s = _tree_of_string (String.sub s 2 (length - 2)) in
      let right_side, s = _tree_of_string (String.sub s 1
                                             (String.length s - 1)) in
      Node (element, left_side, right_side), String.sub s 1 (String.length s - 1)
  in
  fst (_tree_of_string s);;

let example_layout_tree =
  let leaf x = Node (x, Empty, Empty) in
  Node('a', Node('b', leaf 'd', leaf 'e'),
       Node('c', Empty, Node('f', leaf 'g', Empty)));;
assert (string_of_tree example_layout_tree = "a(b(d,e),c(,f(g,)))");;
assert (tree_of_string "a(b(d,e),c(,f(g,)))" = example_layout_tree);;
assert (tree_of_string "" = Empty)

let rec preorder = function
  | Empty -> []
  | Node (e, l, r) -> e :: preorder l @ preorder r

let rec inorder = function
  | Empty -> []
  | Node (e, l, r) -> (inorder l) @ [e] @ (inorder r)

let pre_in_tree p i =
  let rec split member = function
    | [] -> [], []
    | h :: t ->
      if h = member then [], t
      else
        let s1, s2 = split member t in
        h :: s1, s2
  in
  let rec _pre_in_tree p i =
    if List.length i = 0 then Empty, p else
    if List.length i = 1 then Node (List.hd i, Empty, Empty), List.tl p
    else
      let current_node = List.hd p in
      let left_i, right_i = split current_node i in
      let left_subtree, p = _pre_in_tree (List.tl p) left_i in
      let right_subtree, p = _pre_in_tree p right_i in
      Node (current_node, left_subtree, right_subtree), p
  in
  fst (_pre_in_tree p i);;

assert (preorder (Node (1, Node (2, Empty, Empty), Empty)) = [1; 2]);;
assert (preorder (Node (1, Empty, Node (2, Empty, Empty))) = [1; 2])
let p = preorder example_tree
let i = inorder example_tree;;
assert (pre_in_tree p i = example_tree)

type 'a mult_tree = T of 'a * 'a mult_tree list

let rec count_nodes = function
  | T (_, []) -> 1
  | T(_, l) -> 1 + List.fold_left (+) 0 (List.map count_nodes l);;

assert (count_nodes (T('a', [T('f',[]) ])) = 2)

let rec string_of_tree (T (e, l)) =
  Char.escaped e ^ String.concat "" (List.map string_of_tree l) ^ "^"

let tree_of_string s =
  let remove_first_char s =
    String.sub s 1 (String.length s - 1)
  in
  let rec _tree_of_string s =
    let element = s.[0] in
    let s = ref (remove_first_char s) in
    let list = ref [] in
    while !s.[0] <> '^' do
      let new_child_node, new_s = _tree_of_string !s in
      s := new_s;
      list := new_child_node :: !list
    done;
    T (element, List.rev !list), remove_first_char !s
  in
  fst (_tree_of_string s)

let t = T('a', [T('f',[T('g',[])]); T('c',[]); T('b',[T('d',[]); T('e',[])])]);;
assert (string_of_tree t = "afg^^c^bd^e^^^");;
assert (tree_of_string "afg^^c^bd^e^^^" =
        T ('a', [T ('f', [T ('g', [])]); T ('c', []);
                 T ('b', [T ('d', []); T ('e', [])])]))

let ipl tree =
  let rec _ipl depth (T (_, l)) =
    List.fold_left (fun acc e -> acc + _ipl (depth + 1) e) depth l
  in
  _ipl 0 tree;;

assert (ipl t = 9)

let rec bottom_up (T (e, l)) =
  (List.fold_left (fun acc e -> acc @ bottom_up e) [] l) @ [e];;

assert (bottom_up (T('a', [T('b', [])])) = ['b'; 'a']);;
assert (bottom_up t = ['g'; 'f'; 'c'; 'd'; 'e'; 'b'; 'a'])

let rec lispy (T (e, l)) =
  let e = Char.escaped e in
  if l = [] then e
  else "(" ^ e ^ List.fold_left (fun acc e -> acc ^ " " ^ lispy e) "" l ^ ")";;

assert (lispy (T('a', [])) = "a");;
assert (lispy (T('a', [T('b', [])])) = "(a b)");;
assert (lispy t = "(a (f g) c (b d e))")

type 'a graph_term = { nodes : 'a list; edges : ('a * 'a) list }
let example_graph =
  { nodes = ['b'; 'c'; 'd'; 'f'; 'g'; 'h'; 'k'];
    edges = ['h', 'g';  'k', 'f';  'f', 'b';  'f', 'c';  'c', 'b'] }

let paths g start destination =
  let rec path_extensions path edges =
    let edge_fits (a, b) =
      a = List.hd path && not (List.mem b path)
    in
    match edges with
    | [] -> []
    | (a, b) :: t ->
      if edge_fits (a, b) then work_on_edge (a, b) path t else
      if edge_fits (b, a) then work_on_edge (b, a) path t
      else path_extensions path t
  and work_on_edge (a, b) path remaining_edges =
    let new_path = b :: path in
    let extensions =
      if b = destination then [new_path]
      else path_extensions new_path g.edges in
    extensions @ path_extensions path remaining_edges
  in
  List.map List.rev (path_extensions [start] g.edges)

(*assert (paths example_graph 'f' 'b' = [['f'; 'c'; 'b']; ['f'; 'b']]);;*)

let cycles g a =
  let rec find_neighbours = function
    | [] -> []
    | (x, y) :: t ->
      if x = a then y :: find_neighbours t else
      if y = a then x :: find_neighbours t
      else find_neighbours t
  in
  List.map (fun path -> a :: path)
    (List.fold_left (fun acc neighbour -> acc @ paths g neighbour a) []
       (find_neighbours g.edges))

(*assert (cycles example_graph 'f' =
        [['f'; 'b'; 'c'; 'f']; ['f'; 'c'; 'f']; ['f'; 'c'; 'b'; 'f'];
         ['f'; 'b'; 'f']; ['f'; 'k'; 'f']])*)

module type GRAPH = sig
  type node = char
  type t
  val of_adjacency : (node * node list) list -> t
  val dfs_fold : t -> node -> ('a -> node -> 'a) -> 'a -> 'a
end

module M : GRAPH = struct
  type node = char
  type t = (node * node list) list
  let of_adjacency list = list
  let dfs_fold graph node f acc =
    let rec _dfs_fold node acc visited =
      if List.mem node visited then acc, visited
      else
        let acc = f acc node
        and visited = node :: visited in
        match List.assoc_opt node graph with
        | None -> acc, visited
        | Some neighbours ->
          List.fold_left (fun (a, v) n -> _dfs_fold n a v)
            (acc, visited) neighbours
    in
    fst (_dfs_fold node acc [])
end

let g = M.of_adjacency
    ['u', ['v'; 'x'];
     'v',      ['y'];
     'w', ['z'; 'y'];
     'x',      ['v'];
     'y',      ['x'];
     'z',      ['z'];
    ];;
assert (List.rev (M.dfs_fold g 'w' (fun acc c -> c :: acc) []) =
        ['w'; 'z'; 'y'; 'x'; 'v'])

let queens_positions n =
  let rec available_rows set_rows row_to_test =
    if row_to_test > n then [] else
    if List.mem row_to_test set_rows then
      available_rows set_rows (row_to_test + 1)
    else row_to_test :: available_rows set_rows (row_to_test + 1)
  in
  let rec clear_diagonal column next_column_fn = function
    | [] -> true
    | current_position :: other_positions ->
      column < 1 || column > n ||
      (current_position <> column && clear_diagonal (next_column_fn column)
         next_column_fn other_positions)
  in
  let rec good_diagonals = function
    | [] -> true
    | queen :: remaining_queens ->
      clear_diagonal (queen + 1) (fun x -> x + 1) remaining_queens &&
      clear_diagonal (queen - 1) (fun x -> x - 1) remaining_queens &&
      good_diagonals remaining_queens
  in
  let rec _queens_positions set available =
    if List.length set = n then
      if good_diagonals set then [set] else []
    else
      match available with
      | [] -> []
      | h :: t ->
        let new_set = (List.hd available) :: set in
        (_queens_positions new_set (available_rows new_set 1)) @
        (_queens_positions set t)
  in
  _queens_positions [] (available_rows [] 1);;

assert (queens_positions 4 = [[3; 1; 4; 2]; [2; 4; 1; 3]]);;
assert (List.length (queens_positions 8) = 92)

let rec full_words n =
  let word_of_digit = function
    | 1 -> "one"
    | 2 -> "two"
    | 3 -> "three"
    | 4 -> "four"
    | 5 -> "five"
    | 6 -> "six"
    | 7 -> "seven"
    | 8 -> "eight"
    | 9 -> "nine"
    | _ -> "zero"
  in
  if n < 10 then word_of_digit n
  else full_words (n / 10) ^ "-" ^ word_of_digit (n mod 10);;

assert (full_words 175 = "one-seven-five");;
assert (full_words 23485 = "two-three-four-eight-five");;
assert (full_words 0 = "zero")

exception NotAnIdentifier

let identifier s =
  let consume_char_between s first last =
    if s = "" then raise NotAnIdentifier
    else
      let code = Char.code s.[0] in
      let correct = code >= Char.code first && code <= Char.code last in
      if correct then String.sub s 1 (String.length s - 1)
      else raise NotAnIdentifier
  in
  let consume_letter s =
    consume_char_between s 'a' 'z'
  in
  let consume_hyphen s =
    if s = "" then s else
    if s.[0] = '-' then String.sub s 1 (String.length s - 1)
    else s
  in
  let consume_letter_or_digit s =
    try consume_letter s
    with NotAnIdentifier -> consume_char_between s '0' '9'
  in
  let rec _identifier s =
    if s = "" then true
    else
      let s = consume_hyphen s in
      let s = consume_letter_or_digit s in
      _identifier s
  in
  try
    let s = consume_letter s in
    _identifier s
  with NotAnIdentifier -> false;;

assert (identifier "this-is-a-long-identifier");;
assert (not (identifier "this-ends-in-"));;
assert (not (identifier "two--hyphens"));;
assert (not (identifier "-dash-first"))

module Entries = Set.Make(
  struct
    let compare = Pervasives.compare
    type t = int
  end)

module Board = struct
  let of_list l = l
  let print board =
    List.iteri (fun i row -> List.iteri
                   (fun j n ->
                      if j = 2 || j = 5 then Printf.printf "%d | " n
                      else Printf.printf "%d  " n) row;
                 if i = 2 || i = 5 then
                   print_endline "\n--------+---------+--------"
                 else if i < 8 then print_endline "\n        |         |        "
                 else print_newline ()
               ) board
end

let sudoku board =
  let square board i j =
    let i = i / 3 * 3
    and j = j / 3 * 3 in
    List.concat (List.map (fun row -> [List.nth row j; List.nth row (j + 1);
                                       List.nth row (j + 2)])
                   [List.nth board i; List.nth board (i + 1);
                    List.nth board (i + 2)])
  in
  let rec possible_entries board i j n =
    if n > 9 then [] else
    if List.mem n (List.nth board i) ||
       List.mem n (List.map (fun row -> List.nth row j) board) ||
       List.mem n (square board i j) then
      possible_entries board i j (n + 1)
    else n :: possible_entries board i j (n + 1)
  in
  let rec new_row row j n =
    if j > 0 then List.hd row :: new_row (List.tl row) (j - 1) n
    else n :: List.tl row
  in
  let rec new_board board i j n =
    if i > 0 then List.hd board :: new_board (List.tl board) (i - 1) j n
    else new_row (List.hd board) j n :: List.tl board
  in
  let rec choose_cell board (min_size, min_i, min_j) i j =
    if i > 8 then min_size, min_i, min_j else
    if j > 8 then choose_cell board (min_size, min_i, min_j) (i + 1) 0 else
    if List.nth (List.nth board i) j <> 0 then
      choose_cell board (min_size, min_i, min_j) i (j + 1)
    else
      let domain_size = List.length (possible_entries board i j 1) in
      let size, min_i, min_j = if domain_size < min_size then domain_size, i, j
        else min_size, min_i, min_j in
      choose_cell board (size, min_i, min_j) i (j + 1)
  in
  let rec _sudoku board =
    let domain_size, i, j = choose_cell board (9, 0, 0) 0 0 in
    if domain_size = 9 then not (List.mem 0 (List.flatten board)), board else
    if domain_size = 0 then false, board
    else explore_domain board i j (possible_entries board i j 1)
  and explore_domain board i j = function
    | [] -> false, board
    | n :: rest_of_domain ->
      let successful, found_board = _sudoku (new_board board i j n) in
      if successful then successful, found_board
      else explore_domain board i j rest_of_domain
  in
  snd (_sudoku board);;

let initial_board =
  Board.of_list [[0; 0; 4;  8; 0; 0;  0; 1; 7];
                 [6; 7; 0;  9; 0; 0;  0; 0; 0];
                 [5; 0; 8;  0; 3; 0;  0; 0; 4];
                 [3; 0; 0;  7; 4; 0;  1; 0; 0];
                 [0; 6; 9;  0; 0; 0;  7; 8; 0];
                 [0; 0; 1;  0; 6; 9;  0; 0; 5];
                 [1; 0; 0;  0; 8; 0;  3; 0; 6];
                 [0; 0; 0;  0; 0; 6;  0; 9; 1];
                 [2; 4; 0;  0; 0; 1;  5; 0; 0]];;
(*Board.print (sudoku initial_board)*)

(*let solve rows columns =
  let board = Array.make_matrix (List.length columns) (List.length rows) '_' in
  let print_board board =  Array.iter (fun row -> Array.iter
                                          (fun cell ->
                                             Printf.printf "|%c" cell) row;
                                        print_endline "|") board
  in
  let numbers_from_xs row =
    let rec _numbers_from_xs row i =
      if i >= Array.length row then [], false
      else
        let numbers, ended_with_x = _numbers_from_xs row (i + 1) in
        if row.(i) = 'X' then
          if ended_with_x then 1 + List.hd numbers :: List.tl numbers, true
          else 1 :: numbers, true
        else
          numbers, false
    in
    fst (_numbers_from_xs row 0)
  in
  let rec compatible partial full =
    List.fold_left (+) 0 partial < List.fold_left (+) 0 full
  in
  let select_column index =
    numbers_from_xs (Array.map (fun row -> row.(index)) board)
  in
  let rec find_compatible_columns row_index column_index =
    Printf.printf "find_compatible_columns %d %d\n" row_index column_index;
    if column_index >= List.length columns then false else
    if board.(row_index).(column_index) = 'X' then
      find_compatible_columns row_index (column_index + 1)
    else if compatible (select_column column_index)
        (List.nth columns column_index) then
      begin
        board.(row_index).(column_index) <- 'X';
        let column = select_column column_index
        and column_numbers = List.nth columns column_index
        and row = numbers_from_xs board.(row_index)
        and row_numbers = List.nth rows row_index in (
          List.iter (fun e -> Printf.printf "%d " e) row;
          print_newline ();
          List.iter (fun e -> Printf.printf "%d " e) row_numbers;
        if (column = column_numbers || compatible column column_numbers) &&
           (row = row_numbers || compatible row row_numbers) then
          if find_row_to_fill 0 then true
          else
            begin
              board.(row_index).(column_index) <- '_';
              find_compatible_columns row_index (column_index + 1)
            end
        else
          begin
            board.(row_index).(column_index) <- '_';
            find_compatible_columns row_index (column_index + 1)
          end
      )
      end
    else find_compatible_columns row_index (column_index + 1)
  and find_row_to_fill row_index =
    Printf.printf "find_row_to_fill %d\n" row_index;
    if row_index >= Array.length board then true
    else
      let numbers = numbers_from_xs board.(row_index)
      and counts = List.nth rows row_index in (
        print_board board;
      if numbers = counts then
        find_row_to_fill (row_index + 1)
      else if compatible numbers counts then
        find_compatible_columns row_index 0
      else false)
  in
  let status = find_row_to_fill 0 in (
    Printf.printf "%b\n" status;
    print_board board);;

solve [[3];[2;1];[3;2];[2;2];[6];[1;5];[6];[1];[2]]
  [[1;2];[3;1];[1;5];[7;1];[5];[3];[4];[3]]*)
