module Cnf = struct

    type cnf = List of (cnf * int) * cnf | Atom of int | Nil ;;

    let atom a = match a with
    | (Atom _) -> true
    | _ -> false ;;
         
    let first l = match l with
    | Nil -> (Nil, 0)
    | (Atom a) -> ((Atom a), 1)
    | (List (head, _)) -> head ;;

    let rest l = match l with
    | Nil -> Nil
    | (Atom _) -> Nil
    | (List (_, tail)) -> tail ;;

    let rec firstn a n = match a, n with
    | a, 0 -> Nil
    | (Atom _), n -> a
    | (List ((x, y), xs)), n -> List ((x, y), (firstn xs (n - 1))) ;;

    let rec restn a n = match a, n with
    | a, 0 -> a
    | (Atom _), n -> Nil
    | (List ((x, y), xs)), n -> restn xs (n - 1) ;;

    let fst (a, b) = a ;;

    let snd (a, b) = b ;;

    let fe a = match a with
    | (Atom _) -> Atom 0
    | _ -> fst (first a) ;;

    let fc a = match a with
    | (Atom x) -> x
    | _ -> snd (first a) ;;

    let rec len x = match x with
    | Nil -> 0
    | (Atom _) -> 0
    | _ -> (len (rest x)) + 1 ;;

    let rec size x = match x with
    | Nil -> 0
    | (Atom _) -> 1
    | _ -> (size (fe x)) + (size (rest x)) ;;

    let rec append a b = match a, b with
    | Nil, b -> b
    | a, Nil -> a
    | (Atom a), b -> b
    | a, b -> List ((first a), (append (rest a) b)) ;;

    let rec cmp a b = match a, b with
    | Nil, Nil -> 0
    | Nil, b -> -1
    | a, Nil -> 1
    | a, (List ((_, 0), b_s)) -> cmp a b_s
    | List ((_, 0), a_s), b -> cmp a_s b
    | (Atom a), (Atom b) -> compare a b
    | (Atom a), _ -> -1
    | _, (Atom b) -> 1
    | List ((a1, a2), a_s), List ((b1, b2), b_s) when cmp a1 b1 != 0 -> cmp a1 b1 
    | List ((a1, a2), a_s), List ((b1, b2), b_s) when a2 != b2 -> compare a2 b2 
    | List ((a1, a2), a_s), List ((b1, b2), b_s) -> cmp a_s b_s ;;

    let rec add a b = match a, b with
    | Nil, b -> b
    | a, Nil -> a
    | (Atom a), (Atom b) -> Atom (a + b)
    | a, b when cmp (fe a) (fe b) = -1 -> b 
    | a, b when cmp (fe a) (fe b) = 0 -> List ((fe a, (fc a) + (fc b)), (rest b)) 
    | a, b -> List ((fe a, fc a), (add (rest a) b)) ;;

    let rec sub a b = match a, b with
    | (Atom a), (Atom b) when a < b -> Atom 0 
    | (Atom a), (Atom b) -> Atom (a - b)
    | a, b when cmp (fe a) (fe b) = -1 -> Atom 0 
    | a, b when cmp (fe a) (fe b) = 1 -> a 
    | a, b when (fc a) < (fc b) -> Atom 0 
    | a, b when (fc a) > (fc b) -> List ((fe a, (fc a) - (fc b)), (rest a))
    | a, b -> sub (rest a) (rest b) ;;
                      
    let rec c a b = match a, b with
      | a, b when cmp (fe b) (fe a) = -1 -> 1 + (c (rest a) b) 
      | a, b -> 0 ;;

    let count a b n = n + (c (restn a n) b) ;;

    let padd a b n = append (firstn a n) (add (restn a n) b) ;;

    let rec pmult a b n = match a, b, n with
    | Nil, Nil, n -> Atom 0
    | (Atom a), (Atom b), n -> Atom (a * b)
    | a, (Atom b), n -> List (((fe a), (fc a) * b), (rest a))
    | a, b, n -> let m = count (fe a) (fe b) n in
        List (((padd (fe a) (fe b) m), (fc b)), (pmult a (rest b) m)) ;;

    let mul a b = pmult a b 0 ;;

    let rest' (List (_, (Atom n))) = n ;;

    let rec pow a = function
      | 0 -> 1
      | 1 -> a
      | n -> 
        let b = pow a (n / 2) in
        b * b * (if n mod 2 = 0 then 1 else a)

    let rec exp1 p b = match p, b with
    | p, b when cmp (fe b) (Atom 1) = 0 -> List (((Atom (fc b), pow p (rest' b))), (Atom 0)) 
    | p, b when atom (rest b) -> let e = List ((sub (fe b) (Atom 1), fc b), (Atom 0)) in
        List ((e, pow p (rest' b)), (Atom 0))
    | p, b -> let c = exp1 p (rest b) in
        List ((List ((sub (fe b) (Atom 1), 1), (fe c)), fc c), (Atom 0)) ;;

    let exp2 a q = match a, q with
    | a, 1 -> a
    | a, q -> mul (List (((mul (fe a) (Atom (q - 1))), 1), (Atom 0))) a ;;

    let rec limitp a = match a with
    | (Atom a) -> a = 0
    | a -> limitp (rest a) ;;

    let rec limitpart a = match a with
    | (Atom a) -> Atom 0
    | a -> List ((first a), (limitpart(rest a))) ;;

    let rec natpart a = match a with
    | (Atom a) -> a
    | a -> natpart(rest a) ;;

    let rec helper a p n q = match q with
    | 0 -> Atom p
    | _ -> padd (mul (exp2 a q) (Atom p)) (helper a p n (q - 1)) n ;;

    let exp3 a q = match q with
    | 0 -> Atom 1
    | 1 -> a
    | _ when limitp a -> exp2 a q 
    | _ -> let c = limitpart a in let n = len a in
        padd (exp2 c q) (helper c (natpart a) n (q - 1)) n ;;
                             
    let exp4 a b = mul (List ((mul (fe a) (limitpart b), 1), (Atom 0))) (exp3 a (natpart b)) ;;

    let exp a b = match a, b with
    | (Atom 1), _ -> Atom 1
    | _, (Atom 0) -> Atom 1
    | (Atom 0), _ -> Atom 0
    | (Atom a), (Atom b) -> Atom (pow a b)
    | (Atom a), b -> exp1 a b
    | a, (Atom b) -> exp3 a b
    | a, b -> exp4 a b ;;

end;;