(* Coursera Programming Languages, Homework 3, Provided Code *)

exception NoAnswer

datatype pattern = Wildcard
		 | Variable of string
		 | UnitP
		 | ConstP of int
		 | TupleP of pattern list
		 | ConstructorP of string * pattern

datatype valu = Const of int
	      | Unit
	      | Tuple of valu list
	      | Constructor of string * valu

fun g f1 f2 p =
    let 
	val r = g f1 f2 
    in
	case p of
	    Wildcard          => f1 ()
	  | Variable x        => f2 x
	  | TupleP ps         => List.foldl (fn (p,i) => (r p) + i) 0 ps
	  | ConstructorP(_,p) => r p
	  | _                 => 0
    end

(**** for the challenge problem only ****)

datatype typ = Anything
	     | UnitT
	     | IntT
	     | TupleT of typ list
	     | Datatype of string

(**** you can put all your code here ****)

fun only_capitals(l: string list) = List.filter(fn x => Char.isUpper(String.sub(x, 0)))(l)

fun longest_string1(l: string list) =
    List.foldl (fn (x, acc) => if String.size(x) > String.size(acc) then x else acc) "" l

fun longest_string2(l: string list) =
    List.foldl (fn (x, acc) => if String.size(x) >= String.size(acc) then x else acc) "" l

fun longest_string_helper f l =
    List.foldl (fn (x, acc) => if f(String.size(x), String.size(acc)) then x else acc) "" l

fun longest_string3(l: string list) = longest_string_helper (fn (sx, sacc) => sx > sacc) l

fun longest_string4(l: string list) = longest_string_helper (fn (sx, sacc) => sx >= sacc) l

val longest_capitalized = longest_string1 o only_capitals

val rev_string = String.implode o List.rev o String.explode

fun first_answer f l = case l of
    [] => raise NoAnswer
    | x::xs => case f x of
        SOME v => v
        | NONE => first_answer f xs

fun all_answers f l =
    let 
        fun helper(l, acc) = case l of
            [] => SOME acc
            | x::xs => case f x of
                SOME v => helper(xs, acc @ v)
                | NONE => NONE
    in
        helper(l, [])
    end 

val count_wildcards = g (fn () => 1) (fn s => 0)

val count_wild_and_variable_lengths = g (fn () => 1) (fn s => String.size(s))

fun count_some_var (v, p) = g (fn () => 0) (fn s => if s = v then 1 else 0) p

fun check_pat(p: pattern) =
    let
        fun all_strings(p: pattern) = case p of
            Variable(x) => [x]
            | TupleP(l) => foldl (fn (x, acc) => acc @ all_strings(x)) [] l
            | ConstructorP(s, p) =>  all_strings(p)
            | _ => []
        
        fun has_repeats(l: string list) = case l of
            [] => false
            | x::xs => if List.exists (fn v => if x = v then true else false) xs  then true else has_repeats(xs)
    in
         not ((has_repeats o all_strings) p)
    end

fun match(v: valu, p: pattern) =
    let
        fun helper(v, p) = case (p, v) of
            (Wildcard, _) => []
            | (UnitP, Unit) => []
            | (ConstP x, Const y) => if x = y then [] else raise NoAnswer
            | (Variable x, y) => [(x, y)]
            | (ConstructorP(s1, p), Constructor(s2, v)) => if s1 = s2 then helper(v, p) else raise NoAnswer
            | (TupleP(ps), Tuple(vs)) => 
                if List.length(ps) <> List.length(vs) then raise NoAnswer
                else List.foldl (fn(x, acc) => acc @ helper x) [] (ListPair.zip(vs, ps))
            | _ => raise NoAnswer
    in 
        ((SOME (helper(v, p))) handle NoAnswer => NONE)
    end

fun first_match v ps = case ps of
    [] => NONE
    | p::ps => case match(v,p) of
        NONE => (first_match v ps)
        | x => x
