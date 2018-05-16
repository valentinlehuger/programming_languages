(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* put your solutions for problem 1 here *)

fun all_except_option(s: string, lst: string list) =
    let 
        fun is_in(l: string list) = case l of
            [] => false
            | x::xs => if same_string(x, s) then true else is_in(xs)
        fun helper(l: string list) =  case l of
            x::xs => if same_string(x, s) then xs else x::helper(xs)
    in
        if is_in(lst) then SOME (helper(lst)) else NONE
    end


fun get_substitutions1(subs: string list list, s: string) = case subs of
    [] => []
    | x::xs => case all_except_option(s, x) of
        NONE => get_substitutions1(xs, s)
        | SOME valid => valid @ get_substitutions1(xs, s)


fun get_substitutions2(subs: string list list, s: string) = let
        fun helper(subs : string list list, acc: string list) = case subs of
            [] => acc
            | x::xs => case all_except_option(s, x) of
                NONE => helper(xs, acc)
                | SOME valid => helper(xs, acc @ valid)
    in
        helper(subs, [])
    end

fun similar_names(subs: string list list,
                  full_name: {first:string, last:string, middle:string}) = 
    let fun helper(lst: string list, acc: {first:string, last:string, middle:string} list) =
        case (lst, full_name) of
            ([], _) => acc
            | (x::xs, {first=_, middle=m, last=l}) => helper(xs, acc @ [{first=x, middle=m, last=l}])
    in
        case full_name of 
            {first=f, middle=_, last=_} => helper(get_substitutions2(subs, f), [full_name])
    end

(* you may assume that Num is always used with values 2, 3, ..., 10
   though it will not really come up *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int 
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw 

exception IllegalMove

(* put your solutions for problem 2 here *)

fun card_color(c: card) = case c of
    (Clubs, _) => Black
    | (Spades, _) => Black
    | _ => Red

fun card_value(c: card) = case c of
    (_, Num x) => x
    | (_, Ace) => 11
    | _ => 10

fun remove_card(cs: card list, c: card, e: exn) = case cs of
    [] => raise e
    | x::xs => if x = c then xs else x::remove_card(xs, c, e)

fun all_same_color(cs: card list) = case cs of
    [] => true
    | x::[] => true
    | x::y::xs => if card_color(x) <> card_color(y) then false else all_same_color(y::xs)

fun sum_cards(cs: card list) =
    let
        fun helper(cs: card list, acc: int) = case cs of
            [] => acc
            | x::xs => helper(xs, acc + card_value(x))
    in
        helper(cs, 0)
    end

fun score(cs: card list, goal: int) = 
    let val sum = sum_cards(cs)
        val prelimary = case sum > goal of
            true => 3 * (sum - goal)
            | _ => goal - sum
    in
        case all_same_color(cs) of
            true => prelimary div 2
            | _ => prelimary
    end

fun officiate(cards: card list, moves: move list, goal: int) =
    let
        fun helper(moves: move list, cards: card list, hand: card list) =
            case (moves, cards) of
                ([], _) => score(hand, goal)
                | ((Discard c)::ms, cs) => helper(ms, cs, remove_card(hand, c, IllegalMove))
                | (Draw::ms, []) => score(hand, goal)
                | (Draw::ms, c::cs) =>
                    case sum_cards(c::hand) > goal of
                        true => score(c::hand, goal)
                        | false => helper(ms, cs, c::hand)
    in
        helper(moves, cards, [])
    end
