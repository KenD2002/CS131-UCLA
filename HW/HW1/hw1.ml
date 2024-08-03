(** Question 1 *)

(** is_in is an auxiliary function checking if an item is contained in a list
    input:  elem:'a
            lst:'a list
    output: bool
    type: val is_in : 'a -> 'a list -> bool = <fun> *)
let rec is_in = fun elem lst ->
    match lst with
    | [] -> false
    | h::t -> (elem = h) || (is_in elem t)

(** subset is a function checking if list a is a subset of list b
    input:  a:'a list
            b:'a list
    output: bool
   type: val subset : 'a list -> 'a list -> bool = <fun> *)
let rec subset = fun a b ->
    match a with
    | [] -> true
    | h::t -> (is_in h b) && (subset t b)


(** Question 2 *)

(** equal_sets is a function checking if list a and list b are equal
    input:  a:'a list 
            b:'a list
    output: bool
    val equal_sets : 'a list -> 'a list -> bool = <fun> *)
let rec equal_sets = fun a b ->
    (subset a b) && (subset b a)


(** Question 3 *)

(** set_union is a function returning a list representing the union of list a and list b
    input:  a:'a list ; b:'a list
    output: 'a list
    val set_union : 'a list -> 'a list -> 'a list = <fun> *)
let rec set_union = fun a b ->
    match a with
    | [] -> b
    | h::t -> set_union t (h::b)


(** Question 4 *)

(** set_all_union is a function returning a list representing the union of all the members of the set a
    input:  a:'a list list
    output: 'a list
    val set_all_union : 'a list list -> 'a list = <fun> *)
let rec set_all_union = fun a ->
    match a with 
    | [] -> []
    | h::t -> List.append h (set_all_union t)


(** Question 5 *)

(** It is not possible to implement such function self_member s in OCaml. Say there is an element e in s, which we
    want to test if e is exactly s. Then e is of some type 'a list, so s must be of some type 'a list list. Then if
    e is s, then s must be of both the type 'a list list and 'a list, which is impossible. *)


(** Question 6 *)

(** computed_fixed_point returns the computed fixed point for f with respect to x, assuming that eq is the equality
    predicate for f's domain.
    input:  eq: 'a -> 'a -> bool = <fun>
            f: 'a -> 'a = <fun> 
            x: 'a 
    output: 'a 
    val computed_fixed_point : ('a -> 'a -> bool) -> ('a -> 'a) -> 'a -> 'a = <fun> *)
let rec computed_fixed_point = fun eq f x ->
    if (eq x (f x)) 
        then x 
        else computed_fixed_point eq f (f x)


(** Question 7 *)

(** apply_n_times is an auxiliary function returning the result of x after applying n times of function f
    input:  f: 'a -> 'a = <fun>
            n: int
            x: 'a
    output: 'a
    val apply_n_times : ('a -> 'a) -> int -> 'a -> 'a = <fun> *)
let rec apply_n_times = fun f n x ->
    if (n > 0) 
        then apply_n_times f (n - 1) (f x)
        else x

(** computed_periodic_point returns the computed periodic point for f with period p and with respect to x, assuming
    that eq is the equality predicate for f's domain.
    input:  eq: 'a -> 'a -> bool = <fun> 
            f: 'a -> 'a = <fun>
            p: int
            x: 'a 
    output: 'a 
    val computed_periodic_point : ('a -> 'a -> bool) -> ('a -> 'a) -> int -> 'a -> 'a = <fun> *)
let rec computed_periodic_point = fun eq f p x ->
    if eq x (apply_n_times f p x) 
        then x
        else computed_periodic_point eq f p (f x)


(** Question 8 *)

(** whileseq returns the longest list \[x; s x; s (s x); ...] such that p e is true for every 
    element e in the list. 
    input:  s: 'a -> 'a = <fun> 
            p: 'a -> bool = <fun>
            x: 'a 
    output: 'b list 
    val whileseq : ('a -> 'a) -> ('a -> bool) -> 'a -> 'a list = <fun> *)
let rec whileseq = fun s p x ->
    if (p x) 
        then (x::(whileseq s p (s x)))
        else []


(** Question 9 *)

(** A symbol used in a grammar. It can be either a nonterminal symbol or a terminal symbol; each
    kind of symbol has a value, whose type is arbitrary. A symbol has the following OCaml type *)
type ('nonterminal, 'terminal) symbol =
    | N of 'nonterminal
    | T of 'terminal

(** is_symbol_terminable is an auxiliary function returning whether a symbol is terminable or not 
    input:  symbol: ('a, 'b) symbol 
            terminable_symbol_list: ('a, 'b) symbol list
    output: bool
    val is_symbol_terminable : ('a, 'b) symbol  -> ('a, 'b) symbol list -> bool = <fun> *)
let is_symbol_terminable = fun symbol terminable_symbol_list ->
    match symbol with
    | T terminal -> true
    | N nonterminal -> is_in symbol terminable_symbol_list

(** is_rhs_all_terminable is an auxiliary function returning whether the rhs (of a rule) only contains terminable 
    symbols (from the terminable_symbol_list) 
    input:  rhs: ('a, 'b) symbol list
            terminable_symbol_list: ('a, 'b) symbol list
    output: bool
    val is_rhs_all_terminable : ('a, 'b) symbol list -> ('a, 'b) symbol list -> bool = <fun> *)
let rec is_rhs_all_terminable = fun rhs terminable_symbol_list ->
    match rhs with
    | [] -> true
    | h::t -> if (is_symbol_terminable h terminable_symbol_list) 
                then (is_rhs_all_terminable t terminable_symbol_list)
                else false

(** construct_terminable_symbol_list is an auxiliary function returning a ('a, 'b) symbol list that contains all 
    terminable symbols in the rules (after one iteration)
    input:  rules: (('a, 'b) symbol * ('a, 'b) symbol list) list
            terminable_symbol_list: ('a, 'b) symbol list
    output: ('a, 'b) symbol list
    val construct_terminable_symbol_list : (('a, 'b) symbol * ('a, 'b) symbol list) list -> ('a, 'b) symbol list -> ('a, 'b) symbol list = <fun> 
    However, since the pattern match for rules has no restrict type requirement for its first element, symbol, so the actual type of the function is
    val construct_terminable_symbol_list : ('a * ('a, 'b) symbol list) list -> ('a, 'b) symbol list -> ('a, 'b) symbol list = <fun> *)
let rec construct_terminable_symbol_list = fun rules terminable_symbol_list ->
    match rules with
    | [] -> terminable_symbol_list
    | (symbol, rhs)::t -> if (is_rhs_all_terminable rhs terminable_symbol_list) 
                            then (construct_terminable_symbol_list t ((N symbol)::terminable_symbol_list))
                            else (construct_terminable_symbol_list t terminable_symbol_list)

(** rules_with_terminable_symbol_list is an auxiliary function returning a ((('a, 'b) symbol, ('a, 'b) symbol list) list, ('a, 'b) symbol list) 
    this function is actually a wrapper that returns a tuple that the first element is the rules in a grammar and the second element is the 
    list containing all terminable symbols. This is treated as the f used for computed_fixed_point.
    input:  (rules, terminable_symbol_list): ((('a, 'b) symbol * ('a, 'b) symbol list) list * ('a, 'b) symbol list)
    output: (rules, terminable_symbol_list): ((('a, 'b) symbol * ('a, 'b) symbol list) list * ('a, 'b) symbol list)
    val rules_with_terminable_symbol_list : ((('a, 'b) symbol * ('a, 'b) symbol list) list * ('a, 'b) symbol list) -> ((('a, 'b) symbol * ('a, 'b) symbol list) list * ('a, 'b) symbol list) = <fun> 
    However, actual type:
    val rules_with_terminable_symbol_list : ('a * ('a, 'b) symbol list) list * ('a, 'b) symbol list -> ('a * ('a, 'b) symbol list) list * ('a, 'b) symbol list = <fun> *)
let rules_with_terminable_symbol_list = fun (rules, terminable_symbol_list) ->
    (rules, construct_terminable_symbol_list rules terminable_symbol_list)

(** equal_terminable_symbol_list is an auxiliary function returning a bool indicating whether two results from 
    rules_with_terminable_symbol_list are the same thing. It is treated as the eq used for computed_fixed_point.
    input:  (rules1, terminable_symbol_list1): ((('a, 'b) symbol * ('a, 'b) symbol list) list * ('a, 'b) symbol list)
            (rules2, terminable_symbol_list2): ((('a, 'b) symbol * ('a, 'b) symbol list) list * ('a, 'b) symbol list)
    output: bool
    val equal_terminable_symbol_list : ((('a, 'b) symbol * ('a, 'b) symbol list) list * ('a, 'b) symbol list) -> ((('a, 'b) symbol * ('a, 'b) symbol list) list * ('a, 'b) symbol list) -> bool = <fun>
    However, since equal_sets is only applied to terminable_symbol_list1 terminable_symbol_list2, so rules1 and rules2 may have arbitrary types
    val equal_terminable_symbol_list : 'a * 'b list -> 'c * 'b list -> bool = <fun> *)
let equal_terminable_symbol_list = fun (rules1, terminable_symbol_list1) (rules2, terminable_symbol_list2) ->
	equal_sets terminable_symbol_list1 terminable_symbol_list2

(** final_terminable_symbol_list is an auxiliary function that uses computed_fixed_point to derive the tuple (rules, terminable_symbol_list)
    where the rules are unchanged as the original rules, and terminable_symbol_list is eventually containing all terminable symbols
    input:  rules: (('a, 'b) symbol * ('a, 'b) symbol list) list
            terminable_symbol_list: ('a, 'b) symbol list
    output: ((('a, 'b) symbol * ('a, 'b) symbol list) list * ('a, 'b) symbol list)
    val final_terminable_symbol_list : (('a, 'b) symbol * ('a, 'b) symbol list) list -> ('a, 'b) symbol list -> ((('a, 'b) symbol * ('a, 'b) symbol list) list * ('a, 'b) symbol list) = <fun> 
    However, since computed_fixed_point can take arbitrary tipes, so the real type of the function is
    val final_terminable_symbol_list : ('a * ('a, 'b) symbol list) list -> ('a, 'b) symbol list -> ('a * ('a, 'b) symbol list) list * ('a, 'b) symbol list = <fun> *)
let final_terminable_symbol_list = fun rules terminable_symbol_list ->
    computed_fixed_point (equal_terminable_symbol_list) (rules_with_terminable_symbol_list) (rules, terminable_symbol_list) 

(** remove_bad_rules is an auxiliary function that checks rule-by-rule to see if a rule's rhs only contains
    terminable symbols. If not, then blind alley occurs and we filter this rule out. It returns a list containing
    all good rules.
    input:  (rules, terminable_symbol_list): ((('a, 'b) symbol * ('a, 'b) symbol list) list * ('a, 'b) symbol list)
    output: (('a, 'b) symbol * ('a, 'b) symbol list) list
    val remove_bad_rules : ((('a, 'b) symbol * ('a, 'b) symbol list) list * ('a, 'b) symbol list) -> (('a, 'b) symbol * ('a, 'b) symbol list) list = <fun>
    However, the actual type is
    val remove_bad_rules : ('a * ('b, 'c) symbol list) list * ('b, 'c) symbol list -> ('a * ('b, 'c) symbol list) list = <fun> *)
let rec remove_bad_rules = fun (rules, terminable_symbol_list) ->
    match rules with 
    | (symbol, rhs)::t -> if (is_rhs_all_terminable rhs terminable_symbol_list) 
                            then ((symbol, rhs)::(remove_bad_rules (t, terminable_symbol_list)))
                            else (remove_bad_rules (t, terminable_symbol_list))
    | [] -> []

(** filter_blind_alleys is a function returning the grammar after all blind alley rules are removed.
    input:  g: ('a, 'b) symbol * (('a, 'b) symbol * ('a, 'b) symbol list) list
    output: ('a, 'b) symbol * (('a, 'b) symbol * ('a, 'b) symbol list) list
    val filter_blind_alleys : ('a, 'b) symbol * (('a, 'b) symbol * ('a, 'b) symbol list) list -> ('a, 'b) symbol * (('a, 'b) symbol * ('a, 'b) symbol list) list = <fun>
    However, the actual type is
    val filter_blind_alleys : 'a * ('b * ('b, 'c) symbol list) list -> 'a * ('b * ('b, 'c) symbol list) list = <fun> *)
let filter_blind_alleys = fun g -> 
    match g with 
    | (start, rules) -> (start, (remove_bad_rules (final_terminable_symbol_list rules [])))