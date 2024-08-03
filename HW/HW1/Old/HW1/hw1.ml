(** Question 1*)

(** is_in is an auxiliary function taking 2 arguments: elem:'a, lst:'a list. It returns a bool.
    It checks whether elem is an element in the set(list) lst. *)
let rec is_in elem lst =
    match lst with
    | [] -> false
    | h::t -> (elem = h) || (is_in elem t)

(** subset takes 2 arguments: a:'a list, b:'a list. It returns a bool.
    It checks whether all the elements in a is contained in b. *)
let rec subset a b =
    match a with
    | [] -> true
    | h::t -> (is_in h b) && (subset t b)


(** Question 2*)

(** equal_sets takes 2 arguments: a:'a list, b:'a list. It returns a bool.
    It checks whether both a is subset of b and b is a subset of a are true
    If so, then a and b are representing the same set. *)
let equal_sets a b = (subset a b) && (subset b a)


(** Question 3*)

(** deduplicate_set is an auxiliary function taking 2 arguments: lst:'a list, new_lst:'a list (but
    we will assign this by [] aall the time). It returns 'a list.
    It goes through elemen-wise in lst and check if it is already contained in the new_lst.
    If so then go to next element, if not then attach it to new_lst and then go to next element. *)
let rec deduplicate_set lst new_lst =
    match lst with
    | [] -> new_lst
    | h::t -> if is_in h new_lst then deduplicate_set t new_lst else deduplicate_set t (h::new_lst)

(** set_union takes 2 arguments: a:'a list, b:'a list. It returns 'a list.
    It uses the append method in the List module, and then use the deduplicate_set function to 
    get a list with no duplicates. *)
let set_union a b = deduplicate_set (List.append a b) []


(** Question 4*)

(** set_all_union takes 1 arguments: a:'a list list. It returns 'a list.
    It goes through every element (which is 'a list) in a, and append every element in each 'a list
    into the 'a list being returned. Finally deduplicate_set is used to deduplicate the resulted 'a list. *)
let rec set_all_union a =
    match a with
    | [] -> []
    | h::t -> deduplicate_set (List.append h (set_all_union t)) []


(** Question 5*)

(** Russell's Paradox is not possible to implement such function "self_member s". 
    In order to check whether s is an element of s, we will need to compare s with some element of s.
    Then, this s must be of both type 'a list and 'a, making the implementation impossible. *)


(** Question 6*)

(** computed_fixed_point takes 3 arguments: eq:('a -> 'a -> bool), f:('a -> 'a), x:'a. It returns 'a.
    It checks if x == f(x), if so then return x. Elsewise we let x <- f(x) and re-compare. *)
let rec computed_fixed_point eq f x = 
    if (eq x (f x)) then x 
    else computed_fixed_point eq f (f x)


(** Question 7*)

(** apply_n_times is an auxiliary function takeing 3 arguments: f:('a -> 'a), n:int, x:'a. It returns 'a.
    It applies f to x n times. *)
let rec apply_n_times f n x =
    if (n > 0) then (apply_n_times f (n - 1) (f x))
    else x

(** computed_periodic_point takes 4 arguments: eq:('a -> 'a -> bool), f:('a -> 'a), p:int, x:'a. It returns 'a.
    It checks if x == {f^p}(x), if so then return x. Elsewise we let x <- f(x) and re-compare.*)
let rec computed_periodic_point eq f p x =
    if (eq x (apply_n_times f p x)) then x
    else computed_periodic_point eq f p (f x)


(** Question 8*)

(** whileseq takes 3 arguments: s:('a -> 'a), p:('a -> bool), x:'a. It returns 'a list.
    It checks if p(x) is true, if so then append x to the return list and substitute x by s(x) and redo the function. 
    Elsewise retrun the list. *)
let rec whileseq s p x =
    if (p x) then (x::(whileseq s p (s x)))
    else []


(** Question 9*)    

(** A symbol used in a grammar. It can be either a nonterminal symbol or a terminal symbol; 
    each kind of symbol has a value, whose type is arbitrary. *)
type ('nonterminal, 'terminal) symbol =
  | N of 'nonterminal
  | T of 'terminal


(** is_symbol_terminable is an auxiliary function with 2 arguments: symbol:('a, 'b) symbol, terminable_symbols:('a, 'b) symbol list.
    It returns bool.
    It checks if symbol is of type T,
            or if symbol is already contained in the terminable_symbols. *)
let is_symbol_terminable symbol terminable_symbols = 
	match symbol with 
	| T terminal_symbol -> true
	| N non_terminal_symbol -> (is_in symbol terminable_symbols)


(** rhs_all_terminable is an auxiliary function with 2 arguments: rhs:('a, 'b) symbol list, terminable_symbols:'a list. It returns bool.
    It checks if the right hand side only contains terminable symbols, and return true if so, false otherwise.*)
let rec rhs_all_terminable rhs terminable_symbols = 
	match rhs with 
	| [] -> true
	| h::t -> if (is_symbol_terminable h terminable_symbols) 
                then (rhs_all_terminable t terminable_symbols) 
                else false


(** terminable_rule is an auxiliary function with 2 arguments: rules:('a * (('a, 'b) symbol, 'c) symbol list) list
                                                               terminable_symbols:('a, 'b) symbol list
                                                   It returns: ('a, 'b) symbol list
    Here, rules represents all the rules in a grammmar, like  \[Expr, \[T"("; N Expr; T")"]; 
                                                                   Expr, \[N Num]; 
                                                                   ...]
    and terminable_symbols represents the list of all terminable symbols.
    It checks the element of rules (each rule) one-by-one and see if its rhs only contains terminable symbols.
    If so, then append this rule's symbol on the lhs into terminable_symbols, and move on to the next rule.
    If not, then do nothing and move on to the next rule. *)
let rec terminable_symbol_list rules terminable_symbols = 
	match rules with 
	| (symbol, rhs)::t -> if (rhs_all_terminable rhs terminable_symbols)
						    then ((terminable_symbol_list t ((N symbol)::terminable_symbols)) )
						    else ((terminable_symbol_list t terminable_symbols) )
	| _ -> (terminable_symbols)


(** This is an auxiliary function taking a tuple argument: (rules, terminable_symbols): ('a * ('a, 'b) symbol list) list * ('a, 'b) symbol list
                                                     It returns the same type as input: ('a * ('a, 'b) symbol list) list * ('a, 'b) symbol list 
    It simply attach the terminable symbol lists to the rules and form a tuple.
    This function is for computed_fixed_point to use as its f, where the input and output types are the same. *)
let rules_with_t_list (rules, terminable_symbols) = 
	(rules, terminable_symbol_list rules terminable_symbols)

(** This is an auxiliary function is intended to take 2 tuple arguments: (rules1, terminable_symbols1):('a * ('a, 'b) symbol list) list * ('a, 'b) symbol list
                                                                         (rules2, terminable_symbols1):('a * ('a, 'b) symbol list) list * ('a, 'b) symbol list
                                                                    It returns a bool.
    This function uses equal_sets function to check if the two terminable_symbols lists are the same.
    This function is for comparing x and f(x) for computed_fixed_point's eq argument. *)
let equal_terminable_symbols (rules1, terminable_symbols1) (rules2, terminable_symbols2) =
	equal_sets terminable_symbols1 terminable_symbols2

(* repeatedly check the rules for more terminable symbols until no new ones are added
this condition is determined used computed_fixed_point where x = (rules,[]) 
and f = rules_with_t_list which will add new terminable symbols 
Returns (rules, terminable_symbols) *)

(** This is an auxiliary function with 2 arguments: rules: ('a * ('a, 'b) symbol list) list
                                                    terminable_symbols: ('a, 'b) symbol list
                                                It returns ('a * ('a, 'b) symbol list) list * ('a, 'b) symbol list
    It uses computed_fixed_point to iterate and get all the terminable symbols. *)
let final_terminable_symbols rules terminable_symbols = 
	computed_fixed_point (equal_terminable_symbols) (rules_with_t_list) (rules, terminable_symbols) 

(** This is an auxiliary function with a tuple arguments: (rules, terminable_symbols):('a * ('b, 'c) symbol list) list * ('b, 'c) symbol list
                                                It returns ('a * ('b, 'c) symbol list) list
    This function checks rule by rule and see if its rhs only contains terminable symbols.
    If not, then a blind alley would occur, and we have to remove this rule. *)
let rec remove_bad_rules (rules, terminable_symbols) = 
	match rules with 
    | (symbol, rhs)::t -> if (rhs_all_terminable rhs terminable_symbols) then ((symbol, rhs)::(remove_bad_rules (t, terminable_symbols)))
                                                                         else (remove_bad_rules (t, terminable_symbols))
	| _ -> []


(** filter_blind_alleys takes an argument grammar g: 'a * ('b * ('b, 'c) symbol list) list
                            It returns a grammar g': 'a * ('b * ('b, 'c) symbol list) list
    We are not interested in the starting symbol, so we match g to get its rules, and we get all the terminable symbols in this grammar,
    and then we remove all the bad rules that may cause blind alley. *)
let filter_blind_alleys g = 
	match g with 
	(start, rules) -> (start, (remove_bad_rules (final_terminable_symbols rules []) ) )