(** A symbol used in a grammar. It can be either a nonterminal symbol or a terminal symbol; 
    each kind of symbol has a value, whose type is arbitrary. *)
type ('nonterminal, 'terminal) symbol =
  | N of 'nonterminal
  | T of 'terminal


(** Question 1*)
let rec convert_rules nonterminal rules =
  match rules with
  | [] -> []
  | (lhs, rhs)::rest -> if nonterminal = lhs then rhs::(convert_rules nonterminal rest)
                                             else convert_rules nonterminal rest

let convert_grammar gram1 =
  ((fst gram1), (function nonterminal -> convert_rules nonterminal (snd gram1)))


(** Question 2*)
type ('nonterminal, 'terminal) parse_tree =
  | Node of 'nonterminal * ('nonterminal, 'terminal) parse_tree list
  | Leaf of 'terminal

let get_rest nonterminal node_list =
  match node_list with
  | [] -> Node (nonterminal, [])
  | _ -> Node (nonterminal, node_list)

(** here the pattern Node (nonterminal, []) is not useful if input is proper, but it makes this function exhaustive.*)
let rec parse_tree_leaves tree =
  match tree with
  | Node (nonterminal, []) -> []
  | Node (nonterminal, h::t) -> if (t != []) then (parse_tree_leaves h)@(parse_tree_leaves (get_rest nonterminal t))
                                             else (parse_tree_leaves h)
  | Leaf terminal -> [terminal]


(** Question 3*)

(** rule_fun_all is the function declared inside hw2 style grammar, which takes a type (nonterminal symbol) and returns a list containing all rules with lhs = this type.
    rules_nonterm_sym is the list containing all rules with lhs = some nonterminal symbol. *)
let rec check_nonterm_3 rule_fun_all rules_nonterm_sym acceptor frag =
  match rules_nonterm_sym with
  | [] -> None
  | h::t -> match check_rule_3 rule_fun_all h acceptor frag with
            | None -> check_nonterm_3 rule_fun_all t acceptor frag
            | Some ret -> Some ret

and check_rule_3 rule_fun_all rules_nonterm_sym acceptor frag =
  match rules_nonterm_sym with
  | [] -> acceptor frag
  | _ -> match frag with
         | [] -> None
         | h::t -> match rules_nonterm_sym with
                   | (T terminal)::rest -> if h = terminal then (check_rule_3 rule_fun_all rest acceptor t) else None
                   | (N nonterminal)::rest -> check_nonterm_3 rule_fun_all (rule_fun_all nonterminal) (check_rule_3 rule_fun_all rest acceptor) frag


(** (fst gram) is the starting symbol.
    (snd gram) is the function taking a type (nonterminal symbol) and returning a list containing all rules with lhs = this type.
    ((snd gram) (fst gram)) is the list containing all rules with lhs = some nonterminal symbol (= starting symbol here). *)                   
let make_matcher gram = fun acceptor frag -> check_nonterm_3 (snd gram) ((snd gram) (fst gram)) acceptor frag


(** Question 4*)

let append_matchers matcher1 matcher2 acceptor frag rules_applied = matcher1 (fun frag1 -> matcher2 acceptor frag1) frag rules_applied

(** nonterminal is the type of the nonterminal symbol we currently want to match.
    rule_fun_all is the function declared inside hw2 style grammar, which takes a type (nonterminal symbol) and returns a list containing all rules with lhs = this type.
    rules_nonterm_sym is the list containing all rules with lhs = some nonterminal symbol.
    rules_applied stores the rules we successfully applied to parse the frag. *)
let rec check_nonterm_4 nonterminal rule_fun_all rules_nonterm_sym acceptor frag rules_applied = 
  match rules_nonterm_sym with
  | [] -> None
  | h::t -> match check_rule_4 rule_fun_all h acceptor frag (List.append rules_applied [(nonterminal, h)]) with
            | None -> check_nonterm_4 nonterminal rule_fun_all t acceptor frag rules_applied
            | Some ret -> Some ret

and check_rule_4 rule_fun_all rules_nonterm_sym acceptor frag rules_applied = 
  match rules_nonterm_sym with
  | [] -> acceptor frag rules_applied
  | _ -> match frag with
         | [] -> None
         | h::t -> match rules_nonterm_sym with
                   | (T terminal)::rest -> if h = terminal then (check_rule_4 rule_fun_all rest acceptor t rules_applied) else None 
                   | (N nonterminal)::rest -> (append_matchers (check_nonterm_4 nonterminal rule_fun_all (rule_fun_all nonterminal)) (check_rule_4 rule_fun_all rest) acceptor frag rules_applied)

let empty_acceptor frag rules_applied = 
  match frag with
  | [] -> Some rules_applied
  | h::t -> None

(** Here we use up-down to build a parse tree. The (fst gram) here is essentially the root of our tree. 
    This make_rules returns a list of rules we applied. 
    in each rule of this list, if the rhs of this list is not all terminal symbols,
    it would expand from the left-most nonterminal symbol as the next element in this list.
    For instance, make_rules awkish_grammar \["3"; "+" ; "2"; "+"; "1"] would return
      Some
       \[(Expr, \[N Term; N Binop; N Expr]);    \\ root expand
         (Term, \[N Num]);                      \\ the left-most nonTerm N Term expand
         (Num, \[T "3"]);                       \\ continue expand
         (Binop, \[T "+"]);                     \\ then the second nonTerm N Binop expand
         (Expr, \[N Term; N Binop; N Expr]);    \\ then the final nonterm N Expr expand into a new \[N Term; N Binop; N Expr]
         (Term, \[N Num]);                      \\ the left-most nonTerm N Term expand
         (Num, \[T "2"]);                       \\ continue expand
         (Binop, \[T "+"]);                     \\ then the second nonTerm N Binop expand
         (Expr, \[N Term]);                     \\ then the final nonterm N Expr expand
         (Term, \[N Num]);                      \\ continue expand
         (Num, \[T "1"])]                       \\ continue expand *)
let make_rules gram frag = (check_nonterm_4 (fst gram) (snd gram) ((snd gram) (fst gram)) empty_acceptor frag [])


let rec build_node symbols used_list = 
  match symbols with
  | [] -> (used_list, [])
  | h::t -> (let (rule_list, subtree) = (build_subtree h used_list) in
              (let (rule_list_rest, list_rest) = (build_node t rule_list) in
                (rule_list_rest, subtree::list_rest)))

and build_subtree root used_list = 
  match root with
  | T rt -> (used_list, Leaf rt)
  | N rt -> (match used_list with
              | [] -> ([], Node (rt, []))
              | h::t -> (match (build_node (snd h) t) with
                         | (rule_list, lis) -> (rule_list, Node (rt, lis))))



let make_parser gram frag = 
  match make_rules gram frag with
  | None -> None
  | Some [] -> None
  | Some list -> Some (List.hd (snd (build_node [N (fst (List.hd list))] list)))





