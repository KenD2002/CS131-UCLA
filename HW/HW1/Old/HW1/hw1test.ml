let my_subset_test0 = subset [] []
let my_subset_test1 = subset [1;2;3] [4;2;1;3]
let my_subset_test2 = not(subset [1;2;3] [1;2])

let my_equal_sets_test0 = equal_sets [] []
let my_equal_sets_test1 = equal_sets [1;2;3] [3;1;2;1]
let my_equal_sets_test2 = not(equal_sets [1;2;3] [3;1;2;1;4])

let my_set_union_test0 = equal_sets (set_union [] [1;2;3;4]) [1;2;3;4]
let my_set_union_test1 = equal_sets (set_union [4;2;3] [1;2;3]) [1;2;3;4]
let my_set_union_test2 = equal_sets (set_union [] []) []
let my_set_union_test3 = equal_sets (set_union [1;2;3] [4;5;6]) [1;2;3;4;5;6]

let my_set_all_union_test0 = equal_sets (set_all_union []) []
let my_set_all_union_test1 = equal_sets (set_all_union [[1;2];[3;4];[5;6]]) [1;2;3;4;5;6]
let my_set_all_union_test2 = equal_sets (set_all_union [[1;2;3];[3;4;5];[4;5;6]]) [1;2;3;4;5;6]
let my_set_all_union_test3 = equal_sets (set_all_union [[1;2;3];[3;2;1];[];[1;6]]) [1;2;3;6]

let my_computed_fixed_point_test0 = computed_fixed_point (=) (sqrt) 1000000000. = 1.
let my_computed_fixed_point_test1 = computed_fixed_point (=) (fun x -> x *. x) 5. = infinity
let my_computed_fixed_point_test2 = computed_fixed_point (=) (fun x -> x /. 10.) 9999999999. = 0.

let my_computed_periodic_point_test0 = computed_periodic_point (=) (fun x -> -x) 0 (10) = 10
let my_computed_periodic_point_test1 = computed_periodic_point (=) (fun x -> -x) 2 (10) = 10
let my_computed_periodic_point_test2 = computed_periodic_point (=) (fun x -> -x) 4 (10) = 10

let my_whileseq_test0 = equal_sets(whileseq (( * ) 2) (( > ) 65) 1) [1;2;4;8;16;32;64]
let my_whileseq_test1 = equal_sets(whileseq (sqrt) (( < ) 3.) 256.) [256.;16.;4.]

type awksub_nonterminals =
  | Expr | Lvalue | Incrop | Binop | Num

let awksub_rules =
   [Expr, [T"("; N Expr; T")"];
    Expr, [N Num];
    Expr, [N Expr; N Binop; N Expr];
    Expr, [N Lvalue];
    Expr, [N Incrop; N Lvalue];
    Expr, [N Lvalue; N Incrop];
    Lvalue, [T"$"; N Expr];
    Incrop, [T"++"];
    Incrop, [T"--"];
    Binop, [T"+"];
    Binop, [T"-"];
    Num, [T"0"];
    Num, [T"1"];
    Num, [T"2"];
    Num, [T"3"];
    Num, [T"4"];
    Num, [T"5"];
    Num, [T"6"];
    Num, [T"7"];
    Num, [T"8"];
    Num, [T"9"]]

let awksub_grammar = Expr, awksub_rules

let my_filter_blind_alleys_test0 = filter_blind_alleys awksub_grammar = awksub_grammar

let my_filter_blind_alleys_test1 = filter_blind_alleys (Expr, List.tl awksub_rules) = (Expr, List.tl awksub_rules)

let my_filter_blind_alleys_test2 =
 filter_blind_alleys (Expr,
      [Expr, [N Num];
       Expr, [N Lvalue];
       Expr, [N Expr; N Lvalue];
       Expr, [N Lvalue; N Expr];
       Expr, [N Expr; N Binop; N Expr];
       Lvalue, [N Lvalue; N Expr];
       Lvalue, [N Expr; N Lvalue];
       Lvalue, [N Incrop; N Lvalue];
       Lvalue, [N Lvalue; N Incrop];
       Incrop, [T"++"]; Incrop, [T"--"];
       Binop, [T"+"]; Binop, [T"-"];
       Num, [T"0"]; Num, [T"1"]; Num, [T"2"]; Num, [T"3"]; Num, [T"4"];
       Num, [T"5"]; Num, [T"6"]; Num, [T"7"]; Num, [T"8"]; Num, [T"9"]])
  = (Expr,
     [Expr, [N Num];
      Expr, [N Expr; N Binop; N Expr];
      Incrop, [T"++"]; Incrop, [T"--"];
      Binop, [T "+"]; Binop, [T "-"];
      Num, [T "0"]; Num, [T "1"]; Num, [T "2"]; Num, [T "3"]; Num, [T "4"];
      Num, [T "5"]; Num, [T "6"]; Num, [T "7"]; Num, [T "8"]; Num, [T "9"]])

let my_filter_blind_alleys_test3 =
  filter_blind_alleys (Expr, List.tl (List.tl (List.tl awksub_rules))) =
    filter_blind_alleys (Expr, List.tl (List.tl awksub_rules))

type giant_nonterminals =
  | Conversation | Sentence | Grunt | Snore | Shout | Quiet

let giant_grammar =
  Conversation,
  [Snore, [T"ZZZ"];
   Quiet, [];
   Grunt, [T"khrgh"];
   Shout, [T"aooogah!"];
   Sentence, [N Quiet];
   Sentence, [N Grunt];
   Sentence, [N Shout];
   Conversation, [N Snore];
   Conversation, [N Sentence; T","; N Conversation]]

let my_filter_blind_alleys_test4 =
  filter_blind_alleys giant_grammar = giant_grammar

let my_filter_blind_alleys_test5 =
  filter_blind_alleys (Sentence, List.tl (snd giant_grammar)) =
    (Sentence,
     [Quiet, []; Grunt, [T "khrgh"]; Shout, [T "aooogah!"];
      Sentence, [N Quiet]; Sentence, [N Grunt]; Sentence, [N Shout]])

let my_filter_blind_alleys_test6 =
  filter_blind_alleys (Sentence, List.tl (List.tl (snd giant_grammar))) =
    (Sentence,
     [Grunt, [T "khrgh"]; Shout, [T "aooogah!"];
      Sentence, [N Grunt]; Sentence, [N Shout]])

let awksub_rules_bad =
  [Expr, [T"("; N Expr; T")"];
   Expr, [N Num];
   Expr, [N Expr; N Binop; N Expr];
   Expr, [N Lvalue];
   Lvalue, [N Lvalue];
   Lvalue, [N Lvalue; N Lvalue;];
   Lvalue, [N Lvalue; N Lvalue; N Lvalue];
   Binop, [T"+"];
   Binop, [T"-"];
   Num, [T"0"];
   Num, [T"1"];
   Num, [T"2"];
   Num, [T"3"];
   Num, [T"4"];
   Num, [T"5"];
   Num, [T"6"];
   Num, [T"7"];
   Num, [T"8"];
   Num, [T"9"]]

let awksub_grammar_bad = Expr, awksub_rules_bad

let awksub_rules_better =
  [Expr, [T"("; N Expr; T")"];
   Expr, [N Num];
   Expr, [N Expr; N Binop; N Expr];
   Binop, [T"+"];
   Binop, [T"-"];
   Num, [T"0"];
   Num, [T"1"];
   Num, [T"2"];
   Num, [T"3"];
   Num, [T"4"];
   Num, [T"5"];
   Num, [T"6"];
   Num, [T"7"];
   Num, [T"8"];
   Num, [T"9"]]

let awksub_grammar_better = Expr, awksub_rules_better


let my_filter_blind_alleys_test7 = filter_blind_alleys awksub_grammar_bad = awksub_grammar_better
