let accept_all string = Some string
let accept_empty_suffix = function
  | _::_ -> None
  | x -> Some x

type giant_nonterminals =
  | Conversation | Sentence | Grunt | Snore | Shout
 
let strange_giant_grammar =
  Conversation,
  [Snore, [T"ZZZ"];
  Grunt, [T"khrgh"];
  Shout, [T"yahoo!"];
  Sentence, [N Grunt];
  Sentence, [N Shout];
  Conversation, [N Snore];
  Conversation, [N Sentence; T","; N Conversation];
  Conversation, [N Sentence; N Sentence]]

let hw2_style_giant_grammar = convert_grammar strange_giant_grammar



let make_matcher_test = (make_matcher hw2_style_giant_grammar accept_all ["yahoo!" ; ","; "khrgh"; "khrgh"; "ZZZ"; "ZZZ"]) = Some ["ZZZ"; "ZZZ"]

let make_parser_test = (make_parser hw2_style_giant_grammar ["yahoo!" ; ","; "khrgh"; ","; "khrgh"; "yahoo!"])
  = Some
  (Node (Conversation,
    [Node (Sentence, [Node (Shout, [Leaf "yahoo!"])]); Leaf ",";
     Node (Conversation,
      [Node (Sentence, [Node (Grunt, [Leaf "khrgh"])]); Leaf ",";
       Node (Conversation,
        [Node (Sentence, [Node (Grunt, [Leaf "khrgh"])]);
         Node (Sentence, [Node (Shout, [Leaf "yahoo!"])])])])]))