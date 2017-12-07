% vim: set syntax=prolog

:- consult('helpers/run').
:- use_module(library(pairs)).
:- use_module(library(regex)).
:- use_module('helpers/io').
:- use_module('helpers/lists').

main :-
  read_input(Nodes),
  node_with_wrong_weight(Nodes, Node, Siblings),
  Name - Weight - _ = Node,
  new_weight(Node, Siblings, NewWeight),
  put_node_weight(Nodes, Name, NewWeight, UpdatedNodes),
  \+ node_with_wrong_weight(UpdatedNodes, _, _),
  format("~s: ~d -> ~d\n", [Name, Weight, NewWeight]).

read_input(NodeTree) :-
  current_input(S),
  read_lines(S, Lines),
  maplist(parse, Lines, NodePairs),
  root(NodePairs, Root),
  list_to_assoc(NodePairs, Nodes),
  tree(Nodes, Root, NodeTree),
  !.

parse(Line, Node) :-
  regex("(\\w+) \\((\\d+)\\)( -> ([a-z, ]+))?", [], Line, Captures),
  (
    [ACodes, WeightString, _, StringOfBs] = Captures,
    string_codes(A, ACodes),
    number_string(Weight, WeightString),
    split_string(StringOfBs, ", ", "", SplitBs),
    exclude(=(""), SplitBs, Bs),
    Node = A - (Weight - Bs)
    ;
    [ACodes, WeightString] = Captures,
    string_codes(A, ACodes),
    number_string(Weight, WeightString),
    Node = A - (Weight - [])
  ).

root(NodePairs, Root) :-
  edges(NodePairs, Edges),
  pairs_keys_values(Edges, As, Bs),
  member(Root, As),
  \+ member(Root, Bs).

edges(NodePairs, Edges) :-
  maplist(node_edge, NodePairs, NestedEdges),
  flatten(NestedEdges, Edges).
node_edge(A - (_ - Bs), Edges) :-
  repeated(A, As),
  zip(As, Bs, Edges).

tree(Nodes, NodeName, Tree) :-
  get_assoc(NodeName, Nodes, Weight - ChildrenNames),
  maplist(tree(Nodes), ChildrenNames, Children),
  Tree = NodeName - Weight - Children.

node_with_wrong_weight(Node, OutputNode, Siblings) :- node_with_wrong_weight(Node, [], OutputNode, Siblings).
node_with_wrong_weight(Node, Siblings, OutputNode, OutputSiblings) :-
  Siblings \= [],
  forall(member(Sibling, Siblings), (
    weight(Node, NodeWeight),
    weight(Sibling, SiblingWeight),
    NodeWeight \= SiblingWeight
  )),
  OutputNode = Node,
  OutputSiblings = Siblings.
node_with_wrong_weight(_ - _ - Children, _, OutputNode, OutputSiblings) :-
  select(Child, Children, OtherChildren),
  node_with_wrong_weight(Child, OtherChildren, OutputNode, OutputSiblings).

put_node_weight(Name - _ - Children, Name, NewWeight, Name - NewWeight - Children).
put_node_weight(Name - Weight - Children, NodeName, NewWeight, Updated) :-
  maplist(reordered_put_node_weight(NodeName, NewWeight), Children, UpdatedChildren),
  Updated = Name - Weight - UpdatedChildren.
reordered_put_node_weight(Name, NewWeight, Node, Updated) :-
  put_node_weight(Node, Name, NewWeight, Updated).

new_weight(Node, Siblings, NewWeight) :-
  member(Sibling, Siblings),
  _ - NodeWeight - _ = Node,
  weight(Node, NodePlusChildrenWeight),
  weight(Sibling, SiblingPlusChildrenWeight),
  Change is SiblingPlusChildrenWeight - NodePlusChildrenWeight,
  NewWeight = NodeWeight + Change.

weight(Node, Weight) :-
  _ - NodeWeight - Children = Node,
  maplist(weight, Children, ChildrenWeights),
  sum_list(ChildrenWeights, TotalChildrenWeight),
  Weight is NodeWeight + TotalChildrenWeight,
  asserta( (weight(Node, Weight) :- !) ).
