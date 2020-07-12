int_max_(M) :-
    M is 2147483647.

rand_(P) :-
    int_max_(M),
    rand_int(M, P).

map_build([], null) :- !.
map_build([(Key, Value) | Tail], TreeMap) :-
    map_build(Tail, NewTail),
    map_put(NewTail, Key, Value, TreeMap).

split_(null, Key, null, null) :- !.
split_(node((NodeKey, NodeValue, NodePrior), NodeChildLeft, NodeChildRight), Key, Left,
    node((NodeKey, NodeValue, NodePrior), NewLeft, NodeChildRight)) :-
    Key =< NodeKey,
    split_(NodeChildLeft, Key, Left, NewLeft).
split_(node((NodeKey, NodeValue, NodePrior), NodeChildLeft, NodeChildRight), Key,
    node((NodeKey, NodeValue, NodePrior), NodeChildLeft, NewRight), Right) :-
    Key > NodeKey,
    split_(NodeChildRight, Key, NewRight, Right).

merge_(null, Right, Right) :- !.
merge_(Left, null, Left) :- !.

merge_(node((LeftNodeKey, LeftNodeValue, LeftNodePrior), LeftNodeChildLeft, LeftNodeChildRight),
    node((RightNodeKey, RightNodeValue, RightNodePrior), RightNodeChildLeft, RightNodeChildRight),
    node((LeftNodeKey, LeftNodeValue, LeftNodePrior), LeftNodeChildLeft, NewRight)) :-
    LeftNodePrior > RightNodePrior,
    merge_(LeftNodeChildRight,
    node((RightNodeKey, RightNodeValue, RightNodePrior), RightNodeChildLeft, RightNodeChildRight), NewRight).

merge_(node((LeftNodeKey, LeftNodeValue, LeftNodePrior), LeftNodeChildLeft, LeftNodeChildRight),
    node((RightNodeKey, RightNodeValue, RightNodePrior), RightNodeChildLeft, RightNodeChildRight),
    node((RightNodeKey, RightNodeValue, RightNodePrior), NewLeft, RightNodeChildRight)) :-
    LeftNodePrior =< RightNodePrior,
    merge_(node((LeftNodeKey, LeftNodeValue, LeftNodePrior), LeftNodeChildLeft, LeftNodeChildRight), RightNodeChildLeft, NewLeft).

map_put(TreeMap, Key, Value, Result) :-
    map_get(TreeMap, Key, _),
    map_remove(TreeMap, Key, NewTree),
    rand_(Prior),
    map_insert_(NewTree, (Key, Value, Prior), Result),
    !.
map_put(TreeMap, Key, Value, Result) :-
    rand_(Prior),
    map_insert_(TreeMap, (Key, Value, Prior), Result).

map_insert_(null, (NodeKey, NodeValue, NodePrior), node((NodeKey, NodeValue, NodePrior), null, null)).
map_insert_(node((NodeKey, NodeValue, NodePrior), NodeChildLeft, NodeChildRigth), (NewKey, NewValue, NewPrior),
    node((NewKey, NewValue, NewPrior), NewChildLeft, NewChildRight)) :-
    NewPrior > NodePrior,
    split_(node((NodeKey, NodeValue, NodePrior), NodeChildLeft, NodeChildRigth), NewKey, NewChildLeft, NewChildRight).
map_insert_(node((NodeKey, NodeValue, NodePrior), NodeChildLeft, NodeChildRigth), (NewKey, NewValue, NewPrior),
    node((NodeKey, NodeValue, NodePrior), NewChildLeft, NodeChildRigth)) :-
    NewPrior =< NodePrior,
    NewKey < NodeKey, map_insert_(NodeChildLeft, (NewKey, NewValue, NewPrior), NewChildLeft).
map_insert_(node((NodeKey, NodeValue, NodePrior), NodeChildLeft, NodeChildRigth), (NewKey, NewValue, NewPrior),
    node((NodeKey, NodeValue, NodePrior), NodeChildLeft, NewChildRight)) :-
    NewPrior =< NodePrior,
    NewKey >= NodeKey,
    map_insert_(NodeChildRigth, (NewKey, NewValue, NewPrior), NewChildRight).

map_remove(null, _, null).
map_remove(node((NodeKey, _, _), NodeChildLeft, NodeChildRight), NodeKey, ResultTree) :-
    merge_(NodeChildLeft, NodeChildRight, ResultTree).
map_remove(node((NodeKey, NodeValue, NodePrior), NodeChildLeft, NodeChildRight), Key,
           node((NodeKey, NodeValue, NodePrior), ResultTree, NodeChildRight)) :-
    Key < NodeKey,
    map_remove(NodeChildLeft, Key, ResultTree).
map_remove(node((NodeKey, NodeValue, NodePrior), NodeChildLeft, NodeChildRight), Key,
           node((NodeKey, NodeValue, NodePrior), NodeChildLeft, ResultTree)) :-
    Key > NodeKey,
    map_remove(NodeChildRight, Key, ResultTree).

map_get(node((NodeKey, NodeValue, NodePrior), NodeChildLeft, NodeChildRight), NodeKey, NodeValue) :- !.
map_get(node((NodeKey, NodeValue, NodePrior), NodeChildLeft, NodeChildRight), Key, Res) :-
    NodeKey < Key,
    map_get(NodeChildRight, Key, Res).
map_get(node((NodeKey, NodeValue, NodePrior), NodeChildLeft, NodeChildRight), Key, Res) :-
    NodeKey > Key,
    map_get(NodeChildLeft, Key, Res).

floor_if_(TempKey, Key, NewKey, TempKey) :-
    TempKey =< NewKey.
floor_if_(TempKey, Key, NewKey,  Key) :-
    TempKey > NewKey.

floorKey_(null, _, M) :- int_max_(M).
floorKey_(node((NodeKey, NodePrior, NodeValue), NodeChildLeft, NodeChildRight), NodeKey, NodeKey) :- !.
floorKey_(node((NodeKey, NodePrior, NodeValue), NodeChildLeft, NodeChildRight), NewKey, Res) :-
    NodeKey < NewKey,
    floorKey_(NodeChildRight, NewKey, Temp),
    floor_if_(Temp, NodeKey, NewKey, Res).
floorKey_(node((NodeKey, NodePrior, NodeValue), NodeChildLeft, NodeChildRight), NewKey, Res) :-
    NodeKey > NewKey,
    floorKey_(NodeChildLeft, NewKey, Res).

map_floorKey(node((NodeKey, NodePrior, NodeValue), NodeLeftChild, NodeRightChild), NewKey, Res) :-
    floorKey_(node((NodeKey, NodePrior, NodeValue), NodeLeftChild, NodeRightChild), NewKey, Res),
    int_max_(M),
    Res < M.
