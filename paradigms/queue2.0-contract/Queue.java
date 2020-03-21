package queue;

import java.util.function.Function;
import java.util.function.Predicate;

// Inv: size >= 0
// forall i = 0 .. size - 1 : Q[i] != null
public interface Queue {

    // Pre: element != null
    // Post: size' = size + 1
    // forall i = 0 .. size' - 2 : Q'[i] = Q[i]
    // Q'[size' - 1] = x
    void enqueue(Object element);

    // Pre: size > 0
    // Post: res = Q[0]
    // size' = size - 1
    // forall i = 0 .. size' - 1 : Q'[i] = Q[i + 1]
    Object dequeue();

    // Pre: size > 0
    // Post: res = Q[0]
    // Q' = Q
    Object element();

    // Pre: true
    // Post: Res = (size == 0)
    // Q' = Q
    boolean isEmpty();

    // Pre: true
    // size = 0 && Q = {}
    void clear();

    // Pre: true
    // Post: res = size
    // Q' = Q
    int size();

    // Pre: p != null
    // Post: res = new Queue
    // typeOf(res) == typeOf(this):
    // res = [Q[index_1], Q[index_2], ... Q[index_k]]
    // forall i = 0 .. k : 0 <= index_i < size
    // forall i = 1 .. k : index_(i - 1) < index_i
    // forall element in Q : p.test(element) == true
    // forall element : (element in Q && !(element in res)) p.test(element) == false
    Queue filter(Predicate<Object> p);

    // Pre: f != null
    // Post: res = new Queue
    // typeOf(res) == typeOf(Q)
    // res.size = size
    // forall i = 0 .. res.size - 1 : res[i] == f.apply(Q[i])
    Queue map(Function<Object, Object> f);
}
