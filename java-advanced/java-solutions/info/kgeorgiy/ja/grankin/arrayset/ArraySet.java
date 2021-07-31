package info.kgeorgiy.ja.grankin.arrayset;

import java.util.*;

public class ArraySet<E extends Comparable<E>> extends AbstractSet<E> implements NavigableSet<E> {

    private final ReversibleList<E> arrayList;
    private final Comparator<? super E> comparator;

    public ArraySet() {
        this(Collections.emptyList(), null);
    }

    public ArraySet(final Comparator<? super E> comparator) {
        this(Collections.emptyList(), comparator);
    }

    public ArraySet(final Collection<? extends E> collection) {
        this(collection, null);
    }

    @Override
    public Iterator<E> iterator() {
        return arrayList.iterator();
    }

    @Override
    public int size() {
        return arrayList.size();
    }

    public ArraySet(final Collection<? extends E> collection, final Comparator<? super E> comparator) {
        final NavigableSet<E> set = new TreeSet<>(comparator);
        set.addAll(collection);
        this.arrayList = new ReversibleList<>(set);
        this.comparator = comparator;
    }

    @Override
    public Comparator<? super E> comparator() {
        return comparator;
    }

    @Override
    public E first() {
        return elementByIndex(0);
    }

    @Override
    public E last() {
        return elementByIndex(size() - 1);
    }

    @Override @SuppressWarnings("unchecked")
    public boolean contains(final Object o) {
        return Collections.binarySearch(arrayList, (E) Objects.requireNonNull(o), comparator) >= 0;
    }

    @Override
    public E lower(final E e) {
        return elementByIndexOrNull(indexOf(e, LowerAndInclusive.LOWER));
    }

    @Override
    public E floor(final E e) {
        return elementByIndexOrNull(indexOf(e, LowerAndInclusive.LOWER_AND_INCLUSIVE));
    }

    @Override
    public E ceiling(final E e) {
        return elementByIndexOrNull(indexOf(e, LowerAndInclusive.INCLUSIVE));
    }

    @Override
    public E higher(final E e) {
        return elementByIndexOrNull(indexOf(e, LowerAndInclusive.NONE));
    }

    @Override
    public NavigableSet<E> descendingSet() {
        return new ArraySet<>(new ReversibleList<>(arrayList, true), Collections.reverseOrder(comparator));
    }

    @Override
    public Iterator<E> descendingIterator() {
        return descendingSet().iterator();
    }

    @Override
    public NavigableSet<E> subSet(final E fromElement, final boolean fromInclusive, final E toElement, final boolean toInclusive) {
        if (compareTwoElements(fromElement, toElement) > 0) {
            throw new IllegalArgumentException();
        }
        return customSubSetImplementation(fromElement, fromInclusive, toElement, toInclusive);
    }

    @Override
    public NavigableSet<E> headSet(final E toElement, final boolean inclusive) {
        return isEmpty() ? this : customSubSetImplementation(first(), true, toElement, inclusive);
    }

    @Override
    public NavigableSet<E> tailSet(final E fromElement, final boolean inclusive) {
        return isEmpty() ? this : customSubSetImplementation(fromElement, inclusive, last(), true);
    }

    @Override
    public SortedSet<E> subSet(final E fromElement, final E toElement) {
        return subSet(fromElement, true, toElement, false);
    }

    @Override
    public SortedSet<E> headSet(final E toElement) {
        return headSet(toElement, false);
    }

    @Override
    public SortedSet<E> tailSet(final E fromElement) {
        return tailSet(fromElement, true);
    }

    @Override
    public E pollFirst() {
        throw new UnsupportedOperationException();
    }

    @Override
    public E pollLast() {
        throw new UnsupportedOperationException();
    }

    @Override
    public void clear() {
        throw new UnsupportedOperationException();
    }

    @Override
    public boolean addAll(final Collection<? extends E> collection) {
        throw new UnsupportedOperationException();
    }

    @Override
    public boolean removeAll(final Collection<?> collection) {
        throw new UnsupportedOperationException();
    }

    @Override
    public boolean add(final E element) {
        throw new UnsupportedOperationException();
    }

    @Override
    public boolean remove(final Object o) {
        throw new UnsupportedOperationException();
    }

    private enum LowerAndInclusive {
        NONE(false, false),
        INCLUSIVE(false, true),
        LOWER(true, false),
        LOWER_AND_INCLUSIVE(true, true);

        private final boolean lower;
        private final boolean inclusive;

        public boolean isLower() {
            return lower;
        }

        public boolean isInclusive() {
            return inclusive;
        }

        LowerAndInclusive(final boolean lower, final boolean inclusive) {
            this.lower = lower;
            this.inclusive = inclusive;
        }

        public static LowerAndInclusive valueOf(final boolean lower, final boolean inclusive) {
            return switch ((lower ? 2 : 0) + (inclusive ? 1 : 0)) {
                case 1 -> LowerAndInclusive.INCLUSIVE;
                case 2 -> LowerAndInclusive.LOWER;
                case 3 -> LowerAndInclusive.LOWER_AND_INCLUSIVE;
                default -> LowerAndInclusive.NONE;
            };
        }

    }

    private E elementByIndex(final int index) {
        if (isEmpty()) {
            throw new NoSuchElementException();
        } else {
            return arrayList.get(index);
        }
    }

    private E elementByIndexOrNull(final int index) {
        return index >= 0 && index < size() ? arrayList.get(index) : null;
    }

    private int indexOf(final E element, final LowerAndInclusive lowerAndInclusive) {
        final int index = Collections.binarySearch(arrayList, element, comparator);
        if (index >= 0) {
            int result = index;
            if (!lowerAndInclusive.isInclusive()) {
                result += (lowerAndInclusive.isLower() ? -1 : 1);
            }
            return result;
        } else {
            return -index - 1 + (lowerAndInclusive.isLower() ? -1 : 0);
        }
    }

    private int compareTwoElements(final E first, final E second) {
        return Objects.isNull(comparator) ? first.compareTo(second) : comparator.compare(first, second);
    }

    private NavigableSet<E> customSubSetImplementation(final E fromElement, final boolean fromInclusive, final E toElement, final boolean toInclusive) {
        int indexFrom = indexOf(fromElement, LowerAndInclusive.valueOf(false, fromInclusive));
        int indexTo = indexOf(toElement, LowerAndInclusive.valueOf(true, toInclusive));
        if (indexFrom > indexTo) {
            return new ArraySet<E>(comparator);
        } else {
            return new ArraySet<>(arrayList.subList(indexFrom, indexTo + 1), comparator);
        }
    }

    private static class ReversibleList<T> extends AbstractList<T> implements RandomAccess {

        private final List<T> list;
        private boolean isReversed = false;

        public ReversibleList(final Collection<T> collection) {
            this.list = List.copyOf(collection);
        }

        public ReversibleList(final ReversibleList<T> reversibleList, final boolean isReversed) {
            this.list = reversibleList.list;
            this.isReversed = reversibleList.isReversed ^ isReversed;
        }

        private int indexFromTail(final int index) {
            return size() - index - 1;
        }

        @Override
        public ReversibleList<T> subList(final int fromIndex, final int toIndex) {
            if (isReversed) {
                return new ReversibleList<>(list.subList(indexFromTail(toIndex - 1), indexFromTail(toIndex) + 1));
            } else {
                return new ReversibleList<>(list.subList(fromIndex, toIndex));
            }
        }

        @Override
        public T get(final int index) {
            return list.get(isReversed ? indexFromTail(index) : index);
        }

        @Override
        public int size() {
            return list.size();
        }
    }

}
