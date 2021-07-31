package info.kgeorgiy.ja.grankin.student;

import info.kgeorgiy.java.advanced.student.Group;
import info.kgeorgiy.java.advanced.student.GroupName;
import info.kgeorgiy.java.advanced.student.GroupQuery;
import info.kgeorgiy.java.advanced.student.Student;

import java.util.*;
import java.util.function.BinaryOperator;
import java.util.function.Function;
import java.util.function.Predicate;
import java.util.stream.Collectors;

public class StudentDB implements GroupQuery {

    private final Comparator<Student> nameComparator = Comparator
            .comparing(Student::getLastName)
            .thenComparing(Student::getFirstName)
            .reversed()
            .thenComparing(Student::getId);

    private List<Group> getGroupsSortedFromStudentsBy(final Collection<Student> students,
                                                      final Function<List<Student>, List<Student>> mapper) {
        return students.stream()
                .sorted()
                .collect(Collectors.groupingBy(Student::getGroup))
                .entrySet().stream()
                .map(e -> new Group(e.getKey(), mapper.apply(e.getValue())))
                .sorted(Comparator.comparing(Group::getName))
                .collect(Collectors.toList());
    }

    private GroupName getLargestGroupFromStudentsBy(
            final Collection<Student> students,
            final Function<List<Student>, Integer> mapper,
            final Comparator<GroupName> keyComparator) {
        return students.stream()
                .collect(Collectors.groupingBy(Student::getGroup))
                .entrySet().stream()
                .collect(Collectors.toMap(Map.Entry::getKey, e -> mapper.apply(e.getValue())))
                .entrySet().stream()
                .max(Map.Entry.
                        <GroupName, Integer>comparingByValue()
                        .thenComparing(Map.Entry.comparingByKey(keyComparator)))
                .map(Map.Entry::getKey)

                .orElse(null);
    }

    private List<Student> findStudentsBy(final Collection<Student> students,
                                         final Predicate<Student> predicate) {
        return students.stream().filter(predicate).sorted(nameComparator).collect(Collectors.toList());
    }

    private List<Student> sortStudentBy(final Collection<Student> students,
                                        final Comparator<Student> comparator) {
        return students.stream().sorted(comparator).collect(Collectors.toList());
    }

    private <T> List<T> mapStudentList(final Collection<Student> students, final Function<Student, T> mapper) {
        return students.stream().map(mapper).collect(Collectors.toList());
    }

    @Override
    public List<Group> getGroupsByName(final Collection<Student> students) {
        return getGroupsSortedFromStudentsBy(students, s -> s.stream().sorted(nameComparator).collect(Collectors.toList()));
    }

    @Override
    public List<Group> getGroupsById(final Collection<Student> students) {
        return getGroupsSortedFromStudentsBy(students, Function.identity());
    }

    @Override
    public GroupName getLargestGroup(final Collection<Student> students) {
        return getLargestGroupFromStudentsBy(students, List::size, Comparator.naturalOrder());
    }

    @Override
    public GroupName getLargestGroupFirstName(final Collection<Student> students) {
        return getLargestGroupFromStudentsBy(students, s -> getDistinctFirstNames(s).size(), Comparator.reverseOrder());
    }

    @Override
    public List<String> getFirstNames(final List<Student> students) {
        return mapStudentList(students, Student::getFirstName);
    }

    @Override
    public List<String> getLastNames(final List<Student> students) {
        return mapStudentList(students, Student::getLastName);
    }

    @Override
    public List<GroupName> getGroups(final List<Student> students) {
        return mapStudentList(students, Student::getGroup);
    }

    @Override
    public List<String> getFullNames(final List<Student> students) {
        return mapStudentList(students, s -> String.format("%s %s", s.getFirstName(), s.getLastName()));
    }

    @Override
    public Set<String> getDistinctFirstNames(final List<Student> students) {
        return students.stream()
                .map(Student::getFirstName)
                .collect(Collectors.toCollection(TreeSet::new));
    }

    @Override
    public String getMaxStudentFirstName(final List<Student> students) {
        return students.stream()
                .max(Comparator.comparingInt(Student::getId))
                .map(Student::getFirstName)
                .orElse("");
    }

    @Override
    public List<Student> sortStudentsById(final Collection<Student> students) {
        return sortStudentBy(students, Comparator.comparing(Student::getId));
    }

    @Override
    public List<Student> sortStudentsByName(final Collection<Student> students) {
        return sortStudentBy(students, nameComparator);
    }

    @Override
    public List<Student> findStudentsByFirstName(final Collection<Student> students, final String name) {
        return findStudentsBy(students, s -> s.getFirstName().equals(name));
    }

    @Override
    public List<Student> findStudentsByLastName(final Collection<Student> students, final String name) {
        return findStudentsBy(students, s -> s.getLastName().equals(name));
    }

    @Override
    public List<Student> findStudentsByGroup(final Collection<Student> students, final GroupName group) {
        return findStudentsBy(students, s -> s.getGroup().equals(group));
    }

    @Override
    public Map<String, String> findStudentNamesByGroup(final Collection<Student> students, final GroupName group) {
        return findStudentsByGroup(students, group).stream()
                .collect(Collectors.toMap(
                        Student::getLastName,
                        Student::getFirstName,
                        BinaryOperator.minBy(Comparator.naturalOrder())
                ));
    }
}
