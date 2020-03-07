import copy


def read_matrix(m):

    if m == 0:
        return []

    temp_array = []

    temp_c = []

    for i in [j for j in input_file.readline().split()]:
        temp_c.append(float(i))
        if len(temp_c) == m:
            temp_array.append(temp_c)
            temp_c = []

    return temp_array


def check_matrix_sizes_for_sum(matrix1, matrix2):
    flag = (len(matrix1) == len(matrix2))

    if not flag:
        return False

    for i in range(len(matrix1)):
        flag &= (len(matrix1[i]) == len(matrix2[i]))

    return flag


def difference_matrix(matrix1, matrix2):
    temp_matrix2 = copy.deepcopy(matrix2)
    for i in range(len(temp_matrix2)):
        for j in range(len(temp_matrix2[i])):
            temp_matrix2[i][j] *= -1
    return sum_matrix(matrix1, temp_matrix2)


def sum_matrix(matrix1, matrix2):
    temp_matrix1 = copy.deepcopy(matrix1)
    if not check_matrix_sizes_for_sum(temp_matrix1, matrix2):
        output_file.write(str(0))
        exit()
    for i in range(len(temp_matrix1)):
        for j in range(len(temp_matrix1[i])):
            temp_matrix1[i][j] += matrix2[i][j]
    return temp_matrix1


def transpose_matrix(matrix, n, m):
    trans_matrix = []
    for i in range(m):
        temp = []
        for j in range(n):
            temp.append(matrix[j][i])
        trans_matrix.append(temp)
    return trans_matrix


def multiply_matrix_on_num(matrix, x):
    temp_matrix = copy.deepcopy(matrix)
    for i in temp_matrix:
        for j in range(len(i)):
            i[j] *= x
    return temp_matrix


def check_matrix_sizes_for_multiply(matrix1, matrix2):
    return len(matrix2) == len(matrix1[0])


def multiply_matrix_on_matrix(matrix1, matrix2):
    if not check_matrix_sizes_for_multiply(matrix1, matrix2):
        output_file.write(str(0))
        exit()

    temp_matrix1 = copy.deepcopy(matrix1)
    temp_matrix2 = copy.deepcopy(matrix2)
    temp = []
    my_ans = []
    s = 0
    for k in range(len(temp_matrix1)):
        for j in range(len(temp_matrix2[0])):
            for i in range(len(temp_matrix1[0])):
                s += temp_matrix1[k][i] * temp_matrix2[i][j]
            temp.append(s)
            s = 0
        my_ans.append(temp)
        temp = []
    return my_ans


input_file = open("input.txt", "r")
output_file = open("output.txt", "w")

alpha, betta = map(float, input_file.readline().split())

nA, mA = map(int, input_file.readline().split())
a = read_matrix(mA)

nB, mB = map(int, input_file.readline().split())
b = read_matrix(mB)

nC, mC = map(int, input_file.readline().split())
c = read_matrix(mC)

nD, mD = map(int, input_file.readline().split())
d = read_matrix(mD)

nF, mF = map(int, input_file.readline().split())
f = read_matrix(mF)

first = sum_matrix(multiply_matrix_on_num(a, alpha), multiply_matrix_on_num(transpose_matrix(b, nB, mB), betta))
trans_first = transpose_matrix(first, len(first), len(first[0]))
ans = difference_matrix(multiply_matrix_on_matrix(multiply_matrix_on_matrix(c, trans_first), d), f)

output_file.write('1\n')
output_file.write(str(len(ans)) + ' ' + str(len(ans[0])) + '\n')
for row in ans:
    output_file.write(' '.join([str(elem) for elem in row]))
    output_file.write('\n')
