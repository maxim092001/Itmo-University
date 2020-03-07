import math


def angle(x, y):
    len_x = length(x)
    len_y = length(y)

    answer = dot_product(x, y) / (len_x * len_y)
    answer = math.acos(answer)

    return answer * 180 / math.pi


def in_float(array):
    for ind in range(len(array)):
        array[ind] = float(array[ind])


def minus_vec(x, y, x1, y1):
    return x * y - x1 * y1


def multiply_vectors(x, y):
    t = [0, 0, 0]
    t[0] = minus_vec(x[1], y[2], x[2], y[1])
    t[1] = minus_vec(x[2], y[0], x[0], y[2])
    t[2] = minus_vec(x[0], y[1], x[1], y[0])
    return t


def minus_vector(array):
    t = [0, 0, 0]

    for index in range(len(array)):
        t[index] -= array[index]

    return t


def length(array):
    sm = 0

    for index in range(len(array)):
        sm += array[index] * array[index]

    return math.sqrt(sm)


def dot_product(x, y):
    t = 0

    for index in range(len(x)):
        t += x[index] * y[index]

    return t


if __name__ == '__main__':

    file_in = open('input.txt', 'r')
    file_out = open('output.txt', 'w')

    v = file_in.readline().split()
    a = file_in.readline().split()
    m = file_in.readline().split()
    w = file_in.readline().split()

    in_float(v)
    in_float(a)
    in_float(m)
    in_float(w)

    gun_right = multiply_vectors(a, [0, 0, 1])
    ang_m = angle([0, 0, 1], m)
    gun_left = multiply_vectors([0, 0, 1], a)

    enemy = [0, 0, 0]
    
    for i in range(len(w)):
        enemy[i] = w[i] - v[i]

    angle_l = angle(gun_left, enemy)
    angle_r = angle(gun_right, enemy)

    if (angle_l > 60) and (angle_r > 60):
        file_out.write(str(0) + "\n")
    else:
        if ang_m > 60:
            file_out.write(str(0) + "\n")
        else:
            temp = multiply_vectors(a, [0, 0, 1])

            if angle_l <= 60:
                file_out.write(str(1) + "\n")

                if angle(a, enemy) > 90:
                    angle_l = -angle_l

                if angle(m, temp) > 90:
                    ang_m = -ang_m

                file_out.write(str(angle_l) + "\n")
            else:
                file_out.write(str(-1) + "\n")

                if angle(a, enemy) > 90:
                    angle_r = -angle_r

                if angle(m, temp) < 90:
                    ang_m = -ang_m
                file_out.write(str(angle_r) + "\n")

            file_out.write(str(ang_m) + "\n")

    file_out.write("Trifanov saika\n")
