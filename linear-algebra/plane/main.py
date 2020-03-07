INF = 1e9


class Line(object):

    def __init__(self, a, v):
        self.vector = v
        self.point = a

    def representation_of(self, plane):
        if plane.point_intersection(self) == -INF:
            return -INF

        crossing = plane.point_intersection(self)

        perpendicular = plane.make_perpendicular()

        v_vertical = self.vector.vector_projection_on(perpendicular) * (-1)
        v_horizontal = self.vector + v_vertical

        return Line(crossing, v_vertical + v_horizontal)

    def __str__(self) -> str:
        return "{0} {1} \n".format(self.vector, self.point)

    def __add__(self, other):
        return Line(self.point + other, self.vector + other)

    def __mul__(self, other):
        return Line(self.point * other, self.vector * other)

    def __sub__(self, other):
        return Line(self.point - other, self.vector - other)


class Plane(object):

    def __init__(self, local_a, local_b, local_c):

        self.A = (local_b.y - local_a.y) * (local_c.z - local_a.z) - (local_c.y - local_a.y) * (local_b.z - local_a.z)
        self.B = -((local_b.x - local_a.x) * (local_c.z - local_a.z) - (local_c.x - local_a.x) * (
                local_b.z - local_a.z))
        self.C = (local_b.x - local_a.x) * (local_c.y - local_a.y) - (local_c.x - local_a.x) * (local_b.y - local_a.y)
        self.D = -local_a.x * self.A - local_a.y * self.B - local_a.z * self.C

    def __str__(self) -> str:
        return "{0} {1} {2} {3}".format(self.A, self.B, self.C, self.D)

    def make_perpendicular(self):
        return Vector(self.A, self.B, self.C)

    def point_intersection(self, line):
        local_temp = (self.A * line.vector.x + self.B * line.vector.y + self.C * line.vector.z)

        if local_temp == 0:
            return -INF

        calc = (-1) * (self.A * line.point.x + self.B * line.point.y + self.C * line.point.z + self.D) / local_temp

        if calc <= 0:
            return -INF

        return Vector(line.vector.x * calc + line.point.x, line.vector.y * calc + line.point.y,
                      line.vector.z * calc + line.point.z)


class Vector(object):

    def __init__(self, x, y, z):
        self.x = x
        self.y = y
        self.z = z
        self.count = self.x ** 2 + self.y ** 2 + self.z ** 2

    def __add__(self, other):
        return Vector(self.x + other.x, self.y + other.y, self.z + other.z)

    def __sub__(self, other):
        return Vector(self.x - other.x, self.y - other.y, self.z - other.z)

    def __mul__(self, other):
        if type(other) == type(self):
            return self.x * other.x + self.y * other.y + self.z * other.z
        else:
            return Vector(self.x * other, self.y * other, self.z * other)

    def __str__(self) -> str:
        return "{0} {1} {2}".format(str(self.x), str(self.y), str(self.z))

    def len(self):
        return self.count ** 0.5

    def len_2(self):
        return self.count

    def vector_projection_on(self, b):
        return b * ((self * b) / (b.len_2()))


def read_data():
    rd = Vector(*map(float, file_in.readline().strip().split()))
    return rd


if __name__ == '__main__':
    file_in = open('input.txt', 'r')
    file_out = open('output.txt', 'w')

    A = read_data()
    B = read_data()
    C = read_data()
    D = read_data()

    temp = read_data()
    ray = Line(read_data(), temp)  # луч

    m = int(file_in.readline().strip())
    n = int(file_in.readline().strip())

    mirrors = []
    vector = [B - A, D - C]

    faces = [
        Plane(A, B, C), Plane(D, B, C),
        Plane(A, B, B + vector[1]), Plane(A, C - vector[0], D - vector[0]),
        Plane(C - vector[0], C, D), Plane(A + vector[1], B + vector[1], D)
    ]  # грани

    for i in range(n):
        f = read_data()
        s = read_data()
        t = read_data()
        mirrors.append(Plane(f, s, t))

    flag = False

    while m > 0:
        local_result_array = [-1, -INF, -1]

        for mirror in mirrors:
            temp = ray.representation_of(mirror)
            if temp != -INF:
                dist = (temp.point - ray.point).len()
                if local_result_array[0] == -1 or local_result_array[0] > dist > 0:
                    local_result_array = [dist, temp, 0]

        for face in faces:
            temp = ray.representation_of(face)
            if temp != -INF:
                dist = (temp.point - ray.point).len()
                if local_result_array[0] == -1 or local_result_array[0] > dist > 0:
                    local_result_array = [dist, temp, 1]

        if local_result_array[2] == -1:
            flag = True
            file_out.write('test ne ochen, vse ploho')
            break
        elif local_result_array[2] == 1:
            flag = True
            file_out.write('1\n{0}\n{1}\n{2}'.format(str(m), str(local_result_array[1].point), str(ray.vector)))
            break

        m -= 1
        ray = local_result_array[1]

    if not flag:
        file_out.write('0\n{0}'.format(str(ray.point)))
        file_out.close()
