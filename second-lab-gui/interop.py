import sys
import requests

from functions import functions


class Points:
    def __init__(self, points_list):
        self._points_list = points_list
        self._slices = {}

    def __getitem__(self, item):
        return self._points_list.__getitem__(item)

    def __iter__(self):
        return self._points_list.__iter__()

    def __len__(self):
        return self._points_list.__len__()

    def _slice(self, j):
        if j not in self._slices.keys():
            self._slices[j] = [point[j] for point in self._points_list]
        return self._slices[j]

    @property
    def xs(self):
        return self._slice(0)

    @property
    def ys(self):
        return self._slice(1)

    @property
    def max_x(self):
        return max(self.xs)

    @property
    def min_x(self):
        return min(self.xs)

    @property
    def max_y(self):
        return max(self.ys)

    @property
    def min_y(self):
        return min(self.ys)

    @property
    def delta_x(self):
        return self.max_x - self.min_x

    @property
    def delta_y(self):
        return self.max_y - self.min_y


def _to_java(method, func):
    if method[0] == "Г":
        return "GradientDescent", func.alphas[0]
    elif method[0] == "Н":
        return "GradientDescentFastest", func.alphas[1]
    else:
        return "ConjugateGradient", 0


def get_points(method, func, start_point, eps) -> Points:
    response = requests.post("http://localhost:9991/", json={
        "method": _to_java(method, func)[0],
        "function": func.coefs,
        "alpha": _to_java(method, func)[1],
        "startPoint": start_point,
        "eps": eps
    })
    if response.status_code != 200:
        print("Error", response, file=sys.stderr)
        return Points([])
    return Points(response.json())


if __name__ == '__main__':
    get_points("f", functions[0], (15, 10), 1e-4)
