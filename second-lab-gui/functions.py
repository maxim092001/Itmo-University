class Function:
    def __init__(self, numpy_str, view, default_x_start, default_x_end, default_y_start, default_y_end, alphas, *args):
        self._numpy_str = numpy_str
        self._view = view
        self._default_x_start = default_x_start
        self._default_x_end = default_x_end
        self._default_y_start = default_y_start
        self._default_y_end = default_y_end
        self._coefs = args
        self._alphas = alphas

    def __str__(self):
        return self._view

    @property
    def coefs(self):
        return self._coefs

    @property
    def py(self):
        return self._numpy_str

    @property
    def default_x_start(self):
        return self._default_x_start

    @property
    def default_x_end(self):
        return self._default_x_end

    @property
    def default_y_start(self):
        return self._default_y_start

    @property
    def default_y_end(self):
        return self._default_y_end

    @property
    def alphas(self):
        return self._alphas


functions = [
    Function("X**2 + Y**2", "x^2 + y^2", -16, 16, -16, 16, (1, 10**6), 1, 0, 1, 0, 0, 0),
    Function(
        "X**2 + 2 * X * Y + 3 * Y**2 + 4 * X + 5 * Y + 6",
        "x^2 + 2xy + 3y^2 + 4x + 5y + 6",
        -20, 35, -20, 20, (0.25, 10**6),
        1, 2, 3, 4, 5, 6
    ),
    Function(
        "64 * X**2 + 126 * X * Y + 64 * Y**2 - 10 * X + 30 * Y + 13",
        "64x^2 + 126xy + 64y^2 - 10x + 30y + 13",
        -20, 35, -35, 20, (1/128, 10**6),
        64, 126, 64, -10, 30, 13
    )
]

function_views = list(map(str, functions))


def by_view(function_view):
    for func in functions:
        if str(func) == function_view:
            return func
    raise Exception
