import tkinter as tk

from matplotlib.backends.backend_tkagg import FigureCanvasTkAgg
import matplotlib.pyplot as pyplot
import numpy as np

from functions import by_view


class Plot:
    PRECISION = 800
    BORDERS = 5 / 100

    def __init__(self, vars_holder):
        self._root = vars_holder.root
        self._vars = vars_holder
        for var in vars_holder.get_all_vars():
            var.trace_add("write", self._renew)

        self._figure = pyplot.figure()
        self._canvas = FigureCanvasTkAgg(self._figure, master=self._root)
        self._points = None

    def _renew(self, *args, **kwargs):
        print("_renew")
        self.draw()

    @property
    def canvas(self):
        return self._canvas

    def set_points(self, points):
        self._points = points

    def clear(self):
        self._figure.clf()

    def draw(self):
        self.clear()

        left, bottom, width, height = 0.1, 0.1, 0.8, 0.8
        axes = self._figure.add_axes([left, bottom, width, height])
        self._setup_contour()

        axes.set_title(self._vars["function"].get())
        self._setup_axes_defs(axes)

        self._setup_path(axes)
        self._canvas.draw()
        self._canvas.flush_events()
        self._canvas.get_tk_widget().pack(side=tk.TOP, fill=tk.BOTH, expand=1)


    def _find_limits(self, func):
        if self._points is None or len(self._points) == 0:
            return func.default_x_start, func.default_x_end, func.default_y_start, func.default_y_end
        return (
            self._points.min_x - self._points.delta_x * Plot.BORDERS,
            self._points.max_x + self._points.delta_x * Plot.BORDERS,
            self._points.min_y - self._points.delta_y * Plot.BORDERS,
            self._points.max_y + self._points.delta_y * Plot.BORDERS
        )

    def _setup_contour(self):
        if self._vars["contour"].get():
            func = by_view(self._vars["function"].get())
            xs, xe, ys, ye = self._find_limits(func)
            x_vals = np.linspace(xs, xe, Plot.PRECISION)
            y_vals = np.linspace(ys, ye, Plot.PRECISION)
            X, Y = np.meshgrid(x_vals, y_vals)
            Z = eval(func.py)
            cp = pyplot.contourf(X, Y, Z)
            pyplot.colorbar(cp)

    def _setup_axes_defs(self, axes):
        if self._vars["axes_def"].get():
            axes.set_xlabel('x')
            axes.set_ylabel('y')

    def _setup_path(self, axes):
        if self._points is not None:
            for i in range(len(self._points) - 1):
                # axes.arrow(self._points.xs[i], self._points.ys[i], self._points.xs[i + 1] - self._points.xs[i],
                #            self._points.ys[i + 1] - self._points.ys[i],
                #            head_width=0.25, head_length=0.11, length_includes_head=True, fc='k', ec='k')

                axes.annotate("s", xy=self._points[i + 1], xytext=self._points[i], arrowprops={
                    "arrowstyle": "->",
                    "lw": 1,
                    "color": "r"
                }, va="center", ha="center")
