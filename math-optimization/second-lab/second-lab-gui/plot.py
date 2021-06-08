import tkinter as tk

from matplotlib.backends.backend_tkagg import FigureCanvasTkAgg
import matplotlib.pyplot as pyplot
import numpy as np

from functions import by_view
from mylogging import log


class Plot:
    PRECISION = 400
    BORDERS = 5 / 100
    LEVELS = 20

    @log
    def __init__(self, vars_holder, root):
        self._vars = vars_holder
        for var in vars_holder.get_all_vars():
            var.trace_add("write", self._renew)

        self._figure = pyplot.figure()
        self._canvas = FigureCanvasTkAgg(self._figure, master=root)
        self._points = None

    @log
    def _renew(self, *args, **kwargs):
        name = args[0]
        self.draw(self._vars.save_path(name))

    @property
    def canvas(self):
        return self._canvas

    @log
    def set_points(self, points):
        self._points = points

    def clear(self):
        self._figure.clf()

    @log
    def draw(self, save_path=False):
        self.clear()

        if not save_path:
            self._points = None

        left, bottom, width, height = 0.1, 0.1, 0.8, 0.8
        axes = self._figure.add_axes([left, bottom, width, height])

        self._setup_contour()
        self._setup_axes_defs(axes)
        self._setup_axes(axes)
        self._setup_path(axes)
        self._canvas.draw()
        self._canvas.flush_events()
        self._canvas.get_tk_widget().pack(side=tk.TOP, fill=tk.BOTH, expand=1)

    def _find_limits(self, func):
        if True or self._points is None or len(self._points) == 0:
            return func.default_x_start, func.default_x_end, func.default_y_start, func.default_y_end
        return (
            self._points.min_x - self._points.delta_x * Plot.BORDERS,
            self._points.max_x + self._points.delta_x * Plot.BORDERS,
            self._points.min_y - self._points.delta_y * Plot.BORDERS,
            self._points.max_y + self._points.delta_y * Plot.BORDERS
        )

    def _setup_contour(self):
        func = by_view(self._vars["function"].get())
        xs, xe, ys, ye = self._find_limits(func)
        x_vals = np.linspace(xs, xe, Plot.PRECISION)
        y_vals = np.linspace(ys, ye, Plot.PRECISION)
        X, Y = np.meshgrid(x_vals, y_vals)
        Z = eval(func.py)
        if self._vars["contour_colour"].get():
            cp_colours = pyplot.contourf(X, Y, Z, levels=Plot.LEVELS)
            if self._vars["axes"].get():
                pyplot.colorbar(cp_colours)
        if self._vars["contour_lines"].get():
            cp_lines = pyplot.contour(X, Y, Z, levels=Plot.LEVELS, colors="black")
            if self._vars["axes"].get():
                pyplot.clabel(cp_lines, inline=1, fontsize=8)
        if not self._vars["contour_colour"].get() and not self._vars["contour_lines"].get():
            pyplot.contour(X, Y, Z, levels=Plot.LEVELS, colors="white")

    def _setup_axes_defs(self, axes):
        if self._vars["axes_def"].get():
            axes.set_title(self._vars["function"].get())
            axes.set_xlabel('x')
            axes.set_ylabel('y')

    @log
    def _setup_path(self, axes):
        n = int(self._vars["step_number"].get())
        if self._points is not None and n > 0:
            for i in range(min(n, len(self._points) - 1)):
                axes.annotate("", xy=self._points[i + 1], xytext=self._points[i], arrowprops={
                    "arrowstyle": ("->" if self._vars["arrows"].get() else "-"),
                    "lw": 1,
                    "color": "r"
                }, va="center", ha="center")

    def _setup_axes(self, axes):
        if not self._vars["axes"].get():
            axes.axis("off")
