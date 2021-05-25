from matplotlib.backends.backend_tkagg import NavigationToolbar2Tk
import tkinter as tk

from plot import Plot
from interop import get_points
from functions import by_view


class Gui:
    def __init__(self, vars_holder):
        self._root = vars_holder.root
        self._vars_holder = vars_holder
        self._plot = Plot(vars_holder)

    def _quit(self):
        self._root.quit()
        self._root.destroy()

    def start(self):
        self._setup_root()
        self._setup_plot()
        self._setup_navigation_toolbar()
        self._setup_settings_toolbar()
        self._setup_update_button()
        self._root.mainloop()

    def _setup_root(self):
        self._root.wm_title("Лабараторная 2: Методы многомерной оптимизации")

    def _setup_plot(self, points=None):
        if points is not None:
            self._plot.set_points(points)
        self._plot.draw()

    def _setup_navigation_toolbar(self):
        toolbar = NavigationToolbar2Tk(self._plot.canvas, self._root)
        toolbar.update()
        self._plot.canvas.get_tk_widget().pack(side=tk.TOP, fill=tk.BOTH, expand=1)

    def _setup_settings_toolbar(self):
        holder = self._vars_holder
        holder.get_option_menu("method")
        holder.get_option_menu("function")
        holder.get_checkbutton("arrows")
        holder.get_checkbutton("axes")
        holder.get_checkbutton("axes_def")
        holder.get_checkbutton("contour")
        for x in holder.get_all_widgets():
            x.pack(side=tk.LEFT)

    def _raise_interop(self):
        self._setup_plot(get_points(
            self._vars_holder["method"].get(), by_view(self._vars_holder["function"].get()), (15, 10), 1e-4))

    def _setup_update_button(self):
        button = tk.Button(master=self._root, text="Поехали", command=self._raise_interop)
        button.pack(side=tk.LEFT)
