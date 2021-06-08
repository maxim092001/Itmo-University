from matplotlib.backends.backend_tkagg import NavigationToolbar2Tk
import tkinter as tk

from plot import Plot
from interop import get_points
from functions import by_view


class Gui:
    def __init__(self, vars_holder):
        # self._root = vars_holder.root
        self._canvas_frame = tk.Frame(vars_holder.root)
        self._view_settings_frame = tk.Frame(vars_holder.root)
        self._params_frame = tk.Frame(vars_holder.root)
        self._steps_frame = tk.Frame(vars_holder.root)
        self._vars_holder = vars_holder
        self._plot = Plot(vars_holder, self._params_frame)

    def _quit(self):
        self._vars_holder.root.quit()
        self._vars_holder.root.destroy()

    def start(self):
        self._setup_root()
        self._setup_plot()
        self._setup_navigation_toolbar()
        self._setup_settings_toolbar()
        self._setup_update_button()
        self._vars_holder.root.mainloop()

    def _setup_root(self):
        self._vars_holder.root.wm_title("Лабараторная 2: Методы многомерной оптимизации")
        self._canvas_frame.pack()
        self._view_settings_frame.pack()
        self._params_frame.pack()
        self._steps_frame.pack()

    def _setup_plot(self, points=None):
        if points is not None:
            self._plot.set_points(points)
            self._plot.draw(True)
        else:
            self._plot.draw()

    def _setup_navigation_toolbar(self):
        toolbar = NavigationToolbar2Tk(self._plot.canvas, self._vars_holder.root)
        toolbar.children['!button4'].pack_forget()
        toolbar.update()
        self._plot.canvas.get_tk_widget().pack(side=tk.TOP, fill=tk.BOTH, expand=1)

    def _setup_settings_toolbar(self):
        holder = self._vars_holder
        widgets = (
            holder.get_checkbutton("arrows", self._view_settings_frame),
            holder.get_checkbutton("axes", self._view_settings_frame),
            holder.get_checkbutton("axes_def", self._view_settings_frame),
            holder.get_checkbutton("contour_colour", self._view_settings_frame),
            holder.get_checkbutton("contour_lines", self._view_settings_frame),

            holder.get_option_menu("method", self._params_frame),
            holder.get_option_menu("function", self._params_frame),
            holder.get_text_field("x0", self._params_frame),
            holder.get_text_field("y0", self._params_frame),
            holder.get_text_field("eps", self._params_frame),

            holder.get_spinbox("step_number", self._steps_frame)
        )

        for widget in widgets:
            widget.pack(side=tk.LEFT)

    def _raise_interop(self):
        self._vars_holder.validate("x0")
        self._vars_holder.validate("y0")
        self._vars_holder.validate("eps")

        points = get_points(
            self._vars_holder["method"].get(),
            by_view(self._vars_holder["function"].get()),
            (float(self._vars_holder["x0"].get()), float(self._vars_holder["y0"].get())),
            float(self._vars_holder["eps"].get())
        )

        self._setup_steps_navigation(points)
        self._setup_plot(points)

    def _setup_update_button(self):
        button = tk.Button(master=self._params_frame, text="Поехали", command=self._raise_interop)
        button.pack(side=tk.LEFT)

    def _setup_steps_navigation(self, points):
        if points is None or len(points) == 0:
            points = (None,)
        self._vars_holder.get_raw_spinbox("step_number$").config(to=len(points)-1)
        self._vars_holder.update_var("step_number_max", str(len(points) - 1))
        self._vars_holder.update_var("step_number", str(len(points) - 1))
