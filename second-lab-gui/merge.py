import tkinter as tk

from matplotlib.backends.backend_tkagg import (
    FigureCanvasTkAgg, NavigationToolbar2Tk)
import matplotlib.pyplot as plt
# Implement the default Matplotlib key bindings.
from matplotlib.backend_bases import key_press_handler
from matplotlib.figure import Figure

import numpy as np

from interop import get_steps

root = tk.Tk()
root.wm_title("Embedding in Tk")


frame1 = tk.Frame()
frame2 = tk.Frame()
frame1.pack()
frame2.pack()

fig = plt.figure(figsize=(6,5))
left, bottom, width, height = 0.1, 0.1, 0.8, 0.8
ax = fig.add_axes([left, bottom, width, height])

start, stop, n_values = -8, 8, 800

x_vals = np.linspace(start, stop, n_values)
y_vals = np.linspace(start, stop, n_values)
X, Y = np.meshgrid(x_vals, y_vals)
Z = np.sqrt(X**2 + Y**2)

cp = plt.contourf(X, Y, Z)
plt.colorbar(cp)

# plt.subplot()

ax.set_title('Contour Plot')
ax.set_xlabel('x (cm)')
ax.set_ylabel('y (cm)')

t = np.arange(0, 3, .01)

canvas = FigureCanvasTkAgg(fig, master=frame1)  # A tk.DrawingArea.
canvas.draw()
canvas.get_tk_widget().pack(side=tk.TOP, fill=tk.BOTH, expand=1)

#########################################

points = get_steps(*[None] * 3)
xs = [p[0] for p in points]
ys = [p[1] for p in points]

# plt.plot(xs, ys)

for i in range(len(points) - 1):
    ax.arrow(xs[i], ys[i], xs[i + 1] - xs[i], ys[i + 1] - ys[i], head_width=0.25, head_length=0.11, length_includes_head=True, fc='k', ec='k')


########################################

toolbar = NavigationToolbar2Tk(canvas, root)
toolbar.update()
canvas.get_tk_widget().pack(side=tk.TOP, fill=tk.BOTH, expand=1)


def on_key_press(event):
    print("you pressed {}".format(event.key))
    key_press_handler(event, canvas, toolbar)


canvas.mpl_connect("key_press_event", on_key_press)


def _quit():
    root.quit()     # stops mainloop
    root.destroy()  # this is necessary on Windows to prevent
                    # Fatal Python Error: PyEval_RestoreThread: NULL tstate


button = tk.Button(master=root, text="Завалить ебало", command=_quit)
button.pack(side=tk.BOTTOM)


method_name = tk.StringVar(root)
method_name.set("Градиентный спуск")
method_chooser = tk.OptionMenu(root, method_name, "Градиентный спуск", "Наискорейший спуск", "Сопряженные градиенты")
method_chooser.pack()

function_name = tk.StringVar(root)
function_name.set("Градиентный спуск")
function_chooser = tk.OptionMenu(root, function_name, "Градиентный спуск", "Наискорейший спуск", "Сопряженные градиенты")
function_chooser.pack()

show_arrows = tk.IntVar()
show_arrows_chooser = tk.Checkbutton(root, text="Показать/Скрыть стрелки", variable=show_arrows)
show_arrows_chooser.pack()

show_axes = tk.IntVar()
show_axes_chooser = tk.Checkbutton(root, text="Показать/Скрыть оси", variable=show_axes)
show_axes_chooser.pack()

show_axes_def = tk.IntVar()
show_axes_def_chooser = tk.Checkbutton(root, text="Показать/Скрыть подписи к осям", variable=show_axes_def)
show_axes_def_chooser.pack()

show_contur = tk.IntVar()
show_contur_chooser = tk.Checkbutton(root, text="Показать/Скрыть линии уровня", variable=show_contur)
show_contur_chooser.pack()

x_start, y_start = tk.StringVar(), tk.StringVar()
eps = tk.StringVar()

x_start_input = tk.Entry(root, textvariable=x_start)
x_start_input.pack()
y_start_input = tk.Entry(root, textvariable=y_start)
y_start_input.pack()
eps_input = tk.Entry(root, textvariable=eps)
eps_input.pack()


tk.mainloop()
# If you put root.destroy() here, it will cause an error if the window is
# closed with the window manager.