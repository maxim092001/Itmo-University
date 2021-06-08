import tkinter as tk
import sys

from functions import function_views


def _cached_widget(method):
    def wrapped_method(self, name, root=None):
        if name not in self._name_to_widget.keys():
            if root is None:
                raise Exception
            self._name_to_widget[name] = method(self, name, root)
        return self._name_to_widget[name]

    return wrapped_method


class VarsHolder:
    def __init__(self, root=None):
        self._root = root
        self._name_to_var = {}
        self._name_to_options = {}
        self._name_to_description = {}
        self._name_to_widget = {}
        self._names_order = []
        self._name_to_validator = {}
        self._name_to_save_path = {}

    @property
    def root(self):
        return self._root

    def save_path(self, name):
        return self._name_to_save_path[name]

    def set_var(self, name, type_, default, save_path=False):
        var = type_(self.root, name=name)
        self._name_to_var[name] = var
        var.set(default)
        self._names_order.append(name)
        self._name_to_save_path[name] = save_path
        return var

    def update_var(self, name, value):
        self._name_to_var[name].set(value)

    def __getitem__(self, item):
        return self._name_to_var.__getitem__(item)

    def set_description(self, name, description):
        self._name_to_description[name] = description

    def set_options(self, name, *args):
        self._name_to_options[name] = args

    def set_validator(self, name, validator):
        self._name_to_validator[name] = validator

    def validate(self, name):
        if name in self._name_to_validator.keys():
            valid, msg = self._name_to_validator[name](self._name_to_var[name].get())
            if not valid:
                print(f"{name} is not valid: {msg}", file=sys.stderr)
            return valid, msg
        else:
            return True, ""

    @_cached_widget
    def get_option_menu(self, name, root):
        return tk.OptionMenu(root, self._name_to_var[name], *self._name_to_options[name])

    @_cached_widget
    def get_checkbutton(self, name, root):
        return tk.Checkbutton(root, text=self._name_to_description[name], variable=self._name_to_var[name])

    @_cached_widget
    def _get_frame(self, name, root):
        return tk.Frame(root)

    @_cached_widget
    def _get_label(self, name, root):
        if "$" in name:
            name = name[:name.find("$")]
        return tk.Label(root, text=self._name_to_description[name])

    @_cached_widget
    def get_text_field(self, name, root):
        frame = self._get_frame(name + "$frame", root)
        label = self._get_label(name, frame)
        entry = tk.Entry(frame, textvariable=self._name_to_var[name], width=8)
        label.pack(side=tk.LEFT)
        entry.pack(side=tk.LEFT)
        return frame

    @_cached_widget
    def get_raw_spinbox(self, name, root):
        if "$" in name:
            name = name[:name.find("$")]
        return tk.Spinbox(root, from_=0, to=10, textvariable=self._name_to_var[name], width=7)

    @_cached_widget
    def get_raw_label(self, name, root):
        return tk.Label(root, textvariable=self._name_to_var[name])

    @_cached_widget
    def get_spinbox(self, name, root):
        frame = self._get_frame(name + "$frame", root)
        left_label = self._get_label(name + "$left_label", frame)
        spinbox = self.get_raw_spinbox(name + "$", frame)
        right_label = self._get_label(name + "_max$right_label", frame)
        out_label = self.get_raw_label(name + "_max", frame)
        left_label.pack(side=tk.LEFT)
        spinbox.pack(side=tk.LEFT)
        right_label.pack(side=tk.LEFT)
        out_label.pack(side=tk.LEFT)
        return frame

    def get_all_widgets(self):
        result = []
        for name in self._names_order:
            result.append(self._name_to_widget[name])
        return result

    def get_all_vars(self):
        result = []
        for key in self._name_to_var.keys():
            if not key.startswith("step_number_max"):
                result.append(self._name_to_var[key])
        return result


_holder = None


# noinspection PyUnresolvedReferences
def init():
    global _holder
    _holder.set_var("method", tk.StringVar, "Градиентный спуск")
    _holder.set_options("method", "Градиентный спуск", "Наискорейший спуск", "Сопряженные градиенты")

    _holder.set_var("function", tk.StringVar, function_views[0])
    _holder.set_options("function", *function_views)

    _holder.set_var("arrows", tk.BooleanVar, True, True)
    _holder.set_description("arrows", "Показать стрелки")

    _holder.set_var("axes", tk.BooleanVar, True, True)
    _holder.set_description("axes", "Показать оси")

    _holder.set_var("axes_def", tk.BooleanVar, True, True)
    _holder.set_description("axes_def", "Показать подписи к осям")

    _holder.set_var("contour_colour", tk.BooleanVar, True, True)
    _holder.set_description("contour_colour", "Показать цвета уровня")

    _holder.set_var("contour_lines", tk.BooleanVar, True, True)
    _holder.set_description("contour_lines", "Показать линии уровня")

    _holder.set_var("x0", tk.StringVar, "10", True)
    _holder.set_description("x0", "X0")

    _holder.set_var("y0", tk.StringVar, "15", True)
    _holder.set_description("y0", "Y0")

    _holder.set_var("eps", tk.StringVar, "1e-3", True)
    _holder.set_description("eps", "Eps")

    _holder.set_var("step_number", tk.StringVar, "0", True)
    _holder.set_description("step_number", "Шаг ")

    _holder.set_var("step_number_max", tk.StringVar, "0", True)
    _holder.set_description("step_number_max", " из ")


def get_holder(root):
    global _holder
    if _holder is None:
        _holder = VarsHolder(root)
        init()
    return _holder
