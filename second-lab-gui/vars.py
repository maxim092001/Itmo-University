import tkinter as tk

from functions import function_views


class VarsHolder:
    def __init__(self, root=None):
        self._root = root
        self._name_to_var = {}
        self._name_to_options = {}
        self._name_to_description = {}
        self._name_to_widget = {}
        self._names_order = []

    @property
    def root(self):
        return self._root

    def set_var(self, name, type_, default):
        var = type_(self.root)
        self._name_to_var[name] = var
        var.set(default)
        self._names_order.append(name)
        return var

    def __getitem__(self, item):
        return self._name_to_var.__getitem__(item)

    def set_description(self, name, description):
        self._name_to_description[name] = description

    def set_options(self, name, *args):
        self._name_to_options[name] = args

    def get_option_menu(self, name):
        if name not in self._name_to_widget.keys():
            self._name_to_widget[name] = tk.OptionMenu(self.root, self._name_to_var[name], *self._name_to_options[name])
        return self._name_to_widget[name]

    def get_checkbutton(self, name):
        if name not in self._name_to_widget.keys():
            self._name_to_widget[name] = tk.Checkbutton(self.root, text=self._name_to_description[name],
                                                        variable=self._name_to_var[name])
        return self._name_to_widget[name]

    def get_all_widgets(self):
        result = []
        for name in self._names_order:
            result.append(self._name_to_widget[name])
        return result

    def get_all_vars(self):
        return self._name_to_var.values()

    def get_python_expr(self, name):
        pass


_holder = None


# noinspection PyUnresolvedReferences
def init():
    global _holder
    _holder.set_var("method", tk.StringVar, "Градиентный спуск")
    _holder.set_options("method", "Градиентный спуск", "Наискорейший спуск", "Сопряженные градиенты")

    _holder.set_var("function", tk.StringVar, function_views[0])
    _holder.set_options("function", *function_views)

    _holder.set_var("arrows", tk.BooleanVar, True)
    _holder.set_description("arrows", "Показать/Скрыть стрелки")

    _holder.set_var("axes", tk.BooleanVar, True)
    _holder.set_description("axes", "Показать/Скрыть оси")

    _holder.set_var("axes_def", tk.BooleanVar, True)
    _holder.set_description("axes_def", "Показать/Скрыть подписи к осям")

    _holder.set_var("contour", tk.BooleanVar, True)
    _holder.set_description("contour", "Показать/Скрыть линии уровня")


def get_holder(root):
    global _holder
    if _holder is None:
        _holder = VarsHolder(root)
        init()
    return _holder
