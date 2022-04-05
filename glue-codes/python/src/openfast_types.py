from ctypes import (POINTER, Structure, c_char,
                    c_double, c_float, c_int, c_void_p)
from typing import Any, Type, List

import numpy as np

real = np.float32


def make_int(name: str):
    return (name, c_int)


def make_float(name: str):
    return (name, c_float)


def make_double(name: str):
    return (name, c_double)


def make_char(name: str):
    return (name, c_char)


def make_arr(name: str, _dtype: Type):
    return (name, POINTER(np.ctypeslib.as_ctypes_type(_dtype))), make_int(name+"_Len")


class ArrStructWrapper:
    struct_type: type

    def __init__(self, *args, **kwargs):
        self.c_struct: Structure = self.struct_type()
        # for name, dtype in self.c_struct._arrays_:
        # initialize valid pointer
        # init = c_int(0)
        # setattr(self.c_struct, name+"_Len", init)
        # setattr(self, name, np.zeros(1, dtype=dtype))

    def __getattr__(self, __name: str):
        arr = getattr(self.c_struct, __name)
        length = getattr(self.c_struct, __name+"_Len")
        return np.ctypeslib.as_array(arr, shape=(length,))

    def __setattr__(self, __name: str, __value: Any) -> None:
        if __name == "c_struct":
            self.__dict__[__name] = __value
            return
        if hasattr(self.c_struct, __name):
            setattr(self.c_struct, __name, np.ctypeslib.as_ctypes(__value))
            setattr(self.c_struct, __name+"_Len", __value.shape[0])
        else:
            super().__setattr__(__name, __value)

    def copy_to_arr(self, name: str, new_arr: np.ndarray):
        np.copyto(getattr(self, name), new_arr)

    @property
    def names(self):
        return [field[0] for field in self.struct_type._arrays_]


def make_array_fields(arrays: List):
    return [field for pair in [make_arr(name, dtype) for name, dtype in arrays] for field in pair]


class OpFM_InputType_C(Structure):  # output from fast
    _arrays_ = [
        ("pxVel", real),
        ("pyVel", real),
        ("pzVel", real),
        ("pxForce", real),
        ("pyForce", real),
        ("pzForce", real),
        ("xdotForce", real),
        ("ydotForce", real),
        ("zdotForce", real),
        ("pOrientation", real),
        ("fx", real),
        ("fy", real),
        ("fz", real),
        ("momentx", real),
        ("momenty", real),
        ("momentz", real),
        ("forceNodesChord", real)
    ]
    _fields_ = [("object", c_void_p)] + make_array_fields(_arrays_)


class OpFM_InputType(ArrStructWrapper):
    struct_type = OpFM_InputType_C


class OpFM_OutputType_C(Structure):  # input to fast
    _arrays_ = [("u", real), ("v", real),
                ("w", real), ("WriteOutput", real), ]
    _fields_ = [("object", c_void_p)] + make_array_fields(_arrays_)


class OpFM_OutputType(ArrStructWrapper):
    struct_type = OpFM_OutputType_C


class SC_DX_InputType_C(Structure):
    _arrays_ = [("toSc", real)]
    _fields_ = [("object", c_void_p)] + make_array_fields(_arrays_)


class SC_DX_InputType(ArrStructWrapper):
    struct_type = SC_DX_InputType_C


class SC_DX_OutputType_C(Structure):
    _arrays_ = [("fromSc", real), ("fromScGlob", real)]
    _fields_ = [("object", c_void_p)] + make_array_fields(_arrays_)


class SC_DX_OutputType(ArrStructWrapper):
    struct_type = SC_DX_OutputType_C
