from typing import Protocol, runtime_checkable


@runtime_checkable
class ToDataFramePrcl(Protocol):
    def to_numpy(): ...

    def to_polars(): ...

    def to_pandas(): ...


@runtime_checkable
class SamplePrcl(Protocol):

    def estimate(): ...
    def adjust(): ...


@runtime_checkable
class FramePrcl(Protocol):

    def select(): ...


@runtime_checkable
class Missing(Protocol): ...


@runtime_checkable
class DepVarsPrcl(Protocol):
    # TODO: Add missing values functionality
    y: dict

    @property
    def nrecords(): ...

    @property
    def nvars(): ...

    def to_numpy(): ...

    def to_polars(): ...

    def to_pandas(): ...


@runtime_checkable
class IndepVarsPrcl(Protocol):
    # TODO: Add missing values functionality
    x: dict

    @property
    def nrecords(): ...

    @property
    def nvars(): ...

    def to_numpy(): ...

    def to_polars(): ...

    def to_pandas(): ...


# class FitStatsPrcl(Protocol):
#     method: FitMethod
#     err_stderr: float | dict
#     fe_est: namedtuple  # fixed effects
#     fe_stderr: namedtuple
#     re_stderr: float
#     re_stderr_var: float
