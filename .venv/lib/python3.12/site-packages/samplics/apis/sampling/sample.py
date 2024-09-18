import pandas as pd
import polars as pl

from samplics.types.basic import DataFrame
from samplics.types.protocols import SamplePrcl


class Sample(SamplePrcl):
    data: dict
    schema: dict

    __slots__ = ["data", "schema"]

    def __init__(self, data: dict | DataFrame, schema: dict):
        match data:
            case dict():
                self.data = data
            case pd.DataFrame:
                self.data = data.to_dict()
            case pl.DataFrame:
                self.data = data.to_dict()
            case _:
                raise TypeError("data must be a dictionary or a DataFrame!")

        self.schema = schema

    def estimate(self):
        pass

    def adjust(self):
        pass
