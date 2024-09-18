import pandas as pd
import polars as pl

from samplics.apis.sampling.core import _grs_select
from samplics.apis.sampling.sample import Sample
from samplics.types.basic import DataFrame
from samplics.types.containers import SampleDesign
from samplics.types.protocols import FramePrcl
from samplics.utils.types import SelectMethod


class Frame(FramePrcl):
    data: dict
    schema: dict

    __slots__ = ["data", "schema"]

    def __init__(self, data: dict | DataFrame, schema: dict | None = None):
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

    def select(self, method: SelectMethod, design: SampleDesign) -> Sample:

        match method:
            case SelectMethod.grs:
                self.data["samp"], self.data["hits"] = -_grs_select(
                    probs=self.data["probs"],
                    samp_unit=self.data["id"],
                    samp_size=self.data["size"],
                    stratum=self.data["stratum"],
                    psu=self.data["psu"],
                    # ssu=self.data["ssu"],
                    wr=self.data["wr"],
                )
            case SelectMethod.srs:
                pass
            case SelectMethod.sys:
                pass
            case SelectMethod.pps:
                pass
            case SelectMethod.pps_wr:
                pass
            case SelectMethod.pps_brewer:
                pass
            case SelectMethod.pps_hv:
                pass
            case SelectMethod.pps_murphy:
                pass
            # case SelectMethod.pps_rs:
            #     pass
            case _:
                raise ValueError("Invalid method!")


# Unit tests

# from samplics.apis.sampling import Frame


def test_frame_dict_initialization():
    data = {
        "id": [1, 2, 3, 4, 5],
        "age": [23, 45, 67, 89, 12],
    }
    schema = None
    frame = Frame(data, schema)
    assert frame.data == data
    assert frame.schema == schema
