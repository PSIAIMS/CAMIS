from __future__ import annotations

from os.path import dirname, join
from typing import Optional

import pandas as pd


def _load_dataset(
    file_name: str,
    colnames: Optional[list],
    name: str,
    description: str,
    design: dict,
    source: str,
) -> None:

    module_path = dirname(__file__)
    file_path = join(module_path, "data", file_name)
    df = pd.read_csv(file_path)
    if colnames is not None:
        df = df[colnames]
    nrows, ncols = df.shape

    return {
        "name": name,
        "description": description,
        "nrows": nrows,
        "ncols": ncols,
        "data": df,
        "design": design,
        "source": source,
    }


def load_psu_frame():
    name = "PSU Frame"
    description = "A simulated census data."
    design = {}
    source = ""

    return _load_dataset(
        "psu_frame.csv",
        colnames=None,
        name=name,
        description=description,
        design=design,
        source=source,
    )


def load_psu_sample():
    colnames = ["cluster", "region", "psu_prob"]
    name = "PSU Sample"
    description = "The PSU sample obtained from the simulated PSU frame."
    design = {}
    source = ""

    return _load_dataset(
        "psu_sample.csv",
        colnames=colnames,
        name=name,
        description=description,
        design=design,
        source=source,
    )


def load_ssu_sample():
    colnames = ["cluster", "household", "ssu_prob"]
    name = "SSU Sample"
    description = "The SSU sample obtained from the simulated SSU frame."
    design = {}
    source = ""

    return _load_dataset(
        "ssu_sample.csv",
        colnames=colnames,
        name=name,
        description=description,
        design=design,
        source=source,
    )


def load_nhanes2():
    colnames = [
        "stratid",
        "psuid",
        "race",
        "highbp",
        "highlead",
        "zinc",
        "diabetes",
        "finalwgt",
    ]
    name = "NHANES II Subsample"
    description = "A subset of NHANES II data. This file is not meant to be representative of NHANES II. It is just an subset to illustrate the syntax in this tutorial."
    design = {}
    source = ""

    return _load_dataset(
        "nhanes2.csv",
        colnames=colnames,
        name=name,
        description=description,
        design=design,
        source=source,
    )


def load_nhanes2brr():
    colnames = None
    name = "NHANES II Subsample with bootstrap weights"
    description = "A subset of NHANES II data with bootstrap weights. This file is not meant to be representative of NHANES II. It is just an subset to illustrate the syntax in this tutorial."
    design = {}
    source = ""

    return _load_dataset(
        "nhanes2brr_subset.csv",
        colnames=colnames,
        name=name,
        description=description,
        design=design,
        source=source,
    )


def load_nhanes2jk():
    colnames = None
    name = "NHANES II Subsample with jackknife weights"
    description = "A subset of NHANES II data with jackknife weights. This file is not meant to be representative of NHANES II. It is just an subset to illustrate the syntax in this tutorial."
    design = {}
    source = ""

    return _load_dataset(
        "nhanes2jk_subset.csv",
        colnames=colnames,
        name=name,
        description=description,
        design=design,
        source=source,
    )


def load_nmihs():
    colnames = None
    name = "NMIHS Subsample"
    description = "A subset of nmihs data. This file is not meant to be representative of nmihs. It is just an subset to illustrate the syntax in this tutorial."
    design = {}
    source = ""

    return _load_dataset(
        "nmihs_subset.csv",
        colnames=colnames,
        name=name,
        description=description,
        design=design,
        source=source,
    )


def load_auto():
    colnames = None
    name = "Auto Sample"
    description = "The Auto sample data."
    design = {}
    source = ""

    return _load_dataset(
        "auto.csv",
        colnames=colnames,
        name=name,
        description=description,
        design=design,
        source=source,
    )


def load_birth():
    colnames = None
    name = "Birth Sample"
    description = "The Birth sample data."
    design = {}
    source = ""

    return _load_dataset(
        "birth.csv",
        colnames=colnames,
        name=name,
        description=description,
        design=design,
        source=source,
    )


def load_county_crop():
    colnames = [
        "county_id",
        "corn_area",
        "soybeans_area",
        "corn_pixel",
        "soybeans_pixel",
    ]
    name = "County Crop Sample"
    description = "The County Crop Areas sample data."
    design = {}
    source = ""

    return _load_dataset(
        "countycrop.csv",
        colnames=colnames,
        name=name,
        description=description,
        design=design,
        source=source,
    )


def load_county_crop_means():
    colnames = [
        "county_id",
        "samp_segments",
        "pop_segments",
        "ave_corn_pixel",
        "ave_soybeans_pixel",
    ]
    name = "County Crop Area Means"
    description = "The County Crop Area Means data."
    design = {}
    source = ""

    return _load_dataset(
        "countycrop_means.csv",
        colnames=colnames,
        name=name,
        description=description,
        design=design,
        source=source,
    )


def load_expenditure_milk():
    colnames = [
        "major_area",
        "small_area",
        "samp_size",
        "direct_est",
        "std_error",
        "coef_var",
    ]
    name = "Expenditure on Milk"
    description = "The expenditure on milk data."
    design = {}
    source = ""

    return _load_dataset(
        "expenditure_on_milk.csv",
        colnames=colnames,
        name=name,
        description=description,
        design=design,
        source=source,
    )
