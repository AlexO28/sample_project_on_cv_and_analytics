#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Mon Mar  6 10:51:53 2023

@author: alexey.osipov
"""

import os
import pandas as pd
import rasterio
import numpy as np

THRESHOLD_FOR_BLACK = 30
THRESHOLD_FOR_BLUE = 30
THRESHOLD_FOR_BROWN = 30


def calculate_statistics(folder):
    files = os.listdir(folder)
    results = pd.DataFrame(
        columns=[
            "file",
            "mean_1",
            "mean_2",
            "mean_3",
            "share_black",
            "share_blue",
            "share_brown",
        ]
    )
    for i in range(len(files)):
        print(i)
        file = files[i]
        results_for_file = calculate_statistics_for_file(file, folder)
        results.loc[i] = results_for_file
    return results


def calculate_statistics_for_file(file, folder):
    arr = read_png_file(folder, file)
    means = [-1] * 3
    for i in range(len(arr)):
        means[i] = arr[i].mean()
    share_black = calculate_share_black(arr)
    share_blue = calculate_share_blue(arr)
    share_brown = calculate_share_brown(arr)
    return [file] + means + [share_black, share_blue, share_brown]


def read_png_file(folder, file):
    with rasterio.open(folder + file, "r") as src:
        arr = src.read()
    return arr


def calculate_share_black(arr):
    res = arr[0] < THRESHOLD_FOR_BLACK
    if len(arr) > 1:
        for i in range(1, len(arr)):
            res = res & arr[i]
    return calculate_share(res)


def calculate_share(res):
    share = np.count_nonzero(res) / (res.shape[0] * res.shape[1])
    return share


def calculate_share_blue(arr):
    if len(arr) == 3:
        res = arr[2] > (255 - THRESHOLD_FOR_BLUE)
        return calculate_share(res)
    else:
        return 0


def calculate_share_brown(arr):
    if len(arr) == 3:
        res = arr[1] > (255 - THRESHOLD_FOR_BROWN)
        return calculate_share(res)
    else:
        return 0
