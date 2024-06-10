#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Thur Jul  6 11:12:25 2023

@author: julia.titaeva
"""

import math
import rasterio


def get_slice_size(folder, file, x0, x1, y0, y1):
    with rasterio.open(folder + file, "r") as src:
        lon1, lat1 = src.xy(row=y0, col=x0)
        lon2, lat2 = src.xy(row=y1, col=x1)
    im_size_x = int(calc_distance(lat1, lon1, lat1, lon2))
    im_size_y = int(calc_distance(lat1, lon1, lat2, lon1))
    return str(im_size_x), str(im_size_y)


def calc_distance(lat1, lon1, lat2, lon2):
    """
    Calculate the great circle distance between two points
    on the earth (specified in decimal degrees)
    """
    # convert decimal degrees to radians
    lon1, lat1, lon2, lat2 = map(math.radians, [lon1, lat1, lon2, lat2])
    # haversine formula
    dlon = lon2 - lon1
    dlat = lat2 - lat1
    a = (
        math.sin(dlat / 2) ** 2
        + math.cos(lat1) * math.cos(lat2) * math.sin(dlon / 2) ** 2
    )
    c = 2 * math.asin(math.sqrt(a))
    m = 6371 * c * 1000
    return m
