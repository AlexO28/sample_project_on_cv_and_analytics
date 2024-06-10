#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Fri Mar  3 10:43:06 2023

@author: alexey.osipov
"""
import os
import argparse
import rasterio
from rasterio.windows import Window
import math
import numpy as np

from convert_degrees_to_meters import get_slice_size


def split_all_tifs(
    input_folder,
    output_folder,
    data_prefix,
    flag,
    init_pos=0,
    window_size_x=1250,
    window_size_y=650,
):
    files = os.listdir(input_folder)[init_pos:]
    for file in files:
        print(f"{file} is being processed")
        split_tif_file(
            input_folder,
            output_folder,
            file,
            data_prefix,
            flag,
            window_size_x,
            window_size_y,
        )


def split_tif_file(
    input_folder,
    output_folder,
    file,
    data_prefix,
    flag,
    window_size_x=1250,
    window_size_y=650,
):
    print("processing started")
    arr = read_tif_file(input_folder, file)
    number_x = math.ceil(arr.shape[1] / window_size_x)
    number_y = math.ceil(arr.shape[0] / window_size_y)
    del arr
    for i in range(number_x):
        for j in range(number_y):
            x0 = i * window_size_x
            x1 = (i + 1) * window_size_x
            y0 = j * window_size_y
            y1 = (j + 1) * window_size_y
            arr = read_tif_file_slice(input_folder, file, x0=x0, x1=x1, y0=y0, y1=y1)
            if flag:
                im_size_x, im_size_y = get_slice_size(
                    input_folder, file, x0, x1, y0, y1
                )
            else:
                im_size_x, im_size_y = "", ""
            array_slice = extract_array_subset(arr, window_size_y, window_size_x)
            write_into_png_file(
                array_slice,
                output_folder,
                file,
                data_prefix,
                i,
                j,
                im_size_x,
                im_size_y,
                window_size_x,
                window_size_y,
            )


def extract_array_subset(arr, window_size_y, window_size_x):
    if (arr.shape[1] == window_size_y) & (arr.shape[2] == window_size_x):
        return arr
    new_arr = np.full([arr.shape[0], window_size_y, window_size_x], 255)
    for k in range(arr.shape[0]):
        for i in range(arr.shape[1]):
            for j in range(arr.shape[2]):
                new_arr[k, i, j] = arr[k, i, j]
    return new_arr


def write_into_png_file(
    arr,
    folder,
    file,
    data_prefix,
    i,
    j,
    im_size_x,
    im_size_y,
    window_size_x,
    window_size_y,
):
    file_splitted = file.split(".")
    if len(im_size_x) > 0 and len(im_size_y) > 0:
        file = (
            data_prefix
            + "_"
            + file_splitted[0]
            + "_"
            + str(i)
            + "_"
            + str(j)
            + "_"
            + str(im_size_x)
            + "_"
            + str(im_size_y)
            + ".png"
        )
    else:
        file = (
            data_prefix + "_" + file_splitted[0] + "_" + str(i) + "_" + str(j) + ".png"
        )
    with rasterio.open(
        folder + file,
        "w",
        driver="png",
        width=window_size_x,
        height=window_size_y,
        count=arr.shape[0],
        dtype="uint8",
    ) as dst:
        dst.write(arr)


def read_tif_file_slice(folder, file, x0, x1, y0, y1):
    with rasterio.open(folder + file, "r") as src:
        arr = src.read(window=Window(x0, y0, x1 - x0, y1 - y0))
    return arr


def read_tif_file(folder, file):
    with rasterio.open(folder + file, "r") as src:
        arr = src.read(1)
    return arr


if __name__ == "__main__":
    parser = argparse.ArgumentParser(
        prog="Processing", description="Script to process maxar data"
    )
    parser.add_argument(
        "--input",
        type=str,
        help="Absolute path to input folder with tiff images to process",
    )
    parser.add_argument(
        "--output",
        type=str,
        help="Absolute path to output folder to save processed png images",
    )
    parser.add_argument(
        "--prefix", type=str, help="Prefix for naming input data", default="maxar"
    )
    parser.add_argument("--flag", action="store_true")
    parser.add_argument("--no-flag", dest="flag", action="store_false")
    args = parser.parse_args()

    if not os.path.exists(args.output):
        os.makedirs(args.output)

    split_all_tifs(args.input, args.output, args.prefix, args.flag)
