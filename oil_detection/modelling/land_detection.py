#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Wed Jul  26 16:27:07 2023

@author: julia.titaeva
"""
import math
import numpy as np
import pandas as pd
from PIL import Image


def get_land_mask(output_file_path, window_size_x=13, window_size_y=13):
    # 1 - land, 0 - no land
    # get initial image
    df = pd.read_csv(output_file_path + "temp.txt")
    img = Image.open(output_file_path + df.loc[0, "file"])

    # get oil model image
    arr_oil = np.array(Image.open(output_file_path + "temp_file.png"))
    target_size = (arr_oil.shape[1], arr_oil.shape[0])

    # resize initial image
    arr_resampled = np.array(img.resize(target_size, resample=Image.LANCZOS))

    # get mask
    mask = np.ones((target_size[1], target_size[0]))
    number_x = math.ceil(target_size[1] / window_size_x)
    number_y = math.ceil(target_size[0] / window_size_y)
    for i in range(number_x):
        for j in range(number_y):
            x0 = i * window_size_x
            x1 = (i + 1) * window_size_x
            y0 = j * window_size_y
            y1 = (j + 1) * window_size_y
            if arr_resampled.ndim == 2:
                arr_slice = arr_resampled[x0:x1, y0:y1]
            else:
                arr_slice = arr_resampled[x0:x1, y0:y1, :]
            arr_oil_slice = arr_oil[x0:x1, y0:y1]
            if arr_oil_slice.any():
                mask[x0:x1, y0:y1] = 0
            else:
                if arr_slice.ndim == 2:
                    if arr_slice.mean() >= 100:
                        mask[x0:x1, y0:y1] = 0
                else:
                    if arr_slice[:, :, 2].mean() >= 100 or (
                        arr_slice[:, :, 0].mean() < 70
                        and arr_slice[:, :, 1].mean() < 70
                    ):
                        mask[x0:x1, y0:y1] = 0
    return mask


def apply_mask(mask, output_file_path):
    arr_oil = np.array(Image.open(output_file_path + "temp_file.png"))
    mask_oil = arr_oil / 255.0
    mask_oil = mask_oil.astype(int)

    mask_with_oil = np.copy(mask)
    mask_with_oil[mask_oil == 1] = 2  # 2 - oil, 1 - land, 0 - can't say anything

    # collect image
    channel_red = np.copy(mask_with_oil)
    channel_red[channel_red == 2] = 255
    channel_red[channel_red != 255] = 0

    channel_green = np.copy(mask_with_oil)
    channel_green[channel_green == 1] = 255
    channel_green[channel_green != 255] = 0

    channel_blue = np.zeros(mask.shape)

    img_rgb = Image.fromarray(
        np.dstack(
            (
                channel_red.astype(np.uint8),
                channel_green.astype(np.uint8),
                channel_blue.astype(np.uint8),
            )
        )
    )
    img_rgb.save(output_file_path + "temp_file.png")
