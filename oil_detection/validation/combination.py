#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Thu Sep 14 09:26:00 2023

@author: alexey.osipov
"""
import os
import cv2
import numpy as np
from skimage.transform import rescale


def combine_small_pictures(
    prefix,
    magnification,
    input_folder,
    default_image="maxar_12thAugust_3_3_208.png",
):
    files = os.listdir(input_folder)
    files = [file for file in files if prefix in file]
    x_max, y_max = extract_max_numbers(files)
    res = []
    for i in range(x_max + 1):
        row = []
        print(i)
        for j in range(y_max + 1):
            file_name = form_file_name(prefix, i, j)
            if file_name not in files:
                print([i, j])
                print("skip")
                file_name = default_image
            img = cv2.imread(input_folder + file_name)
            if len(img.shape) == 3:
                img = rescale(
                    img, 1 / magnification, anti_aliasing=True, channel_axis=2
                )
            else:
                img = rescale(img, 1 / magnification, anti_aliasing=True)
            row.append(img)
        row = np.hstack(row)
        res.append(row)
    return np.vstack(res)


def extract_max_numbers(files):
    x_max = 0
    y_max = 0
    for file in files:
        file_str = file.split(".")[0].split("_")
        x_max = max(int(file_str[-2]), x_max)
        y_max = max(int(file_str[-1]), y_max)
    return x_max, y_max


def form_file_name(prefix, i, j):
    return prefix + "_" + str(i) + "_" + str(j) + ".png"
