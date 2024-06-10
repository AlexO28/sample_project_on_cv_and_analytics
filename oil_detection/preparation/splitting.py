#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Tue Mar  7 09:54:33 2023

@author: alexey.osipov
"""
import pandas as pd
import shutil
import numpy as np
import rasterio
import os
from sklearn.model_selection import train_test_split
from calculate_statistics import read_png_file

THRESHOLD_FOR_BLACK = 20
TRAIN_RATIO = 0.8
RANDOM_STATE = 239


def prepare_the_initial_split(file_path, share=0.3):
    statistical_data = pd.read_csv(file_path)
    statistical_data = statistical_data.iloc[:, 1:]
    files_16th = statistical_data.loc[
        statistical_data["file"].str.contains("16thJuly"), "file"
    ].unique()
    files_other = statistical_data[
        ~statistical_data["file"].str.contains("16thJuly")
        & (statistical_data["share_black"] >= 0.001)
    ].drop_duplicates()
    target_length = round(len(statistical_data) * 0.3 - len(files_16th))
    files_other = files_other.sample(n=target_length)["file"].unique()
    all_sample = list(files_16th) + list(files_other)
    statistical_data = statistical_data[statistical_data["file"].isin(all_sample)]
    return statistical_data


def split_data_by_prepared_split(split_data, src_folder, dst_folder):
    split_data.reset_index(inplace=True)
    for j in range(len(split_data)):
        print(j)
        file = split_data.loc[j, "file"]
        if split_data.loc[j, "noOil"] == 1:
            dst_subfolder = "/data_without_oil/"
        else:
            dst_subfolder = "/data_with_oil/"
        shutil.copyfile(src_folder + file, dst_folder + dst_subfolder + file)


def generate_masks(src_folder, dst_folder):
    for file in os.listdir(src_folder):
        arr = read_png_file(src_folder, file)
        masks = arr[0] < THRESHOLD_FOR_BLACK
        if len(arr) > 1:
            for i in range(1, len(arr)):
                masks = masks & arr[i]
        masks = masks.astype(int) * 255
        with rasterio.open(
            dst_folder + file,
            "w",
            driver="png",
            width=arr.shape[2],
            height=arr.shape[1],
            count=1,
            dtype="uint8",
        ) as dst:
            dst.write(np.expand_dims(masks, axis=0))


def make_train_validation_split(folder):
    files_with_oil = os.listdir(folder + "/data_with_oil/")
    files_without_oil = os.listdir(folder + "/data_without_oil/")
    df_with_oil = pd.DataFrame(files_with_oil, columns=["file"])
    df_without_oil = pd.DataFrame(files_without_oil, columns=["file"])
    df_with_oil["noOil"] = False
    df_without_oil["noOil"] = True
    df = pd.concat([df_with_oil, df_without_oil], axis=0)
    split_data = train_test_split(df, train_size=TRAIN_RATIO, random_state=RANDOM_STATE)
    return split_data


def make_train_validation_test_split(file_path, folder_for_split_data):
    statistical_data = pd.read_csv(file_path)
    statistical_data = statistical_data.iloc[:, 1:]
    train_files = pd.read_csv(folder_for_split_data + "/train.txt")
    val_files = pd.read_csv(folder_for_split_data + "/val.txt")
    train_files = train_files["file"].drop_duplicates().values.tolist()
    val_files = val_files["file"].drop_duplicates().values.tolist()
    statistical_data = statistical_data.loc[
        ~statistical_data["file"].isin(train_files + val_files)
    ]
    chosen_files = statistical_data.sample(len(val_files))
    return chosen_files
