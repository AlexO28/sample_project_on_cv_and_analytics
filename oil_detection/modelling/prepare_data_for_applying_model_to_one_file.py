#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Thu Jun  29 16:09:07 2023

@author: julia.titaeva
"""

import os
import shutil
import pandas as pd

from OilDataset import SUBROOT_WITH_OIL, SUBROOT_NO_OIL, SUBROOT_MASKS

IMAGE_PATH = "/../temp_data/"  # temp_data folder should be located in root folder: eco-oil-detection


def get_filename(root_dir):
    filename = [elem for elem in os.listdir(root_dir) if ".png" in elem][0]
    return filename


def generate_txt_file(filename, root_dir):
    data = pd.DataFrame({"file": [filename], "noOil": [True]})
    data.to_csv(root_dir + "temp.txt")


def make_oil_subfolders(root_dir):
    if not os.path.exists(root_dir + SUBROOT_WITH_OIL):
        os.makedirs(root_dir + SUBROOT_WITH_OIL)
    if not os.path.exists(root_dir + SUBROOT_NO_OIL):
        os.makedirs(root_dir + SUBROOT_NO_OIL)
    if not os.path.exists(root_dir + SUBROOT_MASKS):
        os.makedirs(root_dir + SUBROOT_MASKS)


def move_image_to_subfolder(filename, root_dir):
    shutil.copyfile(root_dir + filename, root_dir + SUBROOT_NO_OIL + filename)


def prepare_data():
    input_dir = os.getcwd() + IMAGE_PATH
    if not os.path.exists(input_dir):
        input_dir = os.getcwd() + "/" + IMAGE_PATH.split("/")[-2] + "/"
    if "temp_file.png" in os.listdir(input_dir):
        os.remove(input_dir + "temp_file.png")
    filename = get_filename(input_dir)
    generate_txt_file(filename, input_dir)
    make_oil_subfolders(input_dir)
    move_image_to_subfolder(filename, input_dir)


if __name__ == "__main__":
    prepare_data()
