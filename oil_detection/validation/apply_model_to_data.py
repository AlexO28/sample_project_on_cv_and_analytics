#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Thu Sep 14 13:34:31 2023

@author: alexey.osipov
"""
import os
import pandas as pd


def apply_model_to_all_data():
    files = os.listdir("../temp_data/data_with_oil/")
    curwd = os.getcwd()
    os.chdir("../modelling/")
    import apply_model_to_one_file

    for file in files:
        print(file)
        tab = pd.DataFrame([file], columns=["file"])
        tab["noOil"] = False
        tab.to_csv("../temp_data/temp.txt", sep=",")
        apply_model_to_one_file.main(file_name=file)

    os.chdir(curwd)
