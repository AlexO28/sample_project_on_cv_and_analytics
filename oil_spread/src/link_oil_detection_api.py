#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Wed Jul  12 18:39:11 2023

@author: julia.titaeva
"""
import os
import requests
import pandas as pd


def process_data():
    dummy_results = pd.DataFrame({"done": False, "output_file": ""}, index=[0])
    dummy_results.to_csv("../data/output_file.csv")

    input_data = pd.read_csv("../data/input_data.csv")
    input_file_name = input_data.loc[0, "file_name"]
    input_file_path = input_data.loc[0, "file_path"]

    dimensions = input_file_name.split("_")
    dimensions = dimensions[len(dimensions) - 2], dimensions[len(dimensions) - 1]

    file = {"file": open(input_file_path + input_file_name, "rb")}

    url_windows = "http://host.docker.internal:8080/"
    url_linux = "http://127.0.0.1:8080/"

    try:
        response = requests.get(url=url_windows + "results/temp_file.png")
        if response.status_code == 200:
            url = url_windows
        else:
            url = url_linux
    except:
        url = url_linux

    url_calculate = url + "calculate"
    response = requests.post(url=url_calculate, files=file)
    content = response.json()
    result = dict()
    result["done"] = content["done"]
    result["output_file"] = content["output_file"]

    output_file = content["output_file"]
    output_file_name, output_file_type = output_file.split(".")
    output_file_name += "_" + str(dimensions[0]) + "_" + str(dimensions[1])

    url_get_results = url + "results/" + f"{result['output_file']}"
    response = requests.get(url=url_get_results)
    if response.status_code == 200:
        with open(os.path.join("../data/", output_file_name), "wb") as f:
            f.write(response.content)
    del response

    result["output_file"] = output_file_name

    pd.DataFrame(result, index=[0]).to_csv(
        os.path.join("../data/output_file.csv"), index=False
    )


if __name__ == "__main__":
    process_data()
