#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Fri Jun  30 16:40:05 2023

@author: julia.titaeva
"""
import os
import sys
import shutil
from fastapi import FastAPI, UploadFile
from starlette.responses import Response
from mimetypes import guess_type


os.chdir("./modelling/")
sys.path.append(os.getcwd())
from prepare_data_for_applying_model_to_one_file import prepare_data
from apply_model_to_one_file import main
from land_detection import get_land_mask, apply_mask

os.chdir("./../")

OUTPUT_FILE_PATH = "temp_data/"


app = FastAPI()


@app.post("/calculate")
async def calculate(file: UploadFile):
    if os.path.exists(OUTPUT_FILE_PATH):
        shutil.rmtree(OUTPUT_FILE_PATH)
    os.makedirs(OUTPUT_FILE_PATH)
    try:
        contents = file.file.read()
        with open(OUTPUT_FILE_PATH + file.filename, "wb") as f:
            f.write(contents)
    except Exception as _:
        return {"done": False, "output_file": ""}
    finally:
        file.file.close()

    prepare_data()
    main()
    land_mask = get_land_mask(output_file_path=OUTPUT_FILE_PATH)
    apply_mask(mask=land_mask, output_file_path=OUTPUT_FILE_PATH)
    return {"done": True, "output_file": "temp_file.png"}


@app.get("/results/{filename}")
async def get_results(file_name="temp_file.png"):
    filename = OUTPUT_FILE_PATH + file_name
    if not os.path.isfile(filename):
        return Response(status_code=404)
    with open(filename, "rb") as f:
        content = f.read()
    content_type, _ = guess_type(filename)
    return Response(content, media_type=content_type)
