#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Thu Mar 16 22:53:53 2023

@author: alexey.osipov
"""
import os

import torch
import torch.nn as nn
from torch.utils import data
from PIL import Image
import numpy as np

from prepare_data_for_applying_model_to_one_file import IMAGE_PATH
from OilDataset import OilDataset
from deeplabv3plus import deeplabv3_resnet50
from transforms import ToTensor, Compose, Normalize, ReSize

MODEL_PATH = "/checkpoints/"


def apply(model, loader, device, file_name):
    with torch.no_grad():
        val_iter = iter(loader)
        images, labels = next(val_iter)
        images = images.to(device, dtype=torch.float32)
        outputs = model(images)
        preds = outputs.detach().max(dim=1)[1].cpu().numpy()
        pred = (preds[0] * 255).astype(np.uint8)
        img = Image.fromarray(pred)
        output_dir = os.getcwd() + IMAGE_PATH
        if not os.path.exists(output_dir):
            output_dir = os.getcwd() + "/" + IMAGE_PATH.split("/")[-2] + "/"
        if file_name == "":
            img.save(output_dir + "temp_file.png", "PNG")
        else:
            img.save(output_dir + "/results/" + file_name, "PNG")


def main(model_name="resnet50_0_6649.pth", file_name=""):
    data_transforms = {"temp": Compose([ReSize(), ToTensor(), Normalize()])}
    input_dir = os.getcwd() + IMAGE_PATH
    if not os.path.exists(input_dir):
        input_dir = os.getcwd() + "/" + IMAGE_PATH.split("/")[-2] + "/"
    oilDataset_temp = OilDataset(
        input_dir,
        "temp.txt",
        transform=data_transforms["temp"],
    )
    oil_datasets = {"temp": oilDataset_temp}
    dataloaders = {
        "temp": data.DataLoader(
            oil_datasets["temp"],
            batch_size=1,
            shuffle=True,
            num_workers=0,
            drop_last=True,
        )
    }

    device = torch.device("cuda:0" if torch.cuda.is_available() else "cpu")

    output_stride = 8
    model = deeplabv3_resnet50(num_classes=2, output_stride=output_stride)

    model_path = os.getcwd() + MODEL_PATH + model_name
    if "modelling" not in model_path:
        model_path = os.getcwd() + "/modelling" + MODEL_PATH + model_name
    checkpoint = torch.load(model_path, map_location=torch.device("cpu"))
    model.load_state_dict(checkpoint["model_state"])
    model = nn.DataParallel(model)
    model.to(device)
    model.eval()
    apply(model=model, loader=dataloaders["temp"], device=device, file_name=file_name)


if __name__ == "__main__":
    main()
