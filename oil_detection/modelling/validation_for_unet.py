#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Wed Mar 15 12:35:10 2023

@author: alexey.osipov
"""

import numpy as np
import random
import torch
import torch.nn as nn
from torch.utils import data
import OilDataset
from transforms import (
    ToTensor,
    RandomHorizontalFlip,
    RandomVerticalFlip,
    Normalize,
    Compose,
    ReSize,
)
from unet_model import UNet


def validate(model, loader, device):
    with torch.no_grad():
        val_iter = iter(loader)
        numerator = 0
        denominator = 0
        for i in range(len(loader)):
            print(i, numerator, denominator)
            images, labels = next(val_iter)
            images = images.to(device, dtype=torch.float32)
            outputs = model(images)
            preds = outputs.detach().max(dim=1)[1].cpu().numpy()
            targets = labels.cpu().numpy()

            for j in range(len(images)):
                pred = preds[j]
                target = targets[j]
                arr_shape = pred.shape[0] * pred.shape[1]
                pred = pred.reshape(-1, 1)
                target = target.reshape(-1, 1)
                numerator += sum(
                    [int((pred[k] > 0) and (target[k] > 0)) for k in range(arr_shape)]
                )
                denominator += sum(
                    [int((pred[k] > 0) or (target[k] > 0)) for k in range(arr_shape)]
                )
    return numerator, denominator


def main():
    data_transforms = {
        "test": Compose(
            [
                RandomHorizontalFlip(),
                RandomVerticalFlip(),
                ReSize(scale = 0.2),
                ToTensor(),
                Normalize(),
            ]
        )
    }
    root_dir = "C:\\Users\\Семья\\Downloads\\oil_data\\test_data\\"
    oilDataset_test = OilDataset.OilDataset(
        root_dir,
        "test.txt",
        transform=data_transforms["test"],
    )
    oil_datasets = {"test": oilDataset_test}
    dataloaders = {
        "test": data.DataLoader(
            oil_datasets["test"],
            batch_size=2,
            shuffle=True,
            num_workers=0,
            drop_last=True,
        )
    }

    print("Dataset: %s, Test set: %d" % ("geodataset", len(dataloaders["test"])))
    device = torch.device("cuda:0" if torch.cuda.is_available() else "cpu")
    ckpt = "C:\\playground\\gitlab\\eco-oil-detection\\modelling\\checkpoints\\unet_0_6532.pth"

    random_seed = 239
    torch.manual_seed(random_seed)
    np.random.seed(random_seed)
    random.seed(random_seed)

    model = UNet(n_channels=3, n_classes=2, bilinear=False)
    state_dict = torch.load(ckpt, map_location=device)
    del state_dict["mask_values"]
    model.load_state_dict(state_dict)
    model = nn.DataParallel(model)
    model.to(device)
    print("validation...")
    model.eval()
    numerator, denominator = validate(
        model=model, loader=dataloaders["test"], device=device
    )
    print("numerator")
    print(numerator)
    print("denominator")
    print(denominator)
