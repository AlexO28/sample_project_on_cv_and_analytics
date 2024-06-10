#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Thu Mar  9 11:45:29 2023

@author: alexey.osipov
"""

import pandas as pd
import torch
from torch.utils.data import Dataset
import os
from PIL import Image
import numpy as np


SUBROOT_WITH_OIL = "/data_with_oil/"
SUBROOT_NO_OIL = "/data_without_oil/"
SUBROOT_MASKS = "/masks/"


class OilDataset(Dataset):
    def __init__(
        self,
        root_dir,
        csv_file,
        x_size=1250,
        y_size=650,
        transform=None,
    ):
        tab = pd.read_csv(root_dir + csv_file)
        self.root_dir = root_dir
        self.transform = transform
        self.x_size = x_size
        self.y_size = y_size
        self.dir_with_oil = root_dir + SUBROOT_WITH_OIL
        self.dir_without_oil = root_dir + SUBROOT_NO_OIL
        self.dir_with_masks = root_dir + SUBROOT_MASKS
        self.files = tab['file'].tolist()
        self.noOil = tab['noOil'].tolist()
        self.mask_files = os.listdir(self.root_dir + SUBROOT_MASKS)
        self.no_oil_mask = np.empty((y_size, x_size)).astype('uint8')

    def __len__(self):
        return len(self.files)

    def __getitem__(self, idx):
        if torch.is_tensor(idx):
            idx = idx.tolist()
        filename = self.files[idx]
        noOil = self.noOil[idx]
        if noOil:
            subroot = SUBROOT_NO_OIL
        else:
            subroot = SUBROOT_WITH_OIL
        oil = Image.open(self.root_dir + subroot + filename)
        if filename in self.mask_files:
            mask = Image.open(self.root_dir + SUBROOT_MASKS + filename)
        else:
            mask = Image.fromarray(self.no_oil_mask)
        if self.transform:
            return self.transform(oil, mask)
        else:
            return oil, mask
