##!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Thu Mar  9 16:44:29 2023

@author: alexey.osipov
"""

import torch
from torchvision.transforms import RandomAffine
import torchvision.transforms.functional as F
import random
import numpy as np
from PIL import Image


class ToTensor(object):
    def __call__(self, oil, mask):
        oil = np.asarray(oil)
        mask_arr = np.asarray(mask).copy()
        if len(oil.shape) > 2:
            oil = oil.transpose((2, 0, 1))
        else:
            new_oil = np.empty([3] + list(oil.shape))
            new_oil[0, :, :] = oil
            new_oil[1, :, :] = oil
            new_oil[2, :, :] = oil
            oil = new_oil
        mask_arr[mask_arr > 1] = 1
        oil = torch.from_numpy(oil.astype(np.float32))
        mask_arr = torch.from_numpy(mask_arr)
        return oil, mask_arr


class RandomHorizontalFlip(object):
    def __init__(self, p=0.5):
        self.p = p

    def __call__(self, oil, mask):
        if random.random() < self.p:
            return F.hflip(oil), F.hflip(mask)
        else:
            return oil, mask


class RandomVerticalFlip(object):
    def __init__(self, p=0.5):
        self.p = p

    def __call__(self, oil, mask):
        if random.random() < self.p:
            return F.vflip(oil), F.vflip(mask)
        else:
            return oil, mask


class RandomScale(object):
    def __init__(self, scale=0.5):
        self.scale = scale

    def __call__(self, oil, mask):
        if self.scale > 1:
            self.scale = 1 / self.scale
        if random.random() < 1 / 3:
            random_scale = self.scale
        elif random.random() < 2 / 3:
            random_scale = 1 + self.scale
        else:
            random_scale = 1
        return RandomAffine([0, 0], scale=[random_scale, random_scale])(
            oil
        ), RandomAffine([0, 0], scale=[random_scale, random_scale])(mask)
    

class ReSize(object):
    def __init__(self, scale=0.25):
        self.scale = scale

    def __call__(self, oil, mask):
        assert oil.size == mask.size
        target_size = int(oil.size[1] * self.scale), int(oil.size[0] * self.scale)
        return F.resize(oil, target_size, Image.BILINEAR), F.resize(
            mask, target_size, Image.BILINEAR
        )


class Compose(object):
    def __init__(self, transforms):
        self.transforms = transforms

    def __call__(self, oil, mask):
        for t in self.transforms:
            oil, mask = t(oil, mask)
        return oil, mask


class Normalize(object):
    def __init__(self, mean=[0.485, 0.456, 0.406], std=[0.229, 0.224, 0.225]):
        self.mean = mean
        self.std = std

    def __call__(self, oil, mask):
        return F.normalize(oil, self.mean, self.std), mask


class RandomCrop(object):
    def __init__(self, size=[320, 320]):
        self.size = size

    @staticmethod
    def get_params(oil, output_size):
        w, h = oil.size
        th, tw = output_size
        if w == tw and h == th:
            return 0, 0, h, w
        else:
            i = random.randint(0, h - th)
            j = random.randint(0, w - tw)
            return i, j, th, tw

    def __call__(self, oil, mask):
        assert oil.size == mask.size
        oil_array = np.asarray(oil).copy()
        mask_array = np.asarray(mask).copy()
        i, j, h, w = self.get_params(oil, self.size)
        if len(oil_array.shape) == 3:
            oil_array[:, i:(i+h), j:(j+w)] = 0
        else:
            oil_array[i:(i+h), j:(j+w)] = 0
        mask_array[i:(i+h), j:(j+w)] = 0        
        return Image.fromarray(oil_array), Image.fromarray(mask_array)
