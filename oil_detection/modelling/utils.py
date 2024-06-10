#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Mon Dec 12 16:07:47 2022

@author: alexey.osipov
"""

import torch.nn as nn
import os
from torch.optim.lr_scheduler import _LRScheduler, StepLR


class PolyLR(_LRScheduler):
    def __init__(self, optimizer, max_iters, power=0.9, last_epoch=-1, min_lr=1e-6):
        self.power = power
        self.max_iters = max_iters
        self.min_lr = min_lr
        super(PolyLR, self).__init__(optimizer, last_epoch)

    def get_lr(self):
        return [
            max(
                base_lr * (1 - self.last_epoch / self.max_iters) ** self.power,
                self.min_lr,
            )
            for base_lr in self.base_lrs
        ]


def set_bn_momentum(model, momentum=0.1):
    for m in model.modules():
        if isinstance(m, nn.BatchNorm2d):
            m.momentum = momentum


def mkdir(path):
    if not os.path.exists(path):
        os.mkdir(path)
