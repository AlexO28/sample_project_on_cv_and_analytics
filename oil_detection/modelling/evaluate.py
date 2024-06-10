#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Thu Mar  9 11:26:29 2023

@author: alexey.osipov
"""

import torch
import torch.nn.functional as F

from dice_score import multiclass_dice_coeff


@torch.inference_mode()
def evaluate(net, dataloader, device, amp):
    net.eval()
    num_val_batches = len(dataloader)
    dice_score = 0

    with torch.no_grad():
        val_iter = iter(dataloader)
        for i in range(len(dataloader)):
            image, mask_true = next(val_iter)

            image = image.to(
                device=device, dtype=torch.float32, memory_format=torch.channels_last
            )
            mask_true = mask_true.to(device=device, dtype=torch.long)

            mask_pred = net(image)

            assert(
                mask_true.min() >= 0 and mask_true.max() < net.n_classes
            ), "True mask indices should be in [0, n_classes["
            mask_true = (
                F.one_hot(mask_true, net.n_classes).permute(0, 3, 1, 2).float()
            )
            mask_pred = (
                F.one_hot(mask_pred.argmax(dim=1), net.n_classes)
                .permute(0, 3, 1, 2)
                .float()
            )
            dice_score += multiclass_dice_coeff(
                mask_pred[:, 1:], mask_true[:, 1:], reduce_batch_first=False
            )

    net.train()
    return dice_score / max(num_val_batches, 1)
