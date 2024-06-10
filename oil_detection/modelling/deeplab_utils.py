#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Mon Dec 12 13:36:41 2022

@author: alexey.osipov
"""

import torch
import torch.nn as nn
import torch.nn.functional as F
from collections import OrderedDict


class _SimpleSegmentationModel(nn.Module):
    def __init__(self, backbone, classifier):
        super(_SimpleSegmentationModel, self).__init__()
        self.backbone = backbone
        self.classifier = classifier

    def forward(self, x):
        input_shape = x.shape[-2:]
        features = self.backbone(x)
        x = self.classifier(features)
        x = F.interpolate(x, size=input_shape, mode="bilinear", align_corners=False)
        return x


class IntermediateLayerGetter(nn.ModuleDict):
    def __init__(self, model, return_layers, hrnet_flag=False):
        if not set(return_layers).issubset(
            [name for name, _ in model.named_children()]
        ):
            raise ValueError("return_layers are not present in model")

        self.hrnet_flag = hrnet_flag

        orig_return_layers = return_layers
        return_layers = {k: v for k, v in return_layers.items()}
        layers = OrderedDict()
        for name, module in model.named_children():
            layers[name] = module
            if name in return_layers:
                del return_layers[name]
            if not return_layers:
                break

        super(IntermediateLayerGetter, self).__init__(layers)
        self.return_layers = orig_return_layers

    def forward(self, x):
        out = OrderedDict()
        for name, module in self.named_children():
            if self.hrnet_flag and name.startswith("transition"):
                if name == "transition1":
                    x = [trans(x) for trans in module]
                else:
                    x.append(module(x[-1]))
            else:
                x = module(x)

            if name in self.return_layers:
                out_name = self.return_layers[name]
                if name == "stage4" and self.hrnet_flag:
                    output_h, output_w = x[0].size(2), x[0].size(3)
                    x1 = F.interpolate(
                        x[1],
                        size=(output_h, output_w),
                        mode="bilinear",
                        align_corners=False,
                    )
                    x2 = F.interpolate(
                        x[2],
                        size=(output_h, output_w),
                        mode="bilinear",
                        align_corners=False,
                    )
                    x3 = F.interpolate(
                        x[3],
                        size=(output_h, output_w),
                        mode="bilinear",
                        align_corners=False,
                    )
                    x = torch.cat([x[0], x1, x2, x3], dim=1)
                    out[out_name] = x
                else:
                    out[out_name] = x
        return out
