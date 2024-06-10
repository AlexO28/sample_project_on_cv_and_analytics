#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Mon Dec 12 13:26:58 2022

@author: alexey.osipov
"""

from resnet import resnet101, resnet50
from mobilenetv2 import mobilenet_v2
from deeplab import DeepLabHeadV3Plus, DeepLabV3, DeepLabHead
from deeplab_utils import IntermediateLayerGetter


def _segm_resnet(name, backbone_name, num_classes, output_stride, pretrained_backbone):
    if output_stride == 8:
        replace_stride_with_dilation = [False, True, True]
        aspp_dilate = [12, 24, 36]
    else:
        replace_stride_with_dilation = [False, False, True]
        aspp_dilate = [6, 12, 18]

    if backbone_name == 'resnet101':
        backbone = resnet101(
            pretrained=pretrained_backbone,
            replace_stride_with_dilation=replace_stride_with_dilation,
        )
    elif backbone_name == 'resnet50':
        backbone = resnet50(
            pretrained=pretrained_backbone,
            replace_stride_with_dilation=replace_stride_with_dilation,
        )

    inplanes = 2048
    low_level_planes = 256

    return_layers = {"layer4": "out", "layer1": "low_level"}
    classifier = DeepLabHeadV3Plus(inplanes, low_level_planes, num_classes, aspp_dilate)
    backbone = IntermediateLayerGetter(backbone, return_layers=return_layers)

    model = DeepLabV3(backbone, classifier)
    return model


def _segm_mobilenet(
    name, backbone_name, num_classes, output_stride, pretrained_backbone
):
    if output_stride == 8:
        aspp_dilate = [12, 24, 36]
    else:
        aspp_dilate = [6, 12, 18]

    backbone = mobilenet_v2(pretrained=pretrained_backbone, output_stride=output_stride)

    backbone.low_level_features = backbone.features[0:4]
    backbone.high_level_features = backbone.features[4:-1]
    backbone.features = None
    backbone.classifier = None

    inplanes = 320
    low_level_planes = 24

    if name == "deeplabv3plus":
        return_layers = {
            "high_level_features": "out",
            "low_level_features": "low_level",
        }
        classifier = DeepLabHeadV3Plus(
            inplanes, low_level_planes, num_classes, aspp_dilate
        )
    elif name == "deeplabv3":
        return_layers = {"high_level_features": "out"}
        classifier = DeepLabHead(inplanes, num_classes, aspp_dilate)
    backbone = IntermediateLayerGetter(backbone, return_layers=return_layers)

    model = DeepLabV3(backbone, classifier)
    return model


def _load_model(arch_type, backbone, num_classes, output_stride, pretrained_backbone):
    if backbone.startswith("resnet"):
        model = _segm_resnet(
            arch_type,
            backbone,
            num_classes,
            output_stride=output_stride,
            pretrained_backbone=pretrained_backbone,
        )
    elif backbone == "mobilenetv2":
        model = _segm_mobilenet(
            arch_type,
            backbone,
            num_classes,
            output_stride=output_stride,
            pretrained_backbone=pretrained_backbone,
        )
    else:
        raise NotImplementedError
    return model


def deeplabv3_resnet101(num_classes=2, output_stride=8, pretrained_backbone=True):
    return _load_model(
        "deeplabv3",
        "resnet101",
        num_classes,
        output_stride=output_stride,
        pretrained_backbone=pretrained_backbone,
    )


def deeplabv3_resnet50(num_classes=2, output_stride=8, pretrained_backbone=True):
    return _load_model(
        "deeplabv3",
        "resnet50",
        num_classes,
        output_stride=output_stride,
        pretrained_backbone=pretrained_backbone,
    )


def deeplabv3_mobilenet(
    num_classes=2, output_stride=8, pretrained_backbone=True, **kwargs
):
    return _load_model(
        "deeplabv3",
        "mobilenetv2",
        num_classes,
        output_stride=output_stride,
        pretrained_backbone=pretrained_backbone,
    )
