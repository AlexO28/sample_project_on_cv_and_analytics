#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Thu Mar  9 10:07:30 2023

@author: alexey.osipov
"""

import os
import numpy as np
import random
import torch
import torch.nn as nn
import utils
from torch.utils import data
from transforms import (
    ToTensor,
    RandomScale,
    RandomCrop,
    RandomHorizontalFlip,
    RandomVerticalFlip,
    Normalize,
    Compose,
    ReSize
)
import OilDataset
from deeplabv3plus import deeplabv3_resnet101, deeplabv3_resnet50, deeplabv3_mobilenet
from metrics import StreamSegMetrics


def save_ckpt(path, cur_itrs, model, optimizer, scheduler, best_score):
    torch.save(
        {
            "cur_itrs": cur_itrs,
            "model_state": model.module.state_dict(),
            "optimizer_state": optimizer.state_dict(),
            "scheduler_state": scheduler.state_dict(),
            "best_score": best_score,
        },
        path,
    )
    print("Model saved as %s" % path)


def validate(model, loader, device, metrics):
    with torch.no_grad():
        val_iter = iter(loader)
        for i in range(len(loader)):
            features, masks = next(val_iter)
            features = features.to(device, dtype=torch.float32)
            masks = masks.to(device, dtype=torch.long)

            outputs = model(features)
            preds = outputs.detach().max(dim=1)[1].cpu().numpy()
            targets = masks.cpu().numpy()

            metrics.update(targets, preds)

        score = metrics.get_results()
    return score


def main():
    data_transforms = {
        "train": Compose(
            [
                RandomScale(),
                RandomCrop(),
                RandomHorizontalFlip(),
                RandomVerticalFlip(),
                ReSize(),
                ToTensor(),
                Normalize(),
            ]
        ),
        "val": Compose(
            [
                RandomHorizontalFlip(),
                RandomVerticalFlip(),
                ReSize(),
                ToTensor(),
                Normalize(),
            ]
        ),
    }
    root_dir = "C:/Users/Семья/Downloads/oil_data/"
    oilDataset_train = OilDataset.OilDataset(
        root_dir,
        'train.txt',
        transform=data_transforms["train"],
    )
    oilDataset_val = OilDataset.OilDataset(
        root_dir,
        'val.txt',
        transform=data_transforms["val"],
    )
    oil_datasets = {"train": oilDataset_train, "val": oilDataset_val}
    dataloaders = {
        "train": data.DataLoader(
            oil_datasets["train"],
            batch_size=8,
            shuffle=True,
            num_workers=0,
            drop_last=True,
        ),
        "val": data.DataLoader(
            oil_datasets["val"],
            batch_size=4,
            shuffle=True,
            num_workers=0,
            drop_last=True,
        ),
    }
    print(
        "Dataset: %s, Train set: %d, Val set: %d"
        % ("oildataset", len(dataloaders["train"]), len(dataloaders["val"]))
    )
    device = torch.device("cuda:0" if torch.cuda.is_available() else "cpu")
    ckpt = None
    continue_training = False

    random_seed = 239
    torch.manual_seed(random_seed)
    np.random.seed(random_seed)
    random.seed(random_seed)

    total_itrs = 30e3
    val_interval = 100
    output_stride = 8
    learning_rate = 0.01
    model = deeplabv3_resnet50(num_classes=2, output_stride=output_stride)
    utils.set_bn_momentum(model.backbone, momentum=0.01)
    metrics = StreamSegMetrics(2)
    optimizer = torch.optim.SGD(
        params=[
            {"params": model.backbone.parameters(), "lr": 0.1 * learning_rate},
            {"params": model.classifier.parameters(), "lr": learning_rate},
        ],
        lr=learning_rate,
        momentum=0.9,
        weight_decay=1e-4,
    )
    scheduler = utils.PolyLR(optimizer, total_itrs, power=0.9)
    criterion = nn.CrossEntropyLoss(ignore_index=255, reduction="mean")
    utils.mkdir("checkpoints")

    best_score = 0.0
    cur_itrs = 0
    cur_epochs = 0

    if ckpt is not None and os.path.isfile(ckpt):
        checkpoint = torch.load(ckpt, map_location=torch.device("cpu"))
        model.load_state_dict(checkpoint["model_state"])
        model = nn.DataParallel(model)
        model.to(device)
        if continue_training:
            optimizer.load_state_dict(checkpoint["optimizer_state"])
            scheduler.load_state_dict(checkpoint["scheduler_state"])
            cur_itrs = checkpoint["cur_itrs"]
            best_score = checkpoint["best_score"]
            print("Training state restored from %s" % ckpt)
        print("Model restored from %s" % ckpt)
        del checkpoint
    else:
        print("[!] Retrain")
        model = nn.DataParallel(model)
        model.to(device)

    interval_loss = 0
    while True:
        model.train()
        cur_epochs += 1
        train_iter = iter(dataloaders["train"])
        for j in range(len(dataloaders["train"])):
            cur_itrs += 1
            features, masks = next(train_iter)
            features = features.to(device, dtype=torch.float32)
            masks = masks.to(device, dtype=torch.long)

            optimizer.zero_grad()
            outputs = model(features)
            loss = criterion(outputs, masks)
            loss.backward()
            optimizer.step()

            np_loss = loss.detach().cpu().numpy()
            interval_loss += np_loss

            if (cur_itrs) % 10 == 0:
                interval_loss = interval_loss / 10
                print(
                    "Epoch %d, Itrs %d/%d, Loss=%f"
                    % (cur_epochs, cur_itrs, total_itrs, interval_loss)
                )
                interval_loss = 0.0

            if (cur_itrs) % val_interval == 0:
                save_ckpt(
                    "checkpoints/latest_%s_%s_os%d.pth"
                    % ("resnet", "geodataset", output_stride),
                    cur_itrs,
                    model,
                    optimizer,
                    scheduler,
                    best_score,
                )
                print("validation...")
                model.eval()
                val_score = validate(
                    model=model,
                    loader=dataloaders["val"],
                    device=device,
                    metrics=metrics,
                )
                print(metrics.to_str(val_score))
                if val_score["Mean IoU"] > best_score:
                    best_score = val_score["Mean IoU"]
                    save_ckpt(
                        "checkpoints/best_%s_%s_os%d.pth"
                        % ("resnet", "geodataset", output_stride),
                        cur_itrs,
                        model,
                        optimizer,
                        scheduler,
                        best_score,
                    )

                model.train()

            scheduler.step()

            if cur_itrs >= total_itrs:
                return


if __name__ == "__main__":
    main()
