import logging
import torch
import torch.nn as nn
import torch.nn.functional as F
from torch import optim
from torch.utils import data

from evaluate import evaluate
from unet_model import UNet
from dice_score import dice_loss

from transforms import (
    ToTensor,
    RandomScale,
    RandomCrop,
    RandomHorizontalFlip,
    RandomVerticalFlip,
    Normalize,
    Compose,
    ReSize,
)
import OilDataset


def train_model(
    dataloaders,
    model,
    device,
    epochs=5,
    batch_size=2,
    learning_rate=1e-5,
    save_checkpoint=True,
    amp=False,
    weight_decay=1e-4,
    momentum=0.9,
    gradient_clipping=1.0,
):
    train_loader = dataloaders["train"]
    val_loader = dataloaders["val"]

    logging.info(
        f"""Starting training:
        Epochs:          {epochs}
        Batch size:      {batch_size}
        Learning rate:   {learning_rate}
        Checkpoints:     {save_checkpoint}
        Device:          {device.type}
    """
    )

    optimizer = optim.RMSprop(
        model.parameters(),
        lr=learning_rate,
        weight_decay=weight_decay,
        momentum=momentum,
        foreach=True,
    )
    scheduler = optim.lr_scheduler.ReduceLROnPlateau(optimizer, "max", patience=5)
    grad_scaler = torch.cuda.amp.GradScaler(enabled=amp)
    criterion = nn.CrossEntropyLoss()
    global_step = 0

    for epoch in range(1, epochs + 1):
        model.train()
        epoch_loss = 0
        train_iter = iter(train_loader)
        for j in range(len(train_loader)):
            if j % 10 == 0:
                print(j)
            features, masks = next(train_iter)
            features = features.to(device, dtype=torch.float32)
            masks = masks.to(device, dtype=torch.long)

            masks_pred = model(features)

            loss = criterion(masks_pred, masks)
            loss += dice_loss(
                F.softmax(masks_pred, dim=1).float(),
                F.one_hot(masks, model.n_classes).permute(0, 3, 1, 2).float(),
                multiclass=True,
            )

            optimizer.zero_grad(set_to_none=True)
            grad_scaler.scale(loss).backward()
            torch.nn.utils.clip_grad_norm_(model.parameters(), gradient_clipping)
            grad_scaler.step(optimizer)
            grad_scaler.update()

            global_step += 1
            epoch_loss += loss.item()

            division_step = len(dataloaders["train"]) // (5 * batch_size)
            if division_step > 0:
                if global_step % division_step == 0:
                    print("evaluation")
                    val_score = evaluate(model, val_loader, device, False)
                    scheduler.step(val_score)

                    logging.info("Validation Dice score: {}".format(val_score))

                    state_dict = model.state_dict()
                    state_dict["mask_values"] = masks
                    torch.save(state_dict, "checkpoints/latest_%s_%s_os%d.pth")
                    logging.info(f"Checkpoint {epoch} saved!")
                    model.train()


def main():
    print('dice score => iou')
    data_transforms = {
        "train": Compose(
            [
                RandomScale(),
                RandomCrop(),
                RandomHorizontalFlip(),
                RandomVerticalFlip(),
                ReSize(scale = 0.2),
                ToTensor(),
                Normalize(),
            ]
        ),
        "val": Compose(
            [
                RandomHorizontalFlip(),
                RandomVerticalFlip(),
                ReSize(scale = 0.2),
                ToTensor(),
                Normalize(),
            ]
        ),
    }
    root_dir = "C:\\Users\\Семья\\Downloads\\oil_data\\"
    oilDataset_train = OilDataset.OilDataset(
        root_dir,
        "train.txt",
        transform=data_transforms["train"],
    )
    oilDataset_val = OilDataset.OilDataset(
        root_dir,
        "val.txt",
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
    logging.basicConfig(level=logging.INFO, format="%(levelname)s: %(message)s")
    device = torch.device("cuda" if torch.cuda.is_available() else "cpu")
    logging.info(f"Using device {device}")

    model = UNet(n_channels=3, n_classes=2, bilinear=False)
    model = model.to(memory_format=torch.channels_last)

    logging.info(
        f"Network:\n"
        f"\t{model.n_channels} input channels\n"
        f"\t{model.n_classes} output channels (classes)\n"
        f'\t{"Bilinear" if model.bilinear else "Transposed conv"} upscaling'
    )

    load_dict = False
    DICT_PATH = ""

    if load_dict:
        state_dict = torch.load(DICT_PATH, map_location=device)
        del state_dict["mask_values"]
        model.load_state_dict(state_dict)
        logging.info("Model loaded")

    model.to(device=device)
    train_model(
        model=model,
        epochs=8,
        batch_size=8,
        learning_rate=0.01,
        device=device,
        dataloaders=dataloaders,
    )


if __name__ == "__main__":
    main()
