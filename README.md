# Sample project on CV and analystics

The project consists of 2 parts: the first one is devoted to CV, and the second one is devoted to "analytics".

## Basic information

The project is a trimmed version of a real project created by 2 people.
It just contains the code, it contains neither data, nor models, nor pictures.
It will not run without of the models, data, and pictures. 

## Structure

The CV part is included in folder oil_detection.

The following technologies were used: 
* Docker 
* Python (with fastapi, PyTorch, pandas, numpy, math, rasterio, sklearn)
* R (with Shiny, imager)
* ML (with DeepLabV3+, UNet)

The first part can be run as a Shiny web app, or as a microservice from Docker container.

The "analytics" part is included in folder oil_spread.

The following technologies were used:
* Docker
* Python (with fastapi, requests, pandas, catboost, rpy2)
* R (with Shiny, data.table, TTR, vars, dbscan, deSolve, imager, plot.matrix)
* TeX (with beamer)
* ML (with catboost, EMA, VAR, hdbscan)
* Math (advection diffusion PDE)

The second part can be run as a Shiny web app.

The first part and the second part are supposed to be packed into 2 Docker images and run together:
the second docker image is calling the microservice from the first one as needed.
