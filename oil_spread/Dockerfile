FROM python:3.9

COPY requirements.R ./
COPY requirements.txt ./
COPY app.R ./app/app.R
COPY src ./app/src
COPY data ./app/data

RUN apt-get update && apt-get install -y --no-install-recommends build-essential r-base r-cran-randomforest
RUN pip install --no-cache-dir --upgrade -r requirements.txt
RUN apt-get install -y --no-install-recommends gfortran
RUN apt-get install libblas-dev liblapack-dev
RUN Rscript requirements.R

WORKDIR /app
RUN Rscript src/initialization.R
EXPOSE 3838
CMD ["R", "-e", "shiny::runApp()"]
