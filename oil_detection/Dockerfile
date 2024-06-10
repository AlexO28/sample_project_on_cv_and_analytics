FROM python:3.9

COPY requirements.txt ./
COPY app.py ./
COPY modelling ./modelling
COPY temp_data ./temp_data

RUN apt update
RUN pip install --no-cache-dir --upgrade -r requirements.txt

EXPOSE 8080
CMD ["uvicorn", "app:app", "--host", "0.0.0.0", "--port", "8080"]
