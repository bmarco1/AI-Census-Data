import csv

with open ('census_data.csv') as csvfile:
    readCSV = csv.reader(csvfile, delimiter=",")

    for row in readCSV:
        print(row)
