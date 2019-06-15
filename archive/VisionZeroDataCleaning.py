import os
import pandas
import csv

DATA_PATH = 'C:\\Users\\eriko\\OneDrive\\Documents\\GitHub\\vision_zero\\data\\crash_data'

outputCSV = os.path.join(DATA_PATH, 'crashData.csv')
headers = ['CrashDeathCount', 'CrashIncapacitatingInjuryCount', 'CrashSeverity', 'CrashTotalInjuryCount', 'IntersectionRelated',
           'Latitude', 'Longitude', 'MedianType', 'NumberofLanes', 'PopulationGroup', 'SpeedLimit', 'TrafficControlType', 'WeatherCondition',
           'n_cars', 'n_bike', 'n_peds']
columnData = {}
for header in headers:
    columnData[header] = []
for file in os.listdir(DATA_PATH):
    if not file == 'output.csv':
        with open(os.path.join(DATA_PATH,file), 'r') as f:
            print('opening {}'.format(file))
            reader = csv.DictReader(f)
            for row in reader:
                for header in headers:
                        if header in row:
                            columnData[header].append(row[header])
                        else:
                            columnData[header].append('N/A')

df = pandas.DataFrame(columnData, columns= headers)
df.to_csv(os.path.join(DATA_PATH, "output.csv"))