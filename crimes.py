import numpy as np
import pandas as pd
import seaborn as sns
import folium
import json
from matplotlib import pyplot as plt
from folium import plugin

dados = pd.read_csv("crime.csv", encoding = "latin-1", keep_default_na=False)
locations = dados.groupby('REPORTING_AREA').first()
locations = locations.loc[:, ["lat", "long", "street"]]

area_count = dados.groupby('REPORTING_AREA').count()
area_count = area_count.iloc[:, [0]]
area_count.columns = ["Area Count"]
area_info = area_count.join(locations)
area_info = area_info[area_info.Long != '']

bostonMap = folium.Map(location = [42.3601, -71.0589], zoom_start = 13, tiles = 'CartoDB positron')
for index, row in area_info.iterrows():
    area_count = row["Area Count"]

    # normalize radius to avoid overlapping
    radius = area_count/70

    # determine color 
    if area_count >= 1500:
        color = "#ff0000" #red
    elif area_count >= 500 and area_count < 1500:
        color = "E37222" #orange
    elif area_count < 500:
        color = "#0A8A9F" #blue

    # add pop-up info
    street = row["STREET"]
    popup_text = """Street: {}<br>total crime: {}"""
    popup_text = popup_text.format(street, area_count)

    # plot bubbles
    folium.CircleMarker(
        location = (row["Lat"], row["Long"]),
        radius = radius,
        color = color,
        fill = True,
        popup = popup_text
    ).add_to(bostonMap)


with open('Boston_Neighborhoods.geojson') as f:
    bostonArea = json.load(f)

bostonMap.save('map.html')


#add the shape of Boston County to the map
folium.GeoJson(bostonArea).add_to(bostonMap)


for i,row in dados.iterrows():
    folium.CircleMarker((row.Lat,row.Long), radius=3, weight=2, color='red', fill_color='red', fill_opacity=.5).add_to(bostonMap)







