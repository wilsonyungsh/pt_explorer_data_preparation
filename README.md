# pt_explorer_data_preparation
This is the data preparation part of pt_explorer dashboard, which include generating PT Stop, routesm full schedule and aggregated stop level trend based on GTFS data

# Component
**Data Source** : Translink GTFS data from [API](https://www.data.qld.gov.au/dataset/general-transit-feed-specification-gtfs-translink/resource/e43b6b9f-fc2b-4630-a7c9-86dd5483552b)

**Output** :
1. Public transport stop type(mode) and location
2. Public transport route geometry and mode
3. Servie Trend and characteristics at aggregated Stop Level across service period for the GTFS file used (normally 3 month after publish date)