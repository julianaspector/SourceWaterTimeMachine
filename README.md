# Source Water Time Machine: California Public Water Systems R Shiny App Documentation

## Introduction
The State Water Board’s Division of Drinking Water (DDW) regulates approximately 7,500 public water systems (PWS) in California. A PWS is defined as a system that provides drinking water for human consumption to 15 or more connections or regularly serves 25 or more people daily for at least 60 days out of the year. The DDW allocates permits for PWS and collects an annual report from each system. More information about PWS at this [link]. (https://www.waterboards.ca.gov/drinking_water/certlic/drinkingwater/documents/waterpartnerships/what_is_a_public_water_sys.pdf)

## App Description
This app allows users to locate PWS on a map and determine the source water locations, availability, operation status, type, and how water sources have changed over a limited subset of years (2013-2016). DDW classifies water source types as either wells (for groundwater) or intake facilities (for surface water). The classifications for source water availability are in the table below:

Source Availability | Value
--- | ---
Emergency | E
Interim | I
Other | O
Permanent | P 
Seasonal | S

### PWS Boundaries
PWS boundaries were obtained from the [Water Boundary Tool](https://trackingcalifornia.org/water)(Tracking California website, Public Health Institute). As of September 25, 2019, boundaries for 5,529 PWS were available in the tool. To download this data in KML, CSV, Shapefile, or GeoJSON format, you can go to this [link](https://trackingcalifornia.org/water/login) and create an account. 

### DDW Source Water Points
The source water point information was obtained from a query of the Safe Drinking Water Information System [(SDWIS)](https://sdwis.waterboards.ca.gov/PDWW/) completed by DDW and provided for the datathon at the [2019 California Water Boards Water Data Science Symposium](https://www.waterboards.ca.gov/resources/data_databases/wq_science_symposium.html). A spreadsheet with the query results is available on the [datathon GitHub repo](https://github.com/CAWaterBoardDataCenter/PWStoSources/blob/master/20190619%20DDW%20Source%20Points.xlsx). The query only included source points with active status and water source facility types of intake and well. 

There is additional information available regarding source water operation statuses on the [Electronic Data Transfer Library](https://www.waterboards.ca.gov/drinking_water/certlic/drinkingwater/EDTlibrary.html) in the file SITELOC.DBF. The classifications for source water operation status are in the table below:

Status | Code
--- | ---
Abandoned | AB
Destroyed | DS
Inactive | IR
Standby | SR
Active | AR
Agriculture/Irrigation | AG
Distribution | DT
Combined | CT
Purchased | PT
Unknown | Unknown

### Production and Delivery Information
PWS submit their annual inventory information using the Electronic Annual Report (EAR) submission process on the Drinking Water Information Clearinghouse (DRINC) portal, which is reviewed, and manually updated into the Safe Drinking Water Information System (SDWIS). This is self-reported by PWS. The produced and delivered water reporting data for 2013-2016 is available on the [California Open Data Portal] (https://data.ca.gov/dataset/drinking-water-public-water-system-annually-reported-water-production-and-delivery-information). 

### Instructions to run the app from GitHub


### Acknowledgements
Thanks to SWRCB staff (Dori Bellan, Rafa Maestu, and Jen-Ann Lee) who provided direction for compiling the necessary data. Also thanks to the datathon team at the 2019 Water Data Science Symposium whose fruitful discussion helped inspire many ideas for the app. 
