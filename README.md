# Source Water Time Machine: California Public Water Systems R Shiny App Documentation

## Introduction
The State Water Board’s Division of Drinking Water (DDW) regulates approximately 7,500 public water systems (PWS) in California. A PWS is defined as a system that provides drinking water for human consumption to 15 or more connections or regularly serves 25 or more people daily for at least 60 days out of the year. The DDW allocates permits for PWS and collects an annual report from each system. More information about PWS [here](https://www.waterboards.ca.gov/drinking_water/certlic/drinkingwater/documents/waterpartnerships/what_is_a_public_water_sys.pdf).

## App Description
This app allows users to locate PWS on a map and determine the source water sampling locations, availability, operation status, type, and how water sources have changed over a limited subset of years (2013-2016). DDW classifies water source types as either wells (for groundwater) or intake facilities (for surface water). The classifications for source water availability are in the table below:

Source Availability | Value
--- | ---
Emergency | E
Interim | I
Other | O
Permanent | P 
Seasonal | S

<figure>
  <img src="https://github.com/julianaspector/SourceWaterTimeMachine/blob/master/Images/Map.png">
  <figcaption>Reported source water points for public water system CA1010007 in Fresno County.</figcaption>
</figure>

### PWS Boundaries
PWS boundaries were obtained from the [Water Boundary Tool](https://trackingcalifornia.org/water). As of September 25, 2019, boundaries for 5,529 PWS were available in the tool. To download this data in KML, CSV, Shapefile, or GeoJSON format, you can go to this [link](https://trackingcalifornia.org/water/login) and create an account. 

### DDW Source Water Points
The source water point information was obtained from a query of the Safe Drinking Water Information System [(SDWIS)](https://sdwis.waterboards.ca.gov/PDWW/) completed by DDW and provided for the datathon at the [2019 California Water Boards Water Data Science Symposium](https://www.waterboards.ca.gov/resources/data_databases/wq_science_symposium.html). A spreadsheet with the query results is available on the [datathon GitHub repo](https://github.com/CAWaterBoardDataCenter/PWStoSources/blob/master/20190619%20DDW%20Source%20Points.xlsx). The query only included source points with active status and water source facility types of intake and well. Additionally, these source points only reflect sampling locations and not all potential source waters may be included.

There is additional information available regarding source water operation statuses on the [Electronic Data Transfer Library](https://www.waterboards.ca.gov/drinking_water/certlic/drinkingwater/EDTlibrary.html) in the file SITELOC.DBF. The classifications for source water operation status are in the table below:

Status | Code
--- | ---
Abandoned | AB
Destroyed | DS
Inactive | IR/IU/IT
Standby | SR/SU/ST
Active | AR/AU/AT
Agriculture/Irrigation | AG
Distribution | DT/DR
Combined | CR/CU/CT/CM
Purchased | PR/PU/PT
Unknown | Unknown

For second letter of code: R=Raw, U=Untreated, T=Treated, M=Mixed

<figure>
  <img src="https://github.com/julianaspector/SourceWaterTimeMachine/blob/master/Images/Table.png">
  <figcaption>Table of source water information for public water system CA1010007 in Fresno County.</figcaption>
</figure>

### Production and Delivery Information
PWS submit their annual inventory information using the Electronic Annual Report (EAR) submission process on the Drinking Water Information Clearinghouse (DRINC) portal, which is reviewed, and manually updated into the Safe Drinking Water Information System (SDWIS). This is self-reported by PWS. The produced and delivered water reporting data for 2013-2016 is available on the [California Open Data Portal](https://data.ca.gov/dataset/drinking-water-public-water-system-annually-reported-water-production-and-delivery-information). 

<figure>
  <img src="https://github.com/julianaspector/SourceWaterTimeMachine/blob/master/Images/Graphs.png">
  <figcaption>Pie chart of source water relative fraction composition for public water system CA1010007 in Fresno County.</figcaption>
</figure>


### Accessing the app
The app is hosted online via shinyapps.io at this [link](https://jjspector.shinyapps.io/source_water_time_machine/).

### Acknowledgements
Thanks to SWRCB staff (Dori Bellan, Rafa Maestu, and Jen-Ann Lee) who provided helpful direction for compiling the necessary data. Also thanks to the datathon team at the 2019 Water Data Science Symposium whose fruitful discussion helped inspire many ideas for the app.
