# Source Water Time Machine: California Public Water Systems Source Locations and Information R Shiny App Documentation

## Introduction
The State Water Board’s Division of Drinking Water (DDW) regulates approximately 7,500 public water systems (PWS) in California. A PWS is defined as a system that provides drinking water for human consumption to 15 or more connections or regularly serves 25 or more people daily for at least 60 days out of the year. The DDW allocates permits for PWS and collects an annual report from each system. More information about PWS at this [link]. (https://www.waterboards.ca.gov/drinking_water/certlic/drinkingwater/documents/waterpartnerships/what_is_a_public_water_sys.pdf)

## App Description
This app allows users to locate PWS on a map and determine the source water locations, availability, and type. DDW classifies water source types as either wells (for groundwater) or intake facilities (for surface water). The classifications for source water availability are in the table below:

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





