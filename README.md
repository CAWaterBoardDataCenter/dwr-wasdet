# Development Log

## Water Availability Visualization Sandbox

#### Prior to 9/20/2020:

-   Identified active rights that have PODs in multiple HUC8s.

    -   Script: `identify-multi-huc8-water-rights.R`

-   Evaluated completeness of HUC8 info in WBGIS POD layer

    -   Layer will be used as data source for table info and geospatial info.

#### Week of 9/21/2020:

-   Finished squashing parsing errors while loading California Water Rights LIST.

    -   <https://data.ca.gov/dataset/water-rights>

-   Explored viability of using Publicly available POD layer from

    -   <https://waterrightsmaps.waterboards.ca.gov/viewer/Resources/Images/eWRIMS/download.htm>

#### Week of 9/28/2020:

Established priority dates/years for post-14 rights. While doing so, I found 11 rights for which I could no establish priority dates because all three of the fields I use to establish this date are empty. I hopped on eWRIMS to see if I could pull the priority dates off of the permit/license/certificate PDFs stored within it. For those that had the PDFs available, I added their priority dates to the field in eWRIMS to see if the changes propagate all the way to the CA Open Data Portal water right list dataset. I'm going to ask the records room to pull and scan the remaining documents from the files. Summary spreadsheet attached.

-   Defined priority order in order to stack demand correctly. Currently, it is set as, in order of assigned priority:

    -   Environmental Flow (as a placeholder)
    -   Statement Diversion - It is difficult, if not impossible, to reliably disaggregate Pre-14 claims from riparian claims using information available in eWRIMS. Many Statement filers claim both Riparian *and* Pre-1914 rights on a single statement. For those that claim strictly Pre-1914 rights, the priority information associated with them is spotty, and unverifiable. For now, I'm lumping them together as a common priority senior to Post-14 rights and junior to the Environmental Flow placeholder.
    -   1914
    -   1915
    -   ...
    -   current year

Began processing geospatial data to attach to `wr_info`:

1.  Download and extract geodatabase.

2.  Filter out PODs of rights not in `wr_info`

3.  Filter out PODs that have empty `hu_8_name` values

9/30/20- EXPERIMENTING ON:

-   Dissolve PODs of multi-POD rights into polygons. Hopefully they display as points like in ArcMap. `package::rmapshaper` looks promising...

10/26/20

-   Combine `wr_info`and `pods` prep int one file, renamed `01-prep-wr_info.R`

-   Migrated geospatial handling to `sf` package. It supercedes `sp`.

-   Added weighting factor column for multi-huc rights

    -   Default weighting is to evenly split demand among HUC8s

    -   Need to research and develop weight look-up table

12/4/20

-   Creation of `wr_info` complete.

-   Created GeoJSSON file out of WBD HUC-8 shapefile. Included in `common` folder as part of the app.

-   Plot creation:

    -   Can select HUC8 watershed and priority level at which to highlight junior appropriative rights.

Week of 12/9/20

-   Implementing multi

-   Since the dashboard is HUC-8-based, it makes more sense to organize demands by HUC-8. Rewriting `prep-reported-diversions.R` to accomplish this.

    -   -\> `02-prep-rep-divs-by-huc.R`

### To-Do List

-   Upgrade `prep_demand_daily.R` to process all reported diversions for 2010 on into list, save as .RData for tool to pick up.

-   Cleaned: All upper case, extra spaces removed between names.

    -   Water Right Type (wr_status)

    -   Water Right Status (wr_type)

    -   HUC 8 Name (huc8_name)

    -   HUC 8 ID (huc8_id)

    2\. Geospatial:

    -   Dissolved PODs (not sure if point line or polygon class)

### Acknowledgments

-   Piper Welch, ? College - Conducted research to define intra-HUC demand weighting factors for water rights with PODs in multiple HUC8 wagtersheds.
