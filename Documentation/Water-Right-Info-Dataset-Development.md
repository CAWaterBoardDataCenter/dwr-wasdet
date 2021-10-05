Jeff Yeazell, P.E.

2/14/2021

Introduction
============

The Water Right Info dataset, identified as wr_info in the visualization
model, contains information information each right to which various
demand scenarios can be attached and visualized. The information
contained in wr_info will direct the model how to arrange priorities and
which HUC-8 watershed(s) to focus on, among others. The wr_info dataset
contains the following data:

-   Water Right ID (aka Application Number, Application ID, Statement
    ID)

-   Primary Owner Name

-   Permit/License/Certificate/Registration ID

-   Water Right Type and Status

-   Priority Date

-   Priority

    -   Describes how demands should be arranged by priority

-   HUC-8 Watershed Name

-   POD GIS Shape Information

Discussions on data sources used to create the wr_info dataset, data
gaps and what was done to mitigate them, and the approach used to create
the dataset are presented below.

Data Sources
============

In order to maintain as much transparency as possible, staff elected to
use data sources that are available to the public wherever possible when
developing the wr_info dataset. Two publicly available and actively
maintained datasets, the California Water Rights LIST and the Points of
Diversion Data Download, provide a majority of the information required
to filter the dataset to the degree required in the visualization tool
and to provide associated geospatial information. They are described in
further detail below.

California Water Rights List (Detail Summary List)
--------------------------------------------------

-   URL: <https://data.ca.gov/dataset/water-rights>

-   Updated: Weekly

The California Water Rights List, hosted on the California Open Data
Portal ([https://data.ca.gov)](https://data.ca.gov) includes detailed
information about every water right record in the State Water Resources
Control Board's "Electronic Water Rights Information Management System"
(EWRIMS) database. The list includes basic summary information about
each water right, such as the type and status, and priority information.

### Fields Used in Model

The following California Water Rights List fields are used in the
visualization model. Field names in parentheses are the corresponding
field names in the model.

-   Water Right ID - application_number (wr_id)

-   Primary Owner - primary_owner_name (owner)

-   Certificate, Permit, and License IDs - certificate_id, permit_id,
    and license_id

-   Water Right Type - water_right_type (wr_type)

-   Water Right Status - water_right_status (wr_status)

-   Application Received Date, Application Acceptance Date, and Priority
    Date - application_recd_date, application_acceptance_date, and
    priority_date

Points of Diversion Data
------------------------

-   URL:
    <https://waterrightsmaps.waterboards.ca.gov/viewer/Resources/Images/eWRIMS/download.htm>

-   Updated: Monthly

The Points of Diversion Data Download web page provides a location to
download the EWRIMS Point of Diversion (POD) GIS shapefile. It provides
the geospatial information used to plot the POD locations in the
visualization model, as well as HUC 8 name in which each POD is located.

### Fields Used in Model

In addition to the geospatial data provided in the shapefile, the
following Points of Diversion Data fields are used in the visualization
model. Field names in parentheses are the corresponding field names in
the model.

-   Water Right ID - APPL_ID (wr_id)

-   POD Identification - APPL_POD (appl_pod)

-   HUC 8 Name - HU_8\_NAME (huc8_name)

Data Gaps and Mitigations
=========================

Priority Dates
--------------

The priority_date field for the Post-1914 water rights in the EWRIMS
database (and also the Water Rights List) is just 10 percent complete,
making this field unusable to arrange these rights by priority date as
is. The application_acceptance_date field, however, is 97% complete and
the appl_recd_date field is 25 percent complete. In my seven years of
experience with the data held in ERIMS, the priority date is almost
always the earlier of the three associated date values. For this
dataset, a surrogate priority_date was defined as the earlier of
application_recd_date, application_acceptance_date, or priority_date.
Additional research will be conducted to identify priority dates for a
handful of active Post-14 water rights whose three priority date-related
values are empty. The correct priority dates will be entered into EWRIMS
so that the new data gets promulgated to the Water Right List dataset.

Disaggregating Riparian and Pre-1914 Appropriative Claims of Right
------------------------------------------------------------------

Pre-1914 appropriative claims of right generally follow the same
priority regime as their Post-1914 appropriative counterparts, only they
are more senior in right. The problem is, EWRIMS does not contain
sufficient data to distinguish bonafide Riparian claims from Pre-1914
claims, let alone the latters' priority dates, with any degree of
confidence. Both Riparian and Pre-1914 claims file the same Supplemental
Statement of Diversion and Use each year. XXXXXXXXXXXX

--\> Explain why binning Riparian & pre-14 claims together

Environmental Flows
-------------------

There has been ongoing discussion on whether to include
environmental/public trust demands in the supply-demand visualization
model.

Water Rights With PODs in More Than One HUC-8 Watershed
-------------------------------------------------------

Dataset Development Approach
============================

Presented below are the steps taken to create the wr_info dataset for
use in the supply-demand visualization model. In English, this is what
the prep-data.R script does:

1.  Process California Water Rights List Data

    1.  Download latest edition of dataset as necessary

    2.  Load data file

    3.  Remove records with no Water Right ID

    4.  Keep the following fields:

        1.  Water Right ID

        2.  Owner

        3.  Permit, License, and Certificate Numbers

        4.  Water Right Type and Status

        5.  Application Received and Application Acceptance Dates

        6.  Priority Date.

    5.  Filter the data for the following Water Right Types:

        1.  Appropriative

        2.  Cannabis, Domestic, Irrigation, and Livestock Registrations

        3.  Federal Claims and Stockponds

        4.  Statements of Diversion and Use

        5.  Stockponds

    6.  Filter the data for the following Water Right Statuses:

        1.  Permitted

        2.  Licensed

        3.  Certified

        4.  Registered

        5.  Claimed

    7.  Assign Water Right Class to easily split Post-14 appropriative
        rights (value = 'post14') and claimed rights (value = 'claimed')

    8.  Define Priority of Post-14 Appropriative Rights:

        1.  Filter out rights whose three fields used to define the
            priority date are empty.

        2.  Define priority date of each right as the earlier of the
            Application Received Date, Application Acceptance Date, and
            Priority Date values.

        3.  Define priority of each right as the year of its respective
            priority date. These priorities are modeled to be junior to
            Statement Diversions, with priority decreasing by year.

    9.  Define Priority of Statement Diversions:

        1.  Define priority of each statement as 'Statement Diversion.'
            This priority category is modeled to be junior to Statement
            Diversions, with priority decreasing by year.

    10. Code priority order into priority field:

        -   Environmental Flows \> Statement Diversions \> 1914 \>
            1915 \> ... \> 2020

2.  Process Points of Diversion Data (pods)

    1.  Download latest edition of POD geodatabase file as necessary

    2.  Load POD shapefile

    3.  Remove records whose Water Right ID is not in the wr_info
        dataset

    4.  remove and report on water rights with no HUC-8 watershed
        information.

    5.  Keep the following fields from the shapefile datatable:

        1.  Water Right ID

        2.  POD Number and ID (just in case for now, not sure we need to
            keep it)

        3.  HUC-8 Name

    6.  For each water right, determine the number of PODs associated
        with it.

    7.  For each water right, create a list of HUC-8 watersheds its PODs
        divert from.
