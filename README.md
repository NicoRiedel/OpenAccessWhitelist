Open Access Journal Whitelist
================

This R script creates an ‘Open Access Journal Whitelist’ by aggregating information on Open Access Journals from the [Directory of Open Access Journals (DOAJ)](https://doaj.org/), [Pubmed Central (PMC)](https://www.ncbi.nlm.nih.gov/pmc/journals/) and the [SCimago Journal Rank](http://www.scimagojr.com/). These two data sources ensure that the Journals obey certain quality standards. DOAJ ensures high quality standards for journals; individual journals have to apply at DOAJ and are checked against a list of quality criteria. PMC stores the full-text version of open access articles and increases the visibility of research in that way. The current list focuses on biomedical journals but the included subjects can be adjusted in the 'adjustable parameters' section. Only journals that are assigned to the DOAJ subject categories ‘Medicine’ or ‘Biology’ and that have English or German as full-text language are included.

Re-using this List
--------------

You can adapt and reuse this Open Access Journal Whitelist and adapt it to your field of interest by changing the following adjustable parameters:

Change included journal languages  
`full_text_languages <- c("English", "German")`

Change included subjects  
`subjects_included <- c("Medicine", "Biology", "Biotechnology")`

Change excluded subjects (e.g. subfields that do not need to be covered)  
`subjects_excluded <- c("Agriculture", "Plant culture")`

For a full list of available subject terms see the [DOAJ subject list](https://doaj.org/subjects).

Change included currencies for the Article Processing Charges (APC)  
`APC_currencies_included <- c("EUR - Euro", "GBP - Pound Sterling", "USD - US Dollar", "CHF - Swiss Franc")`

The script creates two files when run: A .csv file which can be loaded into e.g. Excel and a .rds file which is used for the R-Shiny App.

R-Shiny App
---------------

There is also a R-Shiny App included in the 'OAWhitelist_shiny' folder to display the resulting table in an easily searchable and filterable way in the browser. An online version of the original list can be found on http://s-quest.bihealth.org:3838/OAWhitelist/.

Authors
---------------

* **Nico Riedel** - *Concept, Technical implementation*
* **René Bernard** - *Concept*
* **Lisa Liebenau** - *Concept*

Contact
---------------

Please e-mail <nico.riedel@bihealth.de>.

Licence
---------------
This project is licensed under the MIT License - see the LICENSE.md file for details
