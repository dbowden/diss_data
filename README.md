# networkcreation
Scripts to create social networks from ICEWS data

# The Raw Data

The raw data from which I construct networks comes from the ICEWS project V13, and is hosted at the [Harvard Dataverse](https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/28075). The proper files can be found under the "ICEWS Coded Event Data" dataset. There are a number of codebooks, quality checks, etc. included in the folder. The events are contained in yearly files with names such as "events.1995.20150313082510.tab.zip". I accessed the data in March 2016, and use the annual event files for the years 1995-2014.

# Text to CAMEO

To obtain Correlates of War countrycodes, more useful agent codes, and other niceties, I process the raw files using Phil Schrodt's text_to_CAMEO python script ([github repo](https://github.com/philip-schrodt/text_to_CAMEO)). I have made a few slight modifications to Phil's script to retain columns that he removes from the raw data.