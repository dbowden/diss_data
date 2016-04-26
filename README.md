# networkcreation
Scripts to create social networks from ICEWS data. This is primarily intended as a set of notes for myself, but others may find something useful here.

<!-- # The Raw Data

The raw data from which I construct networks comes from the ICEWS project (Boschee, Elizabeth; Lautenschlager, Jennifer; O'Brien, Sean; Shellman, Steve; Starz, James; Ward, Michael, 2015, "ICEWS Coded Event Data", http://dx.doi.org/10.7910/DVN/28075, Harvard Dataverse, V13), and is hosted at the [Harvard Dataverse](https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/28075). The proper files can be found under the "ICEWS Coded Event Data" dataset. There are a number of codebooks, quality checks, etc. included in the folder. The events are contained in yearly files with names such as "events.1995.20150313082510.tab.zip". I accessed the data in March 2016, and use the annual event files for the years 1995-2014.

# Text to CAMEO

To obtain Correlates of War countrycodes, more useful agent codes, and other niceties, I process the raw files using Phil Schrodt's [text_to_CAMEO python script](https://github.com/philip-schrodt/text_to_CAMEO). I have made a few slight modifications to Phil's script to retain columns that he removes from the raw data. The script is in the "raw_ICEWS_data" folder, and requires the "countrynames.txt", "agentnames.txt", and "filnames.txt" files to run (these are all unmodified from Phil's originals). From the terminal, `cd` to wherever you have these files (including raw ICEWS files) and run `python text_to_CAMEO_mod.py `. The resulting files intially wind up in the same directory. I move them to a different one called "processed_with_names" (too big to upload to github).

# Add Group Names

ICEWS tends to be overly disaggregated - it codes inviduals, whereas I am interested in constructing networks of groups. I thus use the actors dictionary provided on [Dataverse](https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/28118) to add the group affiliations when available. This is done in the "icews_actor_names.R" script. The script takes a while to run, and also cleans up two other issues - actor names often have a **Generic (Specific)** format and so I must extract the specific name, and I remove duplicate observations which usually result from the same event being covered in multiple media sources. The script results in a slimmed-down file that can be used to construct dissident networks for individual countries. -->