# WA_Pop_Analysis

Analysis of population of Washington through the period 1990-2010. The analysis is at a city level and observations are noted at County level.

For this analysis, I have used the 1990-2016 county wise population dataset available in the open data portal for Washington state here: http://data.wa.gov/Demographics/WAOFM-April-1-Population-by-State-County-and-City-/tecv-qzfm. 

This dataset did not have location coordinates of the cities. They were derived using Google APIs. I crunched this dataset using R to plot the year-wise and city-wise growth in population and built an interactive visualization for it using Shiny app framework.

Libraries required are:
dplyr for summarise
plyr for ddply;
reshape for melt;
ggmap for geocode;
stringr or string extraction;
ggplot2 for plots;
leaflet for mapping;
RColorBrewer for Coloring the circle markers on map
