# REMAR Predictor Tool

The predictor tool involves analysing tidal data provided by a trusted source.

Using tidal data, the R algorithm is used to find patterns in tidal movement and moon phases with the aim
of predicting reproductive events of mangrove crabs.

Past tidal data was used to test the algorithm and compare its results against observational data.
The confidence level is fairly high (around 85% average).

It produces quite a number of files (3 plots and 1 csv file per tidal monitoring site), as well as a csv file containing all data for all sites including predictions.

This csv file is used to further analyse the data and produce a Masterfile with the [xlsx package](https://www.rdocumentation.org/packages/xlsx/versions/0.6.5)
that contains a sheet per month (of the reproductive season only) and a summary for each of the years.

Example of the final output:

![](https://github.com/musevarg/REMAR-Predictor-Tool/blob/main/predictor/pics/pic2.PNG)
![](https://github.com/musevarg/REMAR-Predictor-Tool/blob/main/predictor/pics/pic1.PNG)

This data is further supported with plots. Three plots are produced. One uses the [oce package](https://cran.r-project.org/web/packages/oce/index.html), the next two only data from the tidal files. Plot 1 (top left) and 2 (bottom left) are meant to be very similar. Plot 3 (bottom right) uses the same data as Plot 2, but uses small multiples (this is particulary useful when dealing with data spanning 5 years or more).

![](https://github.com/musevarg/REMAR-Predictor-Tool/blob/main/predictor/pics/plots.png)
