# If you are missing any packages, uncomment lines 3 and 4 and run them.
# This only needs to be done once, it will then be installed in your local R.
# You DO NOT need to re-do it if you close the RStudio. You can comment the lines again.
#setwd(dirname(rstudioapi::getSourceEditorContext()$path))
#source("install_packages.R")

# Set current working directory
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

# Load the functions 
# (this only needs to be done once. You MAY NEED to re-do it if you close the RStudio.
# If some packages are missing, run lines 3 and 4 above. If the files cannot be found
# make sure you ran line 7.)
source("CrabDateRead_function.R")
source("CrabDateAnalyse_function.R")
source("MoonPhase_function.R")
source("other_functions.R")
source("moons.R")
source("Prep_Analysis.R")
source("accuracyCheck.R")
source("overview.R")
source("north.pred.R")
source("south.pred.R")


# the readAll function will read all files in a folder and produce
# a png output in the export folder where this script is located
# as well as a csv file for each sites (with the compiled results)
# a file that combines all results for all sites
readAll(24, n=2)

# This does the same as above, but does not read the files again and instead uses the ones loaded previously
# It only itends to be faster, if you have done some changes to the code, you may want to run this one
# so you don't have to wait so long for all the files to reload.
readAll(24, read=FALSE, n=2)

# Produce an overview file (make sure csv.all.vertical exists)
# this will only work fine with future files for some reason, still needs more work to produce overview for past
# It produces a file called Masterfile-R.xlsx and contains useful data (analysed)
if (exists("csv.all.vertical")){overview()}




# Analyse with Accuracy check

# This will only work when loading past data for REMAR sites only
# In input file, that's the folder called remar-sites-past
# Here we set n=1 which tells the program to compare the accuracy of the predictions
# against actual REMAR observations and store the results in a file called accuracy.csv
readAll(24, n=1)

# If your files are already loaded, you can run
readAll(24, read=FALSE, n=1)





# the readOne function will read one file only and produce an output in R studio
# this works fine but always assumes n=1, so may give weird accuracy checks when using
# future dates or sites that do not have observations to be compared to.

#readOne(24)
