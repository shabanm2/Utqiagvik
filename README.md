# Utqiagvik

The intention of this repository is to 
a) create an easily accessible interface for sharing code and data amongst mentor and mentee(s)
b) allow for versioning of scripts
c) share scripts with heavy commenting and cleaned replicas for the purpose of teaching students about functions, packages, and general workflow. 
d) create a framework over time that integrates FAIR standards, once metadata can be shared with the public 


Please download all .csv files and the desired .Rmd file for your purposes. 
Change the read.csv lines in the first few chunks of the script to YOUR address / path, in the folder where your .csv file is saved.
Be sure to load all libraries in the first chunk of code prior to running chunks -- avoid switching the orders of the libraries you load (issue with dplyr and plyr if order is switched)

TEMPLATE_.......Rmd will contain the master script for data visualization with heavy emphasis on commenting through the process.
CLEANED_........Rmd will contain the master script for data visualization with little commenting throughout the process for a less-busy coding experience.

Statistics scripts will typically be in seperate .RMD files for the statistical test / data manipulation of interest.


Please cite the authors of this repository if you choose to adapt code from any of these files. 
