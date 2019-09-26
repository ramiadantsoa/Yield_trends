# Yield_trends
This repository contains codes used for the manuscript: "Spatiotemporal trends in crop yields, yield variability, and yield gaps across the USA". Christopher J. Kucharick, Tanjona Ramiadantsoa, Jien Zhang, Anthony R. Ives

Three programs were used: R, Mathematica, and QGIS.

- Mathematica was used for the data treatment of the raw files, manipulations, and non-spatial visualizations. The main file is called: "ag_trends.nb"

- R was used to do the AR analyses. The results were exported as comma separated value (csv) file for manipulation and vizualization in R. The main (wrapper) file is called: "ag_trends.R", the other file: "source_function.R" contains all the functions needed in the main file.

- QGIS was used to visualize the spatial data. The file is called: "ag_trends.gqs"

All the data are in the compressed "clean" folder. 
The filename are organized by crop type, water management type, and data. E.g. "clean_corn_irr_fit_coeff.csv" contains the data for maize, irrigated and the fitted coefficients. 
res = residuals, lin = linear, quad = quadratic, and the suffix s means that the residuals are scaled with the mean.

N.B.: Some files are residuals from past analyses, the relevant ones are only used either in the mathematical file or in the QGIS file.

