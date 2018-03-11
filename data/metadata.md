# Dataset for fish egg sizes 
All data necessary to reproduce this current paper are available in `data/fishEggsMSData.csv`.

## Columns
*Order:* Order name;  
*Family:* Family name;  
*Genus:* Genus name;  
*Species:* Species name;  
*spawningMode:* Type of spawning as defined in Kasimatis & Riginos 2016 Coral Reefs. One of five types: scatterer, demersal, pelagic, mouth brooder, and pouch brooder;  
*FemaleSize_mm:* Female body size (actual individual or average across a sample of individuals) in millimetres from which the eggs have been measured;  
*sizeType:* Type of female body size, including SL (standard length), FL (fork length), and TL (total length);  
*eggSize_mm:* Egg diameter in millimetres which generally represent average across multiple measurements. For ellipsoid type eggs, diameter has been obtained by first estimating the volume of an ellipse, and then calculating the diameter of an spherical egg with equivalent volume;  
*typeOfWork_eggSize:* Type of measurement: "Field" means that observations were immediately done from field-collected material. "Field/Lab" means that field-collected material was kept for a determined period (indicated in the respective time column in months) in the lab prior to observations. Blank values are "Field";  
*timeToEggMeasurement_months:* For the "Field/Lab" values in the typeOfWork_eggSize column, indicates the number of months between specimen collection and trait measurement;  
*Location:* Place of collection;  
*Date:* Date of collection (year and month);  
*SampleSize:* Number of eggs measured;  
*Fertilized:* Logical, TRUE if eggs are fertilized, and FALSE if they are not;  
*Live:* Logical, TRUE if eggs are live, and FALSE if they are preserved;  
*Latitude:* Latitude of place of collection in decimals;  
*Longitude:* Longitude of place of collection in decimals;  
*ReferencePdf:* .pdf file from which the data has been collected (contact the authors in case you want to access some or all of these);  
*lnSize:* natural logarithm of FemaleSize_mm;  
*lnEggSize:* natural logarithm of eggSize_mm;  
*absLatitude:* absolute Latitude.  

# Consistencies between data taxonomy and OTL  

All file `data/otlSppSubstitutions.csv` contains a list of few species that do not match between the Actinopterygii tree from OTL and our dataset. This is only used internally for the purposes of tip matching between data (column *Species*), and tree (*ottFrame*). The relationship between the two can be found in the column *obs*.  
  
# Database references  
  
References in the column *ReferencePdf* in `data/fishEggsMSData.csv` are fully listed in `data/databaseReferences.pdf`.  

# Spawning mode data  
  
The file `data/spawningMode.csv` contains species-specific information on spawning mode associated with a particular reference (column *source*): this could be a link to www.FishBase.org, or a particular paper.  
