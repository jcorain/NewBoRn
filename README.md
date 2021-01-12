# Newborn survey application 

## Purpose

The purpose of this package is to give easy to use interactive platform for newborn needs survey. The given information are only usable for your own survey and can not be seen as medical information. For any medical information contact your medic. 

## Data 

### Data model

The data framework is directly taken and adapted from the protocol of the triemlispital frauenklinik in Zürich. It consists of 11 columns : 
   - Date in the format "dd-mm-yyyy" as character. Mandatory for each row.
   - Hour in the format "HH:MM" as charcater. Mandatory for each row.
   - Weight as a numeric. 
   - Temperature as a numeric.
   - Mother_Milk as a numeric.
   - Powder_Milk as a numeric.
   - Lactation_Left as a boolean.
   - Lactation_Right as a boolean.
   - Vomit as a numeric.
   - Urin as a numeric.
   - Poop as a numeric.
   
For blank values, please fill them with NA. The data has to be saved in .csv for portability. Best practice is to use as filename : <firstname>_<surname>_data.csv but it is not compulsory.

Depending on further needs and applications, the data structure is subject to changes.

### Data creation, update and save

The easiest way to compile with data model is to use the defined function within the package, create_new_data, append_to_data and save_data. The use of those functions is explained within the package documentation.
   
## Graph 

The actual graphics displayed are for differnt survey : 
  - Weight vs Time
  - Temperature vs Time
  - Lactation vs Time
  - Milk feeding vs TIme
  - Dejection versus Time 
  
For the moment, two time granularity are available, either day or hour, for lactation, Milk feeding and dejection graphs. The graphic format is plotly for easy zooming and saving on user need. 

## Shiny application

The data creation, updating, saving and the concomitant plots can be done via a shiny application. Its use seems to me user-friendly (at least for data people) but if some documentation is needed, I will try to provide some. 

## Bug reporting 

To report a bug, please use the gitlab page for the package for the sake of documentation. You can open a new issue on this page and we will try to tackle it down. 

## Contribution

Any contribution is most welcomed. Feel free to contact the maintainer of the package for any added value. 
