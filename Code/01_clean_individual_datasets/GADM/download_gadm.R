# Download GADM

# Filepaths --------------------------------------------------------------------
setwd(file.path(data_file_path, "GADM", "RawData"))

getData('GADM', country='CAN', level=0)
getData('GADM', country='CAN', level=1)
getData('GADM', country='CAN', level=2)
getData('GADM', country='CAN', level=3)

getData('GADM', country='MEX', level=0)
getData('GADM', country='MEX', level=1)
getData('GADM', country='MEX', level=2)
