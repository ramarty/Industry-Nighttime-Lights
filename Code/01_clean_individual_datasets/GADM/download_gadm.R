# Download GADM

# Filepaths --------------------------------------------------------------------
setwd(file.path(raw_data_file_path, "GADM"))

getData('GADM', country='CAN', level=0)
getData('GADM', country='CAN', level=1)
getData('GADM', country='CAN', level=2)
getData('GADM', country='CAN', level=3)


