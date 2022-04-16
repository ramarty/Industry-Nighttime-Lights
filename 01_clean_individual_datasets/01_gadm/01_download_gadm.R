# Download GADM

# Filepaths --------------------------------------------------------------------
getData('GADM', country='CAN', level=0, path = file.path(data_file_path, "GADM", "FinalData", "blank_files"))
getData('GADM', country='CAN', level=1, path = file.path(data_file_path, "GADM", "FinalData", "blank_files"))
getData('GADM', country='CAN', level=2, path = file.path(data_file_path, "GADM", "FinalData", "blank_files"))
getData('GADM', country='CAN', level=3, path = file.path(data_file_path, "GADM", "FinalData", "blank_files"))

getData('GADM', country='MEX', level=0, path = file.path(data_file_path, "GADM", "FinalData", "blank_files"))
getData('GADM', country='MEX', level=1, path = file.path(data_file_path, "GADM", "FinalData", "blank_files"))
getData('GADM', country='MEX', level=2, path = file.path(data_file_path, "GADM", "FinalData", "blank_files"))
