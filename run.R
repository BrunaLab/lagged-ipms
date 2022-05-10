#to run as a background process in RStudio use the "Source" button and choose "Source as Local Job ..."
targets::tar_make_clustermq(workers = 9L)
