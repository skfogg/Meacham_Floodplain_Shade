##
## Byron's meacham floodplain input files
##

q <- read.table("byrons_input_files/usModelFlow_q_equal_2013_3_years.txt", skip = 1, col.names = c("t", "q"))

plot(q~t, data= q, type = "l")


gwt <-  read.table("byrons_input_files/upstreamGW_temps_fitted_3_years.txt", skip = 1, col.names = c("s", "e","temp"))
plot(temp~e, data = gwt, type= "l")

swt <- read.table("byrons_input_files/waterTempBC_3_years.txt", skip = 1, col.names = c("s", "e","temp"))
plot(temp~e, data = swt, type = "l")
lines(temp~e, data = gwt, col = "goldenrod")

at <- read.table("byrons_input_files/airTemperature_3_years.txt", skip = 1, col.names = c("t","temp"))
plot(temp~t, at, type = "l")
lines(temp~s, data = swt, col = "dodgerblue")
lines(temp~s, data = gwt, col = "goldenrod")
