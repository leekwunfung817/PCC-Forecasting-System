# cd "C:\xampp\htdocs\TacticalDataDrivenDecisionSystem\"
# & "C:/Program Files/R/R-4.4.3/bin/Rscript.exe" ./forecasting-cubicspline-daily.r


debug = FALSE
# install.packages("jmv", repos = "https://cran.r-project.org")
# install.packages("jmvconnect", repos = "https://cran.r-project.org")

# install.packages("RMySQL", repos = "https://cran.r-project.org")
# install.packages("RMariaDB", repos = "https://cran.r-project.org")

# install.packages("ggplot2", repos = "https://cran.r-project.org")
# install.packages("dplyr", repos = "https://cran.r-project.org")
# install.packages("tidyr", repos = "https://cran.r-project.org")
# install.packages("lubridate", repos = "https://cran.r-project.org")
# install.packages("ggpubr", repos = "https://cran.r-project.org")


# install.packages("english", repos = "https://cran.r-project.org")









library(grid)

library(png)


library(english)
source("C:\\xampp\\htdocs\\TacticalDataDrivenDecisionSystem\\r_library\\ch2.r")
source("C:\\xampp\\htdocs\\TacticalDataDrivenDecisionSystem\\r_library\\ch3.r")
source("C:\\xampp\\htdocs\\TacticalDataDrivenDecisionSystem\\r_library\\ch4.r")

image_path <- "./forecasting-semantic-model-annually/"
knitr::opts_chunk$set(fig.path = image_path)

# Load required packages
library(RMySQL)
library(ggplot2)
library(dplyr)
library(tidyr)
library(lubridate)
library(ggpubr)

library(rlang)

options(scipen = 999)


# Convert all matrix text to float
matrix_to_float <- function(mat) {
  # Convert matrix to numeric vector and then back to matrix
  numeric_data <- as.numeric(mat)
  # Restore matrix dimensions
  matrix(numeric_data, nrow = nrow(mat), ncol = ncol(mat), 
         dimnames = dimnames(mat))
}

# Replace non-numeric values with NA or a default number (e.g., 0)
matrix_to_float_safe <- function(mat) {
  # Convert elements to numeric, suppress warnings
  numeric_data <- suppressWarnings(as.numeric(mat))
  # Restore matrix structure
  matrix(numeric_data, nrow = nrow(mat), ncol = ncol(mat), 
         dimnames = dimnames(mat))
}





get_conn <- function() {
  # Database connection
  con <- dbConnect(MySQL(),
                  user = "pcc-sdr",         # Replace with your MySQL username
                  password = "6zu_.Ldx2.zCNb_8",         # Replace with your MySQL password
                  dbname = "pcc_forecast_db",
                  host = "127.0.0.1")
  return(con)
}















con <- get_conn()
sql <- paste0("CALL `clean_net_pax`();")
dbGetQuery(con, sql)
dbDisconnect(con)









con <- get_conn()
  
sql <- paste0("
  SELECT FLOOR(MIN(`x`)) `Min_Year`, YEAR(CURRENT_TIMESTAMP) this_year FROM `temp_forecast_daily` ORDER BY x DESC;
  ")
year_range <- dbGetQuery(con, sql)
print("Year Range")
print(year_range)
print(year_range$this_year)
print(year_range$Min_Year)
  
this_year <- as.integer(year_range$this_year)
starting_year <- as.integer(year_range$Min_Year)
  

# Disconnect from database
dbDisconnect(con)


















################################################################ all period


con <- get_conn()

sql <- paste0("
  SELECT
    x, avg(y) y
  FROM temp_forecast_daily 
  WHERE 1=1
  group by x
  order by x asc 
")

print(sql)
ticket_data <- dbGetQuery(con, sql)
dbDisconnect(con)

A=matrix_data <- as.matrix(ticket_data);
A <- matrix_to_float_safe(A);


print('Check point 3')

x_list <- A[, 1] # First column for x values
y_list <- A[, 2] # Second column for y values

x <- A[, 1] # First column for x values
y <- A[, 2] 

# print(A)
# print(x)
# print(y)
max_y=max(y)

print('Check point 4')

M <- spline(A);
print('Check point 4.1')
# year__ is the normalized year decimal
# This function is prepare for graph drawing
# because the graph function will input the only x value as nomalized year
f <- function(year__) {
  # M is the data point matrix that provide the subic spline function to proceed interpolation
  # In the matrix (x->normalized date (year) ) (y->PAX number)
  splinefunc(M,year__)
}

#f = lagrange(A)

print('Check point 5')

h = 0.001
fp  <- function(year_) {
  (((f(year_ + h) - f(year_))/h)*(1/365))
}
fpp  <- function(year_) {
  (((fp(year_ + h) - fp(year_))/h)*(1/365))
}
fppp  <- function(year_) {
  (((fpp(year_ + h) - fpp(year_))/h)*(1/365))
}

fpppp  <- function(year_) {
  (((fppp(year_ + h) - fppp(year_))/h)*(1/365))
}


#print(f)
print('Check point 6')

title_ = paste0("All Interpolation")
image_path_ <- paste0(image_path,title_,".png")
# png(image_path_)
curve(
  f,
  from=starting_year,
  to=this_year,
  col = "black",
  
  xlab = "X-axis", 
  ylab = "Y-axis", 
  
  ylim = c(0, max_y),
  
  main = title_
)

print('Check point 7')

#points(x, y, col = "red", pch = 19, cex = 1.5)  # Add the point in red
#text(x, y, labels = x, pos = 1, cex = 0.7)
#text(x, y, labels = y, pos = 3, cex = 0.7)

x_vals <- x
y_vals <- fp(x)
y_vals[is.na(y_vals)] <- 0
fp_y_max = max(y_vals)
fp_y_min = min(y_vals)

grid(nx = NULL, ny = NULL, col = "gray", lty = "dotted")
# dev.off()
# grid.raster(readPNG(image_path_))
 
print('Check point 8')

title_ = paste0("All First Derivative")
image_path_ <- paste0(image_path,title_,".png")
# png(image_path_)
curve(
  fp,
  from=starting_year,
  to=this_year,
  col = "purple",
  
  xlab = "X-axis", 
  ylab = "Y-axis", 
  
  ylim = c(fp_y_min, fp_y_max),
  
  main = title_,
  add=FALSE
)
grid(nx = NULL, ny = NULL, col = "gray", lty = "dotted")
# dev.off()
# grid.raster(readPNG(image_path_))

fpp_y = fpp(x)
fpp_y[is.na(fpp_y)] <- 0
fpp_y_max = max(fpp_y)
fpp_y_min = min(fpp_y)

print('Check point 9')

title_ = paste0("All Second Derivative")
image_path_ <- paste0(image_path,title_,".png")
# png(image_path_)
curve(
  fpp,
  from=starting_year,
  to=this_year,
  col = "red",
  
  xlab = "X-axis", 
  ylab = "Y-axis", 
  
  ylim = c(fpp_y_min, fpp_y_max),
  
  main = title_ ,
  add=FALSE
)
grid(nx = NULL, ny = NULL, col = "gray", lty = "dotted")
# dev.off()
# grid.raster(readPNG(image_path_))

print('Check point 10')

title_ = paste0("All Third Derivative")
image_path_ <- paste0(image_path,title_,".png")
# png(image_path_)
curve(
  fppp,
  from=starting_year,
  to=this_year,
  col = "orange",
  
  xlab = "X-axis", 
  ylab = "Y-axis", 
  
  ylim = c(fpp_y_min, fpp_y_max),
  
  main = title_ ,
  add=FALSE
)
grid(nx = NULL, ny = NULL, col = "gray", lty = "dotted")
# dev.off()
# grid.raster(readPNG(image_path_))

print('Check point 11')

title_ = paste0("All Fourth Derivative")
image_path_ <- paste0(image_path,title_,".png")
# png(image_path_)
curve(
  fpppp,
  from=starting_year,
  to=this_year,
  col = "green",
  
  xlab = "X-axis", 
  ylab = "Y-axis", 
  
  ylim = c(fpp_y_min, fpp_y_max),
  
  main = title_ ,
  add=FALSE
)
grid(nx = NULL, ny = NULL, col = "gray", lty = "dotted")
# dev.off()
# grid.raster(readPNG(image_path_))


print('Check point 12')


interpolation_derivative <- function(name,from_,to_) {
  title_ = paste0(name)
  image_path_ <- paste0(image_path,title_,".png")
  # png(image_path_)
  curve(
    f,
    from=from_,
    to=to_,
    col = "black",
    
    xlab = "X-axis", 
    ylab = "Y-axis", 
    
    ylim = c(fp_y_min, max_y),
    
    main = title_
  )
  curve(
    fp,
    from=from_,
    to=to_,
    col = "purple",
    
    xlab = "X-axis", 
    ylab = "Y-axis", 
    
    ylim = c(fp_y_min, fp_y_max),
    
    add=TRUE
  )
  curve(
    fpp,
    from=from_,
    to=to_,
    col = "red",
    
    xlab = "X-axis", 
    ylab = "Y-axis", 
    
    ylim = c(fpp_y_min, fpp_y_max),
    add=TRUE
  )
  curve(
    fppp,
    from=from_,
    to=to_,
    col = "orange",
    
    xlab = "X-axis", 
    ylab = "Y-axis", 
    
    ylim = c(fpp_y_min, fpp_y_max),
    add=TRUE
  )
  curve(
    fpppp,
    from=from_,
    to=to_,
    col = "green",
    
    xlab = "X-axis", 
    ylab = "Y-axis", 
    
    ylim = c(fpp_y_min, fpp_y_max),
    add=TRUE
  )
  # dev.off()
  # grid.raster(readPNG(image_path_))
}

print('Check point 13')

max_iteration_avg_combine = 30

get_avg_curve <- function(diff_func,batch_list__) {
  if (debug) {
    print(diff_func)
    print('diff_func:')
    
    #print(batch_list__)
    print('batch_list__:')
  }
  
  random_list = batch_list__
  if (length(random_list) > max_iteration_avg_combine) {
    random_list = random_list[sample(length(random_list), max_iteration_avg_combine)]
  }
  avg_curve <- function(x) {
    #print(x)
    #print('avg_curve:x')
    if (debug) {
      print(random_list)
      print('avg_curve:random_list')
    }
    
    if (is.null(x)) {
      print('x is null, average graph cannot be outputed')
    }
    if (is.null(random_list)) {
      print('random_list is null, average graph cannot be outputed')
    }
    if (debug) {
      print('avg_curve:start loop')
    }
    total = as.double(0)
    total_count = 0
    # print('Sampling length(random_list):')
    # print(length(random_list))

    for (item in random_list) {

      result <- tryCatch({

        # print('X:')
        x__ <- item$from + x
        # print(x__)
        
        # print('Y:')
        y__ <- diff_func(x__)
        # print(y__)
        
        if (debug) {
          print(x__)
          print('avg_curve:x__')
          
          print(y__)
          print('avg_curve:y__')
          
          print(diff_func)
          print('avg_curve:diff_func')
        }
        if (sum(is.na(y__))>0) {
          stop('Invalid graph')
        }
        
        total <- as.double(
          as.double(total) + as.double(y__)
        )
        total_count = (total_count + 1)
        
        if (debug) {
          print(total)
          print('avg_curve:total 1')
        }

      }
      #, warning = function(w) { message("Warning: ", w); }
      , error = function(e) {
        print('iteration ')
        print(total_count)
        message("avg_curve Error: ", e);
        message("avg_curve Error: ", e$message)
        #message("Full traceback:")
        #print(last_trace())
      }
      #, finally = {message("Finished trying to evaluate expression.");}
      );
      #print(result)


    }
    if (debug) {
      print(total)
      print('avg_curve:total 2')
    }
    average_of_total = (total/total_count)
    # print(average_of_total)
    # print('average_of_total:')
    
    if (debug) {
      print(random_list)
      print('avg_curve:random_list 2')
    }
    
    # print('length(random_list):')
    # print(length(random_list))
    # print('total:')
    # print(total)
    # print('average of total:')
    # print(average_of_total)
    return(average_of_total)
  }
  return(avg_curve)
}

print('Check point 14')

get_std_curve <- function(diff_func,batch_list__) {
  random_list <- batch_list__
  if (length(random_list) > max_iteration_avg_combine) {
    random_list <- random_list[sample(length(random_list), max_iteration_avg_combine)]
  }
  std_curve <- function(x) {
    # print(x)
    # print('std_curve:x')
    if (debug) {
      print(random_list)
      print('std_curve:random_list')
    }
    if (is.null(x)) {
      print('x is null, average graph cannot be outputed')
    }
    if (is.null(random_list)) {
      print('random_list is null, average graph cannot be outputed')
    }
    
    
    if (debug) {
      print('std_curve:start loop')
    }

    # print('Sampling length(random_list):')
    # print(length(random_list))

    total = as.double(0)
    total_count = 0
    for (item in random_list) {

      result <- tryCatch({

        x__ <- item$from + x
        y__ <- diff_func(x__)
        
        if (debug) {
          print(x__)
          print('std_curve:x__')
          
          print(y__)
          print('std_curve:y__')
          
          print(diff_func)
          print('std_curve:diff_func')
        }
        
        
        if (sum(is.na(y__))>0) {
          stop('Invalid graph')
        }
        total <- as.double(
          as.double(total) + as.double(y__)
        )
        total_count = (total_count + 1)
        
        if (debug) {
          print(total)
          print('std_curve:total')
        }

      }
      #, warning = function(w) { message("Warning: ", w); }
      , error = function(e) {
        print('iteration ')
        print(total_count)
        message("std_curve Error: ", e);
        message("std_curve Error: ", e$message)
        message("Full traceback:")
        print(last_trace())
      }
      #, finally = {message("Finished trying to evaluate expression.");}
      );
      #print(result)

    }
    average_of_total = as.double(total/total_count)
    # print(average_of_total)
    # print('average_of_total:')
    
    total_variant = as.double(0)
    for (item in random_list) {
      x__ <- item$from + x
      y__ <- diff_func(x__)
      if (sum(is.na(y__))>0) {
        stop('Invalid graph')
      }
      total_variant <- as.double(
        as.double(total_variant) + (
          as.double(y__)-average_of_total
        )^2
      )
    }
    std <- sqrt(total_variant/length(random_list))
    # print('length(random_list):')
    # print(length(random_list))
    # print('total:')
    # print(total)
    # print('average of total:')
    # print(average_of_total)
    return(std)
  }
  return(std_curve)
}

print('Check point 15')

con <- get_conn()
dbExecute(con, "
  CREATE TABLE IF NOT EXISTS curve_points (
    id INT AUTO_INCREMENT PRIMARY KEY,
    name VARCHAR(100),
    x DOUBLE,
    y DOUBLE
  )
")
dbDisconnect(con)

con <- get_conn()
dbExecute(con, "
  DELETE FROM curve_points WHERE 1=1;
")
dbDisconnect(con)

store_curve <- function(curve_name, curve_function, from_, to_) {
  # Generate 30 points
  x_vals <- seq(from_, to_, length.out = 100)
  y_vals <- curve_function(x_vals)

  # Combine into a data frame
  curve_data <- data.frame(x = x_vals, y = y_vals)
  for (i in 1:nrow(curve_data)) {
    con <- get_conn()
    dbExecute(
      con, 
      paste0("INSERT INTO curve_points (name, x, y) VALUES ('",curve_name,"',",curve_data$x[i]," ,",curve_data$y[i],");"),
      params = list()
    )
    dbDisconnect(con)
  }
}

batch_draw_avg <- function(batch_list__,title_) {
  first_iteration = batch_list__[[1]]
  from_ = 0
  to_ = first_iteration$to - first_iteration$from
  
  # print('Average Graph (range from):')
  # print(to_)
  
  fp_avg_curve = get_avg_curve(fp,batch_list__)
  fp_std_curve = get_std_curve(fp,batch_list__)
  
    
  fpp_avg_curve = get_avg_curve(fpp,batch_list__)
  fpp_std_curve = get_std_curve(fpp,batch_list__)
  
  fppp_avg_curve = get_avg_curve(fppp,batch_list__)
  fppp_std_curve = get_std_curve(fppp,batch_list__)
  
  fpppp_avg_curve = get_avg_curve(fpppp,batch_list__)
  fpppp_std_curve = get_std_curve(fpppp,batch_list__)
  path__ <- paste0(image_path,"avg-",title_)
  image_path_ <- paste0(path__,".png")







  store_curve(paste0(path__,'-f1'), fp_avg_curve, from_, to_)
  store_curve(paste0(path__,'-f2'), fpp_avg_curve, from_, to_)
  store_curve(paste0(path__,'-f3'), fppp_avg_curve, from_, to_)
  store_curve(paste0(path__,'-f4'), fpppp_avg_curve, from_, to_)

  path__ <- paste0(image_path,"std-",title_)

  store_curve(paste0(path__,'-s1'), fp_std_curve, from_, to_)
  store_curve(paste0(path__,'-s2'), fpp_std_curve, from_, to_)
  store_curve(paste0(path__,'-s3'), fppp_std_curve, from_, to_)
  store_curve(paste0(path__,'-s4'), fpppp_std_curve, from_, to_)




  # # png(image_path_)
  # curve(
  #   fp_avg_curve,
  #   from=from_,
  #   to=to_,
  #   col = "purple",
    
  #   xlab = "X-axis", 
  #   ylab = "Y-axis", 
    
  #   ylim = c(fp_y_min, fp_y_max),
  #   main = title_,
    
  #   add=FALSE
  # )
  # curve(
  #   fpp_avg_curve,
  #   from=from_,
  #   to=to_,
  #   col = "red",
    
  #   xlab = "X-axis", 
  #   ylab = "Y-axis", 
    
  #   ylim = c(fpp_y_min, fpp_y_max),
  #   add=TRUE
  # )
  # curve(
  #   fppp_avg_curve,
  #   from=from_,
  #   to=to_,
  #   col = "orange",
    
  #   xlab = "X-axis", 
  #   ylab = "Y-axis", 
    
  #   ylim = c(fpp_y_min, fpp_y_max),
  #   add=TRUE
  # )
  # curve(
  #   fpppp_avg_curve,
  #   from=from_,
  #   to=to_,
  #   col = "green",
    
  #   xlab = "X-axis", 
  #   ylab = "Y-axis", 
    
  #   ylim = c(fpp_y_min, fpp_y_max),
  #   add=TRUE
  # )
  # # dev.off()
  # # grid.raster(readPNG(image_path_))
  
  
  
  
  
  
  # image_path_ <- paste0(image_path,"standard diviation",title_,".png")
  # # png(image_path_)
  # curve(
  #   fp_std_curve,
  #   from=from_,
  #   to=to_,
  #   col = "purple",
    
  #   xlab = "X-axis", 
  #   ylab = "Y-axis", 
    
  #   ylim = c(fp_y_min, fp_y_max),
  #   main = title_,
    
  #   add=FALSE
  # )
  # curve(
  #   fpp_std_curve,
  #   from=from_,
  #   to=to_,
  #   col = "red",
    
  #   xlab = "X-axis", 
  #   ylab = "Y-axis", 
    
  #   ylim = c(fpp_y_min, fpp_y_max),
  #   add=TRUE
  # )
  # curve(
  #   fppp_std_curve,
  #   from=from_,
  #   to=to_,
  #   col = "orange",
    
  #   xlab = "X-axis", 
  #   ylab = "Y-axis", 
    
  #   ylim = c(fpp_y_min, fpp_y_max),
  #   add=TRUE
  # )
  # curve(
  #   fpppp_std_curve,
  #   from=from_,
  #   to=to_,
  #   col = "green",
    
  #   xlab = "X-axis", 
  #   ylab = "Y-axis", 
    
  #   ylim = c(fpp_y_min, fpp_y_max),
  #   add=TRUE
  # )
  # dev.off()
  # grid.raster(readPNG(image_path_))
  
  
  
  forecasting_seed <- list(
    title=title_,
    avg = list(
      fp_avg = fp_avg_curve,
      fpp_avg = fpp_avg_curve,
      fppp_avg = fppp_avg_curve,
      fpppp_avg = fpppp_avg_curve
    ),
    std = list(
      fp_std = fp_std_curve,
      fpp_std = fpp_std_curve,
      fppp_std = fppp_std_curve,
      fpppp_std = fpppp_std_curve
    ),
    from=from_,
    to=to_
  )
  # print('forecasting seed:')
  # print(forecasting_seed)
  return(forecasting_seed)
  
}

print('Check point 16')

batch_draw <- function(batch_list__) {
  for (item in batch_list__) {
    interpolation_derivative(
      item$title
      ,from=item$from
      ,to=item$to
    )
  }
  
  
}

print('Check point 17')


batch_list <- list()
for (i in starting_year:(this_year-1)) {
  # For COVID
  if (i == 2020 || i==2021) {
    next
  }
  assoc_array <- list(
    title = paste0("annually ",i)
    ,from=i
    ,to=(i+1)
  )
  batch_list <- append(batch_list, list(assoc_array))
}
annually_forecasting_seed = batch_draw_avg(batch_list, "annually")
annually_batch_list <- batch_list
# batch_draw(batch_list)


print('Check point 18')




batch_list <- list()
con <- get_conn()
sql <- paste0("CALL `select_months`(",starting_year,", ",this_year,");")
print(sql)
monthly_range <- dbGetQuery(con, sql)
dbDisconnect(con)
for (i in 1:nrow(monthly_range)) {
  row <- monthly_range[i, ]
  assoc_array <- list(
    title = paste0("monthly ",row[['year']],' ',row[['month']])
    ,from=row[['from_date_normalized']]
    ,to=row[['to_date_normalized']]
  )
  batch_list <- append(batch_list, list(assoc_array))
}
# print(batch_list)
monthly_forecasting_seed = batch_draw_avg(batch_list, "monthly")
monthly_batch_list <- batch_list
# batch_draw(batch_list)


print('Check point 19')


batch_list <- list()
con <- get_conn()
sql <- paste0("CALL `select_weeks`(",starting_year,", ",this_year,");")
print(sql)
weekly_range <- dbGetQuery(con, sql)
dbDisconnect(con)
for (i in 1:nrow(weekly_range)) {
  row <- weekly_range[i, ]
  assoc_array <- list(
    title = paste0("weekly ",row[['begin']],' to ',row[['end']])
    ,from=row[['min_x']]
    ,to=row[['max_x']]
  )
  batch_list <- append(batch_list, list(assoc_array))
}
weekly_forecasting_seed = batch_draw_avg(batch_list, "weekly")
weekly_batch_list <- batch_list
# batch_draw(batch_list)











print('Check point 20')








# RichardsonExtrapolation_Annually_SpeedBased <- function (begin_x, begin_relative_x, begin_y, begin_speed, include_linear_regression) {

# #  function (denormalized, begin_speed, include_linear_regression) {
  
  
#   # h <- as.double(0.001)
#   year <- begin_x - begin_relative_x
  
#   start_value <- begin_y
#   start_speed <- begin_speed
  
#   current_x <- begin_x
#   current_y <- begin_y
  
#   X <- c(current_x+0)
#   Y <- c(current_y+0)
#   varian_list <- c(current_y+0)
  
#   for (i in seq( begin_relative_x , 1, by=h)) {
    
#     current_speed <- draw_average_annual_speed_curve(i)
#     current_speed <- as.double(current_speed)
    
#     current_x <- current_x+h
#     variation <- as.double(current_speed*h)
    
#     if (include_linear_regression) {
#       variation <- variation + as.double(slope*h)
#     }
    
#     current_y <- current_y+variation
    
#     mean_value <- mean(varian_list)
#     sd_value <- sd(varian_list)
    
#     mean_value <- as.double(mean_value)
#     sd_value <- as.double(sd_value)
    
#     diviation_subtract <- (1/( 1+(((variation-mean_value)/mean_value)) ))
#     diviation_subtract <- as.double(diviation_subtract)
#     variation_changed <- as.double(variation*diviation_subtract)
    
#     X <- c(X,current_x+0)
#     Y <- c(Y,current_y+0)
#     varian_list <- c(varian_list,variation+0)
#   }

#   Sys.sleep(2)

#   title_ = paste0("Forecasting in ",year)
#   if (include_linear_regression) {
#     title_ <- paste0(title_, " linear regression")
#   }
#   image_path_ <- paste0(image_path,title_,".png")
#   # png(image_path_)
#   # Plotting the graph
#   # plot(
#   #   X, Y, 
#   #   type="o", 
#   #   col="blue", 
#   #   xlab="X-axis", 
#   #   ylab="Y-axis", 
#   #   main=title_, 
#   #   axes=TRUE, 
#   #   xlim=c(begin_x, current_x)
#   # )
  
#   # dev.off()
#   # grid.raster(readPNG(image_path_))
  
#   print(paste0(" begin_x : ",begin_x))
#   print(paste0(" year : ",year))
#   print(paste0(" Relative : ",begin_relative_x))
  
#   A=cbind(X,Y)
#   M <- spline(A);M
#   f <- function(year__) {
#     splinefunc(M,year__)
#   }
  
#   return(f)
# }


















print('Check point 21')










debug = FALSE

#dbDisconnect(con)

con <- get_conn()

denormalize__ <- function(predict_point) {
  sql <- paste0("CALL `denormalize`(",predict_point,");")
  res  <- dbSendQuery(con, sql)
  denormalized <- dbFetch(res)
  dbClearResult(res)
  # print('Check point B')
  while (dbMoreResults(con)) {
    res <- dbNextResult(con)
    dbClearResult(res)
  }
  dbClearResult(res)
  return(denormalized)
}

iterate_forcast <- function(start_point,step) {
  changing_differentiate <- format(h*step, scientific = FALSE)
  

  predict_point <- start_point + as.numeric(changing_differentiate)
  # print(paste0("predict point:",predict_point," start-from:",start_point," add:",changing_differentiate))
  
  
  # sql <- paste0("CALL `denormalize`(",predict_point,");")
  # res  <- dbSendQuery(con, sql)
  # denormalized <- dbFetch(res)
  # dbClearResult(res)
  # # print('Check point B')
  # while (dbMoreResults(con)) {
  #   res <- dbNextResult(con)
  #   dbClearResult(res)
  # }
  # dbClearResult(res)
  denormalized <- denormalize__(predict_point)
  

  if (debug) {
    print(denormalized)
    print('denormalized:')
  }
  #print(denormalized$x)
  #print(denormalized$annual_normalized)
  #print(denormalized$normalize_of_week)
  #print(denormalized$normalize_of_month)
  



  ############################################################################
  ############################################################################
  ############################################################################
  ############################################################################
  ############################################################################
  ############################################################################
  ############################################################################
  ############################################################################
  ############################################################################
  # Key logic in prediction/forecasting result

  # Create a function to extract the standard-deviation/average differentiate curves over all season
  margin_of_variation <- function(normalize_x,forecasting_seed) {
    if (debug) {
      print(normalize_x)
      print('normalize_x:')
      print(forecasting_seed$avg$fp_avg)
    }
    avg_marigin = forecasting_seed$avg$fp_avg(normalize_x)
    std_marigin = forecasting_seed$std$fp_std(normalize_x)
    margins = list(
      avg=avg_marigin,
      std=std_marigin
    );
    return(margins);
  }
  if (debug) {
    print(margin_of_variation)
  }
  

  # Get seasonality curve standard-deviation/average differentiate curves over all spans of years
  annually_marigin <- margin_of_variation(
    denormalized$annual_normalized
    , annually_forecasting_seed);


  # Get seasonality curve standard-deviation/average differentiate curves over all spans of months
  monthly_marigin <- margin_of_variation(
    denormalized$normalize_of_month * denormalized$normalize_domain_of_month
    , monthly_forecasting_seed);

  # Get seasonality curve standard-deviation/average differentiate curves over all spans of weeks
  weekly_marigin <- margin_of_variation(
    denormalized$normalize_of_week * denormalized$normalize_domain_of_week
    , weekly_forecasting_seed);

  # holiday
  # recent_three_years
  # recent_six_month
  # recent_three_month
  # recent_six_weeks
  # recent_three_weeks
  # recent_6_day
  
  # Calculate how inaccurate of each seasonality
  annual_std=(1/annually_marigin$std) # annually
  month_std=(1/monthly_marigin$std) # monthly
  week_std=(1/weekly_marigin$std) # weekly
  
  # Get the sum of the accuracy ratio
  sum_std=(annual_std+month_std+week_std)

  # Calculate the normalized value of accuracy from each seasonality
  annual_multiply=(annual_std/sum_std)
  month_multiply=(month_std/sum_std)
  week_multiply=(week_std/sum_std)

  ############################################################################

  # multiply the average value with the normalized accuracy value for each seasonality
  annual_variation = annually_marigin$avg * annual_multiply
  month_variation = monthly_marigin$avg * month_multiply
  week_variation = weekly_marigin$avg * week_multiply

  # Sum up all the variations of all seasonality
  sum_variation = (annual_variation+month_variation+week_variation)

  ############################################################################
  ############################################################################
  ############################################################################
  ############################################################################
  ############################################################################
  ############################################################################
  ############################################################################
  ############################################################################
  ############################################################################

  if (debug) {
    print(annually_marigin)
    print(monthly_marigin)
    print(weekly_marigin)
    
    print('Multiply')
    print(annual_multiply)
    print(month_multiply)
    print(week_multiply)
    
    
    print('Variation distribution')
    print(annual_variation)
    print(month_variation)
    print(week_variation)
    
    print('Sum of variation')
    print(sum_variation)
  }
  # print('Check point H')
  return(list(annual_normalized=denormalized$x,variation=sum_variation))
}


backward_iterate_forcast <- function(backward_normalized) {

    sql <- paste0("CALL `denormalize`( 
      (
        SELECT
            `x`
        FROM
            `temp_forecast_daily`
        WHERE
            `x`< (to_normalized_date( CURRENT_TIMESTAMP )-",backward_normalized,")
        ORDER BY `x` DESC
        LIMIT 1 OFFSET 1
      )
    );")
    print(sql)
    res  <- dbSendQuery(con, sql)
    denormalized <- dbFetch(res)
    dbClearResult(res)
    while (dbMoreResults(con)) {
      res <- dbNextResult(con)
      dbClearResult(res)
    }
    dbClearResult(res)

    print(denormalized)
    x___=denormalized$x

















    y___=f(x___)

    if (is.na(y___)) {
      print(paste0('Cannot find y number on ',x___))



      sql <- paste0("
          SELECT
              `y`
          FROM
              `temp_forecast_daily`
          WHERE
              `x`=",x___,"
          LIMIT 1 OFFSET 1
      ;")
      print(sql)
      res  <- dbSendQuery(con, sql)
      sql_result <- dbFetch(res)
      dbClearResult(res)
      while (dbMoreResults(con)) {
        res <- dbNextResult(con)
        dbClearResult(res)
      }
      dbClearResult(res)

      print(sql_result)

      y___=sql_result$y

      # return(0)
    }
    
    y_2 = y___

    # print(x___)
    # print('x___')
    # print(y___)
    # print('y___')

    x___ = as.numeric(x___)
    y___ = as.numeric(y___)

    # print(x___)
    # print('x___')
    # print(y___)
    # print('y___')

    x___ = round(x___, 3)
    y___ = round(y___, 3)

    print(x___)
    print('x___')
    print(y___)
    print('y___')
    # Sys.sleep(10)



    ############################################################################
    # Key logic in prediction/forecasting result

    # insert the very beginning of the forecasting result (Same as historical data)
    sql <- paste0("INSERT IGNORE INTO `forecast_result`(`forecast-series`,`start-point`,`end-point`,`value`) VALUES('DR',",x___,",",x___,",",y___,");")
    # print(sql)
    res  <- dbSendQuery(con, sql)
    denormalized <- dbFetch(res)
    dbClearResult(res)
    while (dbMoreResults(con)) {
      res <- dbNextResult(con)
      dbClearResult(res)
    }
    dbClearResult(res)













    # query for monthly forecasting seed within 3 months 
    # batch_list <- list()
    # con <- get_conn()
    

    # denormalized <- denormalize__(x___)
    # normalized_origin = denormalized$normalized_origin
    # sql <- paste0("
    #   CALL `select_months`(
    #     `to_normalized_date`(
    #       DATE_FORMAT(
    #         DATE_SUB(
    #           STR_TO_DATE('",normalized_origin,"', '%Y-%m-%d')
    #         , INTERVAL 3 MONTH)
    #       , '%Y-%m-01')
    #     )
    #     ,",x___,");
    # ")
    # print(sql)
    # monthly_range <- dbGetQuery(con, sql)
    # dbDisconnect(con)
    # for (i in 1:nrow(monthly_range)) {
    #   row <- monthly_range[i, ]
    #   assoc_array <- list(
    #     title = paste0("monthly ",row[['year']],' ',row[['month']])
    #     ,from=row[['from_date_normalized']]
    #     ,to=row[['to_date_normalized']]
    #   )
    #   batch_list <- append(batch_list, list(assoc_array))
    # }
    # monthly_3_month_forecasting_seed = batch_draw_avg(batch_list, "monthly")
    # monthly_3_month_batch_list <- batch_list















    forecasting_points <- list()
    loop_iteration=(3/12)/h
    # loop for 3 months
    for (i in 1:loop_iteration) {
      
      
        
      tryCatch({
        forecasted=iterate_forcast(x___,i)
        #print(forecasted)
        y_2=(y_2+forecasted$variation)
        bind_=c(
          x___,
          y___,
          forecasted$annual_normalized, 
          y_2,
          i
        )
        forecasting_points[[i]] <- bind_
        # print(bind_)
        # insert the forecasting result after the starting point (After the first historical data)
        sql <- paste0("INSERT IGNORE INTO `forecast_result`(`forecast-series`,`start-point`,`end-point`,`value`) VALUES('DR',",x___,",",round(forecasted$annual_normalized, 4),",",round(y_2, 4),");")
        # print(sql)
        res  <- dbSendQuery(con, sql)
        denormalized <- dbFetch(res)
        dbClearResult(res)
        while (dbMoreResults(con)) {
          res <- dbNextResult(con)
          dbClearResult(res)
        }
        dbClearResult(res)



      }
      #, warning = function(w) { message("Warning: ", w); }
      , error = function(e) {
        #message("Error: ", e);
        message("Error: ", e$message)
        #message("Full traceback:")
        #print(last_trace())
      }
      #, finally = {message("Finished trying to evaluate expression.");}
      );




    }

}





sql <- paste0("DELETE FROM `forecast_result` WHERE 1;")
print(sql)
res  <- dbSendQuery(con, sql)
dbClearResult(res)




annual_span = 1
month_span = annual_span/12

# how many times to perform the forecasting (current and history)
iteration_ = (3*4) * ( 10 ) # ( year )

for (i in 0:iteration_) {
  # 3 month interval between each predict after previous prediction
  subtract_span = (month_span*3)*i
  print(paste0("start from:",i," subtract_span:",subtract_span))
  tryCatch({
    backward_iterate_forcast(subtract_span)
  }, error = function(e) {
    cat("Caught an error:\n", e$message, "\n")
  })
  Sys.sleep(3)
}

# print('Check point 22')

# Convert list to a data frame for plotting
# df <- do.call(rbind, points)
# colnames(df) <- c("year", "number")

# Plot the points
# plot(df[, "x"], df[, "y"], type = "b", col = "blue", pch = 19,
#   xlab = "X", ylab = "Y", main = "Plot of (x, y) Points")


dbDisconnect(con)

