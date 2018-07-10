# date: 07/07/2018
# author: Yifan Xu
# goal: 
  # 1. write a function that can easily check 
     # whether a range of rows in two data frames are the same
  # 2. write a function that can locate 
     # the discrepency b/w a certain row range of two data frames

##=================================================================================================

# 1. determine whether two data frames are identical
  # i and j specify the starting and ending rows b/w which the data frames are compared
df_comp = function(df_1, df_2, i = 1, j = nrow(df_1)) {
  # first check whether data.table is installed
  if (!"data.table" %in% rownames(installed.packages())) {
    stop("installation and library of 'data.table' package is required for df_comp", call. = FALSE)
    
  # then check whether data.table is loaded
  } else if (!"data.table" %in% (.packages())) {
    stop("'data.table' needs to be loaded for df_comp", call. = FALSE)
    
  # finally check whether the two data frames are the same
  } else {
    df_1 = as.data.table(df_1)
    df_2 = as.data.table(df_2)
    if (all.equal(df_1[i:j], df_2[i:j]) == TRUE) {
      message("the two data frames are identical")
    } else {
      message("the two data frames are different")
    }
  }
}

##=================================================================================================

# 2. pin down the different rows
df_diff = function(df_1, df_2, i = 1, j = min(nrow(df_1), nrow(df_2))) {
  # first check whether data.table is installed
  if (!"data.table" %in% rownames(installed.packages())) {
    stop("installation and library of 'data.table' package is required for df_diff", call. = FALSE)
    
  # then check whether data.table is loaded
  } else if (!"data.table" %in% (.packages())) {
    stop("'data.table' needs to be loaded for df_diff", call. = FALSE)
    
  } else {
    df_1 = as.data.table(df_1)
    df_2 = as.data.table(df_2)
    
  # decide whether df_1 and df_2 both have enough rows to compare
    if (min(nrow(df_1), nrow(df_2)) < max(i, j)) {
      stop("At least one of the two data frames does not have enough rows", call. = FALSE)
  
    } else {
  # send a message when df_1 and df_2 have different rows and the default j is taken
  # to make sure that the user is aware of the fact that only part of the data frames is compared
      if (j == min(nrow(df_1), nrow(df_2)) & nrow(df_1) != nrow(df_2)) {
        warning("the two data frames have different number of rows, ",
                "and by default nrow of the shorter one is taken as j, ",
                "which is the last row that is compared", "\n",
                call. = FALSE, immediate. = TRUE)
      }

  # start comparison and send a message when identical
      if (all.equal(df_1[i:j], df_2[i:j]) == TRUE) {
      message("The rows checked are all identical :)")
      
      } else {
  # now that we know they are different, start finding the difference
      # start an empty list which can store the indices of "bad" rows
      bad_rows = list()
      # print a message when each bad row is found and
      # update the bad rows list w/ the bad rows found
      for (k in i:j) {
        if (all.equal(df_1[k], df_2[k]) != TRUE) {
          message(paste0("row ", k, " is different, with the problem:", "\n",
                         all.equal(df_1[k], df_2[k])))
          bad_rows = c(bad_rows, k)
        }
      }
      # print a message with indices of all the bad rows
      cat("\n")
      message("there are in total ", length(bad_rows),
              " row(s) that differ in (selected part of) the two data frames, ",
              "and below are their indices:", "\n", toString(bad_rows), sep = "")
      }
    }
  }
}
