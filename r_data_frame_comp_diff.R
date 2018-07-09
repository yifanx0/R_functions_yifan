# date: 07/07/2018
# author: Yifan Xu
# goal: 
  # 1. write a function that can easily check 
     # whether a range of rows in two data frames are the same
  # 2. write a function that can locate 
     # the discrepency b/w a certain row range of two data frames
# package required: data.table

##=============================================================================

# 1. determine whether two data frames are identical
  # i and j specify the starting and ending rows 
  # b/w which the data frames are compared
df_comp = function(df_1, df_2, i = 1, j = nrow(df_1)) {
  library(data.table)
  df_1 = as.data.table(df_1)
  df_2 = as.data.table(df_2)
  if (all.equal(df_1[i:j], df_2[i:j]) == TRUE) {
    print("the two data frames are identical")
  } else {
    print("the two data frames are different")
  }
}

##=============================================================================

# 2. pin down the different rows
df_diff = function(df_1, df_2, i = 1, j = nrow(df_1)) {
  library(data.table)
  df_1 = as.data.table(df_1)
  df_2 = as.data.table(df_2)
  # first decide whether the function is doable
  # i.e., want to check whether df_1 and df_2 both have enough rows to compare
  if (min(nrow(df_1), nrow(df_2)) < max(i, j)) {
    print("At least one of the two data frames does not have enough rows")
  } else if (all.equal(df_1[i:j], df_2[i:j]) == TRUE) {
    # this is for if identical
    print("All good! The rows checked are all identical :)")
    
  } else {
    # now that we know they are different, start finding the difference
    # start an empty list which can store the indices of "bad" rows
    bad_rows = list()
    for (k in i:j) {
      if (all.equal(df_1[k], df_2[k]) != TRUE) {
        # print a message when each bad row is found
        cat(paste0("row ", k, " is different, with the problem below:", "\n",
                   all.equal(df_1[k], df_2[k]), "\n\n"))
        # update the bad rows list w/ the bad rows found
        bad_rows = c(bad_rows, k)
      }
    }
    # print a message with indices of all the bad rows
    cat("there are in total ", length(bad_rows),
        " row(s) that differ in (selected part of) the two data frames, ",
        "and below are their indices:", "\n", toString(bad_rows), sep = "")
    }
  }
