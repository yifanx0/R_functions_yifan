# date: 07/07/2018
# author: Yifan Xu
# goal:  
  # to easily check whether a range of rows in two data tables are the same
  # and locate the discrepencies b/w them

##=============================================================================

dt_diff = function(dt_1, dt_2, i = 1, j = min(nrow(dt_1), nrow(dt_2))) {
# decide whether dt_1 and dt_2 both have enough rows to compare
  if (min(nrow(dt_1), nrow(dt_2)) < max(i, j)) {
    stop("At least one of the two data tables does not have enough rows",
          call. = FALSE)
      
  } else {
# send a message when dt_1 and dt_2 have different rows and the default j is taken
# to make sure that the user is aware that only part of the data tables is compared
    if (j == min(nrow(dt_1), nrow(dt_2)) & nrow(dt_1) != nrow(dt_2)) {
      warning("the two data tables have different number of rows, ",
              "and by default nrow of the shorter one is taken as j, ",
              "which is the last row that is compared", "\n",
              call. = FALSE, immediate. = TRUE)
    }

# start comparison and send a message when identical
    if (data.table:::all.equal.data.table(dt_1[i:j], dt_2[i:j]) == TRUE) {
    message("The rows checked are all identical :)")
      
    } else {
# now that we know they are different, start finding the difference
    # start an empty list which can store the indices of "bad" rows
    bad_rows = list()
    # print a message when each bad row is found and
    # update the bad rows list w/ the bad rows found
    for (k in i:j) {
      if (data.table:::all.equal.data.table(dt_1[k], dt_2[k]) != TRUE) {
        message(paste0("row ", k, " is different, with the problem:", "\n",
                        data.table:::all.equal.data.table(dt_1[k], dt_2[k])))
        bad_rows = c(bad_rows, k)
      }
    }
    # print a message with indices of all the bad rows
    cat("\n")
    message("there are in total ", length(bad_rows),
            " row(s) that differ in (selected part of) the two data tables, ",
            "and below are their indices:", "\n", toString(bad_rows), sep = "")
    }
  }
}

