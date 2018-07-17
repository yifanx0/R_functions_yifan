# Some Useful R Functions

This is a repo for some functions I have written in R, inspired when I was solving various problems. Right now, there are only two very simple functions in the repo. I will add more along the way.

## [df_comp & df_diff](https://github.com/yifanx0/R_functions_yifan/blob/master/r_data_frame_comp_diff.R): Comparing Data Frames (Data Tables) and Locating Differences

### df_comp
A simple function built upon all.equal function in data.table. It makes comparison of a certain range of two data tables easier and the returns are more straight-forward.

### df_diff
The function df_diff is also built upon all.equal function in data.table. In addition to simply checking whether two data frames are the same, it also pinpoints the rows where the two data frames are different.