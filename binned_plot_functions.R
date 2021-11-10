# function to plot your survival data "binned" (instead of "jittered")

logitbin <- function (df, n) {
  
  df <- df %>%
    mutate(bingroup = cut(sizeT, breaks = n)) %>%
    group_by(bingroup) %>%
    summarize(survival = mean(survival, na.rm = T), 
              sizeT := mean(sizeT, na.rm = T),
              n_obs = n())
  
  plot <- ggplot(df) +
    geom_point(aes(x = sizeT, y = survival), size = 3) +
    xlab("sizeT") +
    ylab("survival") +
    ylim(c(0,1))
  
  return(plot)
  
}

# function to plot binned survival data, averaged per year!

logitbin_yr <- function (df, n) {
  
  df <- df %>%
    group_by(yearT) %>%
    mutate(bingroup = cut(sizeT, breaks = n)) %>%
    group_by(yearT, bingroup) %>%
    summarize(survival = mean(survival, na.rm = T), 
              sizeT := mean(sizeT, na.rm = T),
              n_obs = n())
  
  plot <- ggplot(df) +
    geom_point(aes(x = sizeT, y = survival), size = 3) +
    xlab("sizeT") +
    ylab("survival") +
    ylim(c(0,1))
  
  return(plot)
  
}
