nearPointsLine <- function(df, lines) {
  
  y_spacing <- diff(unique(df$y))[1]
  x_spacing <- diff(unique(df$x))[1]
  
  lapply(1:max(unique(lines$linegroup)), function(i) {
    tmp <- lines[lines$linegroup == i,]
    
    x_seq <- round(seq(min(tmp$x), max(tmp$x), 1) / x_spacing, 0) * x_spacing
    
    slope <- (tmp$y[2] - tmp$y[1]) / (tmp$x[2] - tmp$x[1]) 
    intercept = tmp$y[1] + slope * tmp$x[1]
    
    
    y_points <- slope * x_seq + intercept
    y_points <- round(y_points / y_spacing, 0) * y_spacing
    
    tmp_line_coords <- data.frame(x =  x_seq , y = y_points)
    
    out <- df %>% inner_join(tmp_line_coords) %>% unique()
    if (nrow(out) > 0) {
      out <- out %>%
        mutate(linegroup = i, pointID = 1:n())
    }
    print(out)
    out
  }) %>% bind_rows()
  
}

nearPointsLine2 <- function(df, lines) {
  
  require(gstat)
  
  y_spacing <- diff(unique(df$y))[1]
  x_spacing <- diff(unique(df$x))[1]
  
  lapply(1:max(unique(lines$linegroup)), function(i) {
    tmp <- lines[lines$linegroup == i,]
    
    x_seq <- seq(min(tmp$x), max(tmp$x), length.out = 20)
    
    slope <- (tmp$y[2] - tmp$y[1]) / (tmp$x[2] - tmp$x[1]) 
    print("Slope:")
    print(slope)
    intercept = tmp$y[1] + (slope * tmp$x[1])
    print("Intercept:")
    print(intercept)
    y_points <- slope * x_seq + intercept
    
    seis_line <- data.frame(x = x_seq, y = y_points)
    colnames(seis_line) <- c("x", "y")
    coordinates(seis_line) <- ~x + y
    coordinates(df) <- ~x + y
    
    grid_out <- data.frame(seis_line)
    grid_out$z <- idw0(z~1, data = df, newdata =  seis_line, idp = 2)
    grid_out$z <- idw0(z~1, data = df, newdata =  seis_line, idp = 2)

    grid_out <- grid_out %>%
      arrange(x) %>%
      mutate(dist = cumsum(sqrt(c(0,diff(x))^2 + c(0,diff(y))^2)),
             linegroup = i)
   
    grid_out
    
  }
  
  ) %>% bind_rows()
  
}
