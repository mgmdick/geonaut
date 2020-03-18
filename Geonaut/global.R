scifi_palette <- c("turquoise4","darkorange", "paleturquoise",  "tan")


perlin_noise <- function( 
  n = 5,   m = 7,    # Size of the grid for the vector field
  N = 50, M = 50, seed = NA # Dimension of the image
) {
  if (!is.na(seed)) set.seed(seed)
  # For each point on this n*m grid, choose a unit 1 vector
  vf <- apply(
    array( rnorm( 2 * n * m ), dim = c(2,n,m) ),
    2:3,
    function(u) u / sqrt(sum(u^2))
  )
  
  
  f <- function(x,y) {
    # Find the grid cell in which the point (x,y) is
    i <- floor(x)
    j <- floor(y)
    stopifnot( i >= 1 || j >= 1 || i < n || j < m )
    # The 4 vectors, from the vector field, at the vertices of the square
    v1 <- vf[,i,j]
    v2 <- vf[,i+1,j]
    v3 <- vf[,i,j+1]
    v4 <- vf[,i+1,j+1]
    # Vectors from the point to the vertices
    u1 <- c(x,y) - c(i,j)
    u2 <- c(x,y) - c(i+1,j)
    u3 <- c(x,y) - c(i,j+1)
    u4 <- c(x,y) - c(i+1,j+1)
    # Scalar products
    a1 <- sum( v1 * u1 )
    a2 <- sum( v2 * u2 )
    a3 <- sum( v3 * u3 )
    a4 <- sum( v4 * u4 )
    # Weighted average of the scalar products
    s <- function(p) 3 * p^2 - 2 * p^3
    p <- s( x - i )
    q <- s( y - j )
    b1 <- (1-p)*a1 + p*a2
    b2 <- (1-p)*a3 + p*a4
    (1-q) * b1 + q * b2
  }
  xs <- seq(from = 1, to = n, length = N+1)[-(N+1)]
  ys <- seq(from = 1, to = m, length = M+1)[-(M+1)]
  outer( xs, ys, Vectorize(f) )
}



generate_plane <- function(x, y,
                           max_slope = 1/12.5,
                           noise_vector = c(6,6),
                           noise_coefficient = 25,
                           max_resource_fraction = 0.85,
                           min_resource_fraction = 0.65,
                           mean_thickness = 10,
                           seed = NA) {
  
  set.seed(Sys.time())
  
  if (!is.na(seed)) set.seed(seed)
  
  x_slope <- runif(1000, -max_slope, max_slope)
  y_slope <- runif(1000, -max_slope, max_slope)
  
  x_slope <- x_slope[x_slope > 0.02 | x_slope < -0.02][1]
  y_slope <- y_slope[y_slope > 0.02 | y_slope < -0.02][1]
  
  #x <- y <- seq(0, 1250, by = 25)
  
  f <- function(x,y){ z <- (x*x_slope) + (y * y_slope)}
  
  z <- outer(x,y,f)
  noise <- perlin_noise(n = noise_vector[1], m = noise_vector[2], N = length(x), M = length(y), seed = seed) * noise_coefficient
  z <- z + noise
  #z2 <- outer(x,y,f2)
  
  
  if ((sum(z < 0) / length(z)) > max_resource_fraction) {
      while ((sum(z < 0) / length(z)) > max_resource_fraction) {
      z <- z + 5
    }

  }
  

  if ((sum(z < 0) / length(z)) < min_resource_fraction) {
    while ((sum(z < 0) / length(z)) < min_resource_fraction) {
      z <- z - 5
    }
  }
  
  df <- expand.grid(y, x)
  colnames(df) <- c("y", "x")
  df <- cbind(df, z =as.vector(z))
  
  weathering_horizon <- rnorm(1, mean = 30, sd = 5)
  
  thickness <- rnorm(1, mean = mean_thickness, sd = 0.5)
  
  tk_noise <- perlin_noise(n = noise_vector[1], m = noise_vector[2], N = length(x), M = length(y), seed = seed) / 5 * mean_thickness
  
  tk <- thickness + as.vector(tk_noise)
  
  df$tk <- tk
  
  
  df <- df %>%
    mutate(class = ifelse(z > 0, "Surface", "Fresh"),
           class = ifelse(z > -weathering_horizon & z <= 0, "Weathered", class)) %>%
    mutate(tk = ifelse(z > 0, 0, tk),
           z = ifelse(z > 0, 0, z)) %>%
    mutate(tk = ifelse(class == "Weathered", (z / - weathering_horizon) * tk, tk))

  return(df)
  
}


margin_rank <- function(res, co_rd = 1.4, ob_cost = 30, co_price = 100, weathering_rl = NA, final_depth = NA) {
  
  x_spacing <- diff(unique(res$x))[1]
  y_spacing <- diff(unique(res$y))[1]
  
  res$co_price <- co_price
  res$ob_cost <- ob_cost
  
  res$ob_bcm <- -res$z * x_spacing * y_spacing
  
  res$ob_cost <- res$ob_bcm * -ob_cost
  
  if (!is.na(weathering_rl)) {
    res$class <- "Surface"
    res$class[res$z < -weathering_rl & res$z >= -final_depth] <- "Fresh"
    #res$class[res$z < 0 & res$z > -weathering_rl] <- "Weathered"
  }
  
  if (!is.na(final_depth)) {
    res$class <- "Surface"
    res$class[res$wline > 0.95 & res$z >= -final_depth] <- "Fresh"
    #res$class[res$z < 0 & res$z > -weathering_rl] <- "Weathered"
  }
  
  
  print(ggplot(res[res$y == 3000,], aes(x = x, y = z, colour = class)) + geom_point() +geom_hline(aes(yintercept = -weathering_rl)))
  print(paste("ROWS :", nrow(res), sep = ""))
  
  
  print(paste("VIABLE BLOCKS =", sum(res$class == "Fresh"), sep = ""))
  
  res$co_tonnes <- res$tk * (res$class == "Fresh") * co_rd * x_spacing * y_spacing
  res$co_revenue <- res$co_tonnes * co_price
  
  res$profit <- res$co_revenue + res$ob_cost
  
  print(head(res, 20))
  
  return(res)
}

mine_reserve <- function(res, excavators = 1, excavator_rate = 10000000, excavator_cost = 10000000, discount_rate = 0.1, optimal = F) {
  
  bcm_rate <- excavators * excavator_rate
  #discount_rate <- 0.1
  
  # res %>%
  #   filter(profit > 0) %>%
  #   arrange(desc(profit)) %>%
  #   mutate(c_ob = cumsum(ob_bcm),
  #          c_co = cumsum(co_tonnes),
  #          year = cut(c_ob, seq(0, max(c_ob)+bcm_rate, bcm_rate), labels = FALSE)) %>%
  #   ggplot(aes(x = year, y = co_tonnes)) + geom_bar(stat = "identity") +
  #   geom_bar(aes(y = ob_bcm),stat = "identity", position = "stack", fill = "red", alpha = 0.2)
  # 
  # 
  # res %>%
  #   filter(profit > 0) %>%
  #   arrange(desc(profit)) %>%
  #   mutate(c_ob = cumsum(ob_bcm),
  #          c_co = cumsum(co_tonnes),
  #          year = cut(c_ob, seq(0, max(c_ob)+bcm_rate, bcm_rate), labels = FALSE)) %>%
  #   ggplot(aes(x = year, y = co_tonnes)) + geom_bar(stat = "identity") +
  #   geom_bar(aes(y = ob_bcm),stat = "identity", position = "stack", fill = "red", alpha = 0.2)
  # 
  
  if (optimal) {
  res <- res %>%
    filter(profit > 0) %>%
    #filter(class == "Fresh") %>%
    arrange(desc(profit)) %>%
    filter(!is.na(ob_bcm)) %>%
    mutate(c_ob = cumsum(ob_bcm),
           c_co = cumsum(co_tonnes),
           year = cut(c_ob, seq(0, max(c_ob)+bcm_rate, bcm_rate), labels = FALSE)) %>%
    group_by(year) %>%
    summarise(profit = sum(profit) - (excavators * excavator_cost)) %>%
    ungroup() %>%
    summarise(NPV_M = sum( profit / (1+discount_rate)^max(year)) / 1000000,
              Profit_M = sum(profit) / 1000000)
  
  
  } else {
    res <- res %>%
      #filter(profit > 0) %>%
      filter(class == "Fresh") %>%
      arrange(desc(profit)) %>%
      filter(!is.na(ob_bcm)) %>%
      mutate(c_ob = cumsum(ob_bcm),
             c_co = cumsum(co_tonnes),
             year = cut(c_ob, seq(0, max(c_ob)+bcm_rate, bcm_rate), labels = FALSE)) %>%
      group_by(year) %>%
      summarise(profit = sum(profit) - (excavators * excavator_cost)) %>%
      ungroup() %>%
      summarise(NPV_M = sum( profit / (1+discount_rate)^max(year)) / 1000000,
                Profit_M = sum(profit) / 1000000)
    
    
  }
  res
  
}

mine_reserve_plot <- function(res, excavators = 1, excavator_rate = 10000000, excavator_cost = 10000000, discount_rate = 0.1) {
  
  bcm_rate <- excavators * excavator_rate
  #discount_rate <- 0.1
  
  tmp <- res %>%
     #filter(profit > 0) %>%
     filter(class == "Fresh") %>%
     arrange(desc(profit)) %>%
     filter(!is.na(ob_bcm)) %>%
     mutate(c_ob = cumsum(ob_bcm),
            c_co = cumsum(co_tonnes),
            year = cut(c_ob, seq(0, max(c_ob)+bcm_rate, bcm_rate), labels = FALSE))
  
  print(head(tmp, 20))
  ggplot(tmp, aes(x = year, y = profit)) + geom_bar(stat = "identity")
}


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
    intercept = tmp$y[1] - (slope * tmp$x[1])
    print("Intercept:")
    print(intercept)
    y_points <- slope * x_seq + intercept
    
    
    seis_line <- data.frame(x = x_seq, y = y_points)
    colnames(seis_line) <- c("x", "y")
    coordinates(seis_line) <- ~x + y
    coordinates(df) <- ~x + y
    
    grid_out <- data.frame(seis_line)
    grid_out$z <- idw0(z~1, data = df, newdata =  seis_line, idp = 2)
    grid_out$tk <- idw0(tk~1, data = df, newdata =  seis_line, idp = 2)
    
    
    df$wline <- 0
    df$wline[df$class == "Fresh"] <- 1

    grid_out$wline <- idw0(wline~1, data = df, newdata = seis_line, idp = 2)
    
    grid_out <- grid_out %>%
      filter(!is.nan(wline)) %>%
      arrange(x) %>%
      mutate(dist = cumsum(sqrt(c(0,diff(x))^2 + c(0,diff(y))^2)),
             linegroup = i)
    
    grid_out
    
  }
  
  ) %>% bind_rows()
  
}


# idw_grid <- function(df, drilling) {
#   require(gstat)
#   grid <- df[, c("x", "y")]
#   coordinates(grid) <- ~x + y
#   coordinates(drilling) <- ~x + y
#   grid$z <- idw0(z~1, data = drilling, newdata =  grid, idp = 2)
#   grid$tk <- idw0(tk~1, data = drilling, newdata =  grid, idp = 2)
#   data.frame(grid)
# }

idw_grid <- function(df, drilling) {
  require(gstat)
  grid <- df[, c("x", "y", "class")]
  coordinates(grid) <- ~x + y
  coordinates(drilling) <- ~x + y
  grid$z <- idw0(z~1, data = drilling, newdata =  grid, idp = 2)
  grid$tk <- idw0(tk~1, data = drilling, newdata =  grid, idp = 2)
  
  drilling$wline <- 0
  drilling$wline[drilling$class == "Fresh"] <- 1
  grid$wline <- idw0(wline~1, data = drilling, newdata =  grid, idp = 2)
  

  grid$tk[grid$class == "Surface"] <- NA
  grid$z[grid$class == "Surface"] <- NA
  grid$wline[grid$class == "Surface"] <- NA
  grid$class[grid$wline > 0.95] <- "Fresh"
  grid$class[grid$wline <= 0.95] <- "Weathered"
  
  data.frame(grid)
}


krige_grid <- function(df, drilling, model =  "Lin", nug = 0, range = 1000000, sill = 1000000) {
  require(gstat)
  grid <- df[, c("x", "y", "class")]
  coordinates(grid) <- ~x + y
  coordinates(drilling) <- ~x + y
  
  
  x_vgm = vgm(model = model, psill = sill, nugget = nug, range = range)
  
  grid$z <- krige0(z~1, data = drilling, newdata =  grid, model = x_vgm)
  grid$tk <- krige0(tk~1, data = drilling, newdata =  grid, model = x_vgm)
  
  drilling$wline <- 0
  drilling$wline[drilling$class == "Fresh"] <- 1
  grid$wline <- krige0(wline~1, data = drilling, newdata =  grid, model = x_vgm)
  
  
  grid$tk[grid$class == "Surface"] <- NA
  grid$z[grid$class == "Surface"] <- NA
  grid$wline[grid$class == "Surface"] <- NA
  grid$class[grid$wline > 0.95] <- "Fresh"
  grid$class[grid$wline <= 0.95] <- "Weathered"
  
  data.frame(grid)
}


