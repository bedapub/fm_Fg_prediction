

##### Define the functions ######

#R1 AUC solution when CLR!=0 and inhG!=0
f.r1       <-function(fm,CLHint,AUCR,inhH, Qh, CLR,inhG,BP){
  CLRB      <-CLR/BP
  ((CLHint*(Qh+CLRB)+Qh*CLRB)/(AUCR*((inhH*fm*CLHint+(1-fm)*CLHint)*(Qh+CLRB)+Qh*CLRB)*(1-inhG)))-(inhG/(1-inhG))
}

#R2 Cmax solution when CLR!=0 and inhG!=0
f.r2  <-function(fm,Qh,CLHint,inhH,Vb,ka,CMXR,CLR,inhG,BP){
  CLcyp       <- fm*CLHint
  CLnoncyp    <- (1-fm)*CLHint
  CLb         <- Qh*(CLcyp+CLnoncyp)/(Qh+CLcyp+CLnoncyp)
  CLRB        <-CLR/BP
  #Vb          <-V/BP   already defined
  ke          <- (CLb+CLRB)/Vb
  tmax        <- log(ka/ke)/(ka-ke)
  CLbi        <- Qh*(inhH*CLcyp+CLnoncyp)/(Qh+inhH*CLcyp+CLnoncyp)
  kei         <- (CLbi+CLRB)/Vb
  tmaxi       <- log(ka/kei)/(ka-kei)
  (((CLHint+Qh)/((Qh+inhH*fm*CLHint+(1-fm)*CLHint)*(1-inhG)))*(1/CMXR)*((exp(-kei*tmaxi)/exp(-ke*tmax))))-(inhG/(1-inhG))
}


generate_data_pop <- function(Observations, tCLp, etaClp, tCLR, etaCl_R, tRb, etaRb, tVlk, etaVlk, WT_Pop, ETA_WT, input, eta_Height, eta_Age, Sex_ratio, tAUCR, etaAUCR, tCMXR, etaCMXR, t.tmax.po, etatmax.po, l.fm, f.r2, f.r1, tinh, tQh, tInhGI) {
  CLp_all <- vector()
  df_list <- vector("list", Observations)
  CMXR_ <- vector()
  
  for (i in 1:Observations) {
    CLp <- tCLp * exp(etaClp[i])
    CLR <- tCLR * exp(etaCl_R[i])
    
    CLp_all <- c(CLp_all, CLp)
    Rb <- tRb * exp(etaRb[i])
    Vlk <- tVlk * exp(etaVlk[i])
    WT <- WT_Pop[i] * exp(ETA_WT[i])
    tHeight <- input$tHeight
    Height <- tHeight * exp(eta_Height[i])
    tAge <- input$tAge
    Age <- tAge * exp(eta_Age[i])
    BSA <- 0.007184 * WT^(0.425) * Height^(0.725)
    CO <- BSA * (194.15 + 4046.7 * (exp(-0.2117 * Age) - exp(-0.224 * Age)))
    Qh <- CO * (Sex_ratio * 0.28 + (1 - Sex_ratio) * 0.255)
    AUCR <- tAUCR * exp(etaAUCR[i])
    CMXR <- tCMXR * exp(etaCMXR[i])
    
    CLb <- CLp / Rb
    CLRB <- CLR / Rb
    CLH <- CLb - CLRB
    CLHint <- CLH / ((1 - CLH / tQh))
    Vb <- Vlk / Rb
    ke <- CLb / Vb
    tmax.po <- t.tmax.po * exp(etatmax.po[i])
    
    f.tmax <- function(p) { log(p / ke) / (p - ke) }
    ka <- uniroot(function(p) f.tmax(p) - tmax.po, c(0.0001, 10))$root
    
    df_temp <- data.frame(
      "x" = l.fm,
      "C" = f.r2(fm = l.fm, Qh, CLHint, tinh, Vb, ka, CMXR, CLR, tInhGI, Rb),
      "A" = f.r1(fm = l.fm, CLHint, AUCR, tinh, tQh, CLR, tInhGI, Rb),
      "id" = i
    )
    df_list[[i]] <- df_temp
  }
  
  df <- do.call(rbind, df_list)
  return(df)
}

# not longer used
perform_fm_fg_identifiability = function ( x1,y1,Qh, CLHint, fub, Vb, ka, CMXR,AUCR,inh ){
  # Define the point where we want the tangent
  fm <- x1  # replace with the estimated fm
  
  epsilon <- 1e-6
  
  # Calculate the slope of the tangent line
  
  # Define an infinitesimal
  
  
  m <- (f.r2(fm + epsilon, Qh, CLHint, fub, inh, Vb, ka, CMXR) - f.r2(fm, Qh, CLHint, fub, inh, Vb, ka, CMXR)) / epsilon
  
  # Define the tangent line
  tangent_line <- function(x) m * (x - fm) + f.r2(fm, Qh, CLHint, fub, inh, Vb, ka, CMXR)
  
  # Create a sequence of x values
  x_values <- seq(0,  1, length.out = 100)
  
  # Create a data frame for the function
  df_funcC <- data.frame(x = x_values, y = sapply(x_values, function(x) f.r2(x, Qh, CLHint, fub, inh, Vb, ka, CMXR)))
  
  # Create a data frame for the tangent line
  df_tangentC <- data.frame(x = x_values, y = sapply(x_values, tangent_line))
  
  
  
  
  m <- (f.r1(fm + epsilon, CLHint,fub,AUCR,inh) - f.r1(fm, CLHint,fub,AUCR,inh)) / epsilon
  
  # Define the tangent line
  tangent_line <- function(x) m * (x - fm) + f.r1(fm, CLHint,fub,AUCR,inh)
  
  # Create a sequence of x values
  x_values <- seq(0,  1, length.out = 100)
  
  # Create a data frame for the function
  df_funcA <- data.frame(x = x_values, y = sapply(x_values, function(x) f.r1(x, CLHint,fub,AUCR,inh)))
  
  
  # Create a data frame for the tangent line
  df_tangentA <- data.frame(x = x_values, y = sapply(x_values, tangent_line))
  
  # Calculate the slopes of the two lines
  slope1 <- (df_tangentC$y[2] - df_tangentC$y[1]) / (df_tangentA$x[2] - df_tangentA$x[1])
  slope2 <- (df_tangentA$y[2] - df_tangentA$y[1]) / (df_tangentC$x[2] - df_tangentC$x[1])
  
  # Calculate the angle between the two lines
  angle <- atan(abs((slope2 - slope1) / (1 + slope1 * slope2)))
  
  # Convert the angle from radians to degrees
  angle_degrees <- angle * (180 / pi)
  print("angle")
  print(angle_degrees)  
  
  # Create a data frame for the polygon
  polygon <- data.frame(x = c(df_tangentC$x, rev(df_tangentA$x)), y = c(df_tangentC$y, rev(df_tangentA$y)))
  
  # Plot the function and the tangent line
  ggplot() +
    geom_line(data = df_funcC, aes(x, y), color = "orange", size=2)  +
    geom_line(data = df_tangentC, aes(x, y), color = "orange", size = 0.5, linetype = "dashed") +
    geom_line(data = df_funcA, aes(x, y), color = "blue", size=2)  +
    geom_line(data = df_tangentA, aes(x, y), color = "blue", size = 0.5, linetype = "dashed") +
    geom_polygon(data = polygon, aes(x = x, y = y), fill = "purple", alpha = 0.05) +
    geom_point(aes(x = x1, y = y1), color = "black", size=3) +
    theme_bw() +
    labs(x = expression(f[m]), y= expression(F[G])) +
    theme(plot.title = element_text(size = 28),  # Change the size of the main title
          axis.text=element_text(size=16),
          axis.title=element_text(size=20,face="bold")) +
    ggtitle("AUCR and CMAXR Functions and its Tangent Lines") +
    annotate("text", x = 0.15, y=1, label = paste0("Ang = ",round(angle_degrees,0), "°"), size=6.5)+
    scale_x_continuous(breaks = seq(0, 1, by = 0.10))+  # Adjust x-axis ticks
    scale_y_continuous(breaks = seq(0, 1, by = 0.10))+  # Adjust y-axis ticks
    coord_cartesian(xlim = c(0, 1), ylim = c(0, 1),expand = FALSE)      # Limit both x and y axes from 0 to 1
  
}

