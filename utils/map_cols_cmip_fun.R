cols_tas <- c("#F7FCFD","#EBF4F8","#E0ECF4","#CFDFED","#BFD3E6","#AEC7E0","#9EBCDA","#95A9D0","#8C96C6","#8C80BB", "#8C6BB1", "#8A56A7","#88419D","#0000ff","#0049ff","#0072ff","#00a3ff","#00ccff","#00e5ff","#00ffff","#007700","#009900","#00bb00","#00dd00","#00ff00","#7fff00","#cfff00","#ffff00","#ffe500","#ffcc00","#ffad00","#ff9900","#ff7f00","#FF4E00","#F23A00","#E42700","#D81300","#CB0000","#A62137","#9D3673","#813986","#532B6E")
colint_pr <- colorRampPalette(brewer.pal(9,"BuPu"),interpolate = "linear") 
colintYlOrRd <- colorRampPalette( brewer.pal(9,"YlOrRd"),interpolate = "linear")
colintRdYlBu <- colorRampPalette(brewer.pal(10,"RdYlBu"),interpolate = "linear")
colintBlues <- colorRampPalette(brewer.pal(9,"Blues"), interpolate = "linear")
colintReds <- colorRampPalette(brewer.pal(9,"Reds"), interpolate = "linear")
colintBrBG <- colorRampPalette(brewer.pal(11,"BrBG"),interpolate = "linear")
colintGreens <- colorRampPalette(brewer.pal(9,"YlGn"), interpolate = "linear")

cols_tr <- c("#ffe500","#ffcc00","#ffad00","#ff9900","#ff7f00","#FF4E00","#F23A00","#E42700","#D81300","#CB0000","#A62137","#9D3673","#813986","#532B6E")

map_cols_cmip_fun <- function(indic = NA, type = NA,  domain = NA) {
  
  if (indic %in% c("tas","tasmax", "tasmin")) { # pentru toate temperaturile
    
    if (type %in% "climate") {
      df.col <- data.frame(
        cols = cols_tas, 
        vals = c(-40,-38,-36,-34,-32,-30,-28,-26,-24,-22,-20,-18,-16,-14,-12,-10,-8,-6,-4,-2,0,2,4,6,8,10,12,14,16,18,20,22,24,26,28,30,32,34,36,38,40,42)							
      ) 
      leaflet_titleg <- paste0("<html>", gsub(",","",toString(rep("&nbsp;", 5))), "°C","</html>")
      
    } else {
      df.col <- data.frame(
        cols = c(rev(colintBlues(12)),colintReds(13)), 
        vals = seq(-6,6, 0.5)
      )
      leaflet_titleg <- paste0("<html>", gsub(",","",toString(rep("&nbsp;", 5))), "°C","</html>")
    }
    
  }
  
  if (indic  %in% "pr") { # pentru toate temperaturile
    if (type %in% "climate") {
      df.col <- data.frame(
        cols = colint_pr(15), 
        vals = c(0,10,20,30,40,50, 75, 100, 250, 500,750, 1000, 1500, 2000,3000)						
      ) 
      
      leaflet_titleg <- paste0("<html>", gsub(",","",toString(rep("&nbsp;", 5))), "l/m²","</html>")
      
    } else {
      
      df.col <- data.frame(
        cols = colintBrBG(13), 
        vals = c(-50, -40,-30,-20,-10,-5, 0,5,10,20,30,40,50)
      )
      leaflet_titleg <- paste0("<html>", gsub(",","",toString(rep("&nbsp;", 5))), "%","</html>")
      
    }
  }
  
    if (indic %in% c("tr")) { # pentru toate temperaturile
    
    if (type %in% "climate") {
      df.col <- data.frame(
        cols = cols_tr, 
        vals = c(0,5,10,20,30,35,40,45,50,60,70,80,90,100)							
      ) 
      leaflet_titleg <- paste0("<html>", gsub(",","",toString(rep("&nbsp;", 5))), "nights","</html>")
      
    } else {
      df.col <- data.frame(
        cols = c(rev(colintBlues(2)),colintReds(16)), 
        vals = c(-10, -5, 0,5,10,15,20,25,30,35,40,45,50,60,70,80,90,100)
      )
      leaflet_titleg <- paste0("<html>", gsub(",","",toString(rep("&nbsp;", 5))), "nights","</html>")
    }
    
  }

  if (indic %in% c("wsdi")) { # pentru toate temperaturile
    
    if (type %in% "climate") {
      df.col <- data.frame(
        cols = cols_tr, 
        vals = c(0,5,10,20,30,35,40,45,50,60,70,80,90,100)							
      ) 
      leaflet_titleg <- paste0("<html>", gsub(",","",toString(rep("&nbsp;", 5))), "days","</html>")
      
    } else {
      df.col <- data.frame(
        cols = c(rev(colintBlues(2)),colintReds(16)), 
        vals = c(-10, -5, 0,5,10,15,20,25,30,35,40,45,50,60,70,80,90,100)
      )
      leaflet_titleg <- paste0("<html>", gsub(",","",toString(rep("&nbsp;", 5))), "days","</html>")
    }
  }

    if (indic %in% c("csdi")) { # pentru toate temperaturile
    
    if (type %in% "climate") {
      df.col <- data.frame(
        cols = colintBlues(12), 
        vals = c(1:10, 20,50)							
      ) 
      leaflet_titleg <- paste0("<html>", gsub(",","",toString(rep("&nbsp;", 5))), "days","</html>")
      
    } else {
      df.col <- data.frame(
        cols = c(rev(colintReds(11)), colintBlues(10)), 
        vals = -10:10
      )
      leaflet_titleg <- paste0("<html>", gsub(",","",toString(rep("&nbsp;", 5))), "days","</html>")
    }
    
  }
  if (indic %in% c("gsl")) { # pentru toate temperaturile
    
    if (type %in% "climate") {
      df.col <- data.frame(
        cols = colintGreens(13), 
        vals = seq(50, 350, by = 25)						
      ) 
      leaflet_titleg <- paste0("<html>", gsub(",","",toString(rep("&nbsp;", 5))), "days","</html>")
      
    } else {
      df.col <- data.frame(
        cols = colintBrBG(13), 
        vals = c(-50, -40,-30,-20,-10,-5, 0,5,10,20,30,40,50)
      )
      leaflet_titleg <- paste0("<html>", gsub(",","",toString(rep("&nbsp;", 5))), "days","</html>")
    }
    
  }
  

  # print(head(df.col))
  # print(domain)
  ints <- findInterval(domain, df.col$vals, rightmost.closed = T, left.open = F)
   
  # If the domain is entirely below the first value, findInterval returns 0 for both min and max.
  # This causes an issue with bin creation, as it results in a single bin.
  # We adjust `ints` to span the first interval, ensuring at least two bins.
  if (all(ints == 0)) {
    ints <- c(0, 1)
  }
  
  
  # If the domain is entirely below the first value, findInterval returns 0 for both min and max.
  # This causes an issue with bin creation, as it results in a single bin.
  # We adjust `ints` to span the first interval, ensuring at least two bins.
  if (all(ints == 0)) {
    ints <- c(0, 1)
  }
  
  bins <-  df.col$vals[ints[1]:(ints[2] + 1)]
  cols <- df.col$cols[ints[1]:(ints[2])]
  
  # print(bins)
  # print(cols)
  # 
  pal <- colorBin(cols, domain = domain, bins = bins, na.color = "transparent")
  pal2 <- colorBin(cols, domain = domain, bins = bins, reverse = T, na.color = "transparent")
  
  return(list(pal = pal, pal_rev = pal2, tit_leg = leaflet_titleg, minmax = range(df.col$vals)))
  
}