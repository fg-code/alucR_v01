# Date 15.03.2016 
#florian.gollnow@geo.hu-berlin.de

# reassambles the protected areas, no.change areas etc. to the allocation outpt to have the complete scenario map.

# outputs: scenario init.years (new)
# alloc | raster returned from allocation routine


alucR_postprocess <- function(alloc = allocation[[1]], lc, spatial, var.list, traj, init.years, filename = "", ...)
{
    
    nochange <- var.list[[5]][["nochange"]]
    natural <- var.list[[7]][["natural"]]
    naturallookup <- var.list[[7]][["naturallookup"]]
    pseudo.N <- var.list[[8]][["pseudo.N"]]
    lc.N <- var.list[[9]][["lc.N"]]
    
    # trajectory matrix
    if (length(traj[traj == 0 | is.na(traj)]) > 0)
    {
        # for not allowed changes (100 years more than modelling years)
        traj[traj == 0 | is.na(traj)] <- nrow(demand) + 100
    }
    
    # raster settings
    out <- stack(alloc, init.years)
    out <- brick(out, values = FALSE)
    small <- canProcessInMemory(out, 3)
    filename <- trim(filename)
    
    if (!small & filename == "")
    {
        filename <- rasterTmpFile()
    }
    if (filename != "")
    {
        out <- writeStart(out, filename, ...)
        todisk <- TRUE
    } else
    {
        vv <- array(dim = dim(out))
        todisk <- FALSE
    }
    
    bs <- blockSize(out)
    pb <- pbCreate(nsteps = bs$n, ...)
    
    # chunk processing
    if (todisk)
    {
        for (i in 1:bs$n)
        {
            # read chunks
            v.lc <- getValues(lc, row = bs$row[i], nrows = bs$nrows[i])
            v.alloc <- getValues(alloc, row = bs$row[i], nrows = bs$nrows[i])
            v.init.years <- getValues(init.years, row = bs$row[i], nrows = bs$nrows[i])
            if (length(spatial) > 0)
            {
                v.spatial <- getValues(spatial, row = bs$row[i], nrows = bs$nrows[i])
            } else{
			 v.spatial <- NULL
			}
			
            
            # process no.change
            if (length(nochange) > 0)
            {
                nochange_index <- is.element(v.lc, nochange)
                v.alloc[nochange_index] <- v.lc[nochange_index]
            }
            # spatial restrictions
            if (length(v.spatial) > 0)
            {
                sp.rest_index <- !is.na(v.spatial)  # set those to NA which have a value in the restriction layer
                v.alloc[sp.rest_index] <- v.lc[sp.rest_index]
            }
            
            # Reclassify all natural to pseudo natural classes
            if (length(natural) > 0)
            {
                ind <- is.element(v.alloc, natural)
                v.alloc[ind] <- pseudo.N  # natural vegetation to pseudo.N class (including areas of spatial restrictions)
                
                # Reclassify pseudo natural class based on trajectory and succession order
                pseudo.index <- which(is.element(v.alloc, pseudo.N))
                
                if (length(pseudo.index) > 0)
                {
                  if (length(natural) > 1)
                  {
                    for (j in 1:length(pseudo.index))
                    {
                      # can before.n be translated to natural
                      for (a in length(natural):2)
                      {
                        if (traj[naturallookup[a], naturallookup[a - 1]] < v.init.years[pseudo.index[j]] | v.lc[pseudo.index[j]] == natural[a - 1])
                        {
                          v.alloc[pseudo.index[j]] <- natural[a - 1]
                        } else
                        {
                          v.alloc[pseudo.index[j]] <- natural[a]
                        }
                      }
                    }
                  }
                  if (length(natural) == 1)
                  {
                    v.alloc[pseudo.index] <- natural
                  }
                  if (sum(is.element(v.alloc, pseudo.N)) != 0)
                  {
                    print("Warning: error in natural vegetation module")
                  }
                }
            }
            
            ##### 3.4 Saving results and preparing next epoche 3.4.1 Updating transition years vector
            v.init.years <- ifelse(v.alloc == v.lc, v.init.years + 1, 1)  #compare this allocation for transition years, inc if changed, reset to 1 if change
            
            v <- cbind(v.alloc, v.init.years)
            
            #### save to raster write to raster
            out <- writeValues(out, v, bs$row[i])
            pbStep(pb, i)
        }
        out <- writeStop(out)
    } else
    {
        # if(todisk==FALSE)
        
        
        v.lc <- getValues(lc)
        v.alloc <- getValues(alloc)
        v.init.years <- getValues(init.years)
        if (length(spatial) > 0)
        {
            v.spatial <- getValues(spatial)
        }
        
        # process no.change
        if (length(nochange) > 0)
        {
            nochange_index <- is.element(v.lc, nochange)
            v.alloc[nochange_index] <- v.lc[nochange_index]
        }
        # spatial restrictions
        if (length(v.spatial) > 0)
        {
            sp.rest_index <- !is.na(v.spatial)  # set those to NA which have a value in the restriction layer
            v.alloc[sp.rest_index] <- v.lc[sp.rest_index]
        }
        
        # Reclassify all natural to pseudo natural classes
        if (length(natural) > 0)
        {
            ind <- is.element(v.alloc, natural)
            v.alloc[ind] <- pseudo.N  # natural vegetation to pseudo.N class (including areas of spatial restrictions)
            
            # Reclassify pseudo natural class based on trajectory and succession order
            pseudo.index <- which(is.element(v.alloc, pseudo.N))
            
            if (length(pseudo.index) > 0)
            {
                if (length(natural) > 1)
                {
                  for (j in 1:length(pseudo.index))
                  {
                    # can before.n be translated to natural
                    for (a in length(natural):2)
                    {
                      if (traj[naturallookup[a], naturallookup[a - 1]] < v.init.years[pseudo.index[j]] | v.lc[pseudo.index[j]] == natural[a - 1])
                      {
                        v.alloc[pseudo.index[j]] <- natural[a - 1]
                      } else
                      {
                        v.alloc[pseudo.index[j]] <- natural[a]
                      }
                    }
                  }
                }
                if (length(natural) == 1)
                {
                  v.alloc[pseudo.index] <- natural
                }
                if (sum(is.element(v.alloc, pseudo.N)) != 0)
                {
                  print("Warning: error in natural vegetation module")
                }
            }
        }
        
        ##### 3.4 Saving results and preparing next epoche 3.4.1 Updating transition years vector
        v.init.years <- ifelse(v.alloc == v.lc, v.init.years + 1, 1)  #compare this allocation for transition years, inc if changed, reset to 1 if change
        
        v <- cbind(v.alloc, v.init.years)
        
        #### save to raster write to raster
        out <- setValues(out, v)
        pbStep(pb, i)
    }
    
    pbClose(pb)
    return(out)
} 