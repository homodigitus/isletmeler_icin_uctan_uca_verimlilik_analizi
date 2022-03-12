library(Benchmarking)
library(tidyverse)


# merge and split dataset ----
merge.and.split.datasets <- function(.df1, .df2, .df3, input.list){
  
  #key.var    <- enquo(key.var)
  #filter.var <- as_label(filter.var)
  #key.var          <- quote(key.var)
  #key.var          <- as_label(key.var)
  
  # merge datasets
  df <- .df1 %>% 
    inner_join(.df2, by="SKU") %>% 
    inner_join(.df3, by="SKU")
  
  # split datasets
  
  # filter data and select inputs
  sehirici <- df %>% 
    filter((SEHIRICI == 1)) %>% 
    select_(.dots = input.list)
  
  # sehirici SKU list
  sehirici.SKU <- sehirici$SKU
  
  # filter data and select inputs
  sehirlerarasi <- df %>% 
    filter(!SKU %in% sehirici.SKU) %>% 
    select_(.dots = input.list)
  
  result <- list(sehirici=sehirici, sehirlerarasi=sehirlerarasi)
  
  return(result)
  
}



# add cluster and index ----
add.cluster.and.index <- function(.data, group.var, arrange.var){
  
  # enquo variables
  group.var <- enquo(group.var)
  arrange.var <- enquo(arrange.var)
  
  # ordered data
  df.ordered <- .data %>% 
    arrange(!!group.var, !!arrange.var) %>% 
    group_by(!!group.var) %>% 
    mutate(INDEX = row_number(),
           KUME_N = group_indices(),
           KUME_S = n()
    ) %>% 
    relocate(INDEX, KUME_N) %>% 
    relocate(!!group.var, .before = !!arrange.var) %>%
    relocate(KUME_S, .before = !!group.var) %>% 
    ungroup()
  
  return(df.ordered)
  
}



# general dea function ----
general.dea.func <- function(df, inputs, output, output.var, index.col, ...){
  
  # perform dea
  res <- dea(data.frame(inputs), data.frame(output), ...)
  
  # add efficiency score
  df$EFFICIENCY <- round(res$eff, 3)
  
  # add projection
  df$PROJECTION <- round(rowSums(res$lambda %*% diag(output[[output.var]]), 3))
  
  # add peers
  p <- as_tibble(peers(res))
  name.list <- names(p)
  name.list.last <- length(name.list)
  p <- p %>% 
    unite("REF", as.name(name.list[1]):as.name(name.list[name.list.last]), na.rm = TRUE, remove = T)
  df$REFERENCE <- p$REF
  
  # benchmark count
  b <- as_tibble(table(peers(res)), .name_repair = make.names)
  names(b) <- c(index.col, "BENCHMARK_COUNT")
  b <- b %>%  mutate_if(is.character, as.integer)
  df <- df %>% left_join(b, by=index.col)
  
  # add lambda scores as weight
  l <- as_tibble(res$lambda)
  l <- l %>% mutate_all(na_if, 0) %>% round(2)
  name.list <- names(l)
  name.list.last <- length(name.list)
  l <- l %>% unite("LAMBDA_SCORES", as.name(name.list[1]):as.name(name.list[name.list.last]), na.rm = TRUE, remove = T)
  df$LAMDA_SCORES <- l$LAMBDA_SCORES
  
  # add benchmark ratio
  df$BENCHMARK_RATIO <- round(df$BENCHMARK_COUNT / nrow(df), 2)
  
  return(df)
  
}



# grouped dea function ----
grouped.dea.func <- function(df, input.vars, output.var, group.col, subgroup.col, rts="vrs", orientation=2){
  
  # unique groups
  group.col <- as.name(group.col)
  group.list <- df %>% distinct({{group.col}})
  
  # dea list
  dea.list <- list()
  
  # loop for all groups
  for (i in 1:nrow(group.list)){
    
    # filter data
    sdf <- df %>% 
      filter({{group.col}} == i)
    
    # inputs
    inputs <- sdf %>% select_(.dots = input.vars)
    
    # output
    output <- sdf %>% select_(.dots = output.var)
    
    # perform dea
    res <- dea(data.frame(inputs), data.frame(output), RTS=rts, ORIENTATION = orientation)
    
    # add efficiency
    sdf$ETKINLIK <- round(res$eff, 3)
    
    # add projection
    sdf$PROJEKSIYON <- round(rowSums(res$lambda %*% diag(output[[output.var]]), 3))
    
    # add peers
    p <- as_tibble(peers(res))
    name.list <- names(p)
    name.list.last <- length(name.list)
    p <- p %>% 
      unite("REF", as.name(name.list[1]):as.name(name.list[name.list.last]), na.rm = TRUE, remove = T)
    sdf$REFERANS <- p$REF
    
    # benchmark count
    b <- as_tibble(table(peers(res)), .name_repair = make.names)
    names(b) <- c(subgroup.col, "BENCHMARK_SAYI")
    b <- b %>%  mutate_if(is.character, as.integer)
    sdf <- sdf %>% left_join(b, by=subgroup.col)
    
    # add lambda scores as weight
    l <- as_tibble(res$lambda)
    l <- l %>% mutate_all(na_if, 0) %>% round(2)
    name.list <- names(l)
    name.list.last <- length(name.list)
    l <- l %>% unite("LAMBDALAR", as.name(name.list[1]):as.name(name.list[name.list.last]), na.rm = TRUE, remove = T)
    sdf$LAMDALAR <- l$LAMBDALAR
    
    # add benchmark ratio
    sdf$BENCHMARK_ORAN <- round(sdf$BENCHMARK_SAYI / sdf$KUME_S, 2)
    
    # append result to list
    dea.list[[i]] = sdf 
    
  }
  
  # concatanate all data
  dea.result <- do.call("rbind", dea.list)
  
  return(dea.result)
  
}



# iterated grouped dea function ----
iterated.grouped.dea.func <- function(df, threshold=0.5, iteration=1, ...){
  
  # most efficient firms list
  most.eff.list <- list()
  most.eff.firm.list <- list()
  
  i=1
  while (i < iteration+1){
    result.loop <- grouped.dea.func(df, ...)
    # most efficients
    most.eff.firms <- result.loop %>%  filter(BENCHMARK_ORAN >= threshold)
    if (nrow(most.eff.firms) == 0){
      cat(paste("There is no data for iteration ", i, ".", " Try decreasing threshold."))
      break
    }else{
      most.eff.list[[i]] <- most.eff.firms
    }
    most.eff.SKU <- most.eff.firms$SKU
    most.eff.firm.list[[i]] <- most.eff.SKU
    
    df <- df %>% filter(!SKU %in% most.eff.SKU)
    df <- df %>% select(SKU, KUME, MARKA_ADET_3KM, MARKA_ADET_3KM_BIG3, POTANSIYEL, KONFOR, KAPASITE, MAHALLE, GUNLUK_HACIM)
    df <- add.cluster.and.index(.data = df, group.var = KUME, arrange.var = SKU)
    i = i+1
  }
    
  
  # most efficient firms
  most.eff.list <- most.eff.list[-length(most.eff.list)]
  most.eff <- do.call("rbind", most.eff.list)
  most.eff$INDEX <- 0
  most.eff$BENCHMARK <- 0
  most.eff.SKU <- most.eff$SKU
  #result.loop <- result.loop %>% filter(!SKU %in% most.eff.SKU)
  
  # concat dataframes
  result <- bind_rows(result.loop, most.eff)
  
  return(result)
  
}



# post process data ----
post.process.data <- function(.result, .raw.data){
  
  df1 <- .result %>%
    select(INDEX, SKU, KUME, GUNLUK_HACIM, PROJEKSIYON, ETKINLIK,
           BENCHMARK_SAYI, REFERANS, LAMDALAR, BENCHMARK_ORAN)
  
  df2 <- df1 %>% 
    inner_join(.raw.data, by="SKU")
  
  df2 <- df2 %>% 
    rename(KUME_ICI_SKU_NO=INDEX, KUME_ICI_BENCHMARK_SKU_NO=REFERANS, BENCHMARK_AGIRLIK=LAMDALAR) %>% 
    mutate(
      VERIMLI_MI = if_else(ETKINLIK == 1, 1, 0),
      ARTIS_ORAN = round((PROJEKSIYON / GUNLUK_HACIM) - 1, 2)
    ) %>% 
    mutate(
      ARTIS_ORAN = if_else(ARTIS_ORAN <0, 0, ARTIS_ORAN)
    ) %>% 
    select(-ETKINLIK) %>% 
    replace_na(list(BENCHMARK_SAYI = 0))
  
  return(df2)
  
}
  
  
  
  
  
  
  
  
  
  
  
  
  













 



