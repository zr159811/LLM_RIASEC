# Functions
# # # # # # # #

# Custom Functions
# # # # # # # # # #

# Function to calculate degrees of freedom for Welch's t-test
welch_df <- function(SD.x, N.x, SD.y, N.y) {
  s1_squared <- SD.x^2 / N.x
  s2_squared <- SD.y^2 / N.y
  df <- (s1_squared + s2_squared)^2 / ((s1_squared^2 / (N.x - 1)) + (s2_squared^2 / (N.y - 1)))
  return(df)
}

# Function to calculate the confidence intervals
calculate_ci <- function(mean.x, SD.x, N.x, mean.y, SD.y, N.y, conf.level) {
  mean_diff <- mean.x - mean.y
  se_diff <- sqrt((SD.x^2 / N.x) + (SD.y^2 / N.y))
  df <- welch_df(SD.x, N.x, SD.y, N.y)
  t_critical <- qt(conf.level + (1 - conf.level) / 2, df)
  
  lower_bound <- mean_diff - t_critical * se_diff
  upper_bound <- mean_diff + t_critical * se_diff
  
  return(c(lower = lower_bound, upper = upper_bound))
}

# Define a function for fuzzy matching
fuzzy_match <- function(list1, list2, threshold = 0.85) {
  matched <- vector("list", length(list1))
  for (i in seq_along(list1)) {
    distances <- stringdist::stringdistmatrix(list1[i], list2, method = "jw") # Jaro-Winkler method
    matched[[i]] <- list2[which.min(distances)] # Find the closest match
    if (min(distances) > threshold) {
      matched[[i]] <- NA # If no close match, return NA
    }
  }
  unique(unlist(matched))
}

find_shared_occupations <- function(group1, group2, method = "jw", threshold = 0.8) {
  # Initialize an empty vector to store shared occupations
  shared_occupations <- vector("list", length = nrow(group1))
  names(shared_occupations) <- group1$occupation
  
  # Loop through each occupation in group1
  for (i in seq_along(shared_occupations)) {
    # Calculate string distances with all occupations in group2
    distances <- stringdist::stringdistmatrix(group1$occupation[i], group2$occupation, method = method)
    
    # Find the best match (minimum distance) and check if it is below the threshold
    best_match_index <- which.min(distances)
    if (!is.infinite(distances[best_match_index]) && distances[best_match_index] <= threshold) {
      # If a match is found within the threshold, store it
      shared_occupations[[i]] <- group2$occupation[best_match_index]
    }
  }
  
  # Remove NAs (where no match was found) and return unique matches
  shared_occupations <- unique(unlist(shared_occupations))
  shared_occupations <- shared_occupations[!is.na(shared_occupations)]
  
  return(shared_occupations)
}

clean_data <- function(vec, factor) {
  df <- data.frame(Value = vec, Factor = factor)
  df <- df %>% filter(str_trim(Value) != "")
  return(df)
}

generate_unique_lists <- function(Vec1, Vec2, Factor1, Factor2, k = 5) {
  library(dplyr)
  library(stringr)
  
  # Helper function to clean data
  clean_data <- function(vec, factor) {
    df <- data.frame(Value = vec, Factor = factor)
    return(df %>% filter(str_trim(Value) != ""))
  }
  
  df1 <- clean_data(Vec1, Factor1)
  df2 <- clean_data(Vec2, Factor2)
  
  df1 <- df1 %>% group_by(Factor, Value) %>% summarise(Count = n(), .groups = 'drop')
  df2 <- df2 %>% group_by(Factor, Value) %>% summarise(Count = n(), .groups = 'drop')
  
  df_merge <- full_join(df1, df2, by = c("Factor", "Value"))
  df_merge$Count.x[is.na(df_merge$Count.x)] <- 0
  df_merge$Count.y[is.na(df_merge$Count.y)] <- 0
  
  # Calculate the top shared and unique responses
  top_shared <- df_merge %>% filter(Count.x > 0 & Count.y > 0) %>%
    group_by(Factor) %>%
    arrange(desc(pmin(Count.x, Count.y))) %>%
    slice_head(n = k) %>%
    ungroup() %>%
    select(Factor, Value, Count.x, Count.y)
  
  top_unique_vec1 <- df_merge %>% filter(Count.x > 0 & Count.y == 0) %>%
    group_by(Factor) %>%
    arrange(desc(Count.x)) %>%
    slice_head(n = k) %>%
    ungroup() %>%
    select(Factor, Value, Count.x)
  
  top_unique_vec2 <- df_merge %>% filter(Count.x == 0 & Count.y > 0) %>%
    group_by(Factor) %>%
    arrange(desc(Count.y)) %>%
    slice_head(n = k) %>%
    ungroup() %>%
    select(Factor, Value, Count.y)
  
  return(list(top_shared_responses = top_shared,
              top_unique_responses_1 = top_unique_vec1,
              top_unique_responses_2 = top_unique_vec2))
}


calculate_overlap_metrics <- function(Vec1, Vec2, Factor1, Factor2, Vec1Name, Vec2Name) {
  require(stringr)
  dat <- data.frame("Jobs"=c(Vec1,Vec2),
                    "AI"=c(Factor1,Factor2),
                    "Comp"=c(rep(Vec1Name,length(Vec1)),rep(Vec2Name,length(Vec2))))
  
  dat <- dat[dat$Jobs!=" ",]
  
  index <- unique(dat$AI)
  holder <- list()
  for(i in 1:length(index)){
    AI <- index[i]
    dati <- dat[dat$AI==AI,]  
    UniqueVec1 <- unique(dati[dati$Comp == Vec1Name,"Jobs"])
    UniqueVec2 <- unique(dati[dati$Comp == Vec2Name,"Jobs"])
    
    Count1 = table(dati[dati$Comp == Vec1Name,"Jobs"])
    Count2 = table(dati[dati$Comp == Vec2Name,"Jobs"])
    
    shared_overlap = sum(pmin(Count1, Count2))
    
    Nvec1 = length(UniqueVec1)
    Nvec2 = length(UniqueVec2)
    Overlap = sum(UniqueVec1 %in% UniqueVec2)
    LenUniqueVec1 = length(UniqueVec1[!UniqueVec1 %in% UniqueVec2])
    LenUniqueVec2 = length(UniqueVec2[!UniqueVec2 %in% UniqueVec1])
    totVec1 = length(dati[dati$Comp == Vec1Name,"Jobs"])
    totVec2 = length(dati[dati$Comp == Vec2Name,"Jobs"])
    PropUniqueVec1 = LenUniqueVec1/Nvec1
    PropUniqueVec2 = LenUniqueVec2/Nvec2
    
    # Calculate intersection and union
    intersection <- intersect(UniqueVec1, UniqueVec2)
    union <- union(UniqueVec1, UniqueVec2)
    
    # Calculate Jaccard similarity
    jaccard_similarity <- length(intersection) / length(union)
    
    # Calculate unique proportions
    Prop_unique1 <- length(setdiff(UniqueVec1, intersection)) / length(union)
    Prop_unique2 <- length(setdiff(UniqueVec2, intersection)) / length(union)
    
    # Ensure total is 1
    total <- jaccard_similarity +  Prop_unique1 +  Prop_unique2
    
    out <- data.frame("AI" = AI,
                     "TotUniqueJobsVec1"= Nvec1,
                     "TotUniqueJobsVec2"= Nvec2,
                     "Unique_Overlap" = Overlap,
                     "UniqueVec1" = LenUniqueVec1,
                     "UniqueVec2" = LenUniqueVec2,
                     "TotalVec1" = totVec1,
                     "TotalVec2" = totVec2,
                     "SharedOverlap" = shared_overlap,
                     "PropVec1" =  Prop_unique1,
                     "PropVec2" =  Prop_unique2,
                     "Jaccard" = jaccard_similarity )
    holder[[i]] <- out
  }  
  
  dat1 <- do.call("rbind",holder)
    
  plot_dat_prop <- dat1 %>%
    select("AI", "PropVec1", "PropVec2","Jaccard") %>%
    pivot_longer(
      cols = -1, # Exclude the first column for the pivot
      names_to = "Variable", 
      values_to = "Value"
    )
  
  plot_dat_freq <- dat1 %>%
    select("AI","UniqueVec1","UniqueVec2","Unique_Overlap") %>%
    pivot_longer(
      cols = -1, # Exclude the first column for the pivot
      names_to = "Variable", 
      values_to = "Value"
    )
  
  # Rename variables to factors
  plot_dat_prop$Variable <- str_replace_all(plot_dat_prop$Variable,pattern = "Vec1",replacement = Vec1Name)
  plot_dat_prop$Variable <- str_replace_all(plot_dat_prop$Variable,pattern = "Vec2",replacement = Vec2Name)
  plot_dat_freq$Variable <- str_replace_all(plot_dat_freq$Variable,pattern = "Vec1",replacement = Vec1Name)
  plot_dat_freq$Variable <- str_replace_all(plot_dat_freq$Variable,pattern = "Vec2",replacement = Vec2Name)

  return(list("tabdat"=dat1,
              "plot_dat_prop"=plot_dat_prop,
              "plot_dat_freq"=plot_dat_freq))

}



# Adjusts rows for unique tables
# Function to check and adjust the number of rows to 5
adjust_rows <- function(df) {
  # Calculate the number of rows to add
  rows_to_add <- 5 - nrow(df)
  
  # Check if rows need to be added
  if (rows_to_add > 0) {
    # Create an empty data frame with the same column names and types
    empty_df <- df["-", ]  # This copies the structure and NA-fills
    # Replicate the empty data frame rows_to_add times
    empty_df <- do.call(rbind, replicate(rows_to_add, empty_df, simplify = FALSE))
    # Append the empty rows to the original data frame
    df <- rbind(df, empty_df)
  }
  return(df)
}


# Count significant
count_sig  <- function(data, factor1, factor2, factor3) {
  data %>%
    # Group by the factors specified in the function arguments using all_of for tidyselect compatibility
    group_by(across(all_of(c(factor1, factor2, factor3)))) %>%
    summarise(
      # Count non-zero CIs for 95% CI
      Count_95_CI = sum(L95 > 0 | U95 < 0, na.rm = TRUE),
      # Count non-zero CIs for 99% CI
      Count_99_CI = sum(L99 > 0 | U99 < 0, na.rm = TRUE),
      .groups = 'drop'  # This ensures the resulting tibble is ungrouped
    )
}


# # # # # #

calculate_overlap_metrics2 <- function(Vec1, Vec2, Factor1, Factor2, Vec1Name, Vec2Name, n_permutations = 10000) {
  require(stringr)
  require(dplyr)
  require(tidyr)
  
  dat <- data.frame("Jobs"=c(Vec1,Vec2),
                    "AI"=c(Factor1,Factor2),
                    "Comp"=c(rep(Vec1Name,length(Vec1)),rep(Vec2Name,length(Vec2))))
  
  dat <- dat[dat$Jobs != " ",]
  
  index <- unique(dat$AI)
  holder <- list()
  
  for(i in 1:length(index)) {
    AI <- index[i]
    dati <- dat[dat$AI == AI,]
    
    UniqueVec1 <- unique(dati[dati$Comp == Vec1Name,"Jobs"])
    UniqueVec2 <- unique(dati[dati$Comp == Vec2Name,"Jobs"])
    
    Count1 <- table(dati[dati$Comp == Vec1Name,"Jobs"])
    Count2 <- table(dati[dati$Comp == Vec2Name,"Jobs"])
    
    shared_overlap <- sum(pmin(Count1, Count2))
    
    Nvec1 <- length(UniqueVec1)
    Nvec2 <- length(UniqueVec2)
    Overlap <- sum(UniqueVec1 %in% UniqueVec2)
    LenUniqueVec1 <- length(UniqueVec1[!UniqueVec1 %in% UniqueVec2])
    LenUniqueVec2 <- length(UniqueVec2[!UniqueVec1 %in% UniqueVec2])
    totVec1 <- length(dati[dati$Comp == Vec1Name,"Jobs"])
    totVec2 <- length(dati[dati$Comp == Vec2Name,"Jobs"])
    PropUniqueVec1 <- LenUniqueVec1 / Nvec1
    PropUniqueVec2 <- LenUniqueVec2 / Nvec2
    
    # Calculate intersection and union
    intersection <- intersect(UniqueVec1, UniqueVec2)
    union_sets <- union(UniqueVec1, UniqueVec2)
    
    # Calculate Jaccard similarity
    jaccard_similarity <- length(intersection) / length(union_sets)
    
    # Calculate unique proportions
    Prop_unique1 <- length(setdiff(UniqueVec1, intersection)) / length(union_sets)
    Prop_unique2 <- length(setdiff(UniqueVec2, intersection)) / length(union_sets)
    
    # Permutation Test
    all_sets <- list(UniqueVec1, UniqueVec2)
    labels <- c(rep(0, length(UniqueVec1)), rep(1, length(UniqueVec2)))
    
    jaccard_distribution <- numeric(n_permutations)
    exclusive_A_distribution <- numeric(n_permutations)
    exclusive_B_distribution <- numeric(n_permutations)
    
    for (j in 1:n_permutations) {
      permuted_labels <- sample(labels)
      permuted_A <- all_sets[[1]][permuted_labels == 0]
      permuted_B <- all_sets[[2]][permuted_labels == 1]
      
      permuted_jaccard <- length(intersect(permuted_A, permuted_B)) / length(union(permuted_A, permuted_B))
      permuted_exclusive <- c(length(setdiff(permuted_A, permuted_B)), length(setdiff(permuted_B, permuted_A)))
      
      jaccard_distribution[j] <- permuted_jaccard
      exclusive_A_distribution[j] <- permuted_exclusive[1]
      exclusive_B_distribution[j] <- permuted_exclusive[2]
    }
    
    # Calculate p-values
    p_value_jaccard <- mean(jaccard_distribution >= jaccard_similarity)
    p_value_exclusive_A <- mean(exclusive_A_distribution >= LenUniqueVec1)
    p_value_exclusive_B <- mean(exclusive_B_distribution >= LenUniqueVec2)
    
    out <- data.frame("AI" = AI,
                      "TotUniqueJobsVec1" = Nvec1,
                      "TotUniqueJobsVec2" = Nvec2,
                      "Unique_Overlap" = Overlap,
                      "UniqueVec1" = LenUniqueVec1,
                      "UniqueVec2" = LenUniqueVec2,
                      "TotalVec1" = totVec1,
                      "TotalVec2" = totVec2,
                      "SharedOverlap" = shared_overlap,
                      "PropVec1" = Prop_unique1,
                      "PropVec2" = Prop_unique2,
                      "Jaccard" = jaccard_similarity,
                      "P_value_Jaccard" = p_value_jaccard,
                      "P_value_UniqueVec1" = p_value_exclusive_A,
                      "P_value_UniqueVec2" = p_value_exclusive_B)
    holder[[i]] <- out
  }
  
  dat1 <- do.call("rbind", holder)
  
  # Proportions
  plot_dat_prop <- dat1 %>%
    select("AI", "PropVec1", "PropVec2", "Jaccard") %>%
    pivot_longer(
      cols = -1, # Exclude the first column for the pivot
      names_to = "Variable", 
      values_to = "Value"
    )
  # Frequencies
  plot_dat_freq <- dat1 %>%
    select("AI", "UniqueVec1", "UniqueVec2", "Unique_Overlap") %>%
    pivot_longer(
      cols = -1, # Exclude the first column for the pivot
      names_to = "Variable", 
      values_to = "Value"
    )
  #P_values
  plot_dat_pval <- dat1 %>%
    select("AI", "P_value_UniqueVec1", "P_value_UniqueVec2", "P_value_Jaccard") %>%
    pivot_longer(
      cols = -1, # Exclude the first column for the pivot
      names_to = "Variable", 
      values_to = "Value"
    )
  
  # Rename variables to factors: Props
  plot_dat_prop$Variable <- str_replace_all(plot_dat_prop$Variable, pattern = "Vec1", replacement = Vec1Name)
  plot_dat_prop$Variable <- str_replace_all(plot_dat_prop$Variable, pattern = "Vec2", replacement = Vec2Name)
  # Rename variables to factors: Freqs 
  plot_dat_freq$Variable <- str_replace_all(plot_dat_freq$Variable, pattern = "Vec1", replacement = Vec1Name)
  plot_dat_freq$Variable <- str_replace_all(plot_dat_freq$Variable, pattern = "Vec2", replacement = Vec2Name)
  # Rename variables to factors: Pvals
  plot_dat_prop$Variable <- str_replace_all(plot_dat_prop$Variable, pattern = "Vec1", replacement = Vec1Name)
  plot_dat_prop$Variable <- str_replace_all(plot_dat_prop$Variable, pattern = "Vec2", replacement = Vec2Name)
  
  return(list("tabdat" = dat1,
              "plot_dat_prop" = plot_dat_prop,
              "plot_dat_freq" = plot_dat_freq,
              "plot_dat_pval" = plot_dat_pval))
}


# calculate_overlaps_with_factor <- function(Vec1, Vec2, Factor1, Factor2, NameVec1 = "Vec1", NameVec2 = "Vec2", k = 5) {
#   library(dplyr)
#   library(stringr)
#   library(tidyr)
#   
#   # Preprocessing: Remove blank entries from vectors
#   clean_data <- function(vec, factor) {
#     df <- data.frame(Value = vec, Factor = factor)
#     df <- df %>% filter(str_trim(Value) != "")
#     return(df)
#   }
#   
#   df1 <- clean_data(Vec1, Factor1)
#   df2 <- clean_data(Vec2, Factor2)
#   
#   df1 <- df1 %>% group_by(Factor, Value) %>% summarise(Count = n(), .groups = 'drop')
#   df2 <- df2 %>% group_by(Factor, Value) %>% summarise(Count = n(), .groups = 'drop')
#   
#   # Perform full join on factor and value
#   df_merge <- full_join(df1, df2, by = c("Factor", "Value"))
#   
#   # Fill NA values with 0 for Counts
#   df_merge$Count.x[is.na(df_merge$Count.x)] <- 0
#   df_merge$Count.y[is.na(df_merge$Count.y)] <- 0
#   
#   # Calculate overlaps and counts for each factor
#   results <- df_merge %>%
#     group_by(Factor) %>%
#     summarise(
#       shared_overlap = sum(pmin(Count.x, Count.y)),
#       unshared_proportion_1 = sum(Count.x) - sum(pmin(Count.x, Count.y)),
#       unshared_proportion_2 = sum(Count.y) - sum(pmin(Count.x, Count.y)),
#       total_count_1 = sum(Count.x),
#       total_count_2 = sum(Count.y),
#       .groups = 'drop'
#     )
#   
#   # Renaming columns for clarity and based on function inputs
#   results <- results %>%
#     rename_with(~paste0(., "_", NameVec1, "_", NameVec2), starts_with("unshared_proportion")) %>%
#     rename_with(~paste0(., "_", NameVec1, "_", NameVec2), starts_with("total_count"))
#   
#   # Data for plotting
#   data_for_plot <- results %>%
#     mutate(
#       Shared = shared_overlap,
#       Unique_1 = get(paste0("unshared_proportion_1_", NameVec1, "_", NameVec2)),
#       Unique_2 = get(paste0("unshared_proportion_2_", NameVec1, "_", NameVec2))
#     ) %>%
#     select(Factor, Shared, Unique_1, Unique_2) %>%
#     pivot_longer(cols = -Factor, names_to = "Category", values_to = "Count") %>%
#     mutate(Category = sub("Unique_1", paste("Unique", NameVec1, sep=" "), Category),
#            Category = sub("Unique_2", paste("Unique", NameVec2, sep=" "), Category))
#   
#   # Calculate the top shared and unique responses, maintaining factor level
#   top_shared <- df_merge %>% filter(Count.x > 0 & Count.y > 0) %>%
#     group_by(Factor) %>%
#     arrange(desc(pmin(Count.x, Count.y))) %>%
#     slice_head(n = k) %>%
#     ungroup() %>%
#     select(Factor, Value, Count.x, Count.y)
#   
#   top_unique_vec1 <- df_merge %>% filter(Count.x > 0 & Count.y == 0) %>%
#     group_by(Factor) %>%
#     arrange(desc(Count.x)) %>%
#     slice_head(n = k) %>%
#     ungroup() %>%
#     select(Factor, Value, Count.x)
#   
#   top_unique_vec2 <- df_merge %>% filter(Count.x == 0 & Count.y > 0) %>%
#     group_by(Factor) %>%
#     arrange(desc(Count.y)) %>%
#     slice_head(n = k) %>%
#     ungroup() %>%
#     select(Factor, Value, Count.y)
#   
#   return(list(overlap_results = results, 
#               plotting_data = data_for_plot,
#               top_shared_responses = top_shared,
#               top_unique_responses_1 = top_unique_vec1,
#               top_unique_responses_2 = top_unique_vec2,
#               k = k))
# }
# 



# 
# make_compare_ven <- function(vec1,vec2,fac1,fac2,lab1 = "FACTOR X",lab2 = "FACTOR Y",kjobs){
#   #Calculate marginal overlap
#   overlaps <- calculate_overlaps_with_factor(vec1,vec2,fac1,fac2,k = kjobs)
#   k = overlaps$k
#   #venns
#   p <- list()
#   for(i in 1:length(unique(overlaps$overlap_results$Factor))){
#     # Isolate Inputs by LLM level
#     metrics <- overlaps$overlap_results[i,]
#     shared <- overlaps$top_shared_responses[overlaps$top_shared_responses$Factor == metrics$Factor,]
#     left <- overlaps$top_unique_responses_vec1[overlaps$top_unique_responses_vec1$Factor == metrics$Factor,]
#     right <- overlaps$top_unique_responses_vec2[overlaps$top_unique_responses_vec2$Factor == metrics$Factor,]
#     
#     circles <- data.frame(
#       x0 = c(-10,10),
#       y0 = c(0,0),
#       r = c(20,20))
#     
#     p[[i]] <- ggplot() +
#       geom_circle(data = circles,aes(x0 = x0,y0 = y0,r = r), fill = "gray40",alpha = 0.2,size =2) +
#       geom_text(aes(x = 0, y = 11.5),label = "Overlap",size = 6, fontface = "bold") +
#       geom_text(aes(x = 0, y = 10),label =
#                   paste0("Unique: ",round(100*metrics$unique_overlap,1),"%"),size = 6) +
#       geom_text(aes(x = 0, y = 8.5),
#                 label = paste0("Weighted: ",round(100*metrics$weighted_overlap,1),"%"),size = 6) +
#       geom_text(aes(x=-8,y =0),
#                 label = paste0("Most frequent shared occupations\n",
#                                paste0(1:k,".) ",shared$Value, sep = "\n", collapse = "")),size = 4,hjust = 0) +
#       geom_text(aes(x=5,y =0),
#                 label = paste0("Frequency\n",lab1,",",lab2,"\n",paste0(1:k,".) ",shared$Count.x,"  ",
#                                                                        shared$Count.y,sep = "\n",collapse = "")),size = 4,hjust = 0) +  
#       geom_text(aes(x=-28,y =0),
#                 label = paste0("Most frequent unique ,",lab1,", occupations\n",
#                                paste0(1:k,".) ",left$Value, sep = "\n", collapse = "")),size = 4,hjust = 0) +
#       geom_text(aes(x=-13,y =0),
#                 label = paste0("Frequency\n",paste0(1:k,".) ",left$Count.x,sep = "\n",collapse = "")),size = 4,hjust = 0) + 
#       geom_text(aes(x=11,y =0),
#                 label = paste0("Most frequent unique ",lab2," occupations\n",
#                                paste0(1:k,".) ",right$Value, sep = "\n", collapse = "")),size = 4,hjust = 0) +
#       geom_text(aes(x=25,y =0),
#                 label = paste0("Frequency\n",paste0(1:k,".) ",right$Count.y,sep = "\n",collapse = "")),size = 4,hjust = 0) +  
#       geom_text(aes(x = -10, y = 21),label = lab1,size = 10, fontface = "bold") +
#       geom_text(aes(x = 10, y = 21),label = lab2 ,size = 10, fontface = "bold")  +
#       theme_void() +
#       ggtitle(label = paste0("Overlap Metrics for ",metrics$Factor[1])) + 
#       theme(plot.title = element_text(size = 30, face = "bold")) +
#       theme(plot.margin=grid::unit(c(-0.5,-0.5,-0.5,-0.5), "mm"))
#   }
#   pstudy1 <- do.call(grid.arrange,p)
#   return(pstudy1)
# }
# 
# 
# 
# make_compare_ven_new <- function(vec1,vec2,fac1,fac2,lab1 = "FACTOR X",lab2 = "FACTOR Y",kjobs){
#   #Calculate marginal overlap
#   overlaps <- calculate_overlaps_with_factor(vec1,vec2,fac1,fac2,k = kjobs)
#   k = overlaps$k
#   
#   
#   # Isolate Inputs by LLM level
#   metrics <- overlaps$overlap_results
#   shared <- overlaps$top_shared_responses
#   left <- overlaps$top_unique_responses_vec1
#   right <- overlaps$top_unique_responses_vec2
#   
#   circles <- data.frame(
#     x0 = c(-10,10),
#     y0 = c(0,0),
#     r = c(20,20)
#   )
#   
#   paste0("GPT-3.5, ", round(100*metrics[metrics$Factor == "GPT-3.5","weighted_overlap"],1),"%\n",
#          "GPT-4, ", round(100*metrics[metrics$Factor == "GPT-4","weighted_overlap"],1),"%\n",
#          "Gemini, ", round(100*metrics[metrics$Factor == "Gemini","weighted_overlap"],1),"%\n",
#          "Llama 2, ", round(100*metrics[metrics$Factor == "Llama 2","weighted_overlap"],1),"%\n")
#   
#   ggplot() +
#     geom_circle(data = circles,aes(x0 = x0,y0 = y0,r = r), fill = "gray40",alpha = 0.2,size =2) +
#     geom_text(aes(x = 0, y = -12),
#               label = paste0("Weighted Overlap\n",
#                              "GPT-3.5, ", round(100*metrics[metrics$Factor == "GPT-3.5","weighted_overlap"],1),"%\n",
#                              "GPT-4, ", round(100*metrics[metrics$Factor == "GPT-4","weighted_overlap"],1),"%\n",
#                              "Gemini, ", round(100*metrics[metrics$Factor == "Gemini","weighted_overlap"],1),"%\n",
#                              "Llama 2, ", round(100*metrics[metrics$Factor == "Llama 2","weighted_overlap"],1),"%\n"), size = 5) +
#     geom_text(aes(x=-6,y =1),
#               label = paste0(shared$Factor," ", shared$Value," (",shared$Count.x,",",shared$Count.y,")","\n", collapse = ""),size = 4,hjust = 0) +
#     geom_text(aes(x = -10, y = 21),label = lab1,size = 5, fontface = "bold") +
#     geom_text(aes(x = 10, y = 21),label = lab2 ,size = 5, fontface = "bold")  +
#     theme_void() +
#     theme(plot.title = element_text(size = 30, face = "bold")) +
#     theme(plot.margin=grid::unit(c(-0.5,-0.5,-0.5,-0.5), "mm"))
#   
#   return(pstudy1)
# }
