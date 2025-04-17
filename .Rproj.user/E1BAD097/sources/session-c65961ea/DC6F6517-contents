# Remove specified columns and transform 'Département de résidence' into a factor
library(dplyr)
new_dataset <- ACC_2023_Chip1_2_ %>%
  select(-`COFACTEURS généraux`, -`ATCD d'anaphylaxie alimentaire (AA)`, -`Clinique par Aliment`, -`TPO`) %>%
  mutate(`Département de résidence` = as.factor(`Département de résidence`))

new_dataset$'Mois du prélèvement' <- as.factor(new_dataset$'Mois du prélèvement')
new_dataset$'ARIA (rhinite)' <- as.factor(new_dataset$'ARIA (rhinite)' )
new_dataset$'Traitement rhinite' <- as.factor(new_dataset$'Traitement rhinite')
new_dataset$'GINA (ancien)' <- as.factor(new_dataset$'GINA (ancien)')
new_dataset$'Traitement (New) GINA' <- as.factor(new_dataset$'Traitement (New) GINA')
   
new_dataset <-as.data.frame(new_dataset)

new_dataset2 <- new_dataset %>%
  select(-`Département de résidence`, -`Puce réalisée`) 

new_dataset2 <-as.data.frame(new_dataset2)

which(unlist(lapply(new_dataset2,is.factor)))

new_dataset2[, 61:ncol(new_dataset2)] <- 
  apply(new_dataset2[, 61:ncol(new_dataset2)], 2, function(x) ifelse(is.na(x), 0, x))


#res.ca<-CA(new_dataset2, ncp = 5, row.sup = NULL, col.sup = NULL, 
#   quanti.sup=c(1, 2,4:19,24:60), quali.sup = which(unlist(lapply(new_dataset2,is.factor))), 
#   axes = c(1,2))

new_dataset2 <- new_dataset2[,4:ncol(new_dataset2)]

new_dataset3 <-new_dataset2[,c(1:5,20:23, 61:ncol(new_dataset2))]

new_dataset3 <- new_dataset3 %>%
  mutate(
    # Découpage de l'âge avec une nouvelle catégorie pour les valeurs manquantes
    Age_cat = case_when(
      is.na(Age) ~ "Données manquantes",
      Age <= 5 ~ "0-5 ans",
      Age <= 10 ~ "6-10 ans",
      Age <= 15 ~ "11-15 ans",
      Age <= 20 ~ "16-20 ans",
      Age <= 40 ~ "21-40 ans",
      Age <= 60 ~ "41-60 ans",
      Age > 60 ~ "61 ans et plus",
      TRUE ~ "Autre"
    ),
    
    # Transformation de Sexe en facteur avec NA comme une nouvelle catégorie
    Sexe = factor(ifelse(is.na(Sexe), "Données manquantes", Sexe), 
                  levels = c(0, 1, "Données manquantes"), 
                  labels = c("Homme", "Femme", "Données manquantes"))
  )

# Transformation des autres variables catégorielles avec NA en nouvelle catégorie
categorical_columns <- c(
  "Mois du prélèvement", "Région de France", "Type d'habitat", 
  "ARIA (rhinite)", "Traitement rhinite", "GINA (ancien)", "Traitement (New) GINA"
)

new_dataset3 <- new_dataset3 %>%
  mutate(across(all_of(categorical_columns), ~ factor(ifelse(is.na(.), "Données manquantes", .))))

new_dataset3 <- as.data.frame(new_dataset3)
new_dataset3 <- new_dataset3[, -1]
new_dataset3[,121]<-as.factor(new_dataset3[,121])

new_dataset4 <- new_dataset3[-85,]

res.ca<-CA(new_dataset4, ncp = 5, row.sup = NULL, col.sup = NULL, 
 quali.sup = which(unlist(lapply(new_dataset3,is.factor))), 
   axes = c(1,2), graph=FALSE)
 
plot(res.ca,invisible = c("quali.sup", "row"), cex = 0.4, selectCol = "contrib 60")

plot(res.ca,invisible = c("quali.sup", "row"), cex = 0.5, selectCol = "contrib 60", autoLab = "yes")




plot(res.ca,invisible = c("row","col"), cex = 0.5 )

plot(res.ca,invisible = c("quali.sup","col"), habillage =1,cex = 0.5)

plot(res.ca, habillage=6,cex = 0.5, label = c("none"))


data_coords <- res.ca$quali.sup$coord[, 1:2]

# Assuming `res.ca$quali.sup$coord` is the input data
data_coords <- res.ca$quali.sup$coord[, 1:2]

# List of unique variables based on rownames
variables <- unique(sub("\\..*", "", rownames(data_coords)))

# Loop through each variable to create a plot
for (var in variables) {
  # Escape any special characters in variable names for grep
  escaped_var <- gsub("([\\^\\$\\.\\|\\(\\)\\[\\]\\*\\+\\?\\\\])", "\\\\\\1", var, perl = TRUE)
  
  # Subset the data for the current variable
  subset_data <- data_coords[grep(paste0("^", escaped_var, "\\."), rownames(data_coords)), ]
  
  # Check if subset_data is not empty
  if (nrow(subset_data) > 0) {
    # Plot with adjusted cex for points and no fixed aspect ratio
    plot(
      subset_data, 
      xlim = c(-1, 4), 
      ylim = c(-1, 1), 
      main = paste("Variable:", var),
      xlab = "Dim 1", 
      ylab = "Dim 2",
      pch = 19, 
      col = "blue", 
      cex = 0.7,  # Adjust the size of the points
      asp = NA    # Disable fixed aspect ratio to keep axes unscaled
    )
    
    # Add text labels to the plot with smaller text size
    text(subset_data, labels = rownames(subset_data), pos = 4, cex = 0.5, col = "darkred")
  } else {
    # If no data is found, print a message
    message(paste("No data found for variable:", var))
  }
}

