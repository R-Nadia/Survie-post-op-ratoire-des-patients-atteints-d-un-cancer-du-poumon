################################
### LIBRAIRIES
################################
library(igraph) 
library(qgraph) 
library(ggplot2) 
library(foreign) 
library(kableExtra)
library(mltools) 
library(data.table)
library(funModeling)
library(dplyr)
library(DMwR) 
library(corrplot)
library(stringr)
library(bnlearn)
library(pcalg) 

# -------------------------------------
################################
### FUNCTIONS
################################
DGN <- c("Dgn1", "Dgn2", "Dgn3", "Dgn4", "Dgn5", "Dgn6", "Dgn8")
ZS <- c("Prz0", "Prz1", "Prz2")
TS <- c("Oc11", "Oc12", "Oc13", "Oc14")

plot_graph <- function(graph,main){
  e = get.edgelist(graph, names=FALSE)
  l = qgraph.layout.fruchtermanreingold(e, vcount=vcount(graph),
                                        area=8*(vcount(graph)^2), repulse.rad=(vcount(graph)^3.1))
  V(graph)$color <- ifelse(attr(V(graph), "names") %in% DGN, "red", 
                           ifelse(attr(V(graph), "names") %in% ZS , "violet", 
                                  ifelse(attr(V(graph), "names") %in% TS , "gray", 
                                         ifelse(attr(V(graph), "names") == "Death" , "green","deepskyblue"))))
  
  plot(graph,layout=l,
       edge.arrow.size=0.2, vertex.size=15,
       vertex.label.color="black",main=main)
}

# ----------------------------------
################################
### EXPLORATORY ANALYSIS
################################
data <- read.arff("C:/Users/NADIA/Desktop/M2 MLDS/01-Network reconstruction and analysis/Projet/ThoraricSurgery.arff")
View(data)
dim(data)
summary(data)
colnames(data)
# Check if there is missing values
sum(is.na(data)) 
table(data$Risk1Yr)

# -------------------------------
# PLOT I : Shows how many patients survived 1 year and how many did not after surgery
ggplot(data) +
  geom_bar(mapping = aes(x = Risk1Yr, fill = Risk1Yr)) +
  theme(legend.position = "none")+scale_fill_brewer(palette="Blues")+
  labs(title = "Bart chart of risk",x="Survive 1st year (Risk1Yr)")+
  theme_minimal()

# -------------------------------
# PLOT II :  Check which DGN code has the greatest impact on the patients
ggplot(data, aes(x="", y=DGN, fill=DGN)) +
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0)+scale_fill_brewer(palette="Blues")+
  theme_void()

# -------------------------------
# PLOT III : Shows count of a given DGN in patients who didnt survive 1st year
data2 <- subset(data, Risk1Yr == "T")
ggplot(data2) +
  geom_bar(mapping = aes(x = DGN, fill = DGN)) +
  theme(legend.position = "none")+scale_fill_brewer(palette="Blues")+
  labs(title = "Type of DGN in patients who didn't survive first year after surgery",x="DGN")+
  theme_minimal()

# ---------------------------------
# VIEWER I : Determine if the outcome and other binary variables are balanced
z <- sapply(data, summary)
as.data.frame(z[c("Risk1Yr", "PRE7", "PRE8", "PRE9", "PRE10", "PRE11",
                  "PRE17", "PRE19", "PRE25", "PRE30", "PRE32")]) %>%
  kable(caption = "Distribution of binary variables") %>%
  kable_styling(latex_options = "scale_down",
                bootstrap_options = c("striped", "hover", "condensed", "responsive"),
                position = "center",
                font_size = 10,
                full_width = FALSE)

# ---------------------------------
# VIEWER II : Show the first 6 rows of the pre-processed dataset
head(data) %>%
  kable(caption = "dataset after pre-processing (first 6 rows)") %>%
  kable_styling(latex_options = "scale_down",
                bootstrap_options = c("striped", "hover", "condensed", "responsive"),
                position = "center",
                font_size = 10,
                full_width = FALSE)

# ---------------------------------
# Rename the columns with more intuitive names
colnames(data) <- c('Daignosis','Forced_Capacity','Forced_Expiration','Zubrod_scale','Pain',' Haemoptysis','Dyspnoea',
                    'Cough','Weakness','Size_of_tumor','diabetes','MI_6months','PAD','Smoker','Asthmatic','Age','Death')
# -----------------------------
# Convert factor variables into multiple variables (integers with values 0 or 1)

## DAIGNOSIS
d <- data.table(
  ID = seq(1,nrow(data),by=1),
  Variable = data$Daignosis)
newdata <- one_hot(d)
colnames(newdata) <- c("ID", "DGN1", "DGN2", "DGN3", "DGN4",
                       "DGN5", "DGN6", "DGN8")
data <- data %>% mutate(ID = row_number()) %>%
  left_join(newdata, by="ID") %>%
  mutate(Dgn3 = DGN3, Dgn2 = DGN2, Dgn4 = DGN4, Dgn6 = DGN6,
         Dgn5 = DGN5, Dgn8 = DGN8, Dgn1 = DGN1) %>%
  select(-Daignosis, -ID, -DGN3, -DGN2, -DGN4, -DGN6, -DGN5, -DGN8, -DGN1)

## ZUBROD SCALE
d <- data.table(
  ID = seq(1,nrow(data),by=1),
  Variable = data$Zubrod_scale)
newdata <- one_hot(d)
colnames(newdata) <- c("ID", "PRZ0", "PRZ1", "PRZ2")
data <- data %>% mutate(ID = row_number()) %>%
  left_join(newdata, by="ID") %>%
  mutate(Prz0 = PRZ0, Prz1 = PRZ1, Prz2 = PRZ2) %>%
  select(-Zubrod_scale, -ID, -PRZ0, -PRZ1, -PRZ2)

## TUMOR SIZE
d <- data.table(
  ID = seq(1,nrow(data),by=1),
  Variable = data$Size_of_tumor)
newdata <- one_hot(d)
colnames(newdata) <- c("ID", "OC11", "OC12", "OC13","OC14")
data <- data %>% mutate(ID = row_number()) %>%
  left_join(newdata, by="ID") %>%
  mutate(Oc11 = OC11, Oc12 = OC12, Oc13 = OC13, Oc14 = OC14) %>%
  select(-Size_of_tumor, -ID, -OC11, -OC12, -OC13, -OC14)

# -------------------------
# Convert logical to numeric
cols <- sapply(data, is.factor)
data[,cols] <- lapply(data[,cols], as.numeric)
data[,cols] <- data[,cols] -1

# ----------------------------
# OUTLIERS : PLOT
outliers <- data %>% select(Forced_Capacity, Forced_Expiration, Age)
outliers_plot <- plot_num(outliers)

# ----------------------------
# OUTLIERS : CALCUL with the Tukey's test
FE <- sum(is.na(prep_outliers(
  data$Forced_Expiration,
  type = "set_na",
  method = "tukey")))

FC <- sum(is.na(prep_outliers(
  data$Forced_Capacity,
  type = "set_na",
  method = "tukey")))

Age <- sum(is.na(prep_outliers(
  data$Age,
  type = "set_na",
  method = "tukey")))
FE
FC
Age

# -----------------------------------
# Extract the rows with the outliers 
FE_rows <- which(is.na(prep_outliers(data$Forced_Expiration,
                                     type = "set_na",
                                     method = "tukey")))
Age_rows <- which(is.na(prep_outliers(data$Age,
                                      type = "set_na",
                                      method = "tukey")))

# Remove outliers from the dataset
data <- data[-c(FE_rows, Age_rows),]
dim(data)

# Plot without outliers
without_outliers <- data %>% select(Forced_Capacity, Forced_Expiration, Age)
without_outliers_plot <- plot_num(without_outliers)

# ---------------------------------

################################
### DATA BALANCING
################################
# We performe the SMOTE method on the data. By using it, the target variable is much more balanced:
data_balanced_fact <- data %>% mutate(Death = factor(Death, labels = c("Alive", "Dead")))
data_balanced <- SMOTE(Death ~ ., data = data_balanced_fact, k=5)

# Check the balance of the target variable
table(data_balanced$Death)

# Recode variable DEATH into integers
data_balanced <- data_balanced %>%
  mutate(Death = as.factor(str_replace_all(Death, "Alive", "0"))) %>%
  mutate(Death = as.factor(str_replace_all(Death, "Dead", "1"))) %>%
  mutate(Death = as.numeric(levels(Death))[Death])

#Remove the constant variables
same <- sapply(data_balanced, function(.col){ all(.col[1L] == .col)})
data_balanced <- data_balanced[!same]


# Check dimension and summary
dim(data_balanced)
summary(data_balanced)

# ------------------------------

############################
####  CORRELATION
############################
# Compute paiwise correlations
res_cor <- cor(data_balanced) 
corrplot(res_cor, type = "upper", order = "hclust",
         col = c("black", "white"), bg = "lightblue", tl.col="black", tl.srt=80)


# Calculate strong correlations
res_cor <- round(res_cor,2)
cm <- as.data.frame(as.table(res_cor))
cm %>% filter(abs(Freq) > 0.5 & abs(Freq) < 1)

# ------------------------------
# Plot the correlation network
names = colnames(data_balanced)
Graph_cor <- qgraph(res_cor, graph = "cor", layout = "spring", threshold = 0.03,
                    alpha = 0.05, nodeNames = names,sampleSize = nrow(data_balanced),
                    legend.cex = 0.3,vsize = 3.5,label.cex=3,posCol = "black",
                    negCol = "#FF5733",details = TRUE)

# ------------------------------
##################################
####  PARTIAL CORRELATION NETWORK
##################################
# Covariance matrix
res_cov = cov(data_balanced)
diag(res_cov)
det(res_cov)

#Regularization by adding a small value lambda to the covariance matrix diagonal
#lambda = 1e-10
for (i in seq(1,26)){
  res_cov[i,i]=res_cov[i,i]+1e-1
  
}
diag(res_cov)
det(res_cov)

# Invert the covariance matrix
inv_cov = solve(res_cov)
inv_cov <- round(inv_cov, 4)
View(inv_cov)

# Plot the partial correlation network
Graph_pcor <- qgraph(inv_cov, graph = "pcor", layout = "spring", threshold = 0.03,
                     alpha = 0.05, nodeNames = names,sampleSize = nrow(data_balanced),
                     legend.cex = 0.3,vsize = 3.5,label.cex=3,posCol = "black",
                     negCol = "#FF5733",details = TRUE)

# ------------------------------
##################################
#### HILL CLIMBING
##################################
dataHC <- bnlearn::hc(data_balanced)
adjHC <- bnlearn::amat(dataHC)
graphHC <- graph_from_adjacency_matrix(adjHC)
#-------------------------------------
# PLOT 
plot_graph(graphHC,"HC plot")
legend(x=2, y=1, c("Death","Zubrod scale", "Tumor size","DGN","Others"), pch=21,
       col="#777777", pt.bg=c("green","violet","gray","red","deepskyblue"), pt.cex=2, cex=.8, bty="n", ncol=1)
#-------------------------------------
# Identify the hubs
sort(hub_score(graphHC)$vector, decreasing = TRUE)[1:5]

# Top 10 nodes and edges in terms of betweenness centrality measure
sort(betweenness(graphHC), decreasing = TRUE) [1:10]

#Edges
ed=edge_betweenness(graphHC)
i=order(ed, decreasing=T)[1:10]
edges=get.edgelist(graphHC)[i,]
colnames(edges) = c('From', 'To')
View(edges)

# ------------------------------
##################################
#### ARACNE
##################################
dataARACNE <- bnlearn::aracne(data_balanced)
adjARACNE <- bnlearn::amat(dataARACNE)
graphARACNE <- graph_from_adjacency_matrix(adjARACNE)
#-------------------------------------
# PLOT
plot_graph(graphARACNE, "ARACNE plot")
legend(x=2, y=1, c("Death","Zubrod scale", "Tumor size","DGN","Others"), pch=21,
       col="#777777", pt.bg=c("green","violet","gray","red","deepskyblue"), pt.cex=2, cex=.8, bty="n", ncol=1)
#-------------------------------------
# Identify the hubs
sort(hub_score(graphARACNE)$vector, decreasing = TRUE)[1:5]

# Top 10 nodes and edges in terms of betweenness centrality measure
sort(betweenness(graphARACNE), decreasing = TRUE) [1:10]

#Edges
ed=edge_betweenness(graphARACNE)
i=order(ed, decreasing=T)[1:10]
edges=get.edgelist(graphARACNE)[i,]
colnames(edges) = c('From', 'To')
View(edges)

# ------------------------------
##################################
#### PC
##################################
# Copy data
dataBis<-data.frame(data)
# Check whether the memory addresses are same
tracemem(dataBis)==tracemem(data)

# Recode continuous variables to cotegorical
dataBis$Age<-cut(dataBis$Age, breaks=c(-Inf, 56, 61, 68, Inf), labels=c(0:3))
table(dataBis$Age)
dataBis$Forced_Capacity <- cut(dataBis$Forced_Capacity, 
                               breaks=c(-Inf, 2.6, 3.16, 3.87, Inf), 
                               labels=c(0:3))
table(dataBis$Forced_Capacity)
dataBis$Forced_Expiration <- cut(dataBis$Forced_Expiration, 
                                 breaks=c(-Inf, 1.95, 2.36, 2.98, Inf), 
                                 labels=c(0:3))
table(dataBis$Forced_Expiration)

sum(is.na(dataBis))

# Network reconstruction
dataPC <- data.matrix(dataBis)
nlevels <- apply(dataPC,2,function(x) length(attr(as.factor(x), "levels")))
suffStat <- list(dm = dataPC, nlev=nlevels, adaptDF = FALSE)
pc_model <- pc(suffStat, indepTest=disCItest, alpha=0.5, labels=colnames(dataPC))
pc <- as.bn(pc_model, check.cycles = FALSE)
adjPC <- amat(pc)
graph_pc <- graph_from_adjacency_matrix(adjPC)
plot_graph(graph_pc,"PC plot")

# Identify the hubs
View(sort(hub_score(graph_pc)$vector, decreasing = TRUE)[1:5])

# Top 10 nodes and edges in terms of betweenness centrality measure
## Nodes
sort(betweenness(graph_pc), decreasing = TRUE)[1:10]
## Edges
ed=edge_betweenness(graph_pc)
i=order(ed, decreasing=T)[1:10]
edges=get.edgelist(graph_pc)[i,]
colnames(edges) = c('From', 'To')
View(edges)
