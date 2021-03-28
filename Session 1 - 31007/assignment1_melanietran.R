dataPath <- setwd('/Users/meltra02/Desktop/MSCA/Quarter 1/31007 - Stats Analysis/Session 1 - 31007')
data <- read.table(paste(dataPath,'Week1_Test_Sample.csv',sep = '/'), header=TRUE)

data$u
data$v

#SAVE
table <- function(data$u, data$v) {
  data_uv <- table(data$u, data$v)
  data_uv <- cbind(data_uv, rowSums(data_uv))
  data_uv <- rbind(data_uv, colSums(data_uv))
  rownames(data_uv)[nrow(data_uv)] <- deparse(substitute(data$v))
  colnames(data_uv)[ncol(data_uv)] <- deparse(substitute(data$u))
  data_uv

data_uv/100
data_uv_df <- as.data.frame.matrix(data_uv)
data.matrix(data_uv_df, rownames.force = NA)

#compute joint distribution 
table_uv <- table(data$u, data$v)/100
uv_df <- as.data.frame.matrix(table_uv)
joint_distribution <- data.matrix(uv_df, rownames.force = NA)
class(joint_distribution)

joint_distribution

#compute marginal distribution 
u_Marginal <- c(sum(joint_distribution[1,]),
                sum(joint_distribution[2,]),
                sum(joint_distribution[3,]))
class(u_Marginal)
length(u_Marginal)
u_Marginal

v_Marginal <- c(sum(joint_distribution[,1]),
                sum(joint_distribution[,2]),
                sum(joint_distribution[,3]), 
                sum(joint_distribution[,4]))
class(v_Marginal)
length(v_Marginal)
v_Marginal

#compute P(u|v=4)
u_Conditional_v <- c(joint_distribution[1,4]/v_Marginal[4], 
                     joint_distribution[2,4]/v_Marginal[4],
                     joint_distribution[3,4]/v_Marginal[4])
class(u_Conditional_v)
length(u_Conditional_v)
u_Conditional_v

#compute P(v|u=3)
v_Conditional_u <- c(joint_distribution[3,1]/u_Marginal[3],
                     joint_distribution[3,2]/u_Marginal[3],
                     joint_distribution[3,3]/u_Marginal[3],
                     joint_distribution[3,4]/u_Marginal[3])

class(v_Conditional_u)
length(v_Conditional_u)
v_Conditional_u

#res
res <-list(Joint_distribution=joint_distribution,
           u_Marginal = u_Marginal,
           v_Marginal = v_Marginal,
           u_Conditional_v = u_Conditional_v,
           v_Conditional_u = v_Conditional_u          )
res
saveRDS(res, file = paste(dataPath,'result.rds',sep = '/'))
