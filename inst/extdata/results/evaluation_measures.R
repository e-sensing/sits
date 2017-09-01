library(sits)
library(diffeR)

glm.data <- sits_conf_fromGZ("./inst/extdata/results/glm_original_labels.json.gz")

conf.mx <- sits_accuracy(glm.data)

table.mx <- conf.mx$table

diffeR::overallQtyD(table.mx)

# Quantity difference is defined as the amount of difference between the
# reference variable and a comparison variable that is due to the less than maximum match in the
# proportions of the categories.
diffeR::quantityDj(table.mx)

diffeR::diffTablej(table.mx)

diffeR::overallAllocD(table.mx)

gwet<-function(ratings){
    #First, convert ratings into a r_ik table.
    # r_ik is the number of raters that voted object i to be of a category k.
    # Some ratings can be missing, so accept NAs in ratings
    ratings<-as.matrix(ratings);
    rLevels<-ratings; dim(rLevels)<-NULL; na.omit(unique(rLevels))->rLevels;
    #If there is only single class, all raters agree
    if(length(rLevels)==1) return(1);
    sapply(rLevels,function(level) rowSums(ratings==level,na.rm=TRUE))->r_ik;

    #Next calculate pi_k, vector of mean number of rater votes for each class
    # over objects
    pi_k<-colMeans(r_ik/rowSums(r_ik));

    #Then calculate p_e
    p_e<-sum(pi_k*(1-pi_k))/(length(pi_k)-1);

    #Now, remove objects that got vote only from a single rater
    r_ik<-r_ik[rowSums(r_ik)>1,];

    #Calculate p_a on the cleared set
    rowSums(r_ik)->r_i;
    p_a<-mean(rowSums(r_ik*(r_ik-1))/(r_i*(r_i-1)));

    #Calculate gamma_1
    (p_a-p_e)/(1-p_e)
}

gwet (table.mx)
