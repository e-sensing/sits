samples <- "~/Downloads/Amostras_Valida_Bayes_0711.gpkg"
samples.sf <- sf::st_read(samples)
samples.sf

# Create factor vectors for caret
unique_ref <- unique(samples.sf$label)
pred_fac <- factor(pred, levels = unique_ref)

ref_fac <- factor(samples.sf$label, levels = unique_ref)
no_smooth_fac <- factor(samples.sf[["no_smooth"]], levels = unique_ref)
bayes_fac <- factor(samples.sf$bayes, levels = unique_ref)
gauss_fac <-  factor(samples.sf$gauss, levels = unique_ref)
# Call caret package to the classification statistics

acc_bayes <- caret::confusionMatrix(bayes_fac, ref_fac)
acc_gauss <- caret::confusionMatrix(gauss_fac, ref_fac)
acc_no_smooth <- caret::confusionMatrix(no_smooth_fac, ref_fac)


