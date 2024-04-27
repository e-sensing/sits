#' srr_stats
#'
#' All of the following standards initially have `@srrstatsTODO` tags.
#' These may be moved at any time to any other locations in your code.
#' Once addressed, please modify the tag from `@srrstatsTODO` to `@srrstats`,
#' or `@srrstatsNA`, ensuring that references to every one of the following
#' standards remain somewhere within your code.
#' (These comments may be deleted at any time.)
#'
#' @srrstatsVerbose TRUE
#'
#' @srrstats {G1.0} *Statistical Software should list at least one primary reference from published academic literature.*
#' @srrstats {G1.1} *Statistical Software should document whether the algorithm(s) it implements are:* - *The first implementation of a novel algorithm*; or - *The first implementation within **R** of an algorithm which has previously been implemented in other languages or contexts*; or - *An improvement on other implementations of similar algorithms in **R***.
#' @srrstats {G1.2} *Statistical Software should include a* Life Cycle Statement *describing current and anticipated future states of development.*
#' @srrstats {G1.3} *All statistical terminology should be clarified and unambiguously defined.*
#' @srrstats {G1.4} *Software should use [`roxygen2`](https://roxygen2.r-lib.org/) to document all functions.*
#' @srrstats {G1.4a} *All internal (non-exported) functions should also be documented in standard [`roxygen2`](https://roxygen2.r-lib.org/) format, along with a final `@noRd` tag to suppress automatic generation of `.Rd` files.*
#' @srrstats {G1.5} *Software includes all code necessary to reproduce results which form the basis of performance claims made in associated publications.*
#' @srrstatsNA {G1.6} *Software should include code necessary to compare performance claims with alternative implementations in other R packages.*
#' @srrstats {G2.0} *The package implements assertions on all input parameters.*
#' @srrstatsTODO {G2.0a} *Provide explicit secondary documentation of any expectations on lengths of inputs*
#' @srrstats {G2.1} *Implement assertions on types of inputs (see the initial point on nomenclature above)*
#' @srrstatsTODO {G2.1a} *Provide explicit secondary documentation of expectations on data types of all vector inputs.*
#' @srrstats {G2.2} *Appropriately prohibit or restrict submission of multivariate input to parameters expected to be univariate.*
#' @srrstats {G2.3} *For univariate character input:*
#' @srrstats {G2.3a} *Use `match.arg()` or equivalent where applicable to only permit expected values.*
#' @srrstats {G2.3b} *Either: use `tolower()` or equivalent to ensure input of character parameters is not case dependent; or explicitly document that parameters are strictly case-sensitive.*
#' @srrstats {G2.4} *Provide appropriate mechanisms to convert between different data types, potentially including:*
#' @srrstats {G2.4a} *explicit conversion to `integer` via `as.integer()`*
#' @srrstats {G2.4b} *explicit conversion to continuous via `as.numeric()`*
#' @srrstats {G2.4c} *explicit conversion to character via `as.character()` (and not `paste` or `paste0`)*
#' @srrstatsNA {G2.4d} *explicit conversion to factor via `as.factor()`*
#' @srrstatsNA {G2.4e} *explicit conversion from factor via `as...()` functions*
#' @srrstatsNA {G2.5} *Where inputs are expected to be of `factor` type, secondary documentation should explicitly state whether these should be `ordered` or not, and those inputs should provide appropriate error or other routines to ensure inputs follow these expectations.*
#' @srrstatsNA {G2.6} *Software which accepts one-dimensional input should ensure values are appropriately pre-processed regardless of class structures.*
#' @srrstats {G2.7} *Software should accept as input as many of the above standard tabular forms as possible, including extension to domain-specific forms.*
#' @srrstats {G2.8} *Software should provide appropriate conversion or dispatch routines as part of initial pre-processing to ensure that all other sub-functions of a package receive inputs of a single defined class or type.*
#' @srrstatsNA {G2.9} *Software should issue diagnostic messages for type conversion in which information is lost (such as conversion of variables from factor to character; standardisation of variable names; or removal of meta-data such as those associated with [`sf`-format](https://r-spatial.github.io/sf/) data) or added (such as insertion of variable or column names where none were provided).*
#' @srrstats {G2.10} *Software should ensure that extraction or filtering of single columns from tabular inputs should not presume any particular default behaviour, and should ensure all column-extraction operations behave consistently regardless of the class of tabular data used as input.*
#' @srrstats {G2.11} *Software should ensure that `data.frame`-like tabular objects which have columns which do not themselves have standard class attributes (typically, `vector`) are appropriately processed, and do not error without reason. This behaviour should be tested. Again, columns created by the [`units` package](https://github.com/r-quantities/units/) provide a good test case.*
#' @srrstats {G2.12} *Software should ensure that `data.frame`-like tabular objects which have list columns should ensure that those columns are appropriately pre-processed either through being removed, converted to equivalent vector columns where appropriate, or some other appropriate treatment such as an informative error. This behaviour should be tested.*
#' @srrstats {G2.13} *Statistical Software should implement appropriate checks for missing data as part of initial pre-processing prior to passing data to analytic algorithms.*
#' @srrstats {G2.14} *Where possible, all functions should provide options for users to specify how to handle missing (`NA`) data, with options minimally including:*
#' @srrstats {G2.14a} *error on missing data*
#' @srrstats {G2.14b} *ignore missing data with default warnings or messages issued*
#' @srrstats {G2.14c} *replace missing data with appropriately imputed values*
#' @srrstats {G2.15} *Functions should never assume non-missingness, and should never pass data with potential missing values to any base routines with default `na.rm = FALSE`-type parameters (such as [`mean()`](https://stat.ethz.ch/R-manual/R-devel/library/base/html/mean.html), [`sd()`](https://stat.ethz.ch/R-manual/R-devel/library/stats/html/sd.html) or [`cor()`](https://stat.ethz.ch/R-manual/R-devel/library/stats/html/cor.html)).*
#' @srrstats {G2.16} *All functions should also provide options to handle undefined values (e.g., `NaN`, `Inf` and `-Inf`), including potentially ignoring or removing such values.*
#' @srrstats {G3.0} *Statistical software should never compare floating point numbers for equality. All numeric equality comparisons should either ensure that they are made between integers, or use appropriate tolerances for approximate equality.*
#' @srrstatsNA {G3.1} *Statistical software which relies on covariance calculations should enable users to choose between different algorithms for calculating covariances, and should not rely solely on covariances from the `stats::cov` function.*
#' @srrstatsNA {G3.1a} *The ability to use arbitrarily specified covariance methods should be documented (typically in examples or vignettes).*
#' @srrstats {G4.0} *Statistical Software which enables outputs to be written to local files should parse parameters specifying file names to ensure appropriate file suffices are automatically generated where not provided.*
#' @srrstatsNA {G5.0} *Where applicable or practicable, tests should use standard data sets with known properties (for example, the [NIST Standard Reference Datasets](https://www.itl.nist.gov/div898/strd/), or data sets provided by other widely-used R packages).*
#' @srrstats {G5.1} *Data sets created within, and used to test, a package should be exported (or otherwise made generally available) so that users can confirm tests and run examples.*
#' @srrstats {G5.2} *Appropriate error and warning behaviour of all functions should be explicitly demonstrated through tests. In particular,*
#' @srrstats {G5.2a} *Every message produced within R code by `stop()`, `warning()`, `message()`, or equivalent should be unique*
#' @srrstats {G5.2b} *Explicit tests should demonstrate conditions which trigger every one of those messages, and should compare the result with expected values.*
#' @srrstats {G5.3} *For functions which are expected to return objects containing no missing (`NA`) or undefined (`NaN`, `Inf`) values, the absence of any such values in return objects should be explicitly tested.*
#' @srrstatsNA {G5.4} **Correctness tests** *to test that statistical algorithms produce expected results to some fixed test data sets (potentially through comparisons using binding frameworks such as [RStata](https://github.com/lbraglia/RStata)).*
#' @srrstatsNA {G5.4a} *For new methods, it can be difficult to separate out correctness of the method from the correctness of the implementation, as there may not be reference for comparison. In this case, testing may be implemented against simple, trivial cases or against multiple implementations such as an initial R implementation compared with results from a C/C++ implementation.*
#' @srrstatsNA {G5.4b} *For new implementations of existing methods, correctness tests should include tests against previous implementations. Such testing may explicitly call those implementations in testing, preferably from fixed-versions of other software, or use stored outputs from those where that is not possible.*
#' @srrstatsNA {G5.4c} *Where applicable, stored values may be drawn from published paper outputs when applicable and where code from original implementations is not available*
#' @srrstats {G5.5} *Correctness tests should be run with a fixed random seed*
#' @srrstatsNA {G5.6} **Parameter recovery tests** *to test that the implementation produce expected results given data with known properties. For instance, a linear regression algorithm should return expected coefficient values for a simulated data set generated from a linear model.*
#' @srrstatsNA {G5.6a} *Parameter recovery tests should generally be expected to succeed within a defined tolerance rather than recovering exact values.*
#' @srrstatsNA {G5.6b} *Parameter recovery tests should be run with multiple random seeds when either data simulation or the algorithm contains a random component. (When long-running, such tests may be part of an extended, rather than regular, test suite; see G5.10-4.12, below).*
#' @srrstatsNA {G5.7} **Algorithm performance tests** *to test that implementation performs as expected as properties of data change. For instance, a test may show that parameters approach correct estimates within tolerance as data size increases, or that convergence times decrease for higher convergence thresholds.*
#' @srrstats {G5.8} **Edge condition tests** *to test that these conditions produce expected behaviour such as clear warnings or errors when confronted with data with extreme properties including but not limited to:*
#' @srrstats {G5.8a} *Zero-length data*
#' @srrstats {G5.8b} *Data of unsupported types (e.g., character or complex numbers in for functions designed only for numeric data)*
#' @srrstats {G5.8c} *Data with all-`NA` fields or columns or all identical fields or columns*
#' @srrstats {G5.8d} *Data outside the scope of the algorithm (for example, data with more fields (columns) than observations (rows) for some regression algorithms)*
#' @srrstatsNA {G5.9} **Noise susceptibility tests** *Packages should test for expected stochastic behaviour, such as through the following conditions:*
#' @srrstatsNA {G5.9a} *Adding trivial noise (for example, at the scale of `.Machine$double.eps`) to data does not meaningfully change results*
#' @srrstatsNA {G5.9b} *Running under different random seeds or initial conditions does not meaningfully change results*
#' @srrstatsNA {G5.10} *Extended tests should included and run under a common framework with other tests but be switched on by flags such as as a `<MYPKG>_EXTENDED_TESTS="true"` environment variable.* - The extended tests can be then run automatically by GitHub Actions for example by adding the following to the `env` section of the workflow:
#' @srrstatsNA {G5.11} *Where extended tests require large data sets or other assets, these should be provided for downloading and fetched as part of the testing workflow.*
#' @srrstatsNA {G5.11a} *When any downloads of additional data necessary for extended tests fail, the tests themselves should not fail, rather be skipped and implicitly succeed with an appropriate diagnostic message.*
#' @srrstatsNA {G5.12} *Any conditions necessary to run extended tests such as platform requirements, memory, expected runtime, and artefacts produced that may need manual inspection, should be described in developer documentation such as a `CONTRIBUTING.md` or `tests/README.md` file.*
#' @srrstats {ML1.0} *Documentation should make a clear conceptual distinction between training and test data (even where such may ultimately be confounded as described above.)*
#' @srrstats {ML1.0a} *Where these terms are ultimately eschewed, these should nevertheless be used in initial documentation, along with clear explanation of, and justification for, alternative terminology.*
#' @srrstats {ML1.1} *Absent clear justification for alternative design decisions, input data should be expected to be labelled "test", "training", and, where applicable, "validation" data.*
#' @srrstatsNA {ML1.1a} *The presence and use of these labels should be explicitly confirmed via pre-processing steps (and tested in accordance with **ML7.0**, below).*
#' @srrstatsNA {ML1.1b} *Matches to expected labels should be case-insensitive and based on partial matching such that, for example, "Test", "test", or "testing" should all suffice.*
#' @srrstatsNA {ML1.2} *Training and test data sets for ML software should be able to be input as a single, generally tabular, data object, with the training and test data distinguished either by* - *A specified variable containing, for example, `TRUE`/`FALSE` or `0`/`1` values, or which uses some other system such as missing (`NA`) values to denote test data); and/or* - *An additional parameter designating case or row numbers, or labels of test data.*
#' @srrstats {ML1.3} *Input data should be clearly partitioned between training and test data (for example, through having each passed as a distinct `list` item), or should enable an additional means of categorically distinguishing training from test data (such as via an additional parameter which provides explicit labels). Where applicable, distinction of validation and any other data should also accord with this standard.*
#' @srrstatsNA {ML1.4} *Training and test data sets, along with other necessary components such as validation data sets, should be stored in their own distinctly labelled sub-directories (for distinct files), or according to an explicit and distinct labelling scheme (for example, for database connections). Labelling should in all cases adhere to **ML1.1**, above.*
#' @srrstats {ML1.5} *ML software should implement a single function which summarises the contents of test and training (and other) data sets, minimally including counts of numbers of cases, records, or files, and potentially extending to tables or summaries of file or data types, sizes, and other information (such as unique hashes for each component).*
#' @srrstats {ML1.6} *ML software which does not admit missing values, and which expects no missing values, should implement explicit pre-processing routines to identify whether data has any missing values, and should generally error appropriately and informatively when passed data with missing values. In addition, ML software which does not admit missing values should:*
#' @srrstatsNA {ML1.6a} *Explain why missing values are not admitted.*
#' @srrstatsTODO {ML1.6b} *Provide explicit examples (in function documentation, vignettes, or both) for how missing values may be imputed, rather than simply discarded.*
#' @srrstatsNA {ML1.7} *ML software which admits missing values should clearly document how such values are processed.*
#' @srrstatsTODO {ML1.7a} *Where missing values are imputed, software should offer multiple user-defined ways to impute missing data.*
#' @srrstatsTODO {ML1.7b} *Where missing values are imputed, the precise imputation steps should also be explicitly documented, either in tests (see **ML7.2** below), function documentation, or vignettes.*
#' @srrstats {ML1.8} *ML software should enable equal treatment of missing values for both training and test data, with optional user ability to control application to either one or both.*
#' @srrstats {ML2.0} *A dedicated function should enable pre-processing steps to be defined and parametrized.*
#' @srrstats {ML2.0a} *That function should return an object which can be directly submitted to a specified model (see section 3, below).*
#' @srrstats {ML2.0b} *Absent explicit justification otherwise, that return object should have a defined class minimally intended to implement a default `print` method which summarizes the input data set (as per **ML1.5** above) and associated transformations (see the following standard).*
#' @srrstatsNA {ML2.1} *ML software which uses broadcasting to reconcile dimensionally incommensurate input data should offer an ability to at least optionally record transformations applied to each input file.*
#' @srrstatsTODO {ML2.2} *ML software which requires or relies upon numeric transformations of input data (such as change in mean values or variances) should allow optimal explicit specification of target values, rather than restricting transformations to default generic values only (such as transformations to z-scores).*
#' @srrstatsTODO {ML2.2a} *Where the parameters have default values, reasons for those particular defaults should be explicitly described.*
#' @srrstatsTODO {ML2.2b} *Any extended documentation (such as vignettes) which demonstrates the use of explicit values for numeric transformations should explicitly describe why particular values are used.*
#' @srrstatsNA {ML2.3} *The values associated with all transformations should be recorded in the object returned by the function described in the preceding standard (**ML2.0**).*
#' @srrstatsNA {ML2.4} *Default values of all transformations should be explicitly documented, both in documentation of parameters where appropriate (such as for numeric transformations), and in extended documentation such as vignettes.*
#' @srrstatsNA {ML2.5} *ML software should provide options to bypass or otherwise switch off all default transformations.*
#' @srrstatsNA {ML2.6} *Where transformations are implemented via distinct functions, these should be exported to a package's namespace so they can be applied in other contexts.*
#' @srrstatsNA {ML2.7} *Where possible, documentation should be provided for how transformations may be reversed. For example, documentation may demonstrate how the values retained via **ML2.3**, above, can be used along with transformations either exported via **ML2.6** or otherwise exemplified in demonstration code to independently transform data, and then to reverse those transformations.*
#' @srrstats {ML3.0} *Model specification should be implemented as a distinct stage subsequent to specification of pre-processing routines (see Section 2, above) and prior to actual model fitting or training (see Section 4, below). In particular,*
#' @srrstatsNA {ML3.0a} *A dedicated function should enable models to be specified without actually fitting or training them, or if this (**ML3**) and the following (**ML4**) stages are controlled by a single function, that function should have a parameter enabling models to be specified yet not fitted (for example, `nofit = FALSE`).*
#' @srrstatsNA {ML3.0b} *That function should accept as input the objects produced by the previous Input Data Specification stage, and defined according to **ML2.0**, above.*
#' @srrstatsNA {ML3.0c} *The function described above (**ML3.0a**) should return an object which can be directly trained as described in the following sub-section (**ML4**).*
#' @srrstatsNA {ML3.0d} *That return object should have a defined class minimally intended to implement a default `print` method which summarises the model specification, including values of all relevant parameters.*
#' @srrstatsNA {ML3.1} *ML software should allow the use of both untrained models, specified through model parameters only, as well as pre-trained models. Use of the latter commonly entails an ability to submit a previously-trained model object to the function defined according to **ML3.0a**, above.*
#' @srrstats {ML3.2} *ML software should enable different models to be applied to the object specifying data inputs and   (see sub-sections 1--2, above) without needing to re-define those preceding steps.*
#' @srrstatsNA {ML3.3} *Where ML software implements its own distinct classes of model objects, the properties and behaviours of those specific classes of objects should be explicitly compared with objects produced by other ML software. In particular, where possible, ML software should provide extended documentation (as vignettes or equivalent) comparing model objects with those from other ML software, noting both unique abilities and restrictions of any implemented classes.*
#' @srrstats {ML3.4} *Where training rates are used, ML software should provide explicit documentation both in all functions which use training rates, and in extended form such as vignettes, of the importance of, and/or sensitivity to, different values of training rates. In particular,*
#' @srrstats {ML3.4a} *Unless explicitly justified otherwise, ML software should offer abilities to automatically determine appropriate or optimal training rates, either as distinct pre-processing stages, or as implicit stages of model training.*
#' @srrstats {ML3.4b} *ML software which provides default values for training rates should clearly document anticipated restrictions of validity of those default values; for example through clear suggestions that user-determined and -specified values may generally be necessary or preferable.*
#' @srrstats {ML3.5} *Parameters controlling optimization algorithms should minimally include:*
#' @srrstats {ML3.5a} *Specification of the type of algorithm used to explore the search space (commonly, for example, some kind of gradient descent algorithm)*
#' @srrstats {ML3.5b} *The kind of loss function used to assess distance between model estimates and desired output.*
#' @srrstats {ML3.6} *Unless explicitly justified otherwise (for example because ML software under consideration is an implementation of one specific algorithm), ML software should:*
#' @srrstats {ML3.6a} *Implement or otherwise permit usage of multiple ways of exploring search space*
#' @srrstatsNA {ML3.6b} *Implement or otherwise permit usage of multiple loss functions.*
#' @srrstatsNA {ML3.7} *For ML software in which algorithms are coded in C++, user-controlled use of either CPUs or GPUs (on NVIDIA processors at least) should be implemented through direct use of [`libcudacxx`](https://github.com/NVIDIA/libcudacxx).*
#' @srrstats {ML4.0} *ML software should generally implement a unified single-function interface to model training, able to receive as input a model specified according to all preceding standards. In particular, models with categorically different specifications, such as different model architectures or optimization algorithms, should be able to be submitted to the same model training function.*
#' @srrstatsNA {ML4.1} *ML software should at least optionally retain explicit information on paths taken as an optimizer advances towards minimal loss. Such information should minimally include:*
#' @srrstatsNA {ML4.1a} *Specification of all model-internal parameters, or equivalent hashed representation.*
#' @srrstatsNA {ML4.1b} *The value of the loss function at each point*
#' @srrstatsNA {ML4.1c} *Information used to advance to next point, for example quantification of local gradient.*
#' @srrstatsNA {ML4.2} *The subsequent extraction of information retained according to the preceding standard should be explicitly documented, including through example code.*
#' @srrstats {ML4.3} *All parameters controlling batch processing and associated terminology should be explicitly documented, and it should not, for example, be presumed that users will understand the definition of "epoch" as implemented in any particular ML software.*
#' @srrstats {ML4.4} *Explicit guidance should be provided on selection of appropriate values for parameter controlling batch processing, for example, on trade-offs between batch sizes and numbers of epochs (with both terms provided as Control Parameters in accordance with the preceding standard, **ML3**).*
#' @srrstatsNA {ML4.5} *ML software may optionally include a function to estimate likely time to train a specified model, through estimating initial timings from a small sample of the full batch.*
#' @srrstats {ML4.6} *ML software should by default provide explicit information on the progress of batch jobs (even where those jobs may be implemented in parallel on GPUs). That information may be optionally suppressed through additional parameters.*
#' @srrstatsNA {ML4.7} *ML software should provide an ability to combine results from multiple re-sampling iterations using a single parameter specifying numbers of iterations.*
#' @srrstatsNA {ML4.8} *Absent any additional specification, re-sampling algorithms should by default partition data according to proportions of original test and training data.*
#' @srrstatsNA {ML4.8a} *Re-sampling routines of ML software should nevertheless offer an ability to explicitly control or override such default proportions of test and training data.*
#' @srrstats {ML5.0} *The result of applying the training processes described above should be contained within a single model object returned by the function defined according to **ML4.0**, above. Even where the output reflects application to a test data set, the resultant object need not include any information on model performance (see **ML5.3**--**ML5.4**, below).*
#' @srrstats {ML5.0a} *That object should either have its own class, or extend some previously-defined class.*
#' @srrstats {ML5.0b} *That class should have a defined `print` method which summarises important aspects of the model object, including but not limited to summaries of input data and algorithmic control parameters.*
#' @srrstatsNA {ML5.1} *As for the untrained model objects produced according to the above standards, and in particular as a direct extension of **ML3.3**, the properties and behaviours of trained models produced by ML software should be explicitly compared with equivalent objects produced by other ML software. (Such comparison will generally be done in terms of comparing model performance, as described in the following standard **ML5.3**--**ML5.4**).*
#' @srrstats {ML5.2} *The structure and functionality of objects representing trained ML models should be thoroughly documented. In particular,*
#' @srrstats {ML5.2a} *Either all functionality extending from the class of model object should be explicitly documented, or a method for listing or otherwise accessing all associated functionality explicitly documented and demonstrated in example code.*
#' @srrstats {ML5.2b} *Documentation should include examples of how to save and re-load trained model objects for their re-use in accordance with **ML3.1**, above.*
#' @srrstats {ML5.2c} *Where general functions for saving or serializing objects, such as [`saveRDS`](https://stat.ethz.ch/R-manual/R-devel/library/base/html/readRDS.html) are not appropriate for storing local copies of trained models, an explicit function should be provided for that purpose, and should be demonstrated with example code.*
#' @srrstats {ML5.3} *Assessment of model performance should be implemented as one or more functions distinct from model training.*
#' @srrstats {ML5.4} *Model performance should be able to be assessed according to a variety of metrics.*
#' @srrstatsTODO {ML5.4a} *All model performance metrics represented by functions internal to a package must be clearly and distinctly documented.*
#' @srrstatsNA {ML5.4b} *It should be possible to submit custom metrics to a model assessment function, and the ability to do so should be clearly documented including through example code.*
#' @srrstats {ML6.0} *Descriptions of ML software should make explicit reference to a workflow which separates training and testing stages, and which clearly indicates a need for distinct training and test data sets.*
#' @srrstatsNA {ML6.1} *ML software intentionally designed to address only a restricted subset of the workflow described here should clearly document how it can be embedded within a typical full ML workflow in the sense considered here.*
#' @srrstatsNA {ML6.1a} *Such demonstrations should include and contrast embedding within a full workflow using at least two other packages to implement that workflow.*
#' @srrstats {ML7.0} *Test should explicitly confirm partial and case-insensitive matching of "test", "train", and, where applicable, "validation" data.*
#' @srrstatsNA {ML7.1} *Tests should demonstrate effects of different numeric scaling of input data (see **ML2.2**).*
#' @srrstatsTODO {ML7.2} *For software which imputes missing data, tests should compare internal imputation with explicit code which directly implements imputation steps (even where such imputation is a single-step implemented via some external package). These tests serve as an explicit reference for how imputation is performed.*
#' @srrstatsNA {ML7.3} *Where model objects are implemented as distinct classes, tests should explicitly compare the functionality of these classes with functionality of equivalent classes for ML model objects from other packages.*
#' @srrstatsNA {ML7.3a} *These tests should explicitly identify restrictions on the functionality of model objects in comparison with those of other packages.*
#' @srrstatsNA {ML7.3b} *These tests should explicitly identify functional advantages and unique abilities of the model objects in comparison with those of other packages.*
#' @srrstatsNA {ML7.4} *ML software should explicit document the effects of different training rates, and in particular should demonstrate divergence from optima with inappropriate training rates.*
#' @srrstats {ML7.5} *ML software which implements routines to determine optimal training rates (see **ML3.4**, above) should implement tests to confirm the optimality of resultant values.*
#' @srrstatsNA {ML7.6} *ML software which implement independent training "epochs" should demonstrate in tests the effects of lesser versus greater numbers of epochs.*
#' @srrstatsTODO {ML7.7} *ML software should explicitly test different optimization algorithms, even where software is intended to implement one specific algorithm.*
#' @srrstatsNA {ML7.8} *ML software should explicitly test different loss functions, even where software is intended to implement one specific measure of loss.*
#' @srrstatsNA {ML7.9} *Tests should explicitly compare all possible combinations in categorical differences in model architecture, such as different model architectures with same optimization algorithms, same model architectures with different optimization algorithms, and differences in both.*
#' @srrstatsNA {ML7.9a} *Such combinations will generally be formed from multiple categorical factors, for which explicit use of functions such as [`expand.grid()`](https://stat.ethz.ch/R-manual/R-devel/library/base/html/expand.grid.html) is recommended.*
#' @srrstatsNA {ML7.10} *The successful extraction of information on paths taken by optimizers (see **ML5.1**, above), should be tested, including testing the general properties, but not necessarily actual values of, such data.*
#' @srrstats {ML7.11} *All performance metrics available for a given class of trained model should be thoroughly tested and compared.*
#' @srrstatsNA {ML7.11a} *Tests which compare metrics should do so over a range of inputs (generally implying differently trained models) to demonstrate relative advantages and disadvantages of different metrics.*
#' @srrstats {SP1.0} *Spatial software should explicitly indicate its domain of applicability, and in particular distinguish whether the software may be applied in Cartesian/rectilinear/geometric domains, curvilinear/geographic domains, or both.*
#' @srrstats {SP1.1} *Spatial software should explicitly indicate its dimensional domain of applicability, in particular through identifying whether it is applicable to two or three dimensions only, or whether there are any other restrictions on dimensionality.*
#' @srrstats {SP2.0} *Spatial software should only accept input data of one or more classes explicitly developed to represent such data.*
#' @srrstats {SP2.0a} *Where new classes are implemented, conversion to other   common classes for spatial data in R should be documented.*
#' @srrstats {SP2.0b} *Class systems should ensure that functions error appropriately, rather than merely warning, in response to data from inappropriate spatial domains.*
#' @srrstats {SP2.1} *Spatial Software should not use the [`sp` package](https://cran.r-project.org/package=sp), rather should use [`sf`](https://cran.r-project.org/package=sf).*
#' @srrstats {SP2.2} *Geographical Spatial Software should ensure maximal compatibility with established packages and workflows, minimally through:*
#' @srrstats {SP2.2a} *Clear and extensive documentation demonstrating how routines from that software may be embedded within, or otherwise adapted to, workflows which rely on these established packages; and*
#' @srrstats {SP2.2b} *Tests which clearly demonstrate that routines from that software may be successfully translated into forms and workflows which rely on these established packages.*
#' @srrstats {SP2.3} *Software which accepts spatial input data in any standard format established in other R packages (such as any of the formats able to be read by [`GDAL`](https://gdal.org), and therefore by the [`sf` package](https://cran.r-project.org/package=sf)) should include example and test code which load those data in spatial formats, rather than R-specific binary formats such as `.Rds`.*
#' @srrstats {SP2.4} *Geographical Spatial Software should be compliant with version 6 or larger of* [`PROJ`](https://proj.org/), *and with* `WKT2` *representations. The primary implication, described in detail in the articles linked to above, is that:*
#' @srrstats {SP2.4a} *Software should not permit coordinate reference systems to be represented merely by so-called "PROJ4-strings", but should use at least WKT2.*
#' @srrstats {SP2.5} *Class systems for input data must contain meta data on associated coordinate reference systems.*
#' @srrstats {SP2.5a} *Software which implements new classes to input spatial data (or the spatial components of more general data) should provide an ability to convert such input objects into alternative spatial classes such as those listed above.*
#' @srrstats {SP2.6} *Spatial Software should explicitly document the types and classes of input data able to be passed to each function.*
#' @srrstats {SP2.7} *Spatial Software should implement validation routines to confirm that inputs are of acceptable classes (or represented in otherwise appropriate ways for software which does not use class systems).*
#' @srrstats {SP2.8} *Spatial Software should implement a single pre-processing routine to validate input data, and to appropriately transform it to a single uniform type to be passed to all subsequent data-processing functions.*
#' @srrstats {SP2.9} *The pre-processing function described above should maintain those metadata attributes of input data which are relevant or important to core algorithms or return values.*
#' @srrstatsNA {SP3.0} *Spatial software which considers spatial neighbours should enable user control over neighbourhood forms and sizes. In particular:*
#' @srrstatsNA {SP3.0a} *Neighbours (able to be expressed) on regular grids should be able to be considered in both rectangular only, or rectangular and diagonal (respectively "rook" and "queen" by analogy to chess).*
#' @srrstatsNA {SP3.0b} *Neighbourhoods in irregular spaces should be minimally able to be controlled via an integer number of neighbours, an area (or equivalent distance defining an area) in which to include neighbours, or otherwise equivalent user-controlled value.*
#' @srrstatsNA {SP3.1} *Spatial software which considers spatial neighbours should wherever possible enable neighbour contributions to be weighted by distance (or other continuous weighting variable), and not rely exclusively on a uniform-weight rectangular cut-off.*
#' @srrstats {SP3.2} *Spatial software which relies on sampling from input data (even if only of spatial coordinates) should enable sampling procedures to be based on local spatial densities of those input data.*
#' @srrstatsNA {SP3.3} *Spatial regression software should explicitly quantify and distinguish autocovariant or autoregressive processes from those covariant or regressive processes not directly related to spatial structure alone.*
#' @srrstatsNA {SP3.4} *Where possible, spatial clustering software should avoid using standard non-spatial clustering algorithms in which spatial proximity is merely represented by an additional weighting factor in favour of explicitly spatial algorithms.*
#' @srrstats {SP3.5} *Spatial machine learning software should ensure that broadcasting procedures for reconciling inputs of different dimensions are **not** applied*.
#' @srrstatsNA {SP3.6} *Spatial machine learning software should document (and, where possible, test) the potential effects of different sampling procedures*
#' @srrstats {SP4.0} *Return values should either:*
#' @srrstats {SP4.0a} *Be in same class as input data, or*
#' @srrstats {SP4.0b} *Be in a unique, preferably class-defined, format.*
#' @srrstats {SP4.1} *Any aspects of input data which are included in output data (either directly, or in some transformed form) and which contain units should ensure those same units are maintained in return values.*
#' @srrstats {SP4.2} *The type and class of all return values should be explicitly documented.*
#' @srrstats {SP5.0} *Implement default `plot` methods for any implemented class system.*
#' @srrstats {SP5.1} *Implement appropriate placement of variables along x- and y-axes.*
#' @srrstats {SP5.2} *Ensure that axis labels include appropriate units.*
#' @srrstats {SP5.3} *Offer an ability to generate interactive (generally `html`-based) visualisations of results.*
#' @srrstats {SP6.0} *Software which implements routines for transforming coordinates of input data should include tests which demonstrate ability to recover the original coordinates.*
#' @srrstatsNA {SP6.1} *All functions which can be applied to both Cartesian and curvilinear data should be tested through application to both.*
#' @srrstatsNA {SP6.1a} *Functions which may yield inaccurate results when applied to data in one or the other forms (such as the preceding examples of centroids and buffers from ellipsoidal data) should test that results from inappropriate application of those functions are indeed less accurate.*
#' @srrstatsNA {SP6.1b} *Functions which yield accurate results regardless of whether input data are rectilinear or curvilinear should demonstrate equivalent accuracy in both cases, and should also demonstrate how equivalent results may be obtained through first explicitly transforming input data.*
#' @srrstatsNA {SP6.2} *Geographical Software should include tests with extreme geographical coordinates, minimally including extension to polar extremes of +/-90 degrees.*
#' @srrstatsNA {SP6.3} *Spatial Software which considers spatial neighbours should explicitly test all possible ways of defining them, and should explicitly compare quantitative effects of different ways of defining neighbours.*
#' @srrstatsNA {SP6.4} *Spatial Software which considers spatial neighbours should explicitly test effects of different schemes to weight neighbours by spatial proximity.*
#' @srrstatsNA {SP6.5} *Spatial Unsupervised Learning Software which uses clustering algorithms should implement tests which explicitly compare results with equivalent results obtained with a non-spatial clustering algorithm.*
#' @srrstats {SP6.6} *Spatial Machine Learning Software should implement tests which explicitly demonstrate the detrimental consequences of sampling test and training data from the same spatial region, rather than from spatially distinct regions.*
#' @srrstats {TS1.0} *Time Series Software should use and rely on explicit class systems developed for representing time series data, and should not permit generic, non-time-series input*
#' @srrstats {TS1.1} *Time Series Software should explicitly document the types and classes of input data able to be passed to each function.*
#' @srrstats {TS1.2} *Time Series Software should implement validation routines to confirm that inputs are of acceptable classes (or represented in otherwise appropriate ways for software which does not use class systems).*
#' @srrstats {TS1.3} *Time Series Software should implement a single pre-processing routine to validate input data, and to appropriately transform it to a single uniform type to be passed to all subsequent data-processing functions (the [`tsbox` package](https://www.tsbox.help/) provides one convenient approach for this).*
#' @srrstats {TS1.4} *The pre-processing function described above should maintain all time- or date-based components or attributes of input data.*
#' @srrstats {TS1.5} *The software should ensure strict ordering of the time, frequency, or equivalent ordering index variable.*
#' @srrstats {TS1.6} *Any violations of ordering should be caught in the pre-processing stages of all functions.*
#' @srrstatsNA {TS1.7} *Accept inputs defined via the [`units` package](https://github.com/r-quantities/units/) for attributing SI units to R vectors.*
#' @srrstats {TS1.8} *Where time intervals or periods may be days or months, be explicit about the system used to represent such, particularly regarding whether a calendar system is used, or whether a year is presumed to have 365 days, 365.2422 days, or some other value.*
#' @srrstatsNA {TS2.0} *Time Series Software which presumes or requires regular data should only allow **explicit** missing values, and should issue appropriate diagnostic messages, potentially including errors, in response to any **implicit** missing values.*
#' @srrstatsTODO {TS2.1} *Where possible, all functions should provide options for users to specify how to handle missing data, with options minimally including:*
#' @srrstats {TS2.1a} *error on missing data; or.
#' @srrstats {TS2.1b} *warn or ignore missing data, and proceed to analyse irregular data, ensuring that results from function calls with regular yet missing data return identical values to submitting equivalent irregular data with no missing values; or*
#' @srrstats {TS2.1c} *replace missing data with appropriately imputed values.*
#' @srrstatsNA {TS2.2} *Consider stationarity of all relevant moments - typically first (mean) and second (variance) order, or otherwise document why such consideration may be restricted to lower orders only.*
#' @srrstatsNA {TS2.3} *Explicitly document all assumptions and/or requirements of stationarity*
#' @srrstatsNA {TS2.4} *Implement appropriate checks for all relevant forms of stationarity, and either:*
#' @srrstatsNA {TS2.4a} *issue diagnostic messages or warnings; or*
#' @srrstatsNA {TS2.4b} *enable or advise on appropriate transformations to ensure stationarity.*
#' @srrstatsNA {TS2.5} *Incorporate a system to ensure that both row and column orders follow the same ordering as the underlying time series data. This may, for example, be done by including the `index` attribute of the time series data as an attribute of the auto-covariance matrix.*
#' @srrstatsNA {TS2.6} *Where applicable, auto-covariance matrices should also include specification of appropriate units.*
#' @srrstatsNA {TS3.0} *Provide tests to demonstrate at least one case in which errors widen appropriately with forecast horizon.*
#' @srrstatsNA {TS3.1} *If possible, provide at least one test which violates TS3.0*
#' @srrstatsNA {TS3.2} *Document the general drivers of forecast errors or horizons, as demonstrated via the particular cases of TS3.0 and TS3.1*
#' @srrstatsNA {TS3.3} *Either:*
#' @srrstatsNA {TS3.3a} *Document, preferable via an example, how to trim forecast values based on a specified error margin or equivalent; or*
#' @srrstatsNA {TS3.3b} *Provide an explicit mechanism to trim forecast values to a specified error margin, either via an explicit post-processing function, or via an input parameter to a primary analytic function.*
#' @srrstats{TS4.0} *Return values should either:*
#' @srrstats {TS4.0a} *Be in same class as input data, for example by using the [`tsbox` package](https://www.tsbox.help/) to re-convert from standard internal format (see 1.4, above); or*
#' @srrstats {TS4.0b} *Be in a unique, preferably class-defined, format.*
#' @srrstats {TS4.1} *Any units included as attributes of input data should also be included within return values.*
#' @srrstats {TS4.2} *The type and class of all return values should be explicitly documented.*
#' @srrstats {TS4.3} *Return values should explicitly include all appropriate units and/or time scales*
#' @srrstatsNA {TS4.4} *Document the effect of any such transformations on forecast data, including potential effects on both first- and second-order estimates.*
#' @srrstatsNA {TS4.5} *In decreasing order of preference, either:*
#' @srrstatsNA {TS4.5a} *Provide explicit routines or options to back-transform data commensurate with original, non-stationary input data*
#' @srrstatsNA {TS4.5b} *Demonstrate how data may be back-transformed to a form commensurate with original, non-stationary input data.*
#' @srrstatsNA {TS4.5c} *Document associated limitations on forecast values*
#' @srrstatsNA {TS4.6} *Time Series Software which implements or otherwise enables forecasting should return either:*
#' @srrstatsNA {TS4.6a} *A distribution object, for example via one of the many packages described in the CRAN Task View on [Probability Distributions](https://cran.r-project.org/web/views/Distributions.html) (or the new [`distributional` package](https://pkg.mitchelloharawild.com/distributional/) as used in the [`fable` package](https://fable.tidyverts.org) for time-series forecasting).*
#' @srrstatsNA {TS4.6b} *For each variable to be forecast, predicted values equivalent to first- and second-order moments (for example, mean and standard error values).*
#' @srrstatsNA {TS4.6c} *Some more general indication of error associated with forecast estimates.*
#' @srrstatsNA {TS4.7} *Ensure that forecast (modelled) values are clearly distinguished from observed (model or input) values, either (in this case in no order of preference) by*
#' @srrstatsNA {TS4.7a} *Returning forecast values alone*
#' @srrstatsNA {TS4.7b} *Returning distinct list items for model and forecast values*
#' @srrstatsNA {TS4.7c} *Combining model and forecast values into a single return object with an appropriate additional column clearly distinguishing the two kinds of data.*
#' @srrstats {TS5.0} *Implement default `plot` methods for any implemented class system.*
#' @srrstats {TS5.1} *When representing results in temporal domain(s), ensure that one axis is clearly labelled "time" (or equivalent), with continuous units.*
#' @srrstats {TS5.2} *Default to placing the "time" (or equivalent) variable on the horizontal axis.*
#' @srrstats {TS5.3} *Ensure that units of the time, frequency, or index variable are printed by default on the axis.*
#' @srrstatsNA {TS5.4} *For frequency visualization, abscissa spanning $[-\pi, \pi]$ should be avoided in favour of positive units of $[0, 2\pi]$ or $[0, 0.5]$, in all cases with appropriate additional explanation of units.*
#' @srrstats {TS5.5} *Provide options to determine whether plots of data with missing values should generate continuous or broken lines.*
#' @srrstatsNA {TS5.6} *By default indicate distributional limits of forecast on plot*
#' @srrstatsNA {TS5.7} *By default include model (input) values in plot, as well as forecast (output) values*
#' @srrstatsNA {TS5.8} *By default provide clear visual distinction between model (input) values and forecast (output) values.*
#' @noRd
NULL

#' NA_standards
#'
#' Any non-applicable standards can have their tags changed from `@srrstatsTODO`
#' to `@srrstatsNA`, and placed together in this block, along with explanations
#' for why each of these standards have been deemed not applicable.
#' (These comments may also be deleted at any time.)
#' @noRd
NULL
