# Package index

## All functions

- [`clusterRepLasso()`](clusterRepLasso.md) : Select features via the
  cluster representative lasso (Bühlmann et. al. 2013)
- [`corFunction()`](corFunction.md) : Absolute value of sample
  correlation between two vectors
- [`createSubsamples()`](createSubsamples.md) : Creates lists of
  subsamples for stability selection.
- [`css()`](css.md) : Cluster Stability Selection
- [`cssLasso()`](cssLasso.md) : Provided fitfun implementing the lasso
- [`cssLoop()`](cssLoop.md) : Helper function run on each subsample
- [`cssPredict()`](cssPredict.md) : Wrapper function to generate
  predictions from cluster stability selected model in one step
- [`cssSelect()`](cssSelect.md) : Obtain a selected set of clusters and
  features using cluster stability selection
- [`formCssDesign()`](formCssDesign.md) : Create design matrix of
  cluster representatives from matrix of raw features using results of
  css function
- [`formatClusters()`](formatClusters.md) : Formats clusters in
  standardized way, optionally estimating cluster prototypes
- [`genClusteredData()`](genClusteredData.md) : Generate randomly
  sampled data including noisy observations of latent variables
- [`genClusteredDataWeighted()`](genClusteredDataWeighted.md) : Generate
  randomly sampled data including noisy observations of latent
  variables, where proxies differ in their relevance (noise level)
- [`genClusteredDataWeightedRandom()`](genClusteredDataWeightedRandom.md)
  : Generate randomly sampled data including noisy observations of
  latent variables, where proxies differ in their relevance (noise
  level)
- [`genZmuY()`](genZmuY.md) : Generates Z, weak signal features in X,
  noise features in X, mu, and y from provided parameters
- [`getAllClustWeights()`](getAllClustWeights.md) : Calculate weights
  for each cluster member of all of the selected clusters.
- [`getClustWeights()`](getClustWeights.md) : Calculate weights for
  members of a cluster using selection proportions
- [`getClusterSelMatrix()`](getClusterSelMatrix.md) : Get cluster
  selection matrix
- [`getClusterSelsFromGlmnet()`](getClusterSelsFromGlmnet.md) : Extracts
  selected clusters and cluster prototypes from the glmnet lasso output
- [`getCssDesign()`](getCssDesign.md) : Obtain a design matrix of
  cluster representatives
- [`getCssPreds()`](getCssPreds.md) : Fit model and generate predictions
  from new data
- [`getCssSelections()`](getCssSelections.md) : Obtain a selected set of
  clusters and features
- [`getLassoLambda()`](getLassoLambda.md) : Get lambda value for lasso
- [`getModelSize()`](getModelSize.md) : Automated estimation of model
  size
- [`getNoiseVar()`](getNoiseVar.md) : Get variance of noise to add to Z
  in order to yield proxies X with desired correlations with Z
- [`getPrototypes()`](getPrototypes.md) : Estimate prototypes from a
  list of clusters
- [`getSelMatrix()`](getSelMatrix.md) : Generates matrix of selection
  indicators from stability selection.
- [`getSelectedClusters()`](getSelectedClusters.md) : From css output,
  obtain names of selected clusters and selection proportions, indices
  of all selected features, and weights of individual cluster members
- [`getSelectedSets()`](getSelectedSets.md) : Converts a selected set
  from X_glmnet to selected sets and selected clusters from the original
  feature space of X.
- [`getSelectionPrototypes()`](getSelectionPrototypes.md) : Identify
  selection prototypes from selected clusters
- [`getSubsamps()`](getSubsamps.md) : Generate list of subsamples
- [`getXglmnet()`](getXglmnet.md) : Converts the provided design matrix
  to an appropriate format for either the protolasso or the cluster
  representative lasso.
- [`identifyPrototype()`](identifyPrototype.md) : Estimate prototypes
  from a single cluster
- [`print(`*`<cssr>`*`)`](print.cssr.md) : Print cluster stability
  selection output
- [`printCssDf()`](printCssDf.md) : Prepares a data.frame summarazing
  cluster stability selection output to print
- [`processClusterLassoInputs()`](processClusterLassoInputs.md) : Check
  the inputs to protolasso and clusterRepLasso, format clusters, and
  identify prototypes for each cluster
- [`protolasso()`](protolasso.md) : Select features via the protolasso
  (Reid and Tibshirani 2016)
