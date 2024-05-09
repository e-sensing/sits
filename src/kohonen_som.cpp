

#include <Rcpp.h>
#include <Rmath.h>

#ifdef _OPENMP
#include <omp.h>
#endif

#include "./sits_types.h"

#define EPS 1e-8 /* relative test of equality of distances */

#define RANDIN  GetRNGstate()
#define RANDOUT PutRNGstate()
#define UNIF unif_rand()

using namespace Rcpp;

/**
 * Gaussian neighborhood function.
 *
 * @description
 * This function computes the gaussian neighborhood function.
 *
 * @param distance          A `double` with the distance between two objects.
 * @param radius            A `double` with the radius of the neighbourhood.
 *
 * @note
 * The implementation of this function was adapted from the `kohonen` R Package.
 * The code is open-source, under the GPL license, and is available on
 * GitHub (https://github.com/rwehrens/kohonen)
 *
 * @return The Gaussian neighborhood value.
 */
double Gaussian(double distance, double radius) {
    return exp(-(distance * distance) / (2 * radius * radius));
}

/**
 * Find best match unit (BMU).
 *
 * @description
 * Finds the best matching codebook unit for the given data object and stores
 * its index and distance in the specified nearest unit index and nearest unit
 * distance references.
 *
 * @param object            A `double*` to the data object.
 * @param codes             A `double*` to the codes object.
 * @param offsets           A `int*` to the offset object.
 * @param numNAs            A `int*` to the number of NA object.
 * @param numCodes          A `int` with the number of codes.
 * @param numLayers         A `int` with the number of layers.
 * @param numVars           A `int*` to the number of variables to be used.
 * @param totalVars         A `int` with the total of variables in the data.
 * @param distanceFunction  A `DistanceFunctionPtr&` with the reference to the
 *                          distance function.
 * @param weights           A `double*` to the weights object.
 * @param index             A `int&` referencing the nearest BMU.
 * @param distance          A `double&` referencing the distance between the
 *                          current object and the nearest BMU.
 * @note
 * The implementation of this function was adapted from the `kohonen` R Package.
 * The code is open-source, under the GPL license, and is available on
 * GitHub (https://github.com/rwehrens/kohonen)
 */
void FindBestMatchingUnit(
    double *object,
    double *codes,
    int *offsets,
    int *numNAs,
    int numCodes,
    int numLayers,
    int *numVars,
    int totalVars,
    const DistanceFunctionPtr &distanceFunction,
    double *weights,
    int &index,
    double &distance)
{
    int nind = 1;
    double dist;

    index = NA_INTEGER;
    distance = DBL_MAX;
    for (int cd = 0; cd < numCodes; ++cd) {

        /* Calculate current unit distance */
        dist = 0.0;
        for (int l = 0; l < numLayers; ++l) {
            dist += weights[l] * (*distanceFunction)(
                &object[offsets[l]],
                &codes[cd * totalVars + offsets[l]],
                numVars[l],
                numNAs[l]
            );
        }

        /* Update best matching unit */
        if (dist <= distance * (1 + EPS)) {
            if (dist < distance * (1 - EPS)) {
                nind = 1;
                index = cd;
            } else {
                if (++nind * UNIF < 1.0) {
                    index = cd;
                }
            }
            distance = dist;
        }
    }

    if (distance == DBL_MAX) {
        distance = NA_REAL;
        index = NA_INTEGER;
    }
}

/**
 * Create SOM Map.
 *
 * @description
 * This function maps data into a map grid, using codes a `Supersom`
 * function.
 *
 * @param data              A `NumericMatrix` with the data to be mapped.
 * @param numVars           A `IntegerVector` with the number of variables
 *                          represented in the `data` variable.
 * @param numNAs            A `IntegerMatrix` Number of points NAs in the
 *                          matrix cells.
 * @param codes             A `NumericMatrix` with the codes from a `Supersom`
 *                          function.
 * @param weights           A `NumericVector` with the map `weights`.
 * @param distanceFunction  A `XPtr<DistanceFunctionPtr>` poiting to
 *                          a distance function.
 * @note
 * The implementation of this function was adapted from the `kohonen` R Package.
 * The code is open-source, under the GPL license, and is available on
 * GitHub (https://github.com/rwehrens/kohonen)
 *
 * @return list with `winners` and `unit distances`.
 */
// [[Rcpp::export]]
List RcppMap(
    NumericMatrix data,
    IntegerVector numVars,
    IntegerMatrix numNAs,
    NumericMatrix codes,
    NumericVector weights,
    XPtr<DistanceFunctionPtr> distanceFunction)
{
    int
        numObjects = data.ncol(),  /* number of objects */
        numLayers = numVars.size(),/* number of layers */
        numCodes = codes.ncol(),   /* number of units in the map */
        totalVars = data.nrow(),   /* total number of variables sum(numVars) */
        i, l, nearest;

    double distance;

    IntegerVector offsets(numLayers);
    IntegerVector winners(numObjects);
    NumericVector unitDistances(numObjects);

    double
        *pCodes = REAL(codes),
        *pWeights = REAL(weights);

    int
        *pNumVars = INTEGER(numVars),
        *pOffsets = INTEGER(offsets);

    /* Get the distance function pointers. */
    DistanceFunctionPtr distanceFunctionPtr = *(distanceFunction);

    /* Compute the layer data offsets and the total object length. */
    totalVars = 0;
    for (l = 0; l < numLayers; l++) {
        offsets[l] = totalVars;
        totalVars += numVars[l];
    }

    /* Loop over all data objects */
    for (i = 0; i < numObjects; i++) {

        /* Find best matching unit index and distance */
        FindBestMatchingUnit(
            &data[i * totalVars],
            pCodes,
            pOffsets,
            &numNAs[i * numLayers],
            numCodes,
            numLayers,
            pNumVars,
            totalVars,
            distanceFunctionPtr,
            pWeights,
            nearest,
            distance
        );

        winners[i] = nearest;
        unitDistances[i] = distance;
    }

    return List::create(
        Named("winners") = winners,
        Named("unitdistances") = unitDistances
    );
}

/**
 * Create a Supersom Map using the online learning algorithm.
 *
 * @description
 * This function creates a SOM using the Supersom extension and the `online`
 * learning algorithm.
 *
 * @param data                   A `NumericMatrix` with the data to be mapped.
 * @param codes                  A `NumericMatrix` with the initial codes data.
 * @param numVars                A `IntegerVector` with the number of variables
 *                               represented in the `data` variable.
 * @param weights                A `NumericVector` with the initial `weights`.
 * @param distanceFunction       A `XPtr<DistanceFunctionPtr>` poiting to
 *                               a distance function.
 * @param numNAs                 A `IntegerMatrix` Number of points NAs in the
 *                               matrix cells.
 * @param neighbourhoodDistances A `NumericMatrix` with the initial
 *                               neighbourhood distances.
 * @param alphas                 A `NumericVector` with the learning rates of
 *                               of each som layer.
 * @param radii                  A `NumericVector` with the radius of the
 *                               neighbourhood.
 * @note
 * The implementation of this function was adapted from the `kohonen` R Package.
 * The code is open-source, under the GPL license, and is available on
 * GitHub (https://github.com/rwehrens/kohonen).
 *
 * @return list with `codes` and `changes`.
 */
// [[Rcpp::export]]
List RcppSupersom(
    NumericMatrix data,
    NumericMatrix codes,
    IntegerVector numVars,
    NumericVector weights,
    XPtr<DistanceFunctionPtr> distanceFunction,
    IntegerMatrix numNAs,
    NumericMatrix neighbourhoodDistances,
    NumericVector alphas,
    NumericVector radii,
    int numEpochs)
{
  int
    numObjects = data.ncol(),     /* number of objects */
    numLayers = numVars.size(),   /* number of layers */
    numCodes = codes.ncol(),      /* number of units in the map */
    totalVars = data.nrow(),      /* total number of variables sum(numVars) */
    cd,                           /* counter over units */
    i,                            /* randomly drawn object */
    j,                            /* counter over variables */
    k,                            /* counter over iterations */
    l,                            /* counter over layers */
    m,                            /* counter over epochs */
    nearest, totalIters, curIter = 0;

  double
      distance,
      tmp,
      threshold,
      alpha;

  IntegerVector offsets(numLayers);
  NumericMatrix changes(numLayers, numEpochs);

  double
    *pCodes = REAL(codes),
    *pWeights = REAL(weights),
    *pChanges = REAL(changes),
    *pData = REAL(data),
    *pNeighbourhoodDistances = REAL(neighbourhoodDistances),
    *pObject;
  int
    *pOffsets = INTEGER(offsets),
    *pNumVars = INTEGER(numVars),
    *pNumNAs = INTEGER(numNAs);

  /* Get the distance function pointers. */
  DistanceFunctionPtr distanceFunctionPtr = *(distanceFunction);

  /* Create the neighborhood influence function pointer */
  NeighbourhoodFunctionPtr neighbourhoodFunctionPtr = &Gaussian;

  /* Compute the layer data offsets and the total object length. */
  totalVars = 0;
  for (l = 0; l < numLayers; l++) {
    offsets[l] = totalVars;
    totalVars += numVars[l];
  }

  totalIters = numEpochs * numObjects;

  RANDIN;

  /* Outer loop: number of iterations */
  for (m = 0; m < numEpochs; m++) {

    /* Inner loop: loop over (bootstrapped) objects */
    for (k = 0; k < numObjects; k++) {

      /* Select random object */
      i = (int)(numObjects * UNIF);

      /* Find best matching unit index and distance */
      pObject = &pData[i * totalVars];

      /* Find best matching unit index and distance */
      FindBestMatchingUnit(
        pObject,
        pCodes,
        pOffsets,
        &pNumNAs[i * numLayers],
        numCodes,
        numLayers,
        pNumVars,
        totalVars,
        distanceFunctionPtr,
        pWeights,
        nearest,
        distance
      );

      if (nearest < 0) {
        ::Rf_error("No nearest neighbour found.");
      }

      /* Linear decays for radius and learning parameter */
      tmp = (double)(curIter) / (double)(totalIters);
      threshold = radii[0] - (radii[0] - radii[1]) * tmp;
      if (threshold < 1.0) {
        threshold = 0.5;
      }
      alpha = alphas[0] - (alphas[0] - alphas[1]) * tmp;

      /* Update changes */
      for (l = 0; l < numLayers; l++) {
        distance = 0.0;
        for (j = pOffsets[l]; j < pOffsets[l] + pNumVars[l]; j++) {
          if (!std::isnan(pObject[j])) {
            tmp = pObject[j] - pCodes[nearest * totalVars + j];
            distance += tmp * tmp;
          }
        }
        if (pNumNAs[i * numLayers + l] > 0) {
          distance = distance * pNumVars[l] / (
              pNumVars[l] - pNumNAs[i * numLayers + l]
          );
        }
        pChanges[m * numLayers + l] += distance;
      }

      /* Update all maps */
      for (cd = 0; cd < numCodes; cd++) {
        tmp = neighbourhoodFunctionPtr(
            pNeighbourhoodDistances[numCodes * nearest + cd],
                                   threshold
        );
        if (tmp > 0) {
          for (j = 0; j < totalVars; j++) {
            if (!std::isnan(pObject[j])) {
              pCodes[cd * totalVars + j] += tmp * alpha * (
                  pObject[j] - pCodes[cd * totalVars + j]
              );
            }
          }
        }
      }

      ++curIter;
    }

    /* Mean of the nearest layer distances of this iteration */
    for (l = 0; l < numLayers; l++) {
      pChanges[m * numLayers + l] =
        sqrt(pChanges[m * numLayers + l] / pNumVars[l]) / numObjects;
    }
  }

  RANDOUT;

  return List::create(
    Named("codes") = codes,
    Named("changes") = changes
  );
}

/**
 * Create a Supersom Map using the batch learning algorithm.
 *
 * @description
 * This function creates a SOM using the Supersom extension and the `batch`
 * learning algorithm.
 *
 * @param data                   A `NumericMatrix` with the data to be mapped.
 * @param codes                  A `NumericMatrix` with the initial codes data.
 * @param numVars                A `IntegerVector` with the number of variables
 *                               represented in the `data` variable.
 * @param weights                A `NumericVector` with the initial `weights`.
 * @param distanceFunction       A `XPtr<DistanceFunctionPtr>` poiting to
 *                               a distance function.
 * @param numNAs                 A `IntegerMatrix` Number of points NAs in the
 *                               matrix cells.
 * @param neighbourhoodDistances A `NumericMatrix` with the initial
 *                               neighbourhood distances.
 * @param alphas                 A `NumericVector` with the learning rates of
 *                               of each som layer.
 * @param radii                  A `NumericVector` with the radius of the
 *                               neighbourhood.
 * @param numEpochs              A `int` with the number of batch epochs.
 *
 * @note
 * The implementation of this function was adapted from the `kohonen` R Package.
 * The code is open-source, under the GPL license, and is available on
 * GitHub (https://github.com/rwehrens/kohonen).
 *
 * @return list with `codes` and `changes`.
 */
// [[Rcpp::export]]
List RcppBatchSupersom(
  NumericMatrix data,
  NumericMatrix codes,
  IntegerVector numVars,
  NumericVector weights,
  XPtr<DistanceFunctionPtr> distanceFunction,
  IntegerMatrix numNAs,
  NumericMatrix neighbourhoodDistances,
  NumericVector radii,
  int numEpochs
  )
{
  int numObjects = data.ncol(),   /* number of objects */
    numLayers = numVars.size(),   /* number of layers */
    numCodes = codes.ncol(),      /* number of units in the map */
    totalVars = data.nrow(),      /* total number of variables sum(numVars) */
    cd,                           /* counter over units */
    i,                            /* randomly drawn object */
    j,                            /* counter over variables */
    l,                            /* counter over layers */
    m,                            /* counter over epochs */
    nearest;
  double dist, tmp, radius;

  IntegerVector offsets(numLayers);
  NumericMatrix changes(numLayers, numEpochs);
  NumericMatrix codeSums(totalVars, numCodes);
  NumericVector codeWeights(numCodes);

  double
    *pCodes = REAL(codes),
    *pWeights = REAL(weights),
    *pCodeSums = REAL(codeSums),
    *pCodeWeights = REAL(codeWeights),
    *pChanges = REAL(changes),
    *pData = REAL(data),
    *pNeighbourhoodDistances = REAL(neighbourhoodDistances),
    *pObject;
  int
    *pOffsets = INTEGER(offsets),
    *pNumVars = INTEGER(numVars),
    *pNumNAs = INTEGER(numNAs);

  /* Get the distance function pointers. */
  DistanceFunctionPtr distanceFunctionPtr = *(distanceFunction);

  /* Create the neighborhood influence function pointer */
  NeighbourhoodFunctionPtr neighbourhoodFunctionPtr = &Gaussian;

  /* Compute the layer data offsets and the total object length. */
  totalVars = 0;
  for (l = 0; l < numLayers; l++) {
    offsets[l] = totalVars;
    totalVars += numVars[l];
  }

  RANDIN;

  /* Outer loop: number of epochs */
  for (m = 0; m < numEpochs; m++) {

    /* Linear decays for radius */
    radius = radii[0] - (radii[0] - radii[1]) * ((double)m / (double)numEpochs);
    if (radius < EPS) {
      radius = EPS;
    }

    /* Initialize new codes vectors and weights */
    std::fill(codeWeights.begin(), codeWeights.end(), 0.0);
    std::fill(codeSums.begin(), codeSums.end(), 0.0);

    /* Loop: number of objects */
    for (i = 0; i < numObjects; i++) {

      /* Find best matching unit index and distance */
      pObject = &pData[i * totalVars];

      /* Find best matching unit index and distance */
      FindBestMatchingUnit(
        pObject,
        pCodes,
        pOffsets,
        &pNumNAs[i * numLayers],
        numCodes,
        numLayers,
        pNumVars,
        totalVars,
        distanceFunctionPtr,
        pWeights,
        nearest,
        dist
      );

      if (nearest < 0) {
        ::Rf_error("No nearest neighbour found.");
      }

      /* Update changes */
      for (l = 0; l < numLayers; l++) {
        dist = 0.0;
        for (j = 0; j < numVars[l]; j++) {
          if (!std::isnan(data[i * totalVars + offsets[l] + j])) {
            tmp = data[i * totalVars + offsets[l] + j] -
              codes[nearest * totalVars + offsets[l] + j];
            dist += tmp * tmp;
          }
        }
        if (numNAs[i * numLayers + l] > 0) {
          dist = dist * numVars[l] / (numVars[l] - numNAs[i * numLayers + l]);
        }
        pChanges[m * numLayers + l] += dist;
      }

      /* Accumulate sums and weights */
      for (cd = 0; cd < numCodes; cd++) {
        tmp = neighbourhoodFunctionPtr(
          pNeighbourhoodDistances[numCodes * nearest + cd], radius);
        if (tmp > 0) {
          for (j = 0; j < totalVars; j++) {
            if (!std::isnan(data[i * totalVars + j])) {
              pCodeSums[cd * totalVars + j] += tmp * data[i * totalVars + j];
            }
          }
          pCodeWeights[cd] += tmp;
        }
      }
    }

    /* Update all maps */
    for (cd = 0; cd < numCodes; cd++) {
      if (pCodeWeights[cd] > 0) {
        for (j = 0; j < totalVars; j++) {
          codes[cd * totalVars + j] =
            pCodeSums[cd * totalVars + j] / pCodeWeights[cd];
        }
      }
    }

    /* Mean of the nearest layer distances of this iteration */
    for (l = 0; l < numLayers; l++) {
      pChanges[m * numLayers + l] =
        sqrt(pChanges[m * numLayers + l] / numVars[l]) / numObjects;
    }
  }

  RANDOUT;

  return List::create(
    Named("codes") = codes,
    Named("changes") = changes
  );
}

/**
 * Create a Supersom Map using the batch learning algorithm with a
 * parallel implementation.
 *
 * @description
 * This function creates a SOM using the Supersom extension and the `batch`
 * learning algorithm with a parallel implementation.
 *
 * @param data                   A `NumericMatrix` with the data to be mapped.
 * @param codes                  A `NumericMatrix` with the initial codes data.
 * @param numVars                A `IntegerVector` with the number of variables
 *                               represented in the `data` variable.
 * @param weights                A `NumericVector` with the initial `weights`.
 * @param distanceFunction       A `XPtr<DistanceFunctionPtr>` poiting to
 *                               a distance function.
 * @param numNAs                 A `IntegerMatrix` Number of points NAs in the
 *                               matrix cells.
 * @param neighbourhoodDistances A `NumericMatrix` with the initial
 *                               neighbourhood distances.
 * @param alphas                 A `NumericVector` with the learning rates of
 *                               of each som layer.
 * @param radii                  A `NumericVector` with the radius of the
 *                               neighborhood.
 * @param numEpochs              A `int` with the number of batch epochs.
 * @param numCores               A `int` with the number of cores to be used in
 *                               the data processing. The `-1` means all cores
 *                               available.
 * @note
 * The implementation of this function was adapted from the `kohonen` R Package.
 * The code is open-source, under the GPL license, and is available on
 * GitHub (https://github.com/rwehrens/kohonen).
 *
 * @note
 * To use this function, the `OpenMP` must be available.
 *
 * @return list with `codes` and `changes`.
 */
#ifdef _OPENMP
// [[Rcpp::plugins(openmp)]]
#endif
// [[Rcpp::export]]
List RcppParallelBatchSupersom(
  NumericMatrix data,
  NumericMatrix codes,
  IntegerVector numVars,
  NumericVector weights,
  XPtr<DistanceFunctionPtr> distanceFunction,
  IntegerMatrix numNAs,
  NumericMatrix neighbourhoodDistances,
  NumericVector radii,
  int numEpochs,
  int numCores
  )
{
  #ifdef _OPENMP

  int numObjects = data.ncol(),   /* number of objects */
    numLayers = numVars.size(),   /* number of layers */
    numCodes = codes.ncol(),      /* number of units in the map */
    totalVars = data.nrow(),      /* total number of variables sum(numVars) */
    cd,                           /* counter over units */
    i,                            /* randomly drawn object */
    j,                            /* counter over variables */
    l,                            /* counter over layers */
    m,                            /* counter over epochs */
    nearest;
  double dist, tmp, radius;

  IntegerVector offsets(numLayers);
  NumericMatrix changes(numLayers, numEpochs);
  NumericMatrix codeSums(totalVars, numCodes);
  NumericVector codeWeights(numCodes);

  double
    *pCodes = REAL(codes),
    *pWeights = REAL(weights),
    *pCodeSums = REAL(codeSums),
    *pCodeWeights = REAL(codeWeights),
    *pChanges = REAL(changes),
    *pData = REAL(data),
    *pNeighbourhoodDistances = REAL(neighbourhoodDistances),
    *pObject;
  int
    *pOffsets = INTEGER(offsets),
    *pNumVars = INTEGER(numVars),
    *pNumNAs = INTEGER(numNAs);

  /* Get the distance function pointers. */
  DistanceFunctionPtr distanceFunctionPtr = *(distanceFunction);

  /* Create the neighborhood influence function pointer */
  NeighbourhoodFunctionPtr neighbourhoodFunctionPtr = &Gaussian;

  /* Compute the layer data offsets and the total object length. */
  totalVars = 0;
  for (l = 0; l < numLayers; l++) {
    offsets[l] = totalVars;
    totalVars += numVars[l];
  }

  /* Set number of cores for parallel code blocks */
  int availablecores = omp_get_num_procs();
  if (numCores <= 0 || numCores > availablecores) {
     numCores = availablecores;
  }
  omp_set_num_threads(numCores);

  RANDIN;

  #pragma omp parallel private(cd, l, j, nearest, dist, tmp)
  {
    #pragma omp single
    {
      m = 0;
    }

    std::vector<double> threadCodeWeights(numCodes);
    std::vector<double> threadCodeSums(numCodes * totalVars);
    std::vector<double> threadChanges(numLayers);

    /* Outer loop: number of epochs */
    while (m < numEpochs) {

      #pragma omp single
      {
        /* Linear decays for radius */
        radius =
          radii[0] - (radii[0] - radii[1]) * ((double)m / (double)numEpochs);
        if (radius < EPS) {
          radius = EPS;
        }

        /* Initialize new codes vectors and weights */
        std::fill(codeWeights.begin(), codeWeights.end(), 0.0);
        std::fill(codeSums.begin(), codeSums.end(), 0.0);
      }

      /* Initialize new codes vectors and weights */
      std::fill(threadCodeWeights.begin(), threadCodeWeights.end(), 0.0);
      std::fill(threadCodeSums.begin(), threadCodeSums.end(), 0.0);
      std::fill(threadChanges.begin(), threadChanges.end(), 0.0);

      /* Loop: number of objects */
      #pragma omp for
      for (i = 0; i < numObjects; i++) {

        pObject = &pData[i * totalVars];

        /* Find best matching unit index and distance */
        FindBestMatchingUnit(
          pObject,
          pCodes,
          pOffsets,
          &pNumNAs[i * numLayers],
          numCodes,
          numLayers,
          pNumVars,
          totalVars,
          distanceFunctionPtr,
          pWeights,
          nearest,
          dist
        );

        if (nearest < 0) {
          ::Rf_error("No nearest neighbour found...");
        }

        /* Update changes */
        for (l = 0; l < numLayers; l++) {
          dist = 0.0;
          for (j = 0; j < numVars[l]; j++) {
            if (!std::isnan(data[i * totalVars + offsets[l] + j])) {
              tmp = data[i * totalVars + offsets[l] + j] -
                codes[nearest * totalVars + offsets[l] + j];
              dist += tmp * tmp;
            }
          }
          if (numNAs[i * numLayers + l] > 0) {
            dist = dist * numVars[l] / (numVars[l] - numNAs[i * numLayers + l]);
          }
          threadChanges[l] += dist;
        }

        /* Accumulate sums and weights */
        for (cd = 0; cd < numCodes; cd++) {
          tmp = neighbourhoodFunctionPtr(
            pNeighbourhoodDistances[numCodes * nearest + cd], radius);
          if (tmp > 0) {
            threadCodeWeights[cd] += tmp;
            for (j = 0; j < totalVars; j++) {
              if (!std::isnan(data[i * totalVars + j])) {
                threadCodeSums[cd * totalVars + j] +=
                  tmp * data[i * totalVars + j];
              }
            }
          }
        }
      }

      #pragma omp critical
      {
        for (cd = 0; cd < numCodes; cd++) {
          if (threadCodeWeights[cd] > 0) {
            codeWeights[cd] += threadCodeWeights[cd];
            for (j = 0; j < totalVars; j++) {
              pCodeSums[cd * totalVars + j] +=
                threadCodeSums[cd * totalVars + j];
            }
          }
        }
        for (l = 0; l < numLayers; l++) {
          pChanges[m * numLayers + l] += threadChanges[l];
        }
      }

      #pragma omp barrier
      #pragma omp single
      {
        /* Update all maps */
        for (cd = 0; cd < numCodes; cd++) {
          if (codeWeights[cd] > 0) {
            for (j = 0; j < totalVars; j++) {
              pCodes[cd * totalVars + j] =
                pCodeSums[cd * totalVars + j] / pCodeWeights[cd];
            }
          }
        }

        /* Mean of the nearest layer distances of this iteration */
        for (l = 0; l < numLayers; l++) {
          pChanges[m * numLayers + l] =
            sqrt(pChanges[m * numLayers + l] / numVars[l]) / numObjects;
        }

        m++;
      }
    }
  }

  RANDOUT;

  return List::create(
    Named("codes") = codes,
    Named("changes") = changes
  );

  #else
    ::Rf_warning("OpenMP not available: cannot run in parallel mode");
    return RcppBatchSupersom(data, codes, numVars, weights, distanceFunctions,
			     numNAs, neighbourhoodDistances, neighbourhoodFct,
			     radii, numEpochs);
  #endif
}
