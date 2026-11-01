#' @title Return machine learning model inside a closure
#' @keywords internal
#' @noRd
#' @author Rolf Simoes, \email{rolfsimoes@@gmail.com}
#'
#' @param  ml_model  Closure that contains ML model and its environment
#' @return           ML model as specified by the original ML function
.ml_model <- function(ml_model) {
    env <- environment(ml_model)

    if (.ml_is_torch_model(ml_model)) {
        return(.torch_model_restore(ml_model))
    }
    if ("model" %in% ls(env, all.names = TRUE)) {
        return(env[["model"]])
    }


    stop(.conf("messages", ".ml_model"))
}
#' @title Return statistics of ML model inside a closure (new version)
#' @keywords internal
#' @noRd
#' @author Rolf Simoes, \email{rolfsimoes@@gmail.com}
#'
#' @param  ml_model  Closure that contains ML model and its environment
#' @return           Data statistics contained in the model closure
.ml_stats <- function(ml_model) {
    # New stats variable
    environment(ml_model)[["ml_stats"]]
}
#' @title Symbols used by code
#' @keywords internal
#' @noRd
#' @author Rolf Simoes, \email{rolfsimoes@@gmail.com}
#' @description Walks calls, pairlists, lists and the formals and body of
#' functions, and returns the names of the symbols found. Constants are
#' ignored. The right side of \code{$} and \code{@@} and the package of
#' \code{::} are names, not variables, and are not returned.
#' @param  x  A function, a call or a list of them
#' @return    Character vector with symbol names
.ml_symbols <- function(x) {
    if (is.symbol(x)) {
        return(setdiff(as.character(x), ""))
    }
    if (is.function(x)) {
        if (is.primitive(x)) {
            return(character(0L))
        }
        return(union(.ml_symbols(formals(x)), .ml_symbols(body(x))))
    }
    if (is.call(x)) {
        head <- x[[1L]]
        if (identical(head, as.name("::")) || identical(head, as.name(":::"))) {
            return(character(0L))
        }
        if (identical(head, as.name("$")) || identical(head, as.name("@"))) {
            return(.ml_symbols(x[[2L]]))
        }
    }
    if (is.call(x) || is.pairlist(x) || is.expression(x) || is.list(x)) {
        x <- as.list(x)
        symbols <- character(0L)
        for (i in seq_along(x)) {
            # an argument without default is the empty symbol
            if (!identical(x[[i]], quote(expr = ))) {
                symbols <- union(symbols, .ml_symbols(x[[i]]))
            }
        }
        return(symbols)
    }
    character(0L)
}
#' @title Frames of an environment up to the namespace
#' @keywords internal
#' @noRd
#' @author Rolf Simoes, \email{rolfsimoes@@gmail.com}
#' @param  env   Environment of a closure
#' @return       List with \code{env} and its parents below the namespace
.ml_frames <- function(env) {
    namespace <- topenv(env)
    frames <- list()
    while (!identical(env, namespace) && !identical(env, emptyenv())) {
        frames[[length(frames) + 1L]] <- env
        env <- parent.env(env)
    }
    frames
}
#' @title Does an environment read one of the frames by inheritance?
#' @keywords internal
#' @noRd
#' @author Rolf Simoes, \email{rolfsimoes@@gmail.com}
#' @param  env     Environment
#' @param  frames  List of environments
#' @return         TRUE if \code{env} or one of its parents is in \code{frames}
.ml_reads_frames <- function(env, frames) {
    while (!.ml_is_stop_env(env)) {
        if (any(purrr::map_lgl(frames, identical, env))) {
            return(TRUE)
        }
        env <- parent.env(env)
    }
    FALSE
}
#' @title Is an environment a namespace or a top level environment?
#' @keywords internal
#' @noRd
#' @param  env  Environment
#' @return      TRUE or FALSE
.ml_is_stop_env <- function(env) {
    isNamespace(env) || identical(env, emptyenv()) ||
        identical(env, globalenv()) || identical(env, baseenv())
}
#' @title Functions and frames that an object reads
#' @keywords internal
#' @noRd
#' @author Rolf Simoes, \email{rolfsimoes@@gmail.com}
#' @description Walks lists, attributes, environments and function
#' environments of \code{x}, without entering \code{frames} or namespaces.
#' Returns the functions that read one of \code{frames} by inheritance, the
#' new frames, and the \code{inherit} expressions of R6 classes. An R6
#' object, such as a torch module, reads the frames of the namespace
#' function where its class was defined, where R6 also evaluates
#' \code{inherit}.
#' @param  x       Object
#' @param  frames  List of environments
#' @return         List with \code{functions}, \code{frames} and
#'                 \code{inherit}
.ml_frame_readers <- function(x, frames) {
    namespace <- topenv(frames[[1L]])
    is_in <- function(e, envs) any(purrr::map_lgl(envs, identical, e))
    # environments visited, by address
    seen <- new.env(hash = TRUE, parent = emptyenv())
    functions <- list()
    new_frames <- list()
    inherit <- list()
    add_frames <- function(e) {
        if (is.environment(e) && identical(topenv(e), namespace)) {
            for (frame in .ml_frames(e)) {
                if (!is_in(frame, frames) && !is_in(frame, new_frames)) {
                    new_frames[[length(new_frames) + 1L]] <<- frame
                }
            }
        }
    }
    visit <- function(x) {
        if (is.environment(x)) {
            address <- format.default(x)
            if (.ml_is_stop_env(x) || is_in(x, frames) ||
                is_in(x, new_frames) || exists(address, envir = seen)) {
                return(invisible())
            }
            assign(address, TRUE, envir = seen)
            if (inherits(x, "R6")) {
                enclos <- get0(".__enclos_env__", envir = x, inherits = FALSE)
                add_frames(if (is.environment(enclos)) parent.env(enclos))
            }
            if (inherits(x, "R6ClassGenerator")) {
                add_frames(get0("parent_env", envir = x, inherits = FALSE))
                inherit[[length(inherit) + 1L]] <<-
                    get0("inherit", envir = x, inherits = FALSE)
            }
            for (name in ls(x, all.names = TRUE)) {
                # active bindings compute values on access, and a binding
                # to a missing argument has no value
                if (!bindingIsActive(name, x)) {
                    visit(tryCatch(
                        get(name, envir = x, inherits = FALSE),
                        error = function(e) NULL
                    ))
                }
            }
            visit(parent.env(x))
        } else if (is.function(x) && !is.primitive(x)) {
            if (.ml_reads_frames(environment(x), c(frames, new_frames))) {
                functions[[length(functions) + 1L]] <<- x
            }
            visit(environment(x))
        } else if (is.list(x)) {
            purrr::walk(unclass(x), visit)
        }
        purrr::walk(attributes(x), visit)
        invisible()
    }
    visit(x)
    list(functions = functions, frames = new_frames, inherit = inherit)
}
#' @title Environment with the objects that code reads
#' @keywords internal
#' @noRd
#' @author Rolf Simoes, \email{rolfsimoes@@gmail.com}
#' @description Looks up the symbols of \code{code} in \code{env} and its
#' parents, up to the namespace, and copies the objects found into a new
#' environment whose parent is the namespace. A function copied from those
#' frames is walked too and moved to the new environment.
#' @param  code  A function, a call or a list of them
#' @param  env   Environment where \code{code} was defined
#' @return       New environment
.ml_env <- function(code, env) {
    namespace <- topenv(env)
    frames <- .ml_frames(env)
    is_frame <- function(e) any(purrr::map_lgl(frames, identical, e))
    result <- new.env(parent = namespace)
    pending <- .ml_symbols(code)
    while (.has(pending)) {
        name <- pending[[1L]]
        pending <- pending[-1L]
        frame <- purrr::detect(frames, function(e) {
            exists(name, envir = e, inherits = FALSE)
        })
        if (is.null(frame) || exists(name, envir = result, inherits = FALSE)) {
            next
        }
        # a binding to a missing argument has no value
        value <- tryCatch(
            get(name, envir = frame, inherits = FALSE),
            error = function(e) NULL
        )
        if (is.function(value) && is_frame(environment(value))) {
            pending <- union(pending, .ml_symbols(value))
            environment(value) <- result
        }
        assign(name, value, envir = result)
    }
    result
}
#' @title Keep in a trained model only what it reads
#' @keywords internal
#' @noRd
#' @author Rolf Simoes, \email{rolfsimoes@@gmail.com}
#' @description Gives a trained model or encoder, new or saved, a new
#' environment with the objects that its predict code reads and those that
#' the accessors read by name (config key \code{ml_model_names}). The
#' samples keep the first row of each label and the training predictors
#' only their column names. Torch modules read by inheritance the frame
#' where they were defined: the training frame is emptied and gets the new
#' environment as parent, and other frames of the namespace keep only what
#' the modules read.
#' @param  model  Closure of a trained model or encoder
#' @return        The model, with its new environment
.ml_model_compact <- function(model) {
    frame <- environment(model)
    # a compacted model has the namespace as parent
    if (identical(parent.env(frame), topenv(frame))) {
        return(model)
    }
    chain <- .ml_frames(frame)
    code <- c(list(model), purrr::map(.conf("ml_model_names"), as.name))
    others <- list()
    reads <- list()
    repeat {
        env <- .ml_env(c(code, reads), frame)
        # cut before the walk, which would visit every series of every sample
        .ml_cut_samples(env)
        readers <- .ml_frame_readers(
            as.list(env, all.names = TRUE), c(chain, others)
        )
        found <- purrr::keep(readers[["functions"]], function(fun) {
            .ml_reads_frames(environment(fun), chain)
        })
        if (!.has(readers[["frames"]]) && length(found) == length(reads)) {
            break
        }
        others <- c(others, readers[["frames"]])
        reads <- found
    }
    # other frames keep the objects that the functions reading them read
    purrr::walk(others, function(other) {
        code_other <- c(
            purrr::keep(readers[["functions"]], function(fun) {
                .ml_reads_frames(environment(fun), list(other))
            }),
            readers[["inherit"]]
        )
        keep <- ls(.ml_env(code_other, other), all.names = TRUE)
        rm(list = setdiff(ls(other, all.names = TRUE), keep), envir = other)
    })
    # the modules read the new environment through the emptied frame
    if (.has(reads)) {
        rm(list = ls(frame, all.names = TRUE), envir = frame)
        parent.env(frame) <- env
    }
    environment(model) <- env
    model
}
#' @title Cut the samples kept by a compacted model
#' @keywords internal
#' @noRd
#' @author Rolf Simoes, \email{rolfsimoes@@gmail.com}
#' @param  env  Environment of a compacted model, changed in place
#' @return      Called for side effects
.ml_cut_samples <- function(env) {
    # accessors read labels and bands, and timeline and dates of row 1
    samples <- env[["samples"]]
    if (.has(samples)) {
        env[["samples"]] <- samples[!duplicated(samples[["label"]]), ]
    }
    # accessors read only the feature names
    if (.has(env[["train_samples"]])) {
        env[["train_samples"]] <- env[["train_samples"]][0L, ]
    }
    invisible(env)
}
#' @title Return samples of ML model inside a closure
#' @keywords internal
#' @noRd
#' @author Rolf Simoes, \email{rolfsimoes@@gmail.com}
#' @param  ml_model  Closure that contains ML model and its environment
#' @return           Samples used for ML construction
.ml_samples <- function(ml_model) {
    environment(ml_model)[["samples"]]
}
#' @title Return class of ML model inside a closure
#' @keywords internal
#' @noRd
#' @author Rolf Simoes, \email{rolfsimoes@@gmail.com}
#' @param  ml_model  Closure that contains ML model and its environment
#' @return           ML model class
.ml_class <- function(ml_model) {
    class(ml_model)[[1L]]
}
#' @title Return names of features used to train ML model
#' @keywords internal
#' @noRd
#' @author Rolf Simoes, \email{rolfsimoes@@gmail.com}
#' @param  ml_model  Closure that contains ML model and its environment
#' @return           Features used to build the model
.ml_features_name <- function(ml_model) {
    # Get feature names from variable used in training
    names(environment(ml_model)[["train_samples"]])[-2L:0L]
}
#' @title Return names of bands used to train ML model
#' @keywords internal
#' @noRd
#' @author Rolf Simoes, \email{rolfsimoes@@gmail.com}
#' @param  ml_model  Closure that contains ML model and its environment
#' @return           Bands used to build the model
.ml_bands <- function(ml_model) {
    .samples_bands(.ml_samples(ml_model))
}
#' @title Return timeline of the samples used to train ML model
#' @keywords internal
#' @noRd
#' @author Rolf Simoes, \email{rolfsimoes@@gmail.com}
#' @param  ml_model  Closure that contains ML model and its environment
#' @return           Timeline of the first training sample
.ml_timeline <- function(ml_model) {
    .samples_timeline(.ml_samples(ml_model))
}
#' @title Return labels of samples of used to train ML model
#' @keywords internal
#' @noRd
#' @author Rolf Simoes, \email{rolfsimoes@@gmail.com}
#' @param  ml_model  Closure that contains ML model and its environment
#' @return           Sample labels used to build the model
.ml_labels <- function(ml_model) {
    .samples_labels(.ml_samples(ml_model))
}
#' @title Return codes of sample labels of used to train ML model
#' @keywords internal
#' @noRd
#' @author Rolf Simoes, \email{rolfsimoes@@gmail.com}
#' @param  ml_model  Closure that contains ML model and its environment
#' @return           Codes of sample labels used to build the model
.ml_labels_code <- function(ml_model) {
    labels <- .ml_labels(ml_model)
    names(labels) <- seq_along(labels)
    labels
}
#' @title Clean GPU memory allocation
#' @keywords internal
#' @noRd
#' @author Felipe Carvalho, \email{felipe.carvalho@@inpe.br}
#' @author Felipe Carlos, \email{efelipecarlos@@gmail.com}
#' @param  ml_model  Closure that contains ML model and its environment
#' @return           Called for side effects
.ml_gpu_clean <- function(ml_model) {
    # Clean torch allocations
    if (.torch_cuda_enabled() && .ml_is_torch_model(ml_model)) {
        torch::cuda_empty_cache()
    }
    invisible(NULL)
}

#' @title normalize the probability results
#' @keywords internal
#' @author Rolf Simoes, \email{rolfsimoes@@gmail.com}
#' @noRd
#' @param  values    Values to be normalized
#' @param  ml_model  Closure that contains ML model and its environment
#' @return           Normalized values
#'
.ml_normalize <- function(values, ml_model) {
    UseMethod(".ml_normalize", ml_model)
}
#' @export
#'
.ml_normalize.torch_model <- function(values, ml_model) {
    # Correct the default behaviour of softmax in torch models
    # Run softmax here instead of inside a torch model
    column_names <- colnames(values)
    values[is.na(values)] <- 0
    values <- softmax(values)
    colnames(values) <- column_names
    values
}
#' @export
#'
.ml_normalize.default <- function(values, ml_model) {
    values[is.na(values)] <- 0
    values
}
#' @title Update multicores for models that do internal multiprocessing
#' @keywords internal
#' @noRd
#' @author Felipe Carvalho, \email{felipe.carvalho@@inpe.br}
#' @author Felipe Carlos, \email{efelipecarlos@@gmail.com}
#' @param  ml_model   Closure that contains ML model and its environment
#' @param  multicores Current multicores setting
#' @return            Updated multicores
#'
.ml_update_multicores <- function(ml_model, multicores) {
    # xgboost model has internal multiprocessing
    if ("xgb_model" %in% .ml_class(ml_model)) {
        multicores <- 1L
    }
    # torch in GPU has internal multiprocessing
    if (.torch_gpu_classification() && .ml_is_torch_model(ml_model)) {
        multicores <- 1L
    }

    multicores
}
#' @title Is the ML model a torch model?
#' @keywords internal
#' @noRd
#' @author Felipe Carvalho, \email{felipe.carvalho@@inpe.br}
#' @author Felipe Carlos, \email{efelipecarlos@@gmail.com}
#' @param  ml_model   Closure that contains ML model and its environment
#' @return            TRUE/FALSE
.ml_is_torch_model <- function(ml_model) {
    inherits(ml_model, "torch_model")
}

#' @title Is the ML torch model temporal?
#' @keywords internal
#' @noRd
#' @author Felipe Carvalho, \email{felipe.carvalho@@inpe.br}
#' @author Felipe Carlos, \email{efelipecarlos@@gmail.com}
#' @param  ml_model   Closure that contains ML model and its environment
#' @return            TRUE/FALSE
.ml_torch_is_temporal <- function(ml_model) {
    .ml_is_torch_model(ml_model) && !"mlp_model" %in% ls(environment(ml_model))
}
