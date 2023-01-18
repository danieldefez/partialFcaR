#' @title
#' R6 class for a partial formal context
#'
#' @description
#' This class implements the data structure and methods for partial formal contexts.
#'
#' @section Public fields:
#' * `I`: the table for the partial formal contexts.
#' * `attributes`: name of the attributes in the partial formal contexts.
#' * `objects`: name of the objects in the context.
#' * `grades_set`: set of grades ( \{0, -1, 1\}) of the attributes.
#' * `concepts`: list of concepts (extent, intent).
#' * `implications`: extracted implications as an \code{ImplicationSet}.
#'
#' @examples
#' # Build and print the formal context
#' fc_planets <- FormalContext$new(planets)
#' print(fc_planets)
#'
#' # Define a set of attributes
#' S <- Set$new(attributes = fc_planets$attributes)
#' S$assign(moon = 1, large = 1)
#'
#' # Compute the closure of S
#' Sc <- fc_planets$closure(S)
#' # Is Sc a closed set?
#' fc_planets$is_closed(Sc)
#'
#' # Clarify and reduce the formal context
#' fc2 <- fc_planets$reduce(TRUE)
#'
#' # Find implications
#' fc_planets$find_implications()
#'
#' # Read a formal context from CSV
#' filename <- system.file("contexts", "airlines.csv", package = "fcaR")
#' fc <- FormalContext$new(filename)
#'
#' # Read a formal context from a CXT file
#' filename <- system.file("contexts", "lives_in_water.cxt", package = "fcaR")
#' fc <- FormalContext$new(filename)
#'
#' @references
#'
#' Guigues J, Duquenne V (1986). “Familles minimales d'implications informatives résultant d'un tableau de données binaires.” _Mathématiques et Sciences humaines_, *95*, 5-18.
#'
#' Ganter B, Wille R (1999). _Formal concept analysis : mathematical foundations_. Springer. ISBN 3540627715.
#'
#' Belohlavek R (2002). “Algorithms for fuzzy concept lattices.” In _Proc. Fourth Int. Conf. on Recent Advances in Soft Computing_. Nottingham, United Kingdom, 200-205.
#'
#' Hahsler M, Grun B, Hornik K (2005). “arules - a computational environment for mining association rules and frequent item sets.” _J Stat Softw_, *14*, 1-25.
#'
#' @export
PartialFormalContext <- R6::R6Class(
  
  classname = "PartialFormalContext",
  
  public = list(
    
    I = NULL,
    
    attributes = NULL,
    
    objects = NULL,
    
    grades_set = c(0,-1,1),
    
    concepts = NULL,
    
    implications = NULL,
    
    #' @description
    #' Creator for the Partial Formal Context class
    #'
    #' @param I           (numeric matrix) The table of the partial formal context.
    #' @param filename    (character) Path of a file to import.
    #' @param remove_const (logical) If \code{TRUE}, remove constant columns. The default is \code{FALSE}.
    #'
    #' @details
    #' Columns of \code{I} should be named, since they are the names of the attributes of the partial formal context.
    #'
    #' If no \code{I} is used, the resulting \code{PartialFormalContext} will be empty and not usable unless for loading a previously saved one. In this case, one can provide a \code{filename} to import. Only RDS, CSV and CXT files are currently supported.
    #'
    #' @return An object of the \code{PartialFormalContext} class.
    #' @export
    #'
    initialize = function(I, filename,
                          remove_const = FALSE) {
      
      if (missing(I)) {
        
        if (!missing(filename) && file.exists(filename)) {
          
          self$load(filename)
          
        }
        
        return(invisible(self))
        
      }
      
      if ((length(I) == 1) && is.character(I) && file.exists(I)) {
        
        self$load(I)
        
        return(invisible(self))
        
      }
      
      version
      
      if (!capabilities()["long.double"] & getRversion() < "4.1.0") {
        
        private$can_plot <- FALSE
        
      }
      
      # Transform the formal context to sparse
      if (inherits(I, "transactions")) {
        
        # If it comes from the arules package
        attributes <- I@itemInfo$labels
        I <- methods::as(I@data, "dgCMatrix")
        objects <- paste0(seq(ncol(I)))
        dimnames(I) <- list(attributes, objects)
        
      } else {
        
        # Or if it comes from a numeric table
        if (length(colnames(I)) > 0) {
          
          attributes <- colnames(I)
          
        } else {
          
          attributes <- paste0("A", seq(ncol(I)))
          
        }
        
        if (length(rownames(I)) > 0) {
          
          objects <- rownames(I)
          
        } else {
          
          objects <- paste0("O", seq(nrow(I)))
          
        }
        
        private$is_partial <- check_partial(I)
        
        if (private$is_partial) {
          
          I <- as.matrix(I)
          
          # Remove the constant columns if specified
          if (remove_const) {
            
            constant_cols <- which(apply(I, 2, max) == apply(I, 2, min))
            
            if (length(constant_cols) > 0) {
              
              str <- paste0("Removed constant columns: ", stringr::str_flatten(attributes[constant_cols], collapse = ", "))
              
              message(stringr::str_wrap(str,
                                        exdent = 2,
                                        width = 75))
              
              I <- I[, -constant_cols]
              attributes <- attributes[-constant_cols]
              
            }
            
          }
          
          #I <- methods::as(Matrix::Matrix(t(I),  sparse = TRUE), "dgCMatrix")
        }
        
      }
      
      self$objects <- objects
      self$attributes <- attributes
      private$is_partial <- check_partial(I)
      
      if (private$is_partial) {
        
        # Assign everything to its corresponding field
        self$I <- I
        #expanded_grades_set <- compute_grades(Matrix::t(I))
        grades_set <- c(0,-1,1)
        #self$grades_set <- unique(c(-1, 0, 1))
        
        colnames(self$I) <- self$attributes
        rownames(self$I) <- self$objects
        
      }else{
        error_not_partial()
      }
      
      #### Start empty implications an concepts ####
      
      # Create a new empty implication set inside
      self$implications <- ImplicationSet$new(attributes = attributes,
                                              I = self$I)
      
      # Create a new empty ConceptLattice inside
      self$concepts <- ConceptLattice$new(extents = NULL,
                                          intents = NULL,
                                          objects = self$objects,
                                          attributes = self$attributes,
                                          I = self$I)
      #### Section end ####
    },
    
    #' @description
    #' Check if the \code{PartialFormalContext} is empty
    #'
    #' @return \code{TRUE} if the \code{PartialFormalContext} is empty, that is, has not been provided with a matrix, and \code{FALSE} otherwise.
    #'
    #' @export
    is_empty = function() {
      
      return(is.null(self$I) && is.null(private$many_valued_I))
      
    },
    
    #' @description
    #' Get the intent of a fuzzy set of objects
    #'
    #' @param S   (\code{Set}) The set of objects to compute the intent for.
    #'
    #' @return A \code{Set} with the intent.
    #'
    #' @export
    intent = function(S) {
      
      if (!private$is_partial) error_not_partial()
      
      if (inherits(S, "Set")) {
        
        if (all(S$get_attributes() == self$objects)) {
          
          S <- S$get_vector()
          
        } else {
          
          S <- match_attributes(S, self$objects)
          S <- S$get_vector()
          warn <- c("The attributes in the input set are not the same",
                    " that in the formal context. Attempting to match",
                    " attribute names gives ",
                    .set_to_string(S, self$objects)) %>%
            stringr::str_flatten()  
          warning(warn, call. = FALSE, immediate. = TRUE)
          # stop("It is not a set of the required type (set of objects).", call. = FALSE)
          
        }
        
      }
      
      if (length(S) == length(self$objects)) {
        
        R <- compute_intent(S,
                            Matrix::as.matrix(Matrix::t(t(self$I))))
        
        if (length(R@i) > 0) {
          
          # Non-empty set:
          R <- Matrix::sparseMatrix(i = R@i + 1,
                                    j = rep(1, length(R@i)),
                                    x = R@x,
                                    dims = c(length(self$attributes), 1))
          
          R <- Set$new(attributes = self$attributes,
                       M = R)
        } else {
          # Empty intent
          R <- Set$new(attributes = self$attributes)
          
        }
        
        return(R)
        
      } else {
        
        stop("It is not a set of the required type (set of objects).", call. = FALSE)
        
      }
      
    },
    
    #' @description
    #' Get the extent of a fuzzy set of attributes
    #'
    #' @param S   (\code{Set}) The set of attributes to compute the extent for.
    #'
    #' @return A \code{Set} with the intent.
    #'
    #' @export
    extent = function(S) {
      
      # TODO: Apply scales to Sets.
      
      if (!private$is_partial) error_not_partial()
      
      if (inherits(S, "Set")) {
        
        if (all(S$get_attributes() == self$attributes)) {
          
          S <- S$get_vector()
          
        } else {
          
          S <- match_attributes(S, self$attributes)
          S <- S$get_vector()
          warn <- c("The attributes in the input set are not the same",
                    " that in the formal context. Attempting to match",
                    " attribute names gives ",
                    .set_to_string(S, self$attributes)) %>%
            stringr::str_flatten()
          warning(warn, call. = FALSE, immediate. = TRUE)
          # stop("It is not a set of the required type (set of attributes).", call. = FALSE)
          
        }
        
      }
      
      if (length(S) == length(self$attributes)) {
        
        R <- compute_extent(S,
                            Matrix::as.matrix(Matrix::t(t(self$I))))
        
        if (length(R@i) > 0) {
          
          # Non-empty set:
          R <- Matrix::sparseMatrix(i = R@i + 1,
                                    j = rep(1, length(R@i)),
                                    x = R@x,
                                    dims = c(length(self$objects), 1))
          
          R <- Set$new(attributes = self$objects,
                       M = R)
        } else {
          
          # Empty extent
          R <- Set$new(attributes = self$objects)
          
        }
        
        return(R)
        
      } else {
        
        stop("It is not a set of the required type (set of objects).", call. = FALSE)
        
      }
      
    },
    
    #' @description
    #' Get the closure of a fuzzy set of attributes
    #'
    #' @param S   (\code{Set}) The set of attributes to compute the closure for.
    #'
    #' @return A \code{Set} with the closure.
    #'
    #' @export
    closure = function(S) {
      
      if (!private$is_partial) error_not_partial()
      
      if (inherits(S, "Set")) {
        
        if (all(S$get_attributes() == self$attributes)) {
          
          S <- S$get_vector()
          
        } else {
          
          S <- match_attributes(S, self$attributes)
          S <- S$get_vector()
          warn <- c("The attributes in the input set are not the same",
                    " that in the formal context. Attempting to match",
                    " attribute names gives ",
                    .set_to_string(S, self$attributes)) %>%
            stringr::str_flatten()
          warning(warn, call. = FALSE, immediate. = TRUE)
          
          # stop("It is not a set of the required type (set of attributes).", call. = FALSE)
          
        }
        
      }
      
      if (length(S) == length(self$attributes)) {
        
        R <- compute_closure(S,
                             Matrix::as.matrix(Matrix::t(t(self$I))))
        
        if (length(R@i) > 0) {
          
          # Non-empty set:
          R <- Matrix::sparseMatrix(i = R@i + 1,
                                    j = rep(1, length(R@i)),
                                    x = R@x,
                                    dims = c(length(self$attributes), 1))
          
          R <- Set$new(attributes = self$attributes,
                       M = R)
        } else {
          
          # Empty closure
          R <- Set$new(attributes = self$attributes)
          
        }
        
        return(R)
        
      } else {
        
        stop("It is not a set of the required type (set of objects).", call. = FALSE)
        
      }
      
    },
    
    #' @description
    #' Object Concept
    #'
    #' @param object (character) Name of the object to compute its associated concept
    #'
    #' @return
    #' The object concept associated to the object given.
    #'
    #' @export
    obj_concept = function(object, value) {
      
      if (!private$is_partial) error_not_partial()
      
      S <- Set$new(attributes = self$objects)
      S$assign(attributes = object, values = value)
      
      B <- self$intent(S)
      A <- self$extent(B)
      
      C <- Concept$new(extent = A, intent = B)
      
      return(C)
      
    },
    
    #' @description
    #' Attribute Concept
    #'
    #' @param attribute (character) Name of the attribute to compute its associated concept
    #'
    #' @return
    #' The attribute concept associated to the attribute given.
    #'
    #' @export
    att_concept = function(attribute) {
      
      if (!private$is_partial) error_not_partial()
      
      S <- Set$new(attributes = self$attributes)
      S$assign(attributes = attribute, values = value)
      
      A <- self$extent(S)
      B <- self$intent(A)
      
      C <- Concept$new(extent = A, intent = B)
      
      return(C)
      
    },
    
    
    #' @description
    #' Is a Concept?
    #'
    #' @param C A \code{Concept} object
    #'
    #' @return
    #' \code{TRUE} if \code{C} is a concept.
    #'
    #' @export
    is_concept = function(C) {
      
      O <- C$get_extent()
      A <- C$get_intent()
      
      # O should be the extent of A, and A should be intent of O
      return((self$extent(A) %==% O) && (self$intent(O) %==% A))
      
    },
    
    #' @description
    #' Testing closure of attribute sets
    #'
    #' @param S A \code{Set} of attributes
    #'
    #' @return
    #' \code{TRUE} if the set \code{S} is closed in this formal context.
    #' @export
    is_closed = function(S) {
      
      Sc <- self$closure(S)
      return(S %==% Sc)
      
    },
    
    compare = function(Set1, Set2){
      return (Set1 %==% Set2)
    },
    
    #' @description
    #' Use of a more simpler algorithm to get all concepts
    #'
    #' @param I matrix with our lattice
    #'
    #' @return
    #' \code{TRUE} if the set \code{S} is closed in this formal context.
    #' @export
    get_all_concepts = function(I, grades_set, attrs, verbose) {
      
      MySet <- Set$new(attributes = attrs)
      ConceptSet <- c(Concept$new(Set$new(attributes = attrs),Set$new(attributes = attrs)))
      count=0
      for(att in attrs){
        MySet$assign(attrs, c(rep(0,count),1,rep(0,length(attrs)-(1+count))))
        myConcept = Concept$new(self$closure(MySet), self$extent(MySet))
        check = map(ConceptSet, self$compare, myConcept)
        
        if(!(TRUE %in% check)){
          ConceptSet <- c(ConceptSet, Concept$new(self$closure(MySet), self$extent(MySet)))
        }
        MySet$assign(attrs, c(rep(0,count),-1, rep(0,length(attrs)-(1+count))))
        myConcept = Concept$new(self$closure(MySet), self$extent(MySet))
        
        check = map(ConceptSet, self$compare, myConcept)
        
        if(!(TRUE %in% check)){
          ConceptSet <- c(ConceptSet, Concept$new(self$closure(MySet), self$extent(MySet)))
        }
        count= count+1
      }
      my_extents <- c()
      my_intents <- c()
      for(concep in ConceptSet){
        my_extents = c(my_extents, concep$get_extent())
        my_intents = c(my_intents, concep$get_intent())
      }
      
      #print(my_extents)
      #print(my_intents)
      
      extent_matrix = matrix(unlist(my_extents), ncol = length(my_extents), byrow = TRUE)
      intent_matrix = matrix(unlist(my_intents), ncol = length(my_intents), byrow = TRUE)

      #print("Primer set")
      #print(extent_matrix[1,1])
      #print("Segundo set")
      #print(extent_matrix[1,2])
      #print("Tercer set")
      #print(extent_matrix[1,3])
      #print("Cuarto set")
      #print(extent_matrix[1,4])
      #print("Quinto set")
      #print(extent_matrix[1,5])
      #print("Sexto set")
      #print(extent_matrix[1,6])
      
      self$concepts <- ConceptLattice$new(extents = extent_matrix,
                                          intents = intent_matrix,
                                          objects = self$objects,
                                          attributes = self$attributes,
                                          I = self$I)
      print(self$concepts)
      
      #return(c(intents = intents, extents = extents, closure_count= length(extents)))
      
    },
    
    #' @description
    #' Function to Test C functions
    #'
    #' @return notin
    #'
    #' @export
    test = function(){
      my_I <- Matrix::as.matrix(Matrix::t(t(self$I)))
      grades_set <- rep(list(self$grades_set), length(self$attributes))
      attrs <- self$attributes
      objs <- self$objects
      
      Test(I = my_I,
           grades_set = grades_set,
           attrs = attrs,
           objs = objs)
      
    },
    
    #' @description
    #' Function to Test Semantic closure
    #'
    #' @return notin
    #'
    #' @export
    test_closure = function(){
      my_I <- Matrix::as.matrix(Matrix::t(t(self$I)))
      grades_set <- rep(list(self$grades_set), length(self$attributes))
      attrs <- self$attributes
      objs <- self$objects
      
      Test_Closure(I = my_I,
           grades_set = grades_set,
           attrs = attrs,
           objs = objs)
      
    },
    
    #' @description
    #' Use Ganter Algorithm to compute concepts
    #'
    #' @param verbose   (logical) TRUE will provide a verbose output.
    #'
    #' @return A list with all the concepts in the formal context.
    #'
    #' @export
    find_concepts = function(verbose = FALSE) {
      
      
      
      private$check_empty()
      
      if (!private$is_partial) error_not_partial()
      
      my_I <- Matrix::as.matrix(Matrix::t(t(self$I)))
      grades_set <- rep(list(self$grades_set), length(self$attributes))
      # grades_set <- self$expanded_grades_set
      attrs <- self$attributes
      objs <- self$objects
      
      #L <- self$get_all_concepts(I = my_I,
      #                     grades_set = grades_set,
      #                      attrs = attrs,
      #                      verbose = verbose)
      
      L <- next_closure_concepts(I = my_I,
                                 grades_set = grades_set,
                                 attrs = attrs,
                                 verbose = verbose)
      
      #L <- next_closure_algorithm_concepts(I = my_I,
       #                                    grades_set = grades_set,
        #                                   attrs = attrs,
         #                                  objs = objs)
      
      
      if (length(self$attributes) == 1) {
        
        
        my_intents <- Matrix::Matrix(t(as.vectLor(L$intents)), sparse = TRUE)
        
        my_extents <- Matrix::Matrix(t(as.vector(L$extents)), sparse = TRUE)
        
      } else {
        
        my_intents <- L$intents
        
        my_extents <- L$extents
        
      }
      
      
      self$concepts <- ConceptLattice$new(extents = my_extents,
                                          intents = my_intents,
                                          objects = self$objects,
                                          attributes = self$attributes,
                                          I = self$I)
      
      if (verbose) {
        
        cat("Number of closures", L$closure_count, "\n")
        
      }
      
      # return(invisible(self$concepts))
      return(invisible(self))
      
    },
    
    #' @description
    #' Load a \code{PartialFormalContext} from a file
    #'
    #' @param filename   (character) Path of the file to load the \code{PartialFormalContext} from.
    #'
    #' @details Currently, only RDS, CSV and CXT files are supported.
    #'
    #' @return The loaded \code{PartialFormalContext}.
    #'
    #' @export
    load = function(filename) {
      
      pattern <- "(?<!^|[.]|/)[.]([^.]+)$"
      
      extension <- filename  %>%
        stringr::str_extract_all(pattern) %>%
        unlist() %>%
        tolower()
      
      if (extension == ".csv") {
        
        I <- read.csv(filename)
        
        if (is.character(I[[1]]) || is.factor(I[[1]])) {
          
          objects <- as.character(I[[1]])
          I <- I[, -1]
          rownames(I) <- objects
          
        }
        
        I <- as.matrix(I)
        
        self$initialize(I)
        
      }
      
      return(invisible(self))
      
    },
    
    #' @description
    #' Dimensions of the formal context
    #'
    #' @return A vector with (number of objects, number of attributes).
    #'
    #' @export
    dim = function() {
      
      return(c(length(self$objects), length(self$attributes)))
      
    },
    
    #' @description
    #' Prints the formal context
    #'
    #' @return Prints information regarding the formal context.
    #' @export
    print = function() {
      
      if (self$is_empty()) {
        
        cat("Empty PartialFormalContext.\n")
        
        return(invisible(self))
        
      }
      
      dims <- self$dim()
      
      if (private$is_partial) {
        
        I <- Matrix::as.matrix(Matrix::t(t(self$I)))
        
        I <- .print_partial(I, latex = FALSE)
        
        objects <- self$objects
        if (nrow(I) > 10) {
          
          I <- I[1:10, ]
          objects <- objects[1:10]
          
        }
        
        
        matp <- .print_matrix(I,
                              objects = objects,
                              attributes = self$attributes)
        M <- matp$mat
        ids <- matp$att_id
        last_attribute <- max(ids) - 1
        
        str <- paste0("PartialFormalContext with ", dims[1],
                      " objects and ",
                      dims[2], " attributes.") %>%
          stringr::str_wrap(width = getOption("width"))
        
        cat(str)
        cat("\n")
        
        cat(M)
        cat("\n")
        
        if (last_attribute < length(self$attributes)) {
          
          remaining <- self$attributes[-seq(last_attribute)]
          
          if (length(remaining) > 6) {
            
            remaining <- c(remaining[1:6], "...")
            
          }
          
          remaining <- remaining %>%
            stringr::str_flatten(", ")
          
          str <- paste0("Other attributes are: ", remaining) %>%
            stringr::str_wrap(width = getOption("width"))
          
          cat(str, "\n")
          
        }
        
      }
      
    },
    
    
    #' @description
    #' Write the context in LaTeX format
    #'
    #' @param table (logical) If \code{TRUE}, surrounds everything between \code{\\begin{table}} and \code{\\end{table}}.
    #' @param label (character) The label for the table environment.
    #' @param caption (character) The caption of the table.
    #' @param fraction (character) If \code{none}, no fractions are produced. Otherwise, if it is \code{frac}, \code{dfrac} or \code{sfrac}, decimal numbers are represented as fractions with the corresponding LaTeX typesetting.
    #'
    #' @return
    #' A table environment in LaTeX.
    #'
    #' @export
    #'
    to_latex = function(table = TRUE,
                        label = "",
                        caption = "",
                        fraction = c("none", "frac", "dfrac", "sfrac")) {
      
      # TODO: export a many-valued context to LaTeX
      if (!private$is_partial) error_not_partial()
      
      fraction <- match.arg(fraction)
      
      I <- Matrix::as.matrix(Matrix::t(t(self$I)))
      
      if (private$is_partial) {
        
        I <- .print_partial(I, latex = TRUE)
        
        
      } else {
        
        if (fraction != "none") {
          
          I <- .to_fraction(I,
                            latex = TRUE,
                            type = fraction)
          
        } else {
          
          decimal_places <- fcaR_options("decimal_places")
          I[] <- I %>%
            formatC(digits = decimal_places) %>%
            stringr::str_replace_all("\\s*", "")
          
        }
        
      }
      
      str <- context_to_latex(I,
                              objects = self$objects,
                              attributes = self$attributes)
      
      if (table) {
        
        str <- c("\\begin{table}",
                 "\\centering",
                 str)
        
        my_caption <- paste0("\\caption{\\label{",
                             label, "}",
                             caption, "}")
        
        str <- c(str, my_caption, "\\end{table}")
        
      }
      
      cat(str)
      
      return(invisible(str))
      
    },
    
    #' @description
    #' Incidence matrix of the partial formal context
    #'
    #' @return The incidence matrix of the partial formal context
    #' @export
    #'
    #' @examples
    #' fc <- PartialFormalContext$new(planets)
    #' fc$incidence()
    incidence = function() {
      
      if (!private$is_partial) {
        
        return(private$many_valued_I)
        
      } else {
        
        I <- Matrix::as.matrix(Matrix::t(self$I))
        dimnames(I) <- list(self$objects, self$attributes)
        
        return(I)
        
      }
      
    },
    
    #' @description
    #' Plot the formal context table
    #'
    #' @param to_latex      (logical) If \code{TRUE}, export the plot as a \code{tikzpicture} environment that can be included in a \code{LaTeX} file.
    #' @param ...          Other parameters to be passed to the \code{tikzDevice} that renders the lattice in \code{LaTeX}, or for the figure caption. See \code{Details}.
    #'
    #' @details
    #' Particular parameters that control the size of the \code{tikz} output are: \code{width}, \code{height} (both in inches), and \code{pointsize} (in points), that should be set to the font size used in the \code{documentclass} header in the \code{LaTeX} file where the code is to be inserted.
    #'
    #' If a \code{caption} is provided, the whole \code{tikz} picture will be wrapped by a \code{figure} environment and the caption set.
    #'
    #' @return If \code{to_latex} is \code{FALSE}, it returns nothing, just plots the graph of the formal context. Otherwise, this function returns the \code{LaTeX} code to reproduce the formal context plot.
    #'
    #' @export
    plot = function(to_latex = FALSE,
                    ...) {
      
      if (!private$is_partial) error_not_partial()
      
      private$check_empty()
      if (!private$can_plot) {
        
        warning("The R system has not the needed capabilities to plot.",
                call. = FALSE)
        return(invisible(FALSE))
        
      }
      
      plot_context(self$I, to_latex, ...)
      
    }
    
  ),
  
  private = list(
    is_partial = FALSE,
    can_plot = TRUE,
    scales = list(),
    bg_implications = NULL,
    bg_implications_basis = NULL,
    
    check_empty = function() {
      
      if (self$is_empty()) {
        
        stop("The formal context is empty. The only allowed method is 'load' to import from a previously saved RDS file.", call. = FALSE)
        
      }
      
      return(invisible(NULL))
      
    }
    
  )
)
