################################################################################
#' analysisVariantClient methods
#' @include commons.R
#' 
#' @description This function implements the OpenCGA calls for managing VariableSets
#' @param OpencgaR an object OpencgaR generated using initOpencgaR and/or opencgaLogin 
#' where the connection and session details are stored
#' @param action action to be performed on the variableSet(s)
#' @param params list containing additional query or body params
#' @seealso \url{https://github.com/opencb/opencga/wiki} and the RESTful API documentation 
#' \url{http://bioinfo.hpc.cam.ac.uk/opencga/webservices/}
#' @export

setMethod("analysisVariantClient", "OpencgaR", function(OpencgaR, action, params=NULL, ...) {
    category <- "analysis"
    subcategory <- "variant"
    switch(action,
           facet=fetchOpenCGA(object=OpencgaR, category=category, 
                              subcategory=subcategory, action=action, 
                              params=params, httpMethod="GET", ...),
           ibs=fetchOpenCGA(object=OpencgaR, category=category, 
                            subcategory=subcategory, action=action, 
                            params=params, httpMethod="GET", ...),
           index=fetchOpenCGA(object=OpencgaR, category=category, 
                              subcategory=subcategory, action=action, 
                              params=params, httpMethod="GET", ...),
           query=fetchOpenCGA(object=OpencgaR, category=category, 
                              subcategory=subcategory, action=action, 
                              params=params, httpMethod="GET", ...),
           #TODO: Implement POST method for query
           samples=fetchOpenCGA(object=OpencgaR, category=category, 
                                subcategory=subcategory, action=action, 
                                params=params, httpMethod="GET", ...),
           stats=fetchOpenCGA(object=OpencgaR, category=category, 
                              subcategory=subcategory, action=action, 
                              params=params, httpMethod="GET", ...),
           validate=fetchOpenCGA(object=OpencgaR, category=category, 
                                 subcategory=subcategory, action=action, 
                                 params=params, httpMethod="POST", ...)
    )
})

