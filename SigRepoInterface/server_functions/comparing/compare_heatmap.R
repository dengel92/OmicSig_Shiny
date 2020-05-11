library(Matrix)
library(ggplot2)
library(reshape2)
library(magrittr)
library(heatmaply)

compare_heatmap <- function() {
    #getting overlaps for each distinct pair
    ree = sqlGeneric(
        "select
        fsv.signature_name as 'signature1_name',
        fsv2.signature_name as 'signature2_name',
        spv.num_intersect /(fsv.sig1_num_features + fsv2.sig2_num_features) as 'fraction_overlap'
        from
        (
        select
        signature_name,
        count(distinct feature_name) as sig1_num_features
        from
        feature_signature_view
        group by
        signature_name) fsv
        join (
        select
        signature_name,
        count(distinct feature_name) as sig2_num_features
        from
        feature_signature_view
        group by
        signature_name) fsv2 on
        (fsv.signature_name <= fsv2.signature_name)
        join signature_pair_view spv on
        ((spv.signature1_name = fsv.signature_name
        and spv.signature2_name = fsv2.signature_name)
        or (spv.signature1_name = fsv2.signature_name
        and spv.signature2_name = fsv.signature_name))
        union
        select
        signature_name,
        signature_name,
        1
        from
        signatures ;"
    )
    
    result_cast = acast(ree, signature1_name ~ signature2_name, value.var =
                            "fraction_overlap")
    #since the overlap isn't dependent on the order of
    #the signatures you're comparing, I force the matrix to be symmetric
    #rather than get all the possible combos in the sql query
    result_cast = forceSymmetric(result_cast)
    #If there's no overlap, there won't be a record that exists in the result.
    #Meaning, you'll get NAs for nonexistent overlaps, which heatmaps don't know
    #what to do with. change them to 0s
    result_cast[which(is.na(result_cast))] = 0
    #There's no easy way to rework the title font size/family currently 
    #from my recent searches
    return(
        heatmaply(
            as.matrix(result_cast), 
            dendrogram = "none", 
            colors=RdYlGn,
            main="Fraction Overlap of Signatures",
            height=800,
            width=800,
            fontsize=15
        )
    )
}
