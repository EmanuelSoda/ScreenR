setClass("screenR_object",
         representation(count_table = "data.frame",
                        annotation_table = "data.frame",
                        groups ="factor",
                        replicates ="vector",
                        normalized_count_table = "data.frame",
                        data_table = "data.frame"))
