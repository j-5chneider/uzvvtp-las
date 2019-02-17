library(timevis)

projectplan <- data.frame(
    content = c("Linz", "Flensburg", "Stuttgart, <br /> Vechta, Tübingen", 
                "EFA > <br />Itemselektion", "Erstellung<br />neuer FB", 
                "alle Standorte", 
                "CFA > <br />Reliabilitäten"),
    start = c("2019-03-04", "2019-03-11", "2019-04-01",
              "2019-04-15", "2019-04-24", 
              "2019-04-29", 
              "2019-05-20"),
    end = c("2019-04-14", "2019-04-14", "2019-04-14", 
            "2019-04-23", "2019-04-28", 
            "2019-05-19", 
            "2019-06-02"),
    group = c(rep("er1", 3), rep("aus1", 2), "er2", "aus2"),
    type = c(rep("range", 7))
)

projectgroups <- data.frame(
    id = c("er1", "aus1", "er2", "aus2"),
    content = c("Erhebung I<br />N=200", "Auswertung I", "Erhebung II<br />N=200", "Auswertung II") 
)


timevis(data = projectplan, groups = projectgroups, options = list(editable = F))
# different options like tool tips?