plot.raneff = function(fit1, fact="Speaker"){

    randoms<-ranef(fit1, condVar = TRUE)
    qq <- attr(ranef(fit1, condVar = TRUE)[[fact]], "postVar")

    rand.interc<-randoms[[fact]]

    df<-data.frame(Intercepts=randoms[[fact]][,1],
                   sd.interc=2*sqrt(qq[,,1:length(qq)]),
                   lev.names=rownames(rand.interc))

    df = df[order(df$Intercepts),]

    df$lev.names<-factor(df$lev.names,levels=df$lev.names[order(df$Intercepts)])

    p <- ggplot(df,aes(lev.names,Intercepts))

    #Added horizontal line at y=0, error bars to points and points with size two
    p <- p + geom_hline(yintercept=0) +
        geom_errorbar(
            aes(ymin=Intercepts-sd.interc,
                ymax=Intercepts+sd.interc),
            width=0,color="black") +
                geom_point(aes(size=2))

    #Removed legends and with scale_shape_manual point shapes set to 1 and 16

    p <- p + guides(size=FALSE,shape=FALSE)# + scale_shape_manual(values=c(1,1,1,16,16,16))

    #Changed appearance of plot (black and white theme) and x and y axis labels
    p <- p + theme_bw() + xlab(fact) + ylab("")

    #Final adjustments of plot
    p <- p + theme(axis.text.x=element_text(size=rel(1.2)),
                   axis.title.x=element_text(size=rel(1.3)),
                   axis.text.y=element_text(size=rel(1.2)),
                   panel.grid.minor=element_blank(),
                   panel.grid.major.x=element_blank())

    #To put levels on y axis you just need to use coord_flip()
    p <- p+ coord_flip()

    return (p)
}
