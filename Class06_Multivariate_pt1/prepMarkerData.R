
# ijob -A berglandlab -c20 -p standard --mem=40G
# module load gcc/11.4.0 openmpi/4.1.4 R/4.3.1; R


library(data.table)
library(SeqArray)

genofile <- seqOpen("/project/berglandlab/Adam/dgrp.gds")

genofile

snp.dt <- data.table(chr=seqGetData(genofile, "chromosome"),
                     pos=seqGetData(genofile, "position"),
                     nAlleles=seqGetData(genofile, "$num_allele"),
                     id=seqGetData(genofile, "variant.id"))


ids <- as.numeric(sample(as.character(snp.dt[chr=="2L"]$id), size=10000))
ids <- ids[order(ids)]

seqSetFilter(genofile, variant.id=ids)

geno <- as.matrix(seqGetData(genofile, "$dosage"))
rownames(geno) <- seqGetData(genofile, "sample.id")
colnames(geno) <- paste("variant", seqGetData(genofile, "variant.id"), sep="_")
str(geno)

geno.dt <- as.data.table(reshape2::melt(geno))
write.csv(geno.dt, file="~/genotypes.csv", quote=F, row.names=F)
