library(sangerseqR)

seq <- read.abif("/Users/ad26693/MEGA SYNC/Sync Subset/dnaResults_Plate_28827_Order_189165/DIFIORE-1_ATEL5F.ab1")
seq1 <- sangerseq(seq)
chr1 <- chromatogram(seq1, width = 200, height = 2, trim5 = 50, trim3 = 100, showcalls = "both")
het1 <- makeBaseCalls(seq1, ratio = 0.33)
chr1 <- chromatogram(het1, width = 200, height = 2, trim5 = 50, trim3 = 100, showcalls = "both")
