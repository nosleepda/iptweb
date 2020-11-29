module MathStatistics.Utilities2
open MathNet.Numerics.Distributions
    
let Student n a = StudentT.InvCDF(0.0, 1.0, n, 1.0 - a / 2.0)

let Student2 n a = StudentT.InvCDF(0.0, 1.0, n, 1.0 - a)
    
let ChiSquared f a = ChiSquared.InvCDF(f, a)
    
let NormalInv a = Normal.InvCDF(0.0, 1.0, 1.0 - a / 2.0)
    
let Normal mean std a = Normal.CDF(mean, std, a)

let Fisher d1 d2 a = FisherSnedecor.InvCDF(d1, d2, 1.0 - a)
    
let randNormal mean stdDev =
    let normalDist = new Normal(mean, stdDev);
    normalDist.Sample()

