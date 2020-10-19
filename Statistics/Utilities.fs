namespace Statistics

open MathNet.Numerics.Distributions

type Utilities() =

    static member Student n a = StudentT.InvCDF(0.0, 1.0, n, 1.0 - a / 2.0)
    
    static member ChiSquared f a = ChiSquared.InvCDF(f, a)
    
    static member NormalInv a = Normal.InvCDF(0.0, 1.0, 1.0 - a / 2.0)
    
    static member Normal mean std a = Normal.InvCDF(mean, std, a)
    
    static member randNormal mean stdDev =
        let normalDist = Normal(mean, stdDev);
        normalDist.Sample()