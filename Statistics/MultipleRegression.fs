namespace MathStatistics

open System
open System.Collections.Generic
open MathNet.Numerics.Distributions
open MathNet.Numerics.LinearAlgebra
open MathNet.Numerics.LinearAlgebra.Double

type MultipleRegression(ysArr: double[], xs1Arr: double[], xs2Arr: double[], xs3Arr: double[], alf: double) =
    
    let xs1 = xs1Arr |> List.ofSeq
    
    let xs2 = xs2Arr |> List.ofSeq
    
    let xs3 = xs3Arr |> List.ofSeq
    
    let ys = ysArr |> List.ofSeq
    
    let factors = [xs1; xs2; xs3]
    
    let n = ys.Length |> double
    
    let divN a x = x / (n - a) 
    
    let my =
        ys
        |> List.sum
        |> divN 0.0
    
    let m2y =
        ys
        |> List.map (fun y -> y ** 2.0)
        |> List.sum
        |> divN 0.0
        
    let sy = sqrt (m2y - my ** 2.0)
    
    let mxs =
        factors
        |> List.map (fun factor ->
           factor
           |> List.sum
           |> divN 0.0)
    
    let m2xs =
        factors
        |> List.map (fun factor ->
           factor
           |> List.map (fun x -> x ** 2.0)
           |> List.sum
           |> divN 0.0)
    
    let sxs =
        (m2xs, mxs)
        ||> List.map2 (fun x2 x -> sqrt (x2 - x ** 2.0))
    
    let yxs =
        factors
        |> List.map (fun factor ->
           (factor, ys)
           ||> List.map2 (fun x y -> x * y)
           |> List.sum
           |> divN 0.0)
    
    let ryxs =
        (yxs, mxs, sxs)
        |||> List.map3 (fun yx mx sx -> (yx - my * mx) / (sy * sx))
    
    let r2yxs = ryxs |> List.map (fun r -> r ** 2.0)
    
    let rxxIter (number:int) (factor:double list) =
        let result = List<double>(3)
        
        for i in number + 1 .. factors.Length - 1 do
            (factor, factors.[i])
            ||> List.map2 (fun currFact otherFact -> currFact * otherFact)
            |> List.sum
            |> divN 0.0
            |> (+) -(mxs.[i] * mxs.[number])
            |> (*) (1.0 / (sxs.[i] * sxs.[number]))
            |> result.Add
            
        result |> List.ofSeq
        
    let rxx =
        factors
        |> List.mapi (fun number factor -> rxxIter number factor)
        |> List.concat
    
    let xst =
        r2yxs |> List.map (fun r2 -> r2 * (n - 2.0) / (1.0 - r2) |> sqrt)
    
    let student a = StudentT.InvCDF(0.0, 1.0, n - 2.0, 1.0 - a / 2.0)
    
    let includedFactors =
        let tCrit = student 0.05
        
        let excludedT = xst |> List.filter (fun x -> x <= tCrit)
        if excludedT.Length = 1 then
            [xst |> List.findIndex (fun x -> x > tCrit); xst |> List.findIndexBack (fun x -> x > tCrit)] |> List.sort
        elif excludedT.Length = 0 then
            let excludedRxx = rxx |> List.filter (fun x -> x >= 0.8)
            let mutable rxxi = 0
            if excludedRxx.Length = 1 then
                rxxi <- rxx |> List.findIndex (fun x -> x >= 0.8)
            else
                rxxi <- rxx |> List.findIndex (fun x -> x = List.max rxx)
                
            let mutable rx1 = 0
            let mutable rx2 = 0
            let mutable xIncluded = 0
            match rxxi with
            | 0 -> rx1 <- 0; rx2 <- 1; xIncluded <- 2
            | 1 -> rx1 <- 0; rx2 <- 2; xIncluded <- 1
            | 2 -> rx1 <- 1; rx2 <- 2; xIncluded <- 0
            | _ -> failwith "Error"
                
            if ryxs.[rx2] > ryxs.[rx1] then
                [xIncluded; rx2] |> List.sort
            else
                [xIncluded; rx1] |> List.sort
                
        else
            let ryxi = [xst |> List.findIndex (fun x -> x <= tCrit); xst |> List.findIndexBack (fun x -> x <= tCrit)]
            let rxxi = ryxi |> List.sum |> (+) -1
            
            if rxx.[rxxi] > 0.8 then
                if ryxs.[ryxi.[1]] > ryxs.[ryxi.[0]] then
                    [xst |> List.findIndex (fun x -> x > tCrit); ryxi.[1]] |> List.sort
                else
                    [xst |> List.findIndex (fun x -> x > tCrit); ryxi.[0]] |> List.sort
            else
                failwith "Error"
    
    let Ryxx =
        let iyx = includedFactors
        let ixx = includedFactors |> List.sum |> (+) -1
        
        (r2yxs.[iyx.[0]] + r2yxs.[iyx.[1]]
         - 2.0 * ryxs.[iyx.[0]] * ryxs.[iyx.[1]] * rxx.[ixx])
        / (1.0 - rxx.[ixx] ** 2.0)
        |> sqrt
        
    let Ryxx' =
        let k = double includedFactors.Length 
        sqrt (1.0 - (1.0 - Ryxx ** 2.0) * (n - 1.0) / (n - k - 1.0))
    
    let coef =
        let x1 = factors.[includedFactors.[0]] |> List.sum
        let x12 = factors.[includedFactors.[0]] |> List.map (fun x -> x ** 2.0) |> List.sum
        let x2 = factors.[includedFactors.[1]] |> List.sum
        let x22 = factors.[includedFactors.[1]] |> List.map (fun x -> x ** 2.0) |> List.sum
        let y = ys |> List.sum
        let x1x2 = (factors.[includedFactors.[0]], factors.[includedFactors.[1]]) ||> List.map2 (fun x1 x2 -> x1 * x2) |> List.sum
        let yx2 = (ys, factors.[includedFactors.[1]]) ||> List.map2 (fun y x2 -> y * x2) |> List.sum
        let yx1 = (ys, factors.[includedFactors.[0]]) ||> List.map2 (fun y x2 -> y * x2) |> List.sum
        let list = [[x1; x2; n]; [x1x2; x22; x2]; [x12; x1x2; x1]]
        let m = DenseMatrix.OfArray (array2D list)
        let fs = [y; yx2; yx1] |> vector
        m.Solve fs |> List.ofSeq
    
    member this.RegressionFunction =
        let index1 = includedFactors.[0] + 1
        let index2 = includedFactors.[1] + 1
        String.Format("y = x{0} * {1:0.####} + x{2} * {3:0.####} + {4:0.####}", index1, coef.[0], index2, coef.[1], coef.[2])
    
    member this.Correlation = Math.Round(Ryxx, 4)
        
    member this.CorrelationCorrected = Math.Round(Ryxx', 4)
    
    member this.SignificanceCorrelation =
        let l = Ryxx' / (1.0 / sqrt (n - 1.0))
        let r = Utilities2.Student2 (n - 3.0) alf
        let bool = if l > r then "" else "не "
        let bool2 = if l > r then ">" else "<"
        String.Format("Множественный коэффициент корреляции {0}значим, \nт.к. {1:0.####} {2} {3:0.####} ", bool, l, bool2, r)
    
    member this.Determination = Math.Round(Ryxx ** 2.0, 4)
    
    member this.Adequacy =
       let l = (n - 3.0) * Ryxx ** 2.0 / (2.0 - 2.0 * Ryxx ** 2.0)
       let r = Utilities2.Fisher 2.0 (n - 3.0) alf
       let bool = if l > r then "" else "не "
       let bool2 = if l > r then ">" else "<"
       String.Format("Эмпирические данные статистически {0}значимы, \nт.к. {1:0.####} {2} {3:0.####} ", bool, l, bool2, r)
    
    member this.Error =
        (factors.[includedFactors.[0]], factors.[includedFactors.[1]])
        ||> List.map2 (fun x1 x2 -> coef.[0] * x1 + coef.[1] * x2 + coef.[2])
        |> List.map2 (fun y yexp -> abs (y - yexp) / y) ys
        |> List.sum
        |> (*) (1.0 / n)
    
    member this.Elasticity =
        let index1 = includedFactors.[0] + 1
        let index2 = includedFactors.[1] + 1
        let k1 = coef.[0] * mxs.[includedFactors.[0]] / my
        let k2 = coef.[1] * mxs.[includedFactors.[1]] / my
        String.Format("X{0} = {1:0.####}  X{2} = {3:0.####}", index1, k1, index2, k2)
    
    member this.Xs1 = xs1 |> Seq.cast<obj> |> List
    
    member this.Xs2 = xs2 |> Seq.cast<obj> |> List
    
    member this.Xs3 = xs3 |> Seq.cast<obj> |> List
    
    member this.Ys = ys |> Seq.cast<obj> |> List