namespace MathStatistics

open System
open System.Collections.Generic
open MathNet.Numerics.Distributions
open MathNet.Numerics.LinearAlgebra
open MathNet.Numerics.LinearAlgebra.Double

type AnalysisTable(xs: double[], ys: double[], ns: double[,], alf: double) =
    let xTitle = xs |> List.ofSeq
    
    let yTitle = ys |> List.ofSeq
    
    let matrix = DenseMatrix.OfArray ns
    
    let ps =
        matrix.RowSums()
        |> List.ofSeq 
    
    let pys = List.map2 (fun p y -> p * y) ps yTitle
    
    let ws =
        matrix.ColumnSums()
        |> List.ofSeq 

    let wxs =
        List.map2 (fun p y -> p * y) ws xTitle
    
    let k = double xTitle.Length 
    
    let n = List.sum ps
    
    let n1 = n - 1.0
    
    let nInt = int n
    
    let mx =
        wxs
        |> List.sum
        |> (*) (1.0 / n)
    
    let my =
        pys
        |> List.sum
        |> (*) (1.0 / n)
    
    let wxmx2 =
        (xTitle, ws)
        ||> List.map2 (fun elem1 elem2 -> (elem1 - mx) ** 2.0 * elem2) 
        |> List.sum
    
    let pymy2 =
        (yTitle, ps)
        ||> List.map2 (fun elem1 elem2 -> (elem1 - my) ** 2.0 * elem2) 
        |> List.sum
        
    let sx =
        wxmx2 / n1    
        |> sqrt
    
    let sy =
        pymy2 / n1
        |> sqrt
    
    let ny =
        matrix.EnumerateColumns()
        |> List.ofSeq
        |> List.map (fun column -> List.ofSeq column
                                   |> List.map2 (fun elem1 elem2 -> elem1 * elem2) yTitle
                                   |> List.sum)
        
    let r =
        ny
        |> List.map2 (fun elem1 elem2 -> elem1 * elem2) xTitle
        |> List.sum
        |> (+) -(n * mx * my)
        |> (*) (1.0 / sqrt (wxmx2 * pymy2))
    
    let b =
        r * sy / sx
        
    let a =
        my - mx * r * sy / sx
        
    let func x = a + b * x
                           
    let fxs =                        
        List.map (fun x -> func x) xTitle   
    
    let nyws =
            ny
            |> List.map2 (fun elem1 elem2 -> elem2 / elem1) ws
    
    let s1 =
        matrix.EnumerateColumns()
        |> List.ofSeq
        |> List.mapi (fun index column -> List.ofSeq column
                                          |> List.map2 (fun y n -> (y - nyws.[index]) ** 2.0 * n) yTitle
                                          |> List.sum)
        |> List.sum
        |> (*) (1.0 / (n - k))
    
    let s2 =
        List.map (fun x -> func x) xTitle
        |> List.map3 (fun elem2 elem3 elem1 -> (elem1 - elem2) ** 2.0 * elem3) nyws ws
        |> List.sum
        |> (*) (1.0 / (k - 2.0))
    
    
    let S =
        (fxs, nyws)
        ||> List.map2 (fun elem1 elem2 -> (elem1 - elem2) ** 2.0)
        |> List.sum
        |> (*) (1.0 / (n - 2.0))
        |> sqrt

    member this.MeanX = Math.Round(mx, 4)
    
    member this.MeanY = Math.Round(my, 4)
    
    member this.Sx = Math.Round(sx, 4)
    
    member this.Sy = Math.Round(sy, 4)
    
    member this.Correlation = Math.Round(r, 4)
    
    member this.SignificanceCorrelation =
        let l = abs r * sqrt (n - 2.0) / sqrt (1.0 - r ** 2.0)
        let r = StudentT.InvCDF(0.0, 1.0, 28.0, 1.0 - 0.05 / 2.0)
        let bool = if l > r then "" else "не "
        let bool2 = if l > r then ">" else "<"
        String.Format("Коэффициент корреляции {0}значим, \nт.к. {1} {2} {3} ", bool, Math.Round(l, 4), bool2, Math.Round(r, 4))
    
    member this.ConfidenceIntervalCorrelation =
        let norm = Utilities2.NormalInv alf 
        let l = Math.Atanh r - (norm) / sqrt(n - 3.0) |> tanh
        let r = Math.Atanh r + (norm) / sqrt(n - 3.0) |> tanh
        String.Format("Доверительный интервал коэффициента корреляции {0} < r < {1} ", Math.Round(l, 4), Math.Round(r, 4))
    
    member this.Determination =
        let r1 =
            List.map2 (fun elem1 elem2 -> (elem1 - my) ** 2.0 * elem2) yTitle ps
            |> List.sum
            
        let r2 =
            fxs
            |> List.map2 (fun elem2 elem1 -> (elem1 - my) ** 2.0 * elem2) ws
            |> List.sum
            
        r2 / r1
    
    member this.Adequacy =
        let l =  s2 / s1
        let r = Utilities2.Fisher (k - 2.0) (n - k) (alf)
        let bool1 = if l > r then "" else "не "
        let bool2 = if l > r then ">" else "<"
        String.Format("Коэффициент детерминации {0}значим, \nт.к. {1} {2} {3} ", bool1, Math.Round(l, 4), bool2, Math.Round(r, 4))
    
    member this.EquationYX =
        String.Format("y = {0} + {1} * x", Math.Round(a, 4), Math.Round(b, 4))
    
    member this.confidenceIntervalB =
        let Sb = S / (sx * sqrt n1)
        let st = Utilities2.Student (n - 2.0) alf
        String.Format("Доверительный интервал {0} < b < {1}", b - st * Sb, b + st * Sb)
    
    member this.confidenceIntervalA =
        let Sa = S * sqrt (1.0 / n + mx ** 2.0 / (n1 * sx ** 2.0))
        let st = Utilities2.Student (n - 2.0) alf
        String.Format("Доверительный интервал {0} < a < {1}", a - st * Sa, a + st * Sa)
    
    member this.Xs = xs |> Seq.cast<obj> |> List
    
    member this.Ys = ys |> Seq.cast<obj> |> List
    
    member this.Fxs = fxs |> Seq.cast<obj> |> List
    
    member this.NyWs = nyws |> Seq.cast<obj> |> List