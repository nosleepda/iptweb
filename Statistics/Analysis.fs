namespace Statistics

open System
open System.Collections.Generic
open MathNet.Numerics.Distributions

type Analysis(xsArr: double[], ysArr: double[], alf: double) =
    let xs = List.ofArray xsArr
    
    let ys = List.ofArray ysArr
    
    let n = xs.Length |> double
    
    let n1 = n - 1.0
    
    let n2 = n - 2.0
    
    let div y = y / n
    
    let div1 y = y / n1
    
    let div2 y = y / n2
    
    let mx = List.sum xs |> div
    
    let my = List.sum ys |> div
    
    let r =
        let r1 = xs |> List.map2 (fun elem1 elem2 -> elem1 * elem2) ys |> List.sum |> (+) -(n * my * mx)
        let r2 = xs |> List.map (fun x -> (x - mx) ** 2.0) |> List.sum |> sqrt
        let r3 = ys |> List.map (fun y -> (y - my) ** 2.0) |> List.sum |> sqrt
        
        r1 / (r2 * r3)
    
    let sx = 
        xs
        |> List.map (fun x -> (x - mx) ** 2.0)
        |> List.sum
        |> div1 
        |> sqrt
    
    let sy =
        ys
        |> List.map (fun y -> (y - my) ** 2.0)
        |> List.sum
        |> div1 
        |> sqrt
    
    let cb = r * sy / sx
    
    let cb1 = r * sx / sy
    
    let ca1 = mx - my * r * sx / sy
    
    let ca = my - mx * r * sy / sx
    
    let S = List.map2 (fun x y -> (y - ca - cb * x) ** 2.0) xs ys |> List.sum |> div2 |> sqrt
    
    
    let f x = ca + cb * x
        
    member this.Xs = xs |> Seq.cast<obj> |> List 
        
    member this.Ys = ys |> Seq.cast<obj> |> List
    
    member this.YsLin = xs |> List.map (fun x -> f x) |>  Seq.cast<obj> |> List

    member this.MeanX = Math.Round(mx, 4)
    
    member this.MeanY = Math.Round(my, 4)
    
    member this.Sx = Math.Round(sx, 4)
    
    member this.Sy = Math.Round(sy, 4)
    
    member this.Cr = Math.Round(r, 4)
    
    member this.ConfidenceIntervalP =
        let l = Math.Atanh r - (Utilities.NormalInv alf) / sqrt(n - 3.0) |> tanh
        let r = Math.Atanh r + (Utilities.NormalInv alf) / sqrt(n - 3.0) |> tanh
        String.Format("Доверительный интервал коэффициента корреляции {0} < r < {1} ", Math.Round(l, 4), Math.Round(r, 4))
    
    member this.SignificanceCr =
        let l = abs r / sqrt (1.0 - r ** 2.0) * sqrt n2
        let r = Utilities.Student n2 alf
        let bool = if l > r then "" else "не"
        let bool2 = if l > r then ">" else "<"
        String.Format("X и Y {0} зависимы, \nт.к. {1} {2} {3} ", bool, Math.Round(l, 4), bool2, Math.Round(r, 4))
    
    member this.EquationYX =
        String.Format("y = {0} + {1} * x", Math.Round(ca, 4), Math.Round(cb, 4))
    
    member this.EquationXY =
        String.Format("x = {0} + {1} * y", Math.Round(ca1, 4), Math.Round(cb1, 4))
    
    member this.R2 =
        let temp = List.map2 (fun x y -> (y - f x) ** 2.0) xs ys |> List.sum
        let r2 = List.map (fun y -> (y - my) ** 2.0) ys |> List.sum |> (/) temp |> (-) 1.0
        Math.Round(r2, 4)
    
    member this.SignificanceFisherSnedecor =
        let l = this.R2 * n2 / (1.0 - this.R2)
        let r = FisherSnedecor.InvCDF(1.0,n2,1.0 - alf)
        let bool = if l > r then "" else "не"
        let bool2 = if l > r then ">" else "<"
        String.Format("Уравнение статистически {0} значимо, \nт.к. {1} {2} {3} ", bool, Math.Round(l, 4), bool2, Math.Round(r, 4))
    
    member this.confidenceIntervalB =
        let Sb = S / (this.Sx * sqrt n1)
        let l = cb - Utilities.Student n2 0.05 * Sb
        let r =  cb + Utilities.Student n2 0.05 * Sb
        String.Format("Доверительный интервал {0} < b < {1} ", Math.Round(l, 4),  Math.Round(r, 4))
    
    member this.confidenceIntervalA =
        let Sa = S * sqrt (1.0 / n + this.MeanX ** 2.0 / (n1 * this.Sx ** 2.0))
        let l = ca - Utilities.Student n2 0.05 * Sa
        let r = ca + Utilities.Student n2 0.05 * Sa
        String.Format("Доверительный интервал {0} < a < {1} ", Math.Round(l, 4), Math.Round(r, 4))
    
    member this.K =
        Math.Round(cb * mx / my, 4)
    