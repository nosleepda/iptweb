namespace MathStatistics

open System
open System.Collections.Generic

type DescriptiveStatistics(sourceData: double[], alf: double) =    
           
    member this.Data = StatisticsData(sourceData)
    
    member this.Mean =
        let mutable sum = 0.0
        for x in this.Data.DictData do
            sum <- sum + x.Key * double x.Value

        Math.Round(sum / this.Data.NDouble, 4)
    
    member this.Variance =
        let mutable sum = 0.0
        for x in this.Data.DictData do
            sum <- sum + ((this.Mean - x.Key) ** 2.0) * (double x.Value)

        sum <- sum / double (if this.Data.N > 50 then this.Data.N - 1 else this.Data.N)

        Math.Round(sum, 4)
    
    member this.StandardDeviation =
        Math.Round(sqrt this.Variance, 4)
    
    member this.Mode =
        let max = this.Data.DictData.Values |> List.ofSeq |> List.max
        let mutable key = 0.0
        for x in this.Data.DictData do
            if max = x.Value then key <- x.Key
        Math.Round(key, 4)
    
    member this.Median: double =
        let mutable m = 0.0
        if this.Data.N % 2 = 0 then
            let kk = this.Data.N / 2
            m <- (this.Data.SortedData.Item kk + this.Data.SortedData.Item(kk + 1)) / 2.0
        else
            let kk = (this.Data.N - 1) / 2
            m <- this.Data.SortedData.Item(kk + 1)
        Math.Round(m, 4)
    
    member this.CoefficientVariation =
        Math.Round(this.StandardDeviation / this.Mean, 4)
    
    member this.Kurtosis =
        let mutable sum = 0.0
        for x in this.Data.DictData do
            sum <- sum + ((x.Key - this.Mean) ** 4.0) * (double x.Value)

        sum <- sum / this.Data.NDouble
        
        Math.Round(sum / (this.StandardDeviation ** 4.0) - 3.0, 4)
        
    member this.Skewness =
        let mutable sum = 0.0
        for x in this.Data.DictData do
            sum <- sum + ((x.Key - this.Mean) ** 3.0) * (double x.Value)
            
        sum <- sum / this.Data.NDouble

        Math.Round(sum / (this.StandardDeviation ** 3.0), 4)
        
    member this.ConfidenceIntervalMean =
        let t = Utilities.Student (this.Data.NDouble - 1.0) alf
        let temp = t * this.StandardDeviation / (sqrt (this.Data.NDouble))
        String.Format("{0} < M < {1}", Math.Round(this.Mean - temp, 4), Math.Round(this.Mean + temp, 4))
    
    member this.ConfidenceIntervalVariance =
        let n1 = (this.Data.NDouble - 1.0)
        let temp a =
            this.Variance * n1 / Utilities.ChiSquared n1 (a / 2.0)
        let l = sqrt (temp (1.0 + alf))
        let r = sqrt (temp (1.0 - alf))
        String.Format("{0} < D < {1}", Math.Round(l, 4), Math.Round(r, 4))
    
    member this.Ws =
        let n = this.Data.NDouble
        this.Data.FrequenciesDiscrete
        |> List.ofSeq
        |> List.map (fun x -> double x / n)
        
    member this.Cumulate =
        let da = List<float>(this.Ws.Length)
        da.Add this.Ws.Head
        for x = 1 to this.Ws.Length - 1 do
            da.Add(da.[x - 1] + this.Ws.[x])
        da
    
    member this.CumulateObj = this.Cumulate |> Seq.cast<obj> |> List 