namespace DescriptiveStatistics.Models

type Analysis = {
    MeanX: double
    MeanY: double
    Correlation: double
    Sx: double
    Sy: double
    ConfidenceIntervalCorrelation: string
    SignificanceCorrelation: string
    EquationYX: string
    EquationXY: string
    Determination: double
    SignificanceDetermination: string
    ConfidenceIntervalB: string
    ConfidenceIntervalA: string
    Elasticity: double
}