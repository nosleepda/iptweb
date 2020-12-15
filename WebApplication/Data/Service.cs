using System;
using System.Collections.Generic;
using System.Threading.Tasks;
using Aglomera;
using Aglomera.Linkage;
using ExamplesUtil;
using MathStatistics;
using Newtonsoft.Json;

namespace WebApplication.Data
{
    public class Service
    {
        public Task<MathStatistics.DescriptiveStatistics> GetStatisticsAsync(string rawData, double a)
        {
            if (string.IsNullOrEmpty(rawData) )
            {
                return Task.FromException<MathStatistics.DescriptiveStatistics>(new ArgumentNullException());
            }

            if (a <= 0)
            {
                return Task.FromException<MathStatistics.DescriptiveStatistics>(new ArithmeticException());
            }
            
            var data = Utils.StringToNumber(rawData);
            var  result = new MathStatistics.DescriptiveStatistics(data, a);

            return Task.FromResult(result);
        }
        
        public Task<MathStatistics.DescriptiveStatistics> GetStatisticsAsync(double[] data, double a)
        {
            if (a <= 0)
            {
                return Task.FromException<MathStatistics.DescriptiveStatistics>(new ArithmeticException());
            }
            
            var  result = new MathStatistics.DescriptiveStatistics(data, a);

            return Task.FromResult(result);
        }
        
        public Task<Hypothesis> GetHypothesisAsync(string rawData, double a)
        {
            if (string.IsNullOrEmpty(rawData) )
            {
                return Task.FromException<Hypothesis>(new ArgumentNullException());
            }

            if (a <= 0)
            {
                return Task.FromException<Hypothesis>(new ArithmeticException());
            }
            
            var data = Utils.StringToNumber(rawData);
            var result = new Hypothesis(data, a);
            
            return Task.FromResult(result);
        }
        
        public Task<Hypothesis> GetHypothesisAsync(double[] data, double a)
        {
            if (a <= 0)
            {
                return Task.FromException<Hypothesis>(new ArithmeticException());
            }
            
            var result = new Hypothesis(data, a);
            
            return Task.FromResult(result);
        }
        
        public Task<Analysis> GetAnalysisAsync(string xs, string ys, double a)
        {
            if (string.IsNullOrEmpty(xs) || string.IsNullOrEmpty(ys))
            {
                return Task.FromException<Analysis>(new ArgumentNullException());
            }

            if (a <= 0)
            {
                return Task.FromException<Analysis>(new ArithmeticException());
            }

            var x = Utils.StringToNumber(xs);
            var y = Utils.StringToNumber(ys);
            
            var s = new Analysis(x, y, alf: a);
            return Task.FromResult(s);
        }
        
        public Task<Analysis> GetAnalysisAsync(double[] xs, double[] ys, double a)
        {
            if (a <= 0)
            {
                return Task.FromException<Analysis>(new ArithmeticException());
            }

            var s = new Analysis(xs, ys, alf: a);
            return Task.FromResult(s);
        }
        
        public Task<AnalysisTable> GetAnalysisTableAsync(string xs, string ys, string[,] ns, double a)
        {
            if (string.IsNullOrEmpty(xs) || string.IsNullOrEmpty(ys) || ns.Length == 0)
            {
                return Task.FromException<AnalysisTable>(new ArgumentNullException());
            }

            if (a <= 0)
            {
                return Task.FromException<AnalysisTable>(new ArithmeticException());
            }
            
            var nsDouble = Utils.StringToNumber(ns);
            var x = Utils.StringToNumber(xs);
            var y = Utils.StringToNumber(ys);
            
            var s = new AnalysisTable(x, y, nsDouble, a);
            return Task.FromResult(s);
        }
        
        public Task<AnalysisTable> GetAnalysisTableAsync(double[] xs, double[] ys, string[,] ns, double a)
        {
            if (ns.Length == 0)
            {
                return Task.FromException<AnalysisTable>(new ArgumentNullException());
            }

            if (a <= 0)
            {
                return Task.FromException<AnalysisTable>(new ArithmeticException());
            }

            var nsDouble = Utils.StringToNumber(ns);

            var s = new AnalysisTable(xs, ys, nsDouble, a);
            return Task.FromResult(s);
        }
        
        public Task<NonlinearDependencies> GetNonlinearDependenciesAsync(string xs, string ys, double a)
        {
            if (string.IsNullOrEmpty(xs) || string.IsNullOrEmpty(ys))
            {
                return Task.FromException<NonlinearDependencies>(new ArgumentNullException());
            }

            if (a <= 0)
            {
                return Task.FromException<NonlinearDependencies>(new ArithmeticException());
            }
            
            var x = Utils.StringToNumber(xs);
            var y = Utils.StringToNumber(ys);
            
            var s = new NonlinearDependencies(x, y, alf: a);
            return Task.FromResult(s);
        }
        
        public Task<NonlinearDependenciesTable> GetNonlinearDependenciesTableAsync(string xs, string ys, string[,] ns, double a)
        {
            if (string.IsNullOrEmpty(xs) || string.IsNullOrEmpty(ys) || ns.Length == 0)
            {
                return Task.FromException<NonlinearDependenciesTable>(new ArgumentNullException());
            }

            if (a <= 0)
            {
                return Task.FromException<NonlinearDependenciesTable>(new ArithmeticException());
            }
            
            var x = Utils.StringToNumber(xs);
            var y = Utils.StringToNumber(ys);
            var nsDouble = Utils.StringToNumber(ns);
            
            var s = new NonlinearDependenciesTable(x, y, nsDouble, a);
            return Task.FromResult(s);
        }
        
        public Task<MultipleRegression> GetMultipleRegressionAsync(string ys, string xs1, string xs2,
            string xs3, double a)
        {
            if (string.IsNullOrEmpty(xs1) || string.IsNullOrEmpty(xs2) || string.IsNullOrEmpty(xs3) ||
                string.IsNullOrEmpty(ys))
            {
                return Task.FromException<MultipleRegression>(new ArgumentNullException());
            }

            if (a <= 0)
            {
                return Task.FromException<MultipleRegression>(new ArithmeticException());
            }

            var x1 = Utils.StringToNumber(xs1);
            var x2 = Utils.StringToNumber(xs2);
            var x3 = Utils.StringToNumber(xs3);
            var y = Utils.StringToNumber(ys);

            var s = new MultipleRegression(y, x1, x2, x3, alf: a);
            return Task.FromResult(s);
        }
        
        public Task<RankCorrelation> GetRankCorrelationAsync(string xs, string ys, double a)
        {
            if (string.IsNullOrEmpty(xs) || string.IsNullOrEmpty(ys))
            {
                return Task.FromException<RankCorrelation>(new ArgumentNullException());
            }

            if (a <= 0)
            {
                return Task.FromException<RankCorrelation>(new ArithmeticException());
            }
            
            var x = Utils.StringToNumber(xs);
            var y = Utils.StringToNumber(ys);
            
            var s = new RankCorrelation(x, y, alf: a);
            return Task.FromResult(s);
        }
        
        public Task<string> GetClusterAnalysisAsync(IReadOnlyCollection<string[]> data)
        {
            if (data.Count == 0)
            {
                return Task.FromException<string>(new ArgumentNullException());
            }

            var instance = Utils.ClusterParse(data);
            var linkage = new SingleLinkage<DataPoint>(new DataPoint(null, null));
            var clusteringAlg = new AgglomerativeClusteringAlgorithm<DataPoint>(linkage);
            var clustering = clusteringAlg.GetClustering(instance);
            var clustersJson = clustering.GetDendrogramJson( true, Formatting.Indented);

            return Task.FromResult(clustersJson);
        }
    }
}