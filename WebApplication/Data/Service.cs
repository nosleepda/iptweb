using System;
using System.Threading.Tasks;
using MathStatistics;

namespace WebApplication.Data
{
    public class Service
    {
        public Task<Hypothesis> GetStatisticsAsync(string rawData, double a)
        {
            if (string.IsNullOrEmpty(rawData) )
            {
                return Task.FromException<Hypothesis>(new ArgumentNullException());
            }

            if (a <= 0)
            {
                return Task.FromException<Hypothesis>(new ArithmeticException());
            }
            
            var dataString = rawData.Replace(" ", "").Split(";");
            var data = Array.ConvertAll(dataString, Utils.StringToDouble);

            var s = new Hypothesis(data, a);
            return Task.FromResult(s);
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
            
            var xString = xs.Replace(" ", "").Split(";");
            var yString = ys.Replace(" ", "").Split(";");
            var x = Array.ConvertAll(xString, Utils.StringToDouble);
            var y = Array.ConvertAll(yString, Utils.StringToDouble);
            
            var s = new Analysis(x, y, alf: a);
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
            
            var xString = xs.Replace(" ", "").Split(";");
            var yString = ys.Replace(" ", "").Split(";");
            var x = Array.ConvertAll(xString, Utils.StringToDouble);
            var y = Array.ConvertAll(yString, Utils.StringToDouble);
            var nsDouble = Utils.ConvertAll(ns, Utils.StringToDouble);
            var s = new AnalysisTable(x, y, nsDouble, a);
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
            
            var xString = xs.Replace(" ", "").Split(";");
            var yString = ys.Replace(" ", "").Split(";");
            var x = Array.ConvertAll(xString, Utils.StringToDouble);
            var y = Array.ConvertAll(yString, Utils.StringToDouble);
            
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
            
            var xString = xs.Replace(" ", "").Split(";");
            var yString = ys.Replace(" ", "").Split(";");
            var x = Array.ConvertAll(xString, Utils.StringToDouble);
            var y = Array.ConvertAll(yString, Utils.StringToDouble);
            var nsDouble = Utils.ConvertAll(ns, Utils.StringToDouble);
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

            var x1String = xs1.Replace(" ", "").Split(";");
            var x2String = xs2.Replace(" ", "").Split(";");
            var x3String = xs3.Replace(" ", "").Split(";");
            var yString = ys.Replace(" ", "").Split(";");
            var x1 = Array.ConvertAll(x1String, Utils.StringToDouble);
            var x2 = Array.ConvertAll(x2String, Utils.StringToDouble);
            var x3 = Array.ConvertAll(x3String, Utils.StringToDouble);
            var y = Array.ConvertAll(yString, Utils.StringToDouble);

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
            
            var xString = xs.Replace(" ", "").Split(";");
            var yString = ys.Replace(" ", "").Split(";");
            var x = Array.ConvertAll(xString, Utils.StringToDouble);
            var y = Array.ConvertAll(yString, Utils.StringToDouble);
            
            var s = new RankCorrelation(x, y, alf: a);
            return Task.FromResult(s);
        }
    }
}