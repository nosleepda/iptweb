using System;
using System.Collections.Generic;
using System.Linq;
using System.Threading.Tasks;
using MathStatistics;

namespace WebApplication.Data
{
    public class AnalysisService
    {
        public Task<(List<object> X, List<object> Y)> ConvertToGraphAsync(string xs, string ys)
        {
            var xString = xs.Replace(" ", "").Split(";");
            var yString = ys.Replace(" ", "").Split(";");
            var x = Array.ConvertAll(xString, StringToObject).ToList();
            var y = Array.ConvertAll(yString, StringToObject).ToList();
            return Task.FromResult((x,y));
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
            var x = Array.ConvertAll(xString, StringToDouble);
            var y = Array.ConvertAll(yString, StringToDouble);
            
            var s = new Analysis(x, y, alf: a);
            return Task.FromResult(s);
        }
        
        public Task<AnalysisTable> GetAnalysisTableAsync(string xs, string ys, string ns, double a)
        {
            if (string.IsNullOrEmpty(xs) || string.IsNullOrEmpty(ys) || string.IsNullOrEmpty(ns))
            {
                return Task.FromException<AnalysisTable>(new ArgumentNullException());
            }

            if (a <= 0)
            {
                return Task.FromException<AnalysisTable>(new ArithmeticException());
            }
            
            var xString = xs.Replace(" ", "").Split(";");
            var yString = ys.Replace(" ", "").Split(";");
            var x = Array.ConvertAll(xString, StringToDouble);
            var y = Array.ConvertAll(yString, StringToDouble);
            var ns2 = new double[x.Length,y.Length];
            var s = new AnalysisTable(x, y, ns2, a);
            return Task.FromResult(s);
        }

        private static object StringToObject(string x)
        {
            return x;
        }
        
        private static object DoubleToObject(double x)
        {
            return x;
        }
        
        private static double StringToDouble(string x)
        {
            return double.Parse(x,System.Globalization.CultureInfo.InvariantCulture);
        }
    }
}