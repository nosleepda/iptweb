using System;
using System.Threading.Tasks;
using MathStatistics;

namespace WebApplication.Data
{
    public class StatisticsService
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
            var data = Array.ConvertAll(dataString, StringToDouble);

            var s = new Hypothesis(data, a);
            return Task.FromResult(s);
        }
        
        private static double StringToDouble(string x)
        {
            return double.Parse(x,System.Globalization.CultureInfo.InvariantCulture);
        }
    }
}