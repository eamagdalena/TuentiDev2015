package tuenti;

import java.util.Vector;

public class XCorrelationJavax {

	static int min(int x, int y) {
		return (x < y ? x : y);
	}

	static int max(int x, int y) {
		return (x > y ? x : y);
	}
	
	private final static double THRESCORR = 1e-30; 
 

	public static Double[] crosscorr(final double[] x, final double y[]){

		//! Calculate the mean of the two series x[], y[]
		double xMean = 0.0;
		for (int i = 0; i < x.length; i++) {
		    if (x[i] >= 0.0 && x[i] <= 15.0)
			xMean += x[i] / x.length;
		}

		double yMean = 0.0;
		for (int i = 0; i < y.length; i++) {
		    if (y[i] >= 0.0 && y[i] <= 15.0)
			yMean += y[i] / y.length;
		}

		//! Calculate the denominator (product of standard deviations)
		double xSumCuadraticDiff = 0.0;
		for (int i = 0; i < x.length; i++) {
			xSumCuadraticDiff += Math.pow(x[i] - xMean, 2);
		}
		
		double ySumCuadraticDiff = 0.0;
		for (int i = 0; i < y.length; i++) {
			ySumCuadraticDiff += Math.pow(y[i] - yMean, 2);
		}

		double denom = Math.sqrt(xSumCuadraticDiff * ySumCuadraticDiff);
		if (denom < THRESCORR){
			return new Double[0];
		}

		//! Calculate the correlation series
		Double[] xcorr = new Double[y.length - x.length + 1];

		for (int delay = 0; delay < xcorr.length; delay++) {
			double xySum = 0.0;
			for (int i = 0; i < x.length; i++) {
				xySum += (x[i] - xMean) * (y[i + delay ] - yMean);
			}

			xcorr[delay] = xySum / denom;
		}	
		return xcorr;
	}

	double findScore(final Double[] wave, final Double[] pattern, int patternSize){
		double score = 0.0;
		int minSubvectorLength = 2;

		for (int subvectorStart = 0; subvectorStart <= waveSize - minSubvectorLength; subvectorStart++) {
			for (int subvectorLength = minSubvectorLength; subvectorLength <= MIN(waveSize - subvectorStart, patternSize); subvectorLength++) { 
				double[] xcorrelation = crosscorr(&(wave[subvectorStart]), subvectorLength, pattern, patternSize);
					
				for (int xcorrelationIndex = 0; xcorrelationIndex < xcorrelation.size(); xcorrelationIndex++) {
					score = MAX(score, xcorrelation[xcorrelationIndex] * subvectorLength);
				}	
	    		}
		}

		return score;
	}

	
	
}
