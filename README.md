#Factor-GARCH Model

This project seeks to present a concise framework for estimating conditional covariance matrices within high-dimensional contexts.

The Factor-GARCH model shares notable similarities with the Go-GARCH model, but it operates under the assumption that the data possess a strict factor structure. In this context, the covariance between two series depends on factor variances and their loadings in these factors. Factor estimation is conducted following the influential work of Stock and Watson (2002), and the number of factors can be consistently estimated using Bai and Ng information criteria.

Our estimates hinge on the assumption that the stochastic process exhibits a strict factor structure. We impose a normalization such that the unconditional covariance matrix of the factors is the identity. Additionally, we assume that the conditional covariance matrix given a set of information is diagonal, and factor loadings are time-invariant.

To illustrate the utility of this methodology, we provide an example wherein we estimate Value at Risk (VaR) for a portfolio comprising 272 assets, each with random time-varying weights.

Factor-GARCH offers significant flexibility, allowing users to choose any univariate specification for the conditional variance of the factors available in the rugarch package. Moreover, with an adaptation of the main code, it is conceivable that this specification could also be compatible with stochastic volatility models for the factors. Although this potential extension is not certain, it is plausible that someone may have explored it within the literature on conditional covariance matrices.

It is crucial to note that this methodology is suitable only in scenarios where there is reasonable justification for the assumption that at least an approximate factor structure holds. In contexts where a sparse covariance matrix is present, this methodology becomes entirely inadequate.
