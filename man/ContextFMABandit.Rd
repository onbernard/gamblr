% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/bandit_context_fma.R
\name{ContextFMABandit}
\alias{ContextFMABandit}
\title{R6 Class representing a contextual bandit with finitely many actions.}
\description{
A \code{\link{ContextFMABandit}} object
}
\details{
A FMABandit object is instantiated

  Policies can be simulated against the arms using

  Plot of cumulative regret can be generated using \code{\link{plot_regret}}
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-new}{\code{ContextFMABandit$new()}}
\item \href{#method-run_linucb_policy}{\code{ContextFMABandit$run_linucb_policy()}}
\item \href{#method-clone}{\code{ContextFMABandit$clone()}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-new"></a>}}
\if{latex}{\out{\hypertarget{method-new}{}}}
\subsection{Method \code{new()}}{
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{ContextFMABandit$new(dt, rewards)}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-run_linucb_policy"></a>}}
\if{latex}{\out{\hypertarget{method-run_linucb_policy}{}}}
\subsection{Method \code{run_linucb_policy()}}{
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{ContextFMABandit$run_linucb_policy(lambda, delta)}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-clone"></a>}}
\if{latex}{\out{\hypertarget{method-clone}{}}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{ContextFMABandit$clone(deep = FALSE)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{deep}}{Whether to make a deep clone.}
}
\if{html}{\out{</div>}}
}
}
}
