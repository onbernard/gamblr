% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/bandit_fma.R
\name{FMABandit}
\alias{FMABandit}
\title{R6 Class handling a set of stochastic arms}
\description{
A \code{\link{FMABandit}} object holds a set of of arms
  represented by their mean and generated reward matrix. It provides methods
  to run policies against these arms.
}
\details{
A FMABandit object is instantiated using the horizon that the
  simulations will span. Arms are supplied via a corresponding reward vector
  through the \code{\link{add_arm}} method.

  Policies can be simulated against the arms using
  \code{\link{run_ucb_policy}} or \code{\link{run_eps_greedy_policy}}.

  Plot of cumulative regret can be generated using \code{\link{plot_regret}}
}
\section{Public fields}{
\if{html}{\out{<div class="r6-fields">}}
\describe{
\item{\code{arms_means}}{Numeric vector containing each arm's mean.}

\item{\code{horizon}}{Integer specifying the number of time steps to run.}

\item{\code{rewards}}{Matrix containing arms rewards. Time step on rows, arms on
columns.}

\item{\code{K}}{Number of arms.}
}
\if{html}{\out{</div>}}
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-new}{\code{FMABandit$new()}}
\item \href{#method-add_arm}{\code{FMABandit$add_arm()}}
\item \href{#method-run_ucb_policy}{\code{FMABandit$run_ucb_policy()}}
\item \href{#method-run_eps_greedy_policy}{\code{FMABandit$run_eps_greedy_policy()}}
\item \href{#method-plot_regret}{\code{FMABandit$plot_regret()}}
\item \href{#method-clone}{\code{FMABandit$clone()}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-new"></a>}}
\if{latex}{\out{\hypertarget{method-new}{}}}
\subsection{Method \code{new()}}{
Create a new BinaryBandit object.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{FMABandit$new(horizon = 1000L)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{horizon}}{Number of time steps. Determines height of reward matrix.}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
A new `FMABandit` object.
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-add_arm"></a>}}
\if{latex}{\out{\hypertarget{method-add_arm}{}}}
\subsection{Method \code{add_arm()}}{
Adds an arm using its reward vector.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{FMABandit$add_arm(R)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{R}}{Rewards vector.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-run_ucb_policy"></a>}}
\if{latex}{\out{\hypertarget{method-run_ucb_policy}{}}}
\subsection{Method \code{run_ucb_policy()}}{
Runs a set of UCB policies against the arms.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{FMABandit$run_ucb_policy(alpha, verbose = TRUE)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{alpha}}{Numeric vector specifying UCB policies parameters.}

\item{\code{verbose}}{Boolean specifying the verbose mode of simulation.}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
A list of simulation result. An item of this list consist of a
  policy name, the result of the simulation of that policy and the system
  time the simulation took.
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-run_eps_greedy_policy"></a>}}
\if{latex}{\out{\hypertarget{method-run_eps_greedy_policy}{}}}
\subsection{Method \code{run_eps_greedy_policy()}}{
Runs a set of Epsilon Greedy policies against the arms.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{FMABandit$run_eps_greedy_policy(epsilon, verbose = TRUE)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{epsilon}}{Numeric vector specifying Epsilon Greedy policies
parameters.}

\item{\code{verbose}}{Boolean specifying the verbose mode of simulation.}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
A list of simulation result. An item of this list consist of a
  policy name, the result of the simulation of that policy and the system
  time the simulation took.
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-plot_regret"></a>}}
\if{latex}{\out{\hypertarget{method-plot_regret}{}}}
\subsection{Method \code{plot_regret()}}{
Plots the cumulative regret of a result.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{FMABandit$plot_regret(result)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{result}}{A simulation as returned by a policy, i.e. the result field
of a run_ucb_policy output item.}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
A plot with as its x axis the time steps and as its y axis the
  cumulative regret.
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-clone"></a>}}
\if{latex}{\out{\hypertarget{method-clone}{}}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{FMABandit$clone(deep = FALSE)}\if{html}{\out{</div>}}
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
