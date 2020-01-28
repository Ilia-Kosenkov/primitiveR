#include "imports.h"
using namespace Rcpp;

SEXP primR_are_equal_f(SEXP x, SEXP y, SEXP eps) {
	const auto x_vals = as<std::vector<double>>(x);
	const auto y_vals = as<std::vector<double>>(y);
	const auto eps_val = as<double>(eps);

	std::vector<bool> result(x_vals.size());
	for (size_t i = 0; i < x_vals.size(); i++) {
		if (std::isnan(x_vals[i]) || std::isnan(y_vals[i]))
			result[i] = false;
		else if (std::isinf(x_vals[i]) || std::isinf(y_vals[i]))
			result[i] = x_vals[i] == y_vals[i];
		else 
		{
			const auto x_abs = fabs(x_vals[i]);
			const auto y_abs = fabs(y_vals[i]);

			if (x_abs == 0.0)
				result[i] = y_abs < eps_val;
			else if (y_abs == 0.0)
				result[i] = x_abs < eps_val;
			else
				result[i] = fabs(x_vals[i] - y_vals[i]) < eps_val * std::max(x_abs, y_abs);
		}
	}

	return wrap(result);
}