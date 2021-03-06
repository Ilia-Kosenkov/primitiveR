#include <cpp11.hpp>
#include <cmath>
using namespace cpp11;

[[cpp11::register]]
logicals primR_are_equal_f(doubles x, doubles y, double eps) {

	const size_t len = x.size();
	writable::logicals result(len);
	
	for (size_t i = 0; i < len; i++) {
		if (std::isnan(x[i]) || std::isnan(y[i]))
			result[i] = false;
		else if (std::isinf(x[i]) || std::isinf(y[i]))
			result[i] = x[i] == y[i];
		else 
		{
			const auto x_abs = fabs(x[i]);
			const auto y_abs = fabs(y[i]);

			if (x_abs == 0.0)
				result[i] = y_abs < eps;
			else if (y_abs == 0.0)
				result[i] = x_abs < eps;
			else
				result[i] = fabs(x[i] - y[i]) < eps * std::max(x_abs, y_abs);
		}
	}

	return result;
}