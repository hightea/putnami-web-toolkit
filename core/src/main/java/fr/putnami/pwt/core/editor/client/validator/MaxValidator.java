/**
 * This file is part of pwt.
 *
 * pwt is free software: you can redistribute it and/or modify it under the terms of the GNU Lesser
 * General Public License as published by the Free Software Foundation, either version 3 of the
 * License, or (at your option) any later version.
 *
 * pwt is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the
 * implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Lesser
 * General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License along with pwt. If not,
 * see <http://www.gnu.org/licenses/>.
 */
package fr.putnami.pwt.core.editor.client.validator;

import java.math.BigDecimal;
import java.math.BigInteger;

public class MaxValidator extends AbstractValidator<Number> {

	private long max;

	public MaxValidator(String message, Number max) {
		super(message);
		this.max = max.longValue();
	}

	@Override
	protected final boolean isValid(Number value) {
		if (value == null) {
			return true;
		}
		if (value instanceof BigDecimal) {
			return ((BigDecimal) value).compareTo(BigDecimal.valueOf(this.max)) <= 0;
		} else if (value instanceof BigInteger) {
			return ((BigInteger) value).compareTo(BigInteger.valueOf(this.max)) <= 0;
		} else {
			return value.longValue() <= this.max;
		}
	}

	@Override
	protected Object[] getParrameters() {
		return new Object[] {this.max};
	}

}
