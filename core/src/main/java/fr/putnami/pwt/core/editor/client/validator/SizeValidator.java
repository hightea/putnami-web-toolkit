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

import java.util.Collection;
import java.util.Map;

public class SizeValidator<T> extends AbstractValidator<T> {

	private final int min;
	private final int max;

	public SizeValidator(String message, int min, int max) {
		super(message);
		this.min = min;
		this.max = max;
	}

	@Override
	protected boolean isValid(T value) {
		if (value instanceof String) {
			return this.isLengthValid(((String) value).length());
		}
		if (value instanceof Collection) {
			return this.isLengthValid(((Collection<?>) value).size());
		}
		if (value instanceof Map) {
			return this.isLengthValid(((Map<?, ?>) value).size());
		}
		return true;
	}

	public int getMin() {
		return this.min;
	}

	public int getMax() {
		return this.max;
	}

	protected final boolean isLengthValid(int length) {
		return this.min <= length && length <= this.max;
	}

	@Override
	protected Object[] getParrameters() {
		return new Object[] {this.min, this.max};
	}
}
