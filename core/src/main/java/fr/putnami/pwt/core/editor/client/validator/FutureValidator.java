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

import com.google.gwt.i18n.client.DateTimeFormat;

import java.util.Date;

public class FutureValidator extends AbstractValidator<Date> {

	private static final DateTimeFormat DF = DateTimeFormat.getFormat("yyyy-MM-dd");

	public FutureValidator(String message) {
		super(message);
	}

	@Override
	protected boolean isValid(Date value) {
		if (value == null) {
			return true;
		}
		Date today = FutureValidator.DF.parse(FutureValidator.DF.format(new Date()));
		return !today.after(value);
	}

}
