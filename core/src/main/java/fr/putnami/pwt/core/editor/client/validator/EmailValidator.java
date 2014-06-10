/**
 * This file is part of pwt.
 *
 * pwt is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * pwt is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with pwt.  If not, see <http://www.gnu.org/licenses/>.
 */
package fr.putnami.pwt.core.editor.client.validator;

import com.google.gwt.regexp.shared.MatchResult;
import com.google.gwt.regexp.shared.RegExp;

public class EmailValidator extends AbstracValidator<String> {

	private static final String EMAIL_VIOLATION_MESSAGE = "constraintsEmail";
	private static final RegExp EMAIL_PATTERN = RegExp.compile("^[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\\.[a-zA-Z]{2,4}$", "");

	public EmailValidator() {
		super(EMAIL_VIOLATION_MESSAGE);
	}

	@Override
	protected boolean isValid(String value) {
		if (value == null || value.length() == 0) {
			return true;
		}
		MatchResult match = EMAIL_PATTERN.exec(value);
		if (match == null) {
			return false;
		}
		return match.getGroup(0).length() == value.length();
	}

	@Override
	protected Object[] getParrameters() {
		return new Object[] {
				EMAIL_PATTERN
		};
	}

}
