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

import java.util.Collection;

import com.google.gwt.regexp.shared.MatchResult;
import com.google.gwt.regexp.shared.RegExp;

public class PatternValidator<T> extends AbstracValidator<T> {

	private RegExp pattern;
	private String regexp;

	public PatternValidator(String message, String regexp) {
		super(message);
		String flagString = "";
		this.pattern = RegExp.compile(regexp, flagString);
		this.regexp = regexp;
	}

	@Override
	protected boolean isValid(T value) {
		if (value == null) {
			return true;
		}
		if (value instanceof String) {
			return regExMatch((String) value);
		}
		if (value instanceof Collection) {
			for (Object val : (Collection) value) {
				if (!(val instanceof String)) {
					return false;
				}
				if (!regExMatch((String) val)) {
					return false;
				}
			}
			return true;
		}
		return false;
	}

	private boolean regExMatch(String value) {
		MatchResult match = pattern.exec(value);
		if (match == null) {
			return false;
		}
		return match.getGroup(0).length() == value.length();
	}

	@Override
	protected Object[] getParrameters() {
		return new Object[] {
			regexp
		};
	}

}
