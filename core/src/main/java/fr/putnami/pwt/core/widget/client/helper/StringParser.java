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
package fr.putnami.pwt.core.widget.client.helper;

import com.google.gwt.text.shared.Parser;

import java.text.ParseException;

public final class StringParser implements Parser<String> {
	private static StringParser instance;

	private StringParser() {
	}

	@Override
	public String parse(CharSequence object) throws ParseException {
		if (object == null || "".equals(object.toString().trim())) {
			return null;
		}
		return object.toString();
	}

	public static StringParser get() {
		if (StringParser.instance == null) {
			StringParser.instance = new StringParser();
		}
		return StringParser.instance;
	}

}
