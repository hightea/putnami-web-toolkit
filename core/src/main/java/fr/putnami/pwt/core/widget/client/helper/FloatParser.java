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
package fr.putnami.pwt.core.widget.client.helper;

import java.text.ParseException;

import com.google.gwt.i18n.client.NumberFormat;
import com.google.gwt.text.shared.Parser;

public class FloatParser implements Parser<Float> {
	private static FloatParser instance;

	public static FloatParser get() {
		if (instance == null) {
			instance = new FloatParser();
		}
		return instance;
	}

	private FloatParser() {
	}

	@Override
	public Float parse(CharSequence object) throws ParseException {
		if ("".equals(object.toString())) {
			return null;
		}

		try {
			return (float) NumberFormat.getDecimalFormat().parse(object.toString());
		}
		catch (NumberFormatException e) {
			throw new ParseException(e.getMessage(), 0);
		}
	}
}
