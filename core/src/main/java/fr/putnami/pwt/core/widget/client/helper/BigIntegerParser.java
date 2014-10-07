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

import com.google.gwt.i18n.client.NumberFormat;
import com.google.gwt.text.shared.Parser;

import java.math.BigInteger;
import java.text.ParseException;

public final class BigIntegerParser implements Parser<BigInteger> {
	private static BigIntegerParser instance;

	private BigIntegerParser() {
	}

	@Override
	public BigInteger parse(CharSequence object) throws ParseException {
		if ("".equals(object.toString())) {
			return null;
		}

		try {
			return BigInteger.valueOf((long) NumberFormat.getDecimalFormat().parse(object.toString()));
		} catch (NumberFormatException e) {
			throw new ParseException(e.getMessage(), 0);
		}
	}

	public static BigIntegerParser get() {
		if (BigIntegerParser.instance == null) {
			BigIntegerParser.instance = new BigIntegerParser();
		}
		return BigIntegerParser.instance;
	}
}
