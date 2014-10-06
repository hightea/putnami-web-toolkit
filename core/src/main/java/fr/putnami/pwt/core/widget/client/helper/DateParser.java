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

import com.google.gwt.i18n.client.DateTimeFormat;
import com.google.gwt.text.shared.Parser;

import java.text.ParseException;
import java.util.Date;

import fr.putnami.pwt.core.widget.client.constant.WidgetParams;

public final class DateParser implements Parser<Date> {

	private static DateParser instance;

	public static DateParser get() {
		if (DateParser.instance == null) {
			DateParser.instance = new DateParser(null);
		}
		return DateParser.instance;
	}

	private final DateTimeFormat formater;

	public DateParser(String format) {
		if (format == null) {
			this.formater = DateTimeFormat.getFormat(WidgetParams.Util.get().inputDateSimpleFormat());
		} else {
			this.formater = DateTimeFormat.getFormat(format);
		}
	}

	@Override
	public Date parse(CharSequence text) throws ParseException {
		if (text == null || "".equals(text.toString().trim())) {
			return null;
		}
		return this.formater.parseStrict(text.toString());
	}

	public Date parseOrNull(CharSequence text) {
		try {
			return this.parse(text);
		} catch (ParseException e) {
			return null;
		}
	}

}
