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

import java.util.Date;

import com.google.gwt.i18n.client.DateTimeFormat;
import com.google.gwt.text.shared.AbstractRenderer;

import fr.putnami.pwt.core.widget.client.constant.WidgetParams;

public final class DateRenderer extends AbstractRenderer<Date> {

	private static DateRenderer instance;

	public static DateRenderer get() {
		if (instance == null) {
			instance = new DateRenderer(null);
		}
		return instance;
	}

	private final DateTimeFormat formater;

	public DateRenderer(String format) {
		if (format == null) {
			formater = DateTimeFormat.getFormat(WidgetParams.Util.get().inputDateSimpleFormat());
		}
		else {
			formater = DateTimeFormat.getFormat(format);
		}
	}

	@Override
	public String render(Date object) {
		if (object == null) {
			return "";
		}

		return formater.format(object);
	}
}
