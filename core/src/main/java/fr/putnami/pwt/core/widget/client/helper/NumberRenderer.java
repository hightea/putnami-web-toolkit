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

import com.google.gwt.i18n.client.NumberFormat;
import com.google.gwt.text.shared.AbstractRenderer;

public final class NumberRenderer extends AbstractRenderer<Number> {

	private static NumberRenderer instance;

	public static NumberRenderer get() {
		if (instance == null) {
			instance = new NumberRenderer(null);
		}
		return instance;
	}

	private final NumberFormat formater;

	public NumberRenderer(String format) {
		if (format == null) {
			formater = NumberFormat.getDecimalFormat();
		}
		else {
			formater = NumberFormat.getFormat(format);
		}
	}

	@Override
	public String render(Number object) {
		if (object == null) {
			return "";
		}

		return formater.format(object);
	}
}
