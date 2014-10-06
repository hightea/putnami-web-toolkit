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

import java.math.BigDecimal;

public final class BigDecimalRenderer extends AbstractRenderer<BigDecimal> {
	private static BigDecimalRenderer instance;

	public static BigDecimalRenderer get() {
		if (instance == null) {
			instance = new BigDecimalRenderer();
		}
		return instance;
	}

	private BigDecimalRenderer() {
	}

	@Override
	public String render(BigDecimal object) {
		if (object == null) {
			return "";
		}

		return NumberFormat.getDecimalFormat().format(object);
	}
}
