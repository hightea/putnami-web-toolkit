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
package fr.putnami.pwt.core.widget.client.constant;

import com.google.gwt.core.shared.GWT;
import com.google.gwt.i18n.client.Constants;

public interface WidgetParams extends Constants {
	final class Util {
		private static WidgetParams instance;

		private Util() {
		}

		public static WidgetParams get() {
			if (Util.instance == null) {
				Util.instance = GWT.create(WidgetParams.class);
			}
			return Util.instance;
		}
	}

	@DefaultStringValue("yyyy-MM-dd")
	String inputDateSimpleFormat();

	@DefaultStringValue("MMMM yyyy")
	String inputDatePickerMonthYearFormat();

	@DefaultStringValue("MMM")
	String inputDatePickerMonthFormat();

	@DefaultBooleanValue(true)
	boolean inputFileProgressEnable();

	@DefaultIntValue(1000)
	int inputFileProgressHideDelay();
}
