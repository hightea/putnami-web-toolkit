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

import com.google.gwt.text.shared.AbstractRenderer;

public final class StringRenderer extends AbstractRenderer<String> {

	private static StringRenderer instance;

	public static StringRenderer get() {
		if (StringRenderer.instance == null) {
			StringRenderer.instance = new StringRenderer();
		}
		return StringRenderer.instance;
	}

	public StringRenderer() {
	}

	@Override
	public String render(String object) {
		if (object == null) {
			return "";
		}

		return object;
	}
}
