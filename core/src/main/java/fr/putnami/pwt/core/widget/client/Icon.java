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
package fr.putnami.pwt.core.widget.client;

import com.google.gwt.dom.client.Document;
import com.google.gwt.user.client.ui.Widget;

import fr.putnami.pwt.core.theme.client.CssStyle;
import fr.putnami.pwt.core.theme.client.ThemeController;
import fr.putnami.pwt.core.widget.client.base.SimpleStyle;
import fr.putnami.pwt.core.widget.client.util.StyleUtils;

public class Icon extends Widget {

	private static final CssStyle STYLE_LIGHT = new SimpleStyle("light");

	public enum Color implements CssStyle {
		DEFAULT("text-default"),
		MUTED("text-muted"),
		INFO("text-info"),
		SUCCESS("text-success"),
		WARNING("text-warning"),
		DANGER("text-danger");

		private final String style;

		private Color(String style) {
			this.style = style;
		}

		@Override
		public String get() {
			return style;
		}

	}

	private static final String ICON_TAG_NAME = "i";

	private String type;
	private Color color;

	private boolean light;

	public Icon() {
		this.setElement(Document.get().createElement(Icon.ICON_TAG_NAME));
	}

	public void setType(String type) {
		StyleUtils.addStyle(this, ThemeController.get().getIconStyle(type));
		this.type = type;
	}

	public String getType() {
		return type;
	}

	public Color getColor() {
		return color;
	}

	public void setColor(Color color) {
		StyleUtils.addStyle(this, color);
		this.color = color;
	}

	public boolean isLight() {
		return light;
	}

	public void setLight(boolean light) {
		StyleUtils.toggleStyle(this, STYLE_LIGHT, light);
		this.light = light;
	}
}
