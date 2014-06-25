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

import com.google.gwt.dom.client.DivElement;

import fr.putnami.pwt.core.theme.client.CssStyle;
import fr.putnami.pwt.core.widget.client.Button.Size;
import fr.putnami.pwt.core.widget.client.Button.Type;
import fr.putnami.pwt.core.widget.client.base.SimpleDropdown;
import fr.putnami.pwt.core.widget.client.base.SimpleStyle;
import fr.putnami.pwt.core.widget.client.util.StyleUtils;

public class ButtonDropdown extends SimpleDropdown {

	private static final CssStyle STYLE_BUTTON = new SimpleStyle("btn");
	public static final CssStyle STYLE_BUTTON_GROUP = new SimpleStyle("btn-group");

	private Type type = Type.DEFAULT;
	private Size size = Size.DEFAULT;

	public ButtonDropdown() {
		super(DivElement.TAG);
		StyleUtils.addStyle(this, STYLE_BUTTON_GROUP);
		StyleUtils.addStyle(getAnchor(), STYLE_BUTTON);
		setType(type);
	}

	public ButtonDropdown(String label) {
		this();
		setLabel(label);
	}

	public void setType(Type type) {
		this.type = type;
		StyleUtils.addStyle(getAnchor(), this.type);
	}

	public void setSize(Size size) {
		this.size = size;
		StyleUtils.addStyle(getAnchor(), this.size);
	}
}
