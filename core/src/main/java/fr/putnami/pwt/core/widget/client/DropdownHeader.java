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
package fr.putnami.pwt.core.widget.client;

import com.google.gwt.dom.client.LIElement;
import com.google.gwt.user.client.ui.IsWidget;

import fr.putnami.pwt.core.theme.client.CssStyle;
import fr.putnami.pwt.core.widget.client.base.AbstractWidget;
import fr.putnami.pwt.core.widget.client.base.SimpleStyle;
import fr.putnami.pwt.core.widget.client.util.StyleUtils;

public class DropdownHeader extends AbstractWidget {
	private static final CssStyle STYLE_HEADER = new SimpleStyle("dropdown-header");

	private String label;

	public DropdownHeader() {
		super(LIElement.TAG);
		StyleUtils.addStyle(this, DropdownHeader.STYLE_HEADER);
	}

	public DropdownHeader(String label) {
		this();
		this.setLabel(label);
	}

	public DropdownHeader(DropdownHeader source) {
		super(source);
		this.setLabel(source.label);
	}

	@Override
	public IsWidget cloneWidget() {
		return new DropdownHeader(this);
	}

	public String getLabel() {
		return this.label;
	}

	public void setLabel(String label) {
		this.label = label;
		this.getElement().setInnerText(label);
	}
}
