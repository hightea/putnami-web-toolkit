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

import com.google.gwt.dom.client.LIElement;
import com.google.gwt.user.client.ui.IsWidget;

import fr.putnami.pwt.core.widget.client.base.AbstractWidget;
import fr.putnami.pwt.core.widget.client.base.CssStyle;
import fr.putnami.pwt.core.widget.client.base.SimpleStyle;
import fr.putnami.pwt.core.widget.client.util.StyleUtils;

public class NavDivider extends AbstractWidget implements Nav.IsNavContent {

	private static final CssStyle STYLE_DIVIDER = new SimpleStyle("divider");

	public NavDivider() {
		super(LIElement.TAG);
		StyleUtils.addStyle(this, STYLE_DIVIDER);
	}

	public NavDivider(NavDivider source) {
		super(source);
	}

	@Override
	public IsWidget cloneWidget() {
		return new NavDivider(this);
	}

	@Override
	public boolean isActive() {
		return false;
	}

	@Override
	public void setActive(boolean active) {
		// Do Nothing
	}
}
