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
import com.google.gwt.event.logical.shared.ValueChangeEvent;
import com.google.gwt.event.logical.shared.ValueChangeHandler;
import com.google.gwt.user.client.History;
import com.google.gwt.user.client.ui.IsWidget;

import fr.putnami.pwt.core.widget.client.Nav.LinkStyle;
import fr.putnami.pwt.core.widget.client.base.SimpleDropdown;
import fr.putnami.pwt.core.widget.client.util.StyleUtils;

public class NavDropdown extends SimpleDropdown implements Nav.IsNavContent, ValueChangeHandler<String> {

	private boolean active = false;

	public NavDropdown() {
		super(LIElement.TAG);
		initHandler();
	}

	public NavDropdown(String label) {
		super(LIElement.TAG, label);
		initHandler();
	}

	private void initHandler() {
		History.addValueChangeHandler(this);
	}

	@Override
	public void setActive(boolean active) {
		this.active = active;
		StyleUtils.toggleStyle(this, LinkStyle.ACTIVE, active);
		if (active && getParent() != null && getParent().getParent() instanceof Nav.IsNavContent) {
			// if dropdown is in dropdown => parent is container and parent.parent is dropdown
			((Nav.IsNavContent) getParent().getParent()).setActive(true);
		}
	}

	@Override
	public boolean isActive() {
		return active;
	}

	@Override
	public void onValueChange(ValueChangeEvent<String> event) {
		setActive(false);
	};

	@Override
	public void add(IsWidget w) {
		super.add(w); // SimpleDropdown#add(IsWidget)
		if (w instanceof Nav.IsNavContent && ((Nav.IsNavContent) w).isActive()) {
			setActive(true);
		}
	}

}
