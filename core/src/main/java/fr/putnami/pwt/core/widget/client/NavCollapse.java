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
import com.google.gwt.place.shared.PlaceChangeEvent;
import com.google.gwt.user.client.ui.IsWidget;

import fr.putnami.pwt.core.event.client.EventBus;
import fr.putnami.pwt.core.widget.client.Nav.LinkStyle;
import fr.putnami.pwt.core.widget.client.base.SimpleCollapse;
import fr.putnami.pwt.core.widget.client.util.StyleUtils;

public class NavCollapse extends SimpleCollapse implements Nav.IsNavContent, PlaceChangeEvent.Handler {

	private boolean active = false;

	public NavCollapse() {
		super(LIElement.TAG);
		this.initHandler();
	}

	public NavCollapse(String label) {
		super(LIElement.TAG, label);
		this.initHandler();
	}

	private void initHandler() {
		EventBus.get().addHandler(PlaceChangeEvent.TYPE, this);
	}

	@Override
	public void setActive(boolean active) {
		this.active = active;
		StyleUtils.toggleStyle(this, LinkStyle.ACTIVE, active);
		if (active) {
			this.open();
			if (this.getParent() != null && this.getParent().getParent() instanceof Nav.IsNavContent) {
				// if dropdown is in dropdown => parent is container and parent.parent is dropdown
				((Nav.IsNavContent) this.getParent().getParent()).setActive(true);
			}
		}
	}

	@Override
	public boolean isActive() {
		return this.active;
	}

	@Override
	public void onPlaceChange(PlaceChangeEvent event) {
		this.setActive(false);
	}

	@Override
	public void add(IsWidget w) {
		super.add(w); // SimpleDropdown#add(IsWidget)
		if (w instanceof Nav.IsNavContent && ((Nav.IsNavContent) w).isActive()) {
			this.setActive(true);
		}
	}

}
