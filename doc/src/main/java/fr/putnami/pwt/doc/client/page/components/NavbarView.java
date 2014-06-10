/**
 * This file is part of pwt-doc.
 *
 * pwt-doc is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * pwt-doc is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with pwt-doc.  If not, see <http://www.gnu.org/licenses/>.
 */
package fr.putnami.pwt.doc.client.page.components;

import com.google.gwt.core.client.GWT;
import com.google.gwt.i18n.client.Messages;
import com.google.gwt.safehtml.shared.SafeHtml;
import com.google.gwt.uibinder.client.UiConstructor;
import com.google.gwt.uibinder.client.UiField;
import com.google.gwt.user.client.ui.Composite;
import com.google.gwt.user.client.ui.Widget;

import fr.putnami.pwt.core.widget.client.NavSpy;
import fr.putnami.pwt.core.widget.client.binder.UiBinderLocalized;

public class NavbarView extends Composite {

	interface Binder extends UiBinderLocalized<Widget, NavbarView> {

		Binder BINDER = GWT.create(Binder.class);
	}

	interface ContentMessages extends Messages {
		@DefaultMessage("A convenient navigation component which collapse when screen is to small")
		SafeHtml navbarDescription();

		@DefaultMessage("Set the <code>brand</code> property and add <code>Nav</code> child with <code>NavLink</code> children.")
		SafeHtml navbarBasicUsageDescription();

		@DefaultMessage("Use <code>Navbar.Type.INVERSE</code> to inverse the navbar type")
		SafeHtml navbarTypeDescription();

		@DefaultMessage("Add <code>NavDropdown</code> children to extends the navbar display with dropdowns.")
		SafeHtml navbarDropdownDescription();

		@DefaultMessage("<code>NavLink</code> and <code>NavDropdown</code> can be customize with <code>iconType</code> property like <code>ButtonDropdown</code>.")
		SafeHtml navbarCustomizeDescription();

		@DefaultMessage("Add <code>Button</code> or <code>ButtonGroup</code> children.")
		SafeHtml navbarButtonDescription();

		@DefaultMessage("Use <code>left</code> and <code>right</code> children to pull left or right Navbar content.")
		SafeHtml navbarAlignmentDescription();

	}

	@UiField(provided = true)
	final NavSpy navSpy;

	@UiConstructor
	public NavbarView(NavSpy navSpy) {
		this.navSpy = navSpy;
		initWidget(Binder.BINDER.createAndBindUi(this));
	}

}
