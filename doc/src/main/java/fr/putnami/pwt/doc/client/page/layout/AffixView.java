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
package fr.putnami.pwt.doc.client.page.layout;

import com.google.gwt.core.client.GWT;
import com.google.gwt.uibinder.client.UiConstructor;
import com.google.gwt.uibinder.client.UiField;
import com.google.gwt.user.client.ui.Composite;
import com.google.gwt.user.client.ui.Widget;

import fr.putnami.pwt.core.widget.client.NavSpy;
import fr.putnami.pwt.core.widget.client.binder.UiBinderLocalized;

public class AffixView extends Composite {

	interface Binder extends UiBinderLocalized<Widget, AffixView> {

		Binder BINDER = GWT.create(Binder.class);
	}

	@UiField(provided = true)
	final NavSpy navSpy;

	@UiConstructor
	public AffixView(NavSpy navSpy) {
		this.navSpy = navSpy;
		initWidget(Binder.BINDER.createAndBindUi(this));
	}

}
