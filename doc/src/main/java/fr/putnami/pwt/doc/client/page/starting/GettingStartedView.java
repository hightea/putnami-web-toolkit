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
package fr.putnami.pwt.doc.client.page.starting;

import com.google.gwt.core.client.GWT;
import com.google.gwt.user.client.ui.Widget;

import fr.putnami.pwt.core.widget.client.binder.UiBinderLocalized;
import fr.putnami.pwt.doc.client.application.Page;

public class GettingStartedView extends Page<GettingStartedPlace> {

	interface Binder extends UiBinderLocalized<Widget, GettingStartedView> {
	}

	@Override
	protected UiBinderLocalized getBinder() {
		return GWT.create(Binder.class);
	}

}
